;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTMX; Base: 10 -*-
;;;; Out-of-Band (OOB) response helpers for HTMX-style partial updates
;;;;
;;;; Server-side utilities for generating hx-swap-oob responses.

(in-package :lol-web/htmx)

;;; ============================================================================
;;; OOB SELECTOR ALLOWLIST + SIGNED-TOKEN ESCAPE HATCH
;;; ============================================================================
;;;
;;; HTMX OOB swaps direct DOM mutation by CSS selector. An unconstrained
;;; selector (`body`, `head`, `html`, top-level form) is a privileged
;;; surface — replacing it lets a compromised channel rewrite the entire
;;; document. The default refuses those selectors; consumers opt content
;;; in via the prefix allowlist or by signing the selector with the
;;; server's secret (mint-oob-selector-token / verify-oob-selector-token).
;;;
;;; The allowlist is a list of (kind value) entries:
;;;   (:id       "comp-123")   — exact element ID match (rendered as #ID)
;;;   (:id-prefix "comp-")     — ID starts with VALUE
;;;   (:class    "card")       — exact class match (rendered as .CLASS)
;;;   (:literal  "#main .row") — verbatim selector
;;; Empty list with no token denies every selector outside the default
;;; deny set (which is a hard refusal regardless of allowlist contents).

(defparameter *oob-selector-denylist*
  '("body" "head" "html" "form")
  "Selectors refused before the allowlist runs — replacing these is a
   privileged operation that the server must never expose to an OOB
   channel. Matched as the trimmed selector string in lower case.")

(defparameter *oob-selector-allowlist* nil
  "List of (:KIND VALUE) entries describing OOB selectors the server
   accepts beyond the denylist. NIL leaves the default permissive surface
   (every selector outside *oob-selector-denylist* is allowed); setting a
   non-NIL list tightens to that list plus any signed-token escape hatch.")

(defparameter *oob-signed-selector-secret* nil
  "Octet vector (>= *token-min-secret-key-bytes*) used to verify selectors
   submitted via signed token. NIL disables the signed-token path; only
   allowlist matches are honoured. Configure at app boot.")

(define-condition unsafe-oob-selector (error)
  ((selector :initarg :selector :reader unsafe-oob-selector-selector)
   (reason   :initarg :reason   :reader unsafe-oob-selector-reason))
  (:report
   (lambda (c s)
     (format s "unsafe-oob-selector: ~S refused (~A)"
             (unsafe-oob-selector-selector c)
             (unsafe-oob-selector-reason c)))))

(defun %oob-trimmed-lower (selector)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return) selector)))

(defun %oob-default-denied-p (selector)
  "True iff SELECTOR matches one of the *oob-selector-denylist* tags.
   The match is case-insensitive on the trimmed selector, and treats
   `form`, `body`, `head`, `html` as both the bare tag and the tag with
   a leading combinator-free space (`* > body`-style escapes still go
   through the allowlist explicitly)."
  (let ((trimmed (%oob-trimmed-lower selector)))
    (some (lambda (denied)
            (or (string= trimmed denied)
                ;; Refuse top-level form selectors regardless of attribute
                ;; predicates: `form[data-foo]` rewrites the form root.
                (and (string= denied "form")
                     (>= (length trimmed) 4)
                     (string= trimmed "form" :end1 4)
                     (or (= (length trimmed) 4)
                         (let ((c (char trimmed 4)))
                           (member c '(#\[ #\. #\#)))))))
          *oob-selector-denylist*)))

(defun %oob-allowlist-match-p (selector)
  "True iff SELECTOR satisfies one of *oob-selector-allowlist*'s entries."
  (some (lambda (entry)
          (destructuring-bind (kind value) entry
            (ecase kind
              (:id       (string= selector (concatenate 'string "#" value)))
              (:id-prefix
               (let ((prefix (concatenate 'string "#" value)))
                 (and (> (length selector) (length prefix))
                      (string= prefix selector :end2 (length prefix)))))
              (:class    (string= selector (concatenate 'string "." value)))
              (:literal  (string= selector value)))))
        *oob-selector-allowlist*))

(defun %oob-signed-selector-p (selector signed-token)
  "True iff SIGNED-TOKEN verifies under *oob-signed-selector-secret* and
   carries SELECTOR verbatim in its payload."
  (and signed-token
       *oob-signed-selector-secret*
       (multiple-value-bind (valid kind payload)
           (lol-web/crypto:verify-token *oob-signed-selector-secret* signed-token)
         (declare (ignore kind))
         (and valid
              payload
              (string= selector (babel:octets-to-string payload :encoding :utf-8))))))

(defun validate-oob-selector (selector &key signed-token)
  "Return SELECTOR on success; signal UNSAFE-OOB-SELECTOR otherwise.

   Order of resolution:
     1. Non-string / empty selectors always fail (:non-string / :empty).
     2. *oob-selector-denylist* matches fail (:default-deny). The deny
        set short-circuits even when the allowlist or a signed token
        would re-admit, so `body` cannot be silently re-admitted by a
        forgiving token payload.
     3. A signed-token whose payload string-equals SELECTOR succeeds.
     4. With *oob-selector-allowlist* non-NIL, an allowlist match
        succeeds; otherwise :not-allowlisted fails.
     5. With *oob-selector-allowlist* NIL, the call succeeds — the
        denylist is the only gate."
  (cond
    ((not (stringp selector))
     (error 'unsafe-oob-selector :selector selector :reason :non-string))
    ((zerop (length selector))
     (error 'unsafe-oob-selector :selector selector :reason :empty))
    ((%oob-default-denied-p selector)
     (error 'unsafe-oob-selector :selector selector :reason :default-deny))
    ((%oob-signed-selector-p selector signed-token)
     selector)
    ((null *oob-selector-allowlist*)
     selector)
    ((%oob-allowlist-match-p selector)
     selector)
    (t
     (error 'unsafe-oob-selector :selector selector :reason :not-allowlisted))))

(defun mint-oob-selector-token (selector &key (ttl-seconds 3600))
  "Mint a signed token that re-admits SELECTOR through the allowlist gate
   for TTL-SECONDS. Requires *oob-signed-selector-secret* to be set; signals
   when the secret is absent so a deploy that forgets to key the secret
   does not silently issue useless tokens."
  (unless *oob-signed-selector-secret*
    (error "mint-oob-selector-token: *oob-signed-selector-secret* is unset."))
  (lol-web/crypto:mint-token *oob-signed-selector-secret*
                             :payload selector
                             :ttl-seconds ttl-seconds))

;;; ============================================================================
;;; OOB RESPONSE HELPERS
;;; ============================================================================

(defun find-tag-end (html)
  "Find the position of > that ends the opening tag, skipping > inside
   single- or double-quoted attribute values. Returns the position of the
   closing > or NIL if not found.

   QUOTE-CHAR tracks the active quote delimiter so the matching closer ends
   the run and the opposite quote is treated as a literal inside it (a `'`
   inside \"...\" and a `\"` inside '...' do not toggle)."
  (let ((quote-char nil)
        (len (length html)))
    (loop for i from 0 below len
          for char = (char html i)
          do (cond
               (quote-char
                (when (char= char quote-char)
                  (setf quote-char nil)))
               ((or (char= char #\") (char= char #\'))
                (setf quote-char char))
               ((char= char #\>)
                (return i)))
          finally (return nil))))

(defun content-starts-with-id-p (html target-id)
  "Check if HTML starts with an element that has the specified ID.
   Returns T if the first element's opening tag contains id=\"TARGET-ID\".
   Correctly handles > characters inside quoted attribute values."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) html)))
    (when (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\<))
      ;; Find end of opening tag (skip > inside quotes)
      (let ((tag-end (find-tag-end trimmed)))
        (when tag-end
          ;; Look for id="target-id" within the opening tag
          (let ((tag-content (subseq trimmed 0 tag-end))
                (id-pattern (format nil "id=\"~a\"" target-id)))
            (search id-pattern tag-content :test #'char-equal)))))))

(defun inject-oob-attribute (html swap-value)
  "Inject hx-swap-oob attribute into the first element's opening tag.
   Handles both regular tags and self-closing tags (e.g., <input />).
   Correctly handles > characters inside quoted attribute values.
   Returns the modified HTML string."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) html)))
    (let ((first-gt (find-tag-end trimmed)))
      (if first-gt
          ;; Check for self-closing tag: look for / before >
          (let* ((before-gt (subseq trimmed 0 first-gt))
                 (slash-pos (position #\/ before-gt :from-end t))
                 ;; Is it a self-closing tag? (/ appears near end, only whitespace between / and >)
                 (self-closing-p (and slash-pos
                                      (every (lambda (c) (member c '(#\Space #\Tab)))
                                             (subseq before-gt (1+ slash-pos)))))
                 ;; Insert position: before the / for self-closing, before > otherwise
                 (insert-pos (if self-closing-p slash-pos first-gt)))
            (concatenate 'string
                         (subseq trimmed 0 insert-pos)
                         (format nil " hx-swap-oob=\"~a\""
                                 (lol-web/escape:safe-attr swap-value))
                         (subseq trimmed insert-pos)))
          ;; Fallback if no > found (shouldn't happen with valid HTML)
          html))))

(defun oob-swap (id content &key (swap "true"))
  "Generate an OOB swap element targeting the element with HTML id ID.
   SWAP can be: true (outerHTML), innerHTML, beforebegin, afterbegin, etc.

   ID is validated against *oob-selector-allowlist* as the selector
   `#ID`; an unallowlisted ID signals UNSAFE-OOB-SELECTOR. Use
   make-oob-swap when the selector is something other than a plain ID
   (class, attribute, signed escape hatch).

   CONTENT must be a SAFE-HTML-STRING — the producer's assertion that
   the markup is safe to emit verbatim. A bare string signals at the
   constructor boundary rather than allowing an unescaped payload to be
   embedded in the response.

   Smart behavior for outerHTML swaps: if content already contains an element
   with the target ID, injects hx-swap-oob attribute directly instead of
   wrapping (which would create duplicate IDs)."
  (check-type content lol-web/html:safe-html-string)
  (validate-oob-selector (concatenate 'string "#" id))
  (let* ((raw (lol-web/html:safe-html-string-value content))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (if (and (string= swap "outerHTML")
             (content-starts-with-id-p trimmed id))
        (inject-oob-attribute trimmed swap)
        (cl-who:with-html-output-to-string (s)
          (:div :id (lol-web/escape:safe-attr id)
                :hx-swap-oob (lol-web/escape:safe-attr swap)
                (cl-who:str raw))))))

(defun make-oob-swap (selector content &key (swap "true") signed-token)
  "Generate an OOB swap element for an arbitrary SELECTOR. SELECTOR is
   gated by *oob-selector-allowlist* (or by a SIGNED-TOKEN minted via
   mint-oob-selector-token); selectors in the default deny set
   (*oob-selector-denylist*) are refused regardless of allowlist or token.

   CONTENT must be a SAFE-HTML-STRING; see `oob-swap' for rationale.

   The emitted element uses the synthetic `hx-swap-oob=\"<SWAP>:<SELECTOR>\"`
   form so HTMX targets the selector instead of the wrapper's id."
  (check-type content lol-web/html:safe-html-string)
  (validate-oob-selector selector :signed-token signed-token)
  (let ((wire-swap (format nil "~A:~A" swap selector))
        (raw (lol-web/html:safe-html-string-value content)))
    (cl-who:with-html-output-to-string (s)
      (:span :style "display:none"
             :hx-swap-oob (lol-web/escape:safe-attr wire-swap)
             (cl-who:str raw)))))

(defmacro with-oob-swaps ((&rest swaps) &body body)
  "Execute BODY and append OOB swap elements.
   SWAPS is a list of (id content &key swap) specifications; each
   CONTENT must evaluate to a SAFE-HTML-STRING."
  `(concatenate 'string
                (progn ,@body)
                ,@(mapcar (lambda (swap-spec)
                            `(oob-swap ,@swap-spec))
                          swaps)))

(defun oob-content (id content)
  "Generate an OOB innerHTML swap that preserves target element attributes.

   Unlike oob-swap which replaces the entire element (including class, hx-*, etc),
   this only replaces the innerHTML of the target element, preserving all attributes.

   CONTENT must be a SAFE-HTML-STRING; see `oob-swap' for rationale.

   Use this when the target element has attributes you want to keep, such as:
   - CSS classes for styling
   - hx-trigger for polling
   - data-* attributes

   Example:
     ;; Target: <div id=\"counter\" class=\"big\" hx-trigger=\"every 1s\">0</div>
     (oob-content \"counter\" (make-safe-html-string \"42\"))
     ;; Result: <span style=\"display:none\" hx-swap-oob=\"innerHTML:#counter\">42</span>
     ;; Target becomes: <div id=\"counter\" class=\"big\" hx-trigger=\"every 1s\">42</div>"
  (check-type content lol-web/html:safe-html-string)
  (validate-oob-selector (concatenate 'string "#" id))
  (let ((raw (lol-web/html:safe-html-string-value content)))
    (htm-str
      (:span :style "display:none"
             :hx-swap-oob (lol-web/escape:safe-attr (format nil "innerHTML:#~A" id))
        (cl-who:str raw)))))
