;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/ESCAPE; Base: 10 -*-
;;;; HTML/attribute escape + URL scheme guard.
;;;;
;;;; Depends only on iterate. HTML escape is inlined as a five-character
;;;; substitution rather than delegating to cl-who:escape-string, so this
;;;; sub-system does not drag in cl-who.

(in-package :lol-web/escape)

(defun escape-html (string)
  "Escape the five HTML/XML metacharacters (& < > \" ') to their entity
   equivalents. Returns NIL when STRING is NIL.

   Example:
     (escape-html \"<script>alert('xss')</script>\")
     => \"&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;\""
  (when string
    (with-output-to-string (out)
      (iter (for char in-string string)
            (case char
              (#\& (write-string "&amp;" out))
              (#\< (write-string "&lt;" out))
              (#\> (write-string "&gt;" out))
              (#\" (write-string "&quot;" out))
              (#\' (write-string "&#39;" out))
              (t   (write-char char out)))))))

(defun escape-attribute (string)
  "Escape STRING for use inside an HTML attribute value. Same five-character
   set as `escape-html` — single- and double-quoted attribute contexts are
   both covered."
  (when string
    (with-output-to-string (out)
      (iter (for char in-string string)
            (case char
              (#\" (write-string "&quot;" out))
              (#\' (write-string "&#39;" out))
              (#\< (write-string "&lt;" out))
              (#\> (write-string "&gt;" out))
              (#\& (write-string "&amp;" out))
              (t   (write-char char out)))))))

(defun %strip-c0-and-space (string)
  "Drop every code point in [#x00, #x20] from STRING. Mirrors the HTML5
   URL parser's pre-scheme whitespace handling: the parser strips C0
   controls and ASCII whitespace from anywhere in the URL before scheme
   resolution, so a verifier must collapse the same set or `jav\\tascript:`
   slips past a naive regex."
  (with-output-to-string (out)
    (iter (for char in-string string)
          (when (> (char-code char) #x20)
            (write-char char out)))))

(defun %scheme-of (string)
  "Lowercase scheme portion of STRING (the prefix before the first colon),
   or NIL when STRING has no colon — i.e. is relative or fragment-only."
  (let ((colon (position #\: string)))
    (when colon
      (string-downcase (subseq string 0 colon)))))

(defparameter *script-bearing-schemes*
  '("javascript" "data" "vbscript")
  "Schemes that `safe-url` rejects. Reject-list, not allow-list — see
   `safe-url-allowlist` for the allow-list variant.")

(defparameter *default-url-allowed-schemes*
  '("http" "https" "mailto" "tel")
  "Default scheme allow-list for `safe-url-allowlist`. Replaceable per
   call via the :allowed-schemes keyword argument.")

(defun safe-url (url)
  "Reject javascript:/data:/vbscript: schemes that can carry executable code.
   Returns NIL for unsafe URLs, the ORIGINAL (un-escaped) URL otherwise.

   Strips C0 controls and ASCII whitespace from anywhere in URL before
   scheme matching — the HTML5 URL parser does the same when resolving the
   scheme, so embedded `\\t \\n \\r \\f` inside what looks like a safe
   scheme would otherwise bypass a naive regex.

   VERDICT, NOT escape-for-emit: the survivor is returned verbatim, NOT
   attribute-escaped. A scheme-safe URL can still carry attribute-breaking
   characters (e.g. `https://ok/\"><script>`); splicing this return value
   straight into an `href`/`src` is an XSS footgun. To emit into an
   attribute, use SAFE-HREF (scheme allow-list THEN escape-attribute), or
   pass this result through ESCAPE-ATTRIBUTE yourself.

   Example:
     (safe-url \"javascript:alert(1)\") => NIL
     (safe-url \"jav\\tascript:alert(1)\") => NIL
     (safe-url \"https://example.com\") => \"https://example.com\""
  (when url
    (let* ((stripped (%strip-c0-and-space url))
           (scheme   (%scheme-of stripped)))
      (cond
        ((null scheme) url)
        ((member scheme *script-bearing-schemes* :test #'string=) nil)
        (t url)))))

(defun safe-url-allowlist (url &key (allowed-schemes *default-url-allowed-schemes*))
  "Allow-list URL guard: return the ORIGINAL (un-escaped) URL when its
   scheme is in ALLOWED-SCHEMES, else NIL. Relative paths and fragment-only
   URLs (no scheme) always pass.

   Strips C0 controls and ASCII whitespace before scheme matching so that
   `jav\\tascript:` and friends cannot smuggle a disallowed scheme past
   the matcher.

   VERDICT, NOT escape-for-emit: same contract as SAFE-URL — the survivor
   is returned verbatim and is NOT attribute-escaped. Emit through
   SAFE-HREF (which composes this allow-list with ESCAPE-ATTRIBUTE) rather
   than splicing the return value straight into an `href`/`src`.

   Example:
     (safe-url-allowlist \"https://example.com\") => \"https://example.com\"
     (safe-url-allowlist \"ftp://example.com\") => NIL
     (safe-url-allowlist \"ftp://example.com\" :allowed-schemes '(\"ftp\"))
       => \"ftp://example.com\""
  (when url
    (let* ((stripped (%strip-c0-and-space url))
           (scheme   (%scheme-of stripped)))
      (cond
        ((null scheme) url)
        ((member scheme allowed-schemes :test #'string-equal) url)
        (t nil)))))

(defun safe-attr (value)
  "Render VALUE safe to splice into an HTML attribute value: coerce to its
   printed representation, then ESCAPE-ATTRIBUTE so embedded quotes and angle
   brackets cannot close the attribute. Returns NIL for NIL input (cl-who
   emits nothing for a NIL attribute value). Use at every cl-who attribute
   sink that interpolates a runtime value — cl-who does NOT auto-escape
   runtime attribute values."
  (when value
    (escape-attribute (if (stringp value) value (princ-to-string value)))))

(defun safe-href (url &key (allowed-schemes *default-url-allowed-schemes*))
  "Render URL safe to splice into an href/src-style attribute. Reject a
   disallowed or script-bearing scheme via SAFE-URL-ALLOWLIST, then
   ESCAPE-ATTRIBUTE the survivor so attribute-breaking characters in an
   otherwise-allowed URL (e.g. `https://evil/\"><script>`) cannot close the
   attribute. Returns NIL when the scheme is rejected — cl-who then emits no
   attribute, the fail-closed choice for a URL sink."
  (let ((safe (safe-url-allowlist url :allowed-schemes allowed-schemes)))
    (when safe
      (escape-attribute safe))))

(defun neutralize-script-close (string)
  "Render VALUE safe to splice into a <script> element body. The HTML
   tokenizer ends script data at a literal `</`, so a value carrying
   `</script>` closes the element early and lets the following markup
   execute — JS-quoting (e.g. parenscript) does NOT prevent this, because
   the close tag is recognized by the HTML parser before the JS ever runs.
   Breaks every `</` into `<\\/`: the HTML tokenizer no longer treats it as
   a close tag, and the JS string value is unchanged. Returns NIL for NIL.
   This is escape-for-emit, not a verdict — always emit the return value."
  (when string
    (let ((s (if (stringp string) string (princ-to-string string))))
      (with-output-to-string (out)
        (loop for i below (length s)
              for char = (char s i)
              do (if (and (char= char #\<)
                          (< (1+ i) (length s))
                          (char= (char s (1+ i)) #\/))
                     (write-string "<\\" out)
                     (write-char char out)))))))

(defparameter *scan-match-timeout-seconds* 0.1
  "Wallclock cap on a single CL-PPCRE:SCAN over attacker-controlled input.
   Compile-time caps bound scanner construction; this bounds match time so a
   catastrophic-backtracking pattern (e.g. ^(a+)+$) cannot hang a worker on a
   crafted value. Legitimate matches complete in microseconds, so the timer
   never fires on the happy path.")

(defun %scan-bounded (regex target &key (timeout *scan-match-timeout-seconds*))
  "CL-PPCRE:SCAN with a wallclock match-time bound. REGEX is a pattern string
   or a compiled scanner; TARGET is the (attacker-controlled) string to match.
   Returns whatever CL-PPCRE:SCAN returns, or NIL when the match is aborted at
   TIMEOUT seconds. Every caller tests the result as a boolean, so an aborted
   (suspected-ReDoS) match reads as `no match` — fail-closed for the validators
   that call it: the value is rejected, or the pattern is simply not applied.
   On non-SBCL there is no interruptible match timer, so the scan is NOT run
   — it fails closed (returns NIL) rather than risk an unbounded match a
   catastrophic-backtracking pattern could hang a worker on."
  #+sbcl
  (handler-case
      (sb-ext:with-timeout timeout
        (cl-ppcre:scan regex target))
    (sb-ext:timeout () nil))
  #-sbcl
  nil)

(defparameter *hx-on-attribute-prefix* "hx-on"
  "Canonical literal prefix of every htmx inline-handler attribute. The client
   runtime (processHxOn) lifts any attribute whose name starts with this prefix
   into a native/JS handler, so the server-side strip and the single-name
   predicate both key on it. One source of truth keeps the sanitizer and the
   runtime from drifting — the colon-form gap (hx-on:click surviving a dash-only
   strip) was exactly such a drift.")

(defun hx-on-attribute-name-p (name)
  "T when NAME is an htmx inline-handler attribute the client runtime lifts to a
   handler — i.e. NAME starts with *HX-ON-ATTRIBUTE-PREFIX* under any separator
   (hx-on-click, hx-on:click, hx-on::after-request, hx-on--after-swap)."
  (let ((p *hx-on-attribute-prefix*))
    (and (stringp name)
         (>= (length name) (length p))
         (string-equal p name :end2 (length p)))))

(defun %html-whitespace-p (char)
  "T for the characters HTML5 treats as whitespace in an attribute list."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %strip-hx-on-from-tag (html start end out)
  "Walk one start tag in HTML beginning at START (an opening `<` followed by a
   name character), writing it to OUT with every HX-ON-ATTRIBUTE-NAME-P
   attribute excised. Models the HTML5 tag tokenizer's attribute states,
   including the boundary recovery that begins a new attribute name immediately
   after a quoted value — so `id=\"x\"hx-on:click=...` is two attributes, not
   one, and a `>` inside a quoted value does not end the tag. Returns the index
   just past the tag's `>` (or END if the tag is unclosed)."
  (write-char #\< out)
  (let ((i (1+ start)))
    (let ((name-start i))
      (loop while (and (< i end)
                       (let ((c (char html i)))
                         (not (or (%html-whitespace-p c) (char= c #\/) (char= c #\>)))))
            do (incf i))
      (write-string html out :start name-start :end i))
    (loop
      (when (>= i end) (return))
      (let ((ws-start i))
        (loop while (and (< i end) (%html-whitespace-p (char html i))) do (incf i))
        (cond
          ((>= i end) (write-string html out :start ws-start :end i) (return))
          ((char= (char html i) #\>)
           (write-string html out :start ws-start :end i)
           (write-char #\> out) (incf i) (return))
          ((char= (char html i) #\/)
           (write-string html out :start ws-start :end (1+ i)) (incf i))
          (t
           (let ((name-start i))
             (loop while (and (< i end)
                              (let ((c (char html i)))
                                (not (or (%html-whitespace-p c) (char= c #\/)
                                         (char= c #\>) (char= c #\=)))))
                   do (incf i))
             (let ((name (subseq html name-start i))
                   (attr-end i)
                   (j i))
               ;; A value belongs to this attribute only if `=` follows (modulo
               ;; whitespace); otherwise the name was a bare boolean attribute.
               (loop while (and (< j end) (%html-whitespace-p (char html j))) do (incf j))
               (when (and (< j end) (char= (char html j) #\=))
                 (incf j)
                 (loop while (and (< j end) (%html-whitespace-p (char html j))) do (incf j))
                 (setf attr-end
                       (cond
                         ((>= j end) j)
                         ((char= (char html j) #\")
                          (let ((close (position #\" html :start (1+ j) :end end)))
                            (if close (1+ close) end)))
                         ((char= (char html j) #\')
                          (let ((close (position #\' html :start (1+ j) :end end)))
                            (if close (1+ close) end)))
                         (t (let ((k j))
                              (loop while (and (< k end)
                                               (let ((c (char html k)))
                                                 (not (or (%html-whitespace-p c)
                                                          (char= c #\>)))))
                                    do (incf k))
                              k)))))
               (unless (hx-on-attribute-name-p name)
                 (write-string html out :start ws-start :end attr-end))
               (setf i attr-end)))))))
    i))

(defun %html-comment-end (html start end)
  "Index just past the close of the HTML5 comment opening at START, where
   (string= \"<!--\" HTML :start1 START :end1 (+ START 4)) holds and END bounds
   the scan. Models the comment-closing automaton — the abrupt closes <!--> and
   <!---> (comment-start / comment-start-dash + `>'), the normal --> close, and
   the comment-end-bang --!> close — and returns END for an unterminated
   comment (a browser also runs such a comment to EOF). The returned index is
   exclusive: live content resumes there.

   Closes at the EARLIEST point a browser would and never later, so a handler
   trailing a comment the walker would otherwise mis-read as open cannot slip
   past the scrub. The omitted comment-less-than-sign states only converge back
   to comment-end, never an earlier close, so they cannot cause an under-close."
  (let ((p (+ start 4))
        (state :start))
    (loop
      (when (>= p end) (return end))
      (let ((c (char html p)))
        (ecase state
          (:start
           (cond ((char= c #\>) (return (1+ p)))
                 ((char= c #\-) (incf p) (setf state :start-dash))
                 (t (setf state :comment))))
          (:start-dash
           (cond ((char= c #\>) (return (1+ p)))
                 ((char= c #\-) (incf p) (setf state :end))
                 (t (setf state :comment))))
          (:comment
           (incf p)
           (when (char= c #\-) (setf state :end-dash)))
          (:end-dash
           (incf p)
           (setf state (if (char= c #\-) :end :comment)))
          (:end
           (cond ((char= c #\>) (return (1+ p)))
                 ((char= c #\!) (incf p) (setf state :end-bang))
                 ((char= c #\-) (incf p))
                 (t (setf state :comment))))
          (:end-bang
           (cond ((char= c #\>) (return (1+ p)))
                 ((char= c #\-) (incf p) (setf state :end-dash))
                 (t (setf state :comment)))))))))

(defun sanitize-hx-on-attrs (html)
  "Return HTML with every htmx inline-handler attribute removed — any attribute
   whose name HX-ON-ATTRIBUTE-NAME-P accepts, in both the dash form
   (hx-on-click) and the colon form (hx-on:click) the client runtime lifts.

   The strip is a parse-based walk of the markup, not a regex over serialized
   text: it tokenizes each start tag's attributes the way an HTML5 parser does,
   so a handler is removed regardless of the character preceding it (leading
   whitespace, or a quote closing the previous value as in `id=\"x\"hx-on:...`)
   and regardless of how its value is quoted. Text content, comments, end tags,
   and declarations are left intact, so `<p>hx-on-click means ...</p>` keeps its
   prose.

   Used at the broadcast-OOB boundary: a `safe-html-string` from the producer
   is unwrapped, scrubbed of hx-on handlers, and emitted on the wire.
   Server-rendered initial HTML keeps hx-on handlers legitimate; amplified-XSS
   via wire-arrived content does not."
  (when (stringp html)
    (let ((n (length html)))
      (with-output-to-string (out)
        (let ((i 0))
          (loop while (< i n) do
            (let ((c (char html i)))
              (if (and (char= c #\<) (< (1+ i) n))
                  (let ((next (char html (1+ i))))
                    (cond
                      ((and (<= (+ i 4) n)
                            (string= html "<!--" :start1 i :end1 (+ i 4)))
                       (let ((stop (%html-comment-end html i n)))
                         (write-string html out :start i :end stop)
                         (setf i stop)))
                      ((or (char= next #\!) (char= next #\/))
                       (let* ((gt (position #\> html :start i))
                              (stop (if gt (1+ gt) n)))
                         (write-string html out :start i :end stop)
                         (setf i stop)))
                      ((alpha-char-p next)
                       (setf i (%strip-hx-on-from-tag html i n out)))
                      (t (write-char c out) (incf i))))
                  (progn (write-char c out) (incf i))))))))))

(defun safe-coerce-keyword (string &key allowed (max-length 64) on-miss)
  "Resolve STRING to an existing keyword via FIND-SYMBOL — never INTERN.
   Returns NIL for non-string input, empty string, or length > MAX-LENGTH
   (default 64). When ALLOWED is a list of keywords the resolved keyword
   must be a member; otherwise treated as a miss. ON-MISS :SIGNAL turns
   a miss into an error.

   Example:
     (safe-coerce-keyword \"complete\" :allowed '(:next :back :complete))
       => :COMPLETE
     (safe-coerce-keyword \"sudo\"     :allowed '(:next :back))    => NIL"
  (let ((resolved
          (when (and (stringp string)
                     (plusp (length string))
                     (<= (length string) max-length))
            (let ((upcased (string-upcase string)))
              (when (<= (length upcased) max-length)
                (find-symbol upcased :keyword))))))
    (cond
      ((null resolved)
       (when (eq on-miss :signal)
         (error "safe-coerce-keyword: input ~S does not name an interned keyword (or input invalid)."
                string))
       nil)
      ((and allowed (not (member resolved allowed :test #'eq)))
       (when (eq on-miss :signal)
         (error "safe-coerce-keyword: keyword ~S not in allowed set ~S."
                resolved allowed))
       nil)
      (t resolved))))
