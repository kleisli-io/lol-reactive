;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/FULLSTACK; Base: 10 -*-
;;;; HMAC-signed hydration envelope used by component-API endpoints whose
;;;; bodies the client must not forge (notably :set-state). The envelope
;;;; shape is ((:payload . <canonical-json-string>) (:tag . <hex-string>));
;;;; payload bytes ride the wire verbatim, so verification can recompute
;;;; the HMAC over the same string without re-canonicalising.

(in-package :lol-web/fullstack)

(defparameter +hydration-domain+ "lolweb-hydration"
  "Domain tag pinned into the HMAC input so a hydration tag cannot collide
   with a token mint under the same key.")

(defparameter +hydration-version+ "v1"
  "Format version pinned into the HMAC input alongside the domain tag.")

(defun %hydration-signing-string (payload-string)
  "Build the HMAC signing input. The #\\Soh separator (ASCII 0x01) cannot
   appear in the JSON payload (jzon emits control codes escaped), so the
   field framing has no collision class."
  (format nil "~A~C~A~C~A"
          +hydration-domain+
          (code-char 1) +hydration-version+
          (code-char 1) payload-string))

(defun sign-hydration-state (payload secret-key)
  "Serialise PAYLOAD to canonical JSON, then return a signed envelope
   ((:payload . <json-string>) (:tag . <hex-string>)). SECRET-KEY is an
   octet vector; signals when NIL so a misconfigured app fails at sign
   time rather than producing tokens nobody can verify."
  (assert secret-key (secret-key)
          "SECRET-KEY required for SIGN-HYDRATION-STATE; configure ~
           :HYDRATION-SECRET-KEY on MAKE-APP.")
  (let* ((payload-str (lol-web/server:encode-json-string payload))
         (tag (lol-web/crypto:hmac-sha256-hex
               secret-key
               (%hydration-signing-string payload-str))))
    (list (cons :payload payload-str)
          (cons :tag tag))))

(defun verify-hydration-state (signed secret-key)
  "Verify SIGNED, an envelope produced by SIGN-HYDRATION-STATE or its
   JSON-decoded equivalent. Returns (VALUES PAYLOAD STATUS):
     :OK          — tag verified; PAYLOAD is the decoded payload value.
     :NO-KEY      — SECRET-KEY is NIL; envelope not inspected.
     :MISSING-TAG — envelope lacks :PAYLOAD or :TAG, or has the wrong shape.
     :BAD-TAG     — tag mismatch; payload not returned."
  (cond
    ((null secret-key) (values nil :no-key))
    ((not (consp signed)) (values nil :missing-tag))
    (t
     (let ((payload-str (cdr (assoc :payload signed)))
           (tag-hex (cdr (assoc :tag signed))))
       (cond
         ((not (and (stringp payload-str) (stringp tag-hex)))
          (values nil :missing-tag))
         (t
          (let ((expected (lol-web/crypto:hmac-sha256-hex
                           secret-key
                           (%hydration-signing-string payload-str))))
            (if (lol-web/server:constant-time-string= tag-hex expected)
                (values (lol-web/server:decode-json-string payload-str) :ok)
                (values nil :bad-tag)))))))))

(defun %request-hydration-key ()
  "Hydration key for the current request, injected by the make-app
   middleware. NIL when no key is configured — callers fail closed."
  (getf lol-web/server:*env* :lol-web.fullstack.hydration-key))
