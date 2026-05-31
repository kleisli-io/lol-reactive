(in-package :lol-web/crypto/test)
(in-suite :lol-web/crypto/test)

(defun %secret (s)
  "Encode a label into a 32-byte secret-key. The label's ASCII bytes go in
   the prefix; the rest is zero-filled to meet MINT-TOKEN's secret-key
   length floor (HMAC-SHA256 block size). Tests stay deterministic
   per-label while satisfying the floor."
  (let ((base (ironclad:ascii-string-to-byte-array s))
        (out  (make-array 32 :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (replace out base)
    out))

;;; ============================================================================
;;; Round trip
;;; ============================================================================

(test token-round-trip-string-payload
  "mint-token + verify-token preserves a UTF-8 payload."
  (let* ((secret (%secret "k"))
         (tok    (mint-token secret :payload "user:42")))
    (multiple-value-bind (ok nonce payload-bytes)
        (verify-token secret tok)
      (is (eql t ok))
      (is (stringp nonce))
      (is (string= "user:42" (babel:octets-to-string payload-bytes))))))

(test token-round-trip-empty-payload
  "Default payload is empty — round-trip yields a zero-length octet vector."
  (let* ((secret (%secret "k"))
         (tok    (mint-token secret)))
    (multiple-value-bind (ok nonce payload-bytes)
        (verify-token secret tok)
      (is (eql t ok))
      (is (stringp nonce))
      (is (zerop (length payload-bytes))))))

(test token-round-trip-octet-payload
  "An octet-vector payload survives the round trip byte-for-byte."
  (let* ((secret (%secret "k"))
         (payload (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-contents '(#xDE #xAD #xBE #xEF)))
         (tok    (mint-token secret :payload payload)))
    (multiple-value-bind (ok nonce payload-out)
        (verify-token secret tok)
      (declare (ignore nonce))
      (is (eql t ok))
      (is (equalp payload payload-out)))))

;;; ============================================================================
;;; Tamper detection
;;; ============================================================================

(defun %parts (token)
  (loop with s = 0
        with e = (length token)
        for dot = (position #\. token :start s)
        collect (subseq token s (or dot e))
        while dot do (setf s (1+ dot))))

(defun %recombine (parts)
  (format nil "~{~A~^.~}" parts))

(test token-rejects-wrong-secret
  "A token minted under one secret fails verification under another."
  (let* ((tok (mint-token (%secret "a") :payload "p")))
    (multiple-value-bind (ok mode)
        (verify-token (%secret "b") tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test token-rejects-tampered-nonce
  "Replacing the nonce segment with another hex string invalidates the tag."
  (let* ((secret (%secret "k"))
         (tok    (mint-token secret :payload "p"))
         (parts  (%parts tok))
         (bad    (%recombine (list "deadbeefdeadbeefdeadbeefdeadbeef"
                                   (second parts)
                                   (third parts)
                                   (fourth parts)))))
    (multiple-value-bind (ok mode)
        (verify-token secret bad)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test token-rejects-tampered-payload
  "Flipping the payload segment invalidates the tag."
  (let* ((secret (%secret "k"))
         (tok    (mint-token secret :payload "good"))
         (parts  (%parts tok))
         (bad    (%recombine (list (first parts)
                                   (second parts)
                                   "ffff"
                                   (fourth parts)))))
    (multiple-value-bind (ok mode)
        (verify-token secret bad)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test token-rejects-tampered-tag
  "Flipping the tag segment fails (the tag is the integrity check itself)."
  (let* ((secret (%secret "k"))
         (tok    (mint-token secret :payload "p"))
         (parts  (%parts tok))
         (bad    (%recombine (list (first parts)
                                   (second parts)
                                   (third parts)
                                   "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"))))
    (multiple-value-bind (ok mode)
        (verify-token secret bad)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test token-rejects-malformed
  "Strings that do not split into four dot-separated parts return :bad-tag."
  (let ((secret (%secret "k")))
    (is (eql :bad-tag (nth-value 1 (verify-token secret ""))))
    (is (eql :bad-tag (nth-value 1 (verify-token secret "only.three.parts"))))
    (is (eql :bad-tag (nth-value 1 (verify-token secret "one.two.three.four.five"))))
    (is (eql :bad-tag (nth-value 1 (verify-token secret "garbage"))))))

;;; ============================================================================
;;; Expiry
;;; ============================================================================

(defun %domain-signed-token (secret nonce-hex expiry-str payload-hex)
  "Hand-construct a token whose HMAC input matches the v1 signing shape.
   Used by tests that need to fix the nonce/expiry without going through
   MINT-TOKEN's random-nonce + clock paths."
  (let* ((signing-input (format nil "lolweb-tok~C~A~C~A~C~A~C~A"
                                (code-char 1) "v1"
                                (code-char 1) nonce-hex
                                (code-char 1) expiry-str
                                (code-char 1) payload-hex))
         (mac (ironclad:make-hmac secret :sha256)))
    (ironclad:update-hmac mac (babel:string-to-octets signing-input))
    (let ((tag-hex (ironclad:byte-array-to-hex-string (ironclad:hmac-digest mac))))
      (format nil "~A.~A.~A.~A" nonce-hex expiry-str payload-hex tag-hex))))

(test token-rejects-expired-token
  "A correctly-tagged token with a past expiry returns :expired."
  (let* ((secret      (%secret "k"))
         (nonce-hex   (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))
         (past        (write-to-string (- (- (get-universal-time) 2208988800) 60)))
         (payload-hex (ironclad:byte-array-to-hex-string
                       (babel:string-to-octets "p")))
         (tok         (%domain-signed-token secret nonce-hex past payload-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :expired mode)))))

;;; ============================================================================
;;; Nonce uniqueness
;;; ============================================================================

(test token-distinct-nonces-across-mints
  "Two consecutive mints produce different nonces (CSPRNG sanity)."
  (let* ((secret (%secret "k"))
         (n1 (nth-value 1 (verify-token secret (mint-token secret))))
         (n2 (nth-value 1 (verify-token secret (mint-token secret)))))
    (is (not (string= n1 n2)))))

;;; ============================================================================
;;; v1 format gates: secret-key floor, empty-segment rejection, domain prefix
;;; ============================================================================

(test token-rejects-short-secret-key-at-mint
  "MINT-TOKEN signals when SECRET-KEY is shorter than the documented floor.
   A 31-byte key (one below the *TOKEN-MIN-SECRET-KEY-BYTES* threshold)
   must error out instead of producing a weakly-keyed token."
  (signals error
    (mint-token (ironclad:random-data 31) :payload "x")))

(test token-rejects-empty-nonce-segment
  "Wire forms with an empty nonce segment never round-trip through MINT-TOKEN
   — VERIFY-TOKEN must reject them upfront (before any HMAC work) so they
   cannot be mistaken for legitimate tokens that happened to start with a dot."
  (let ((secret (%secret "k")))
    (multiple-value-bind (ok mode)
        (verify-token secret ".1779000000.7061796c6f6164.deadbeef")
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test token-rejects-empty-expiry-segment
  "Wire forms with an empty expiry segment are rejected for the same reason
   as the empty-nonce case — an empty expiry cannot PARSE-INTEGER and even
   if it could, an unbounded-lifetime token is not a legitimate shape."
  (let ((secret (%secret "k")))
    (multiple-value-bind (ok mode)
        (verify-token secret "deadbeefdeadbeefdeadbeefdeadbeef..7061796c6f6164.deadbeef")
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test token-rejects-pre-v1-signing-input
  "Tokens HMAC'd over the legacy dot-separated input (no domain tag, no
   version) fail under the v1 verifier — the cross-protocol-forgery defence."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "deadbeefdeadbeefdeadbeefdeadbeef")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (legacy-input (format nil "~A.~A.~A" nonce-hex future payload-hex))
         (mac          (ironclad:make-hmac secret :sha256)))
    (ironclad:update-hmac mac (babel:string-to-octets legacy-input))
    (let* ((tag-hex (ironclad:byte-array-to-hex-string (ironclad:hmac-digest mac)))
           (legacy  (format nil "~A.~A" legacy-input tag-hex)))
      (multiple-value-bind (ok mode)
          (verify-token secret legacy)
        (is (null ok))
        (is (eql :bad-tag mode))))))

;;; ============================================================================
;;; %coerce-octets datum redaction
;;; ============================================================================

(test crypto-mint-token-redacts-payload-on-type-mismatch
  "Condition reports for unsupported payload types must not echo the
   payload bytes — payloads routinely carry secrets."
  (let ((secret (%secret "k"))
        (secret-payload (list :recovery-token "do-not-leak-this-secret-12345"
                              :user-id 42)))
    (handler-case
        (progn (mint-token secret :payload secret-payload)
               (is nil "MINT-TOKEN must signal on unsupported payload type"))
      (error (c)
        (let ((report (princ-to-string c)))
          (is (null (search "do-not-leak-this-secret-12345" report))
              "condition report leaks original payload:~%~A" report)
          (is (null (search ":RECOVERY-TOKEN" report))
              "condition report leaks payload keys:~%~A" report))))))

;;; ============================================================================
;;; verify-token returns :bad-tag on malformed hex / non-numeric expiry
;;; ============================================================================

(test crypto-verify-token-rejects-non-hex-and-non-numeric-segments
  "Non-hex payload (ironclad signals simple-error) and non-numeric expiry
   (parse-integer signals parse-error) both surface as :bad-tag."
  (let ((secret (%secret "k")))
    (multiple-value-bind (ok mode)
        (verify-token secret "deadbeefdeadbeefdeadbeefdeadbeef.1779000000.zzzzz.deadbeef")
      (is (null ok))
      (is (eql :bad-tag mode)))
    (multiple-value-bind (ok mode)
        (verify-token secret "deadbeefdeadbeefdeadbeefdeadbeef.not-a-number.70.deadbeef")
      (is (null ok))
      (is (eql :bad-tag mode)))))

;;; ============================================================================
;;; Docstring honesty: mint-token must warn that payload is not encrypted
;;; ============================================================================

(test token-mint-docstring-warns-payload-not-encrypted
  "MINT-TOKEN docstring carries the caps-line WARNING that payloads are
   integrity-only, not confidential."
  (let ((doc (documentation 'mint-token 'function)))
    (is (stringp doc))
    (is (search "WARNING" doc))
    (is (search "NOT ENCRYPTED" doc))))

(test token-rejects-wrong-domain-tag
  "A token whose HMAC was computed under a different domain string (e.g.,
   a sibling 'lolweb-sess' token type sharing the same secret) must fail
   verification — the domain tag is the cross-protocol defence."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "feedfacefeedfacefeedfacefeedface")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (wrong-input (format nil "lolweb-sess~C~A~C~A~C~A~C~A"
                              (code-char 1) "v1"
                              (code-char 1) nonce-hex
                              (code-char 1) future
                              (code-char 1) payload-hex))
         (mac         (ironclad:make-hmac secret :sha256)))
    (ironclad:update-hmac mac (babel:string-to-octets wrong-input))
    (let* ((tag-hex (ironclad:byte-array-to-hex-string (ironclad:hmac-digest mac)))
           (tok     (format nil "~A.~A.~A.~A" nonce-hex future payload-hex tag-hex)))
      (multiple-value-bind (ok mode)
          (verify-token secret tok)
        (is (null ok))
        (is (eql :bad-tag mode))))))

;;; ============================================================================
;;; Pre-HMAC charset enforcement on signed segments
;;; ============================================================================

(test crypto-verify-token-rejects-non-hex-nonce
  "Adversary-signed token with non-hex nonce must :bad-tag without running
   HMAC. NONCE-HEX is never hex-decoded (used verbatim in signing input,
   returned to caller as a string), so the charset gate is the only line
   of defence."
  (let* ((secret      (%secret "k"))
         (bad-nonce   "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (tok         (%domain-signed-token secret bad-nonce future payload-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test crypto-verify-token-rejects-soh-in-nonce
  "Framing-collision defence: a nonce segment containing the SOH separator
   byte (ASCII 0x01) breaks the unambiguous-field-boundary claim of the
   HMAC signing input. Verifier rejects before computing the tag."
  (let* ((secret      (%secret "k"))
         (soh-nonce   (concatenate 'string
                                   "aabb"
                                   (string (code-char 1))
                                   "ccdd"))
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (tok         (%domain-signed-token secret soh-nonce future payload-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test crypto-verify-token-rejects-non-digit-expiry
  "Adversary-signed token with non-digit expiry must :bad-tag at the
   charset gate, not by tripping PARSE-INTEGER post-HMAC."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "deadbeefdeadbeefdeadbeefdeadbeef")
         (bad-expiry  "not-a-number")
         (payload-hex "70")
         (tok         (%domain-signed-token secret nonce-hex bad-expiry payload-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test crypto-verify-token-rejects-uppercase-hex-nonce
  "MINT-TOKEN emits lowercase hex; the verifier enforces the same
   normalisation. Uppercase variants must :bad-tag."
  (let* ((secret      (%secret "k"))
         (upper-nonce "DEADBEEFDEADBEEFDEADBEEFDEADBEEF")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (tok         (%domain-signed-token secret upper-nonce future payload-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

;;; ============================================================================
;;; Segment length caps
;;; ============================================================================

(defun %tagless-token (nonce-hex expiry-str payload-hex tag-hex)
  "Build a wire-shaped token from raw segments without re-signing. Used
   to drive the length-gate path independently of HMAC."
  (format nil "~A.~A.~A.~A" nonce-hex expiry-str payload-hex tag-hex))

(test crypto-verify-token-rejects-oversize-nonce
  "Nonce-hex longer than *TOKEN-MAX-NONCE-HEX-LENGTH* is rejected before
   any HMAC compute. The default cap is 256; submit a 257-char nonce."
  (let* ((secret      (%secret "k"))
         (big-nonce   (make-string 257 :initial-element #\a))
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (tag-hex     (make-string 64 :initial-element #\0))
         (tok         (%tagless-token big-nonce future payload-hex tag-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test crypto-verify-token-rejects-oversize-expiry
  "Expiry-str longer than +TOKEN-MAX-EXPIRY-STR-LENGTH+ (20) is rejected
   before charset scan + HMAC compute."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "deadbeefdeadbeefdeadbeefdeadbeef")
         (big-expiry  (make-string 21 :initial-element #\9))
         (payload-hex "70")
         (tag-hex     (make-string 64 :initial-element #\0))
         (tok         (%tagless-token nonce-hex big-expiry payload-hex tag-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test crypto-verify-token-rejects-oversize-payload
  "Payload-hex longer than *TOKEN-MAX-PAYLOAD-HEX-LENGTH* is rejected
   before charset scan + HMAC compute. Default is 65536; submit 65537."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "deadbeefdeadbeefdeadbeefdeadbeef")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (big-payload (make-string 65537 :initial-element #\a))
         (tag-hex     (make-string 64 :initial-element #\0))
         (tok         (%tagless-token nonce-hex future big-payload tag-hex)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))

(test crypto-verify-token-rejects-wrong-tag-length
  "Tag-hex must be exactly +TOKEN-TAG-HEX-LENGTH+ (64) chars — the HMAC-
   SHA256 output width. Any other length is forged-by-shape and rejected
   before CONSTANT-TIME-STRING= sees it. Covers FIND-E-L01."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "deadbeefdeadbeefdeadbeefdeadbeef")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70"))
    (dolist (tag-len '(0 32 63 65 128))
      (let* ((tag-hex (make-string tag-len :initial-element #\0))
             (tok     (%tagless-token nonce-hex future payload-hex tag-hex)))
        (multiple-value-bind (ok mode)
            (verify-token secret tok)
          (is (null ok)
              (format nil "tag-hex length ~D must fail" tag-len))
          (is (eql :bad-tag mode)
              (format nil "tag-hex length ~D must yield :bad-tag" tag-len)))))))

(test crypto-verify-token-rejects-non-hex-tag-charset
  "Tag-hex must be lowercase hex. An uppercase or non-hex tag fails the
   charset gate before CONSTANT-TIME-STRING= and before HMAC compute.
   The 64-char length is held constant here to isolate the charset arm."
  (let* ((secret      (%secret "k"))
         (nonce-hex   "deadbeefdeadbeefdeadbeefdeadbeef")
         (future      (write-to-string (+ (- (get-universal-time) 2208988800) 3600)))
         (payload-hex "70")
         (bad-tag     (concatenate 'string
                                   "Z" (make-string 63 :initial-element #\0)))
         (tok         (%tagless-token nonce-hex future payload-hex bad-tag)))
    (multiple-value-bind (ok mode)
        (verify-token secret tok)
      (is (null ok))
      (is (eql :bad-tag mode)))))
