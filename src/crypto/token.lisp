;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/CRYPTO; Base: 10 -*-
;;;; HMAC-SHA256 signed tokens.
;;;;
;;;; Wire format (hex-only, URL-safe, no encoder dep):
;;;;
;;;;   <nonce-hex>.<expiry-unix>.<payload-hex>.<tag-hex>
;;;;
;;;; The tag is HMAC-SHA256 over a domain-tagged, length-explicit signing
;;;; input built from the three signed fields:
;;;;
;;;;   "lolweb-tok" #\Soh "v1" #\Soh <nonce-hex> #\Soh <expiry> #\Soh <payload-hex>
;;;;
;;;; The literal domain string + format version make the HMAC input unambiguous
;;;; across present and future token types under the same key — a second
;;;; token format ("lolweb-sess", "v2", etc.) cannot collide with a v1 token
;;;; signing input even when the dot-separated wire form is textually similar.
;;;; The #\Soh (ASCII 0x01) field separator never appears in hex, which keeps
;;;; the per-segment boundaries unambiguous to the HMAC.
;;;;
;;;; VERIFY-TOKEN returns (values valid-p nonce-hex payload-octets).
;;;; On failure, the second value is the failure mode keyword (:bad-tag or
;;;; :expired) — handy for caller-side logging. The framework stays
;;;; stateless: nonce-hex is returned on success so consumers can track
;;;; single-use replay themselves.
;;;;
;;;; SECRET-KEY must be at least *TOKEN-MIN-SECRET-KEY-BYTES* bytes. A short
;;;; key is a deployment defect, not an attack vector inside the framework,
;;;; but signalling at mint time keeps the misconfiguration observable
;;;; instead of producing tokens with anaemic integrity.

(in-package :lol-web/crypto)

(defparameter *token-min-secret-key-bytes* 32
  "Minimum SECRET-KEY length for MINT-TOKEN. 32 bytes = 256 bits, the
   block size of HMAC-SHA256 — anything shorter is a deployment defect.")

(defparameter *token-max-nonce-hex-length* 256
  "Cap on nonce-hex segment length in VERIFY-TOKEN. Default mint produces
   32 hex chars (16 bytes); 256 is generous headroom for callers that
   raise NONCE-BYTES, and well below the megabyte-scale segment an
   attacker would need to land a DoS through the HMAC compute path.")

(defparameter *token-max-payload-hex-length* 65536
  "Cap on payload-hex segment length in VERIFY-TOKEN. Applications minting
   tokens with large payloads (capability bundles, encrypted blobs) tune
   this upward; the default 65536 hex chars = 32 KiB of payload material
   is large enough for realistic capability strings while still bounding
   the work an attacker can force through %SIGNING-INPUT-BYTES and the
   subsequent HMAC compute by submitting padding-only forged tokens.")

(defconstant +token-tag-hex-length+ 64
  "HMAC-SHA256 produces 32 bytes; the hex encoding is exactly 64 chars.
   Tokens whose tag segment is any other length are rejected before
   HMAC compute, closing the variable-length-tag wedge.")

(defconstant +token-max-expiry-str-length+ 20
  "Cap on expiry-str segment length. A 64-bit Unix time fits in 19
   digits; 20 covers a leading zero or short positive overflow without
   admitting the megabyte-scale segments an attacker would need for the
   HMAC compute wedge. The framing-collision guard still relies on
   %DECIMAL-SEGMENT-P matching every char.")

(defun %unix-time ()
  "Seconds since the Unix epoch. Common Lisp's GET-UNIVERSAL-TIME counts
   from 1900-01-01; subtract the 70-year offset to get Unix time."
  (- (get-universal-time) 2208988800))

(defun %empty-octets ()
  "A length-0 (UNSIGNED-BYTE 8) vector — IRONCLAD:BYTE-ARRAY-TO-HEX-STRING
   requires the element type, the literal #() reader form does not satisfy it."
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %coerce-octets (x)
  "Return X as an (UNSIGNED-BYTE 8) vector. Accepts strings (UTF-8) or
   already-octet vectors verbatim.

   On a type mismatch, signals a SIMPLE-TYPE-ERROR whose :DATUM is the
   symbol :REDACTED-PAYLOAD rather than the actual value. Payloads passed
   to MINT-TOKEN can carry secrets (recovery tokens, capability identifiers,
   session bindings); letting the offending datum land verbatim in condition
   reports or logs is a confidentiality leak. The redaction trades debugging
   detail for safety — TYPE-OF (in the format arguments) still identifies
   the wrong shape without exposing the bytes."
  (typecase x
    (string (babel:string-to-octets x))
    ((vector (unsigned-byte 8)) x)
    (t (error 'simple-type-error
              :datum :redacted-payload
              :expected-type '(or string (vector (unsigned-byte 8)))
              :format-control
              "%COERCE-OCTETS received unsupported payload type ~S; ~
               datum redacted to avoid leaking secrets (e.g. recovery ~
               tokens) into logs or condition reports."
              :format-arguments (list (type-of x))))))

(defun %hex-segment-p (s)
  "T iff every character of S is a lowercase hex digit (0-9 a-f). Empty
   string vacuously satisfies. Used by VERIFY-TOKEN to enforce the same
   charset that MINT-TOKEN emits via IRONCLAD:BYTE-ARRAY-TO-HEX-STRING,
   so the SOH-separator framing claim — no signed segment contains SOH —
   holds at verify time and not just at mint time."
  (declare (type string s)
           (optimize (speed 3) (safety 1)))
  (loop for c across s
        always (or (char<= #\0 c #\9)
                   (char<= #\a c #\f))))

(defun %decimal-segment-p (s)
  "T iff every character of S is a decimal digit (0-9). Matches the
   WRITE-TO-STRING output of MINT-TOKEN's expiry field."
  (declare (type string s)
           (optimize (speed 3) (safety 1)))
  (loop for c across s
        always (char<= #\0 c #\9)))

(defun %signing-input-bytes (nonce-hex expiry-str payload-hex)
  "Build the HMAC signing input as an octet vector.
       \"lolweb-tok\" #\\Soh \"v1\" #\\Soh nonce-hex #\\Soh expiry #\\Soh payload-hex
   The domain string and format version pin the HMAC input to this token
   type; the #\\Soh separator (ASCII 0x01) cannot occur inside any of the
   three segments, which are all hex or decimal."
  (babel:string-to-octets
   (format nil "lolweb-tok~C~A~C~A~C~A~C~A"
           (code-char 1) "v1"
           (code-char 1) nonce-hex
           (code-char 1) expiry-str
           (code-char 1) payload-hex)))

(defun mint-token (secret-key &key payload (ttl-seconds 3600) (nonce-bytes 16))
  "WARNING: Payload is NOT ENCRYPTED — it is recoverable by anyone holding the
   encoded string. Use this for integrity + expiry of public identifiers
   (user IDs, capability tags). For confidentiality, encrypt the payload
   before passing it in.

   Mint a signed, expiring token.

   SECRET-KEY is an octet vector of at least *TOKEN-MIN-SECRET-KEY-BYTES*
   bytes — caller is responsible for keying. A short key signals at mint
   time rather than silently producing a token with weak integrity.
   PAYLOAD is a string or octet vector (NIL → empty payload).
   TTL-SECONDS sets the absolute expiry (default 1 hour).
   NONCE-BYTES sets the random-nonce width (default 16 → 128 bits).

   The encoded token is URL-safe (hex + dots only). The HMAC input is
   domain-tagged + version-tagged so this token shape cannot collide with
   a future v2 format or a sibling token type sharing the same key."
  (declare (type (vector (unsigned-byte 8)) secret-key)
           (type (integer 1 *) ttl-seconds nonce-bytes))
  (assert (>= (length secret-key) *token-min-secret-key-bytes*)
          (secret-key)
          "SECRET-KEY must be at least ~D bytes (got ~D); HMAC-SHA256 keying ~
           below its block size weakens the integrity property."
          *token-min-secret-key-bytes* (length secret-key))
  (let* ((nonce         (ironclad:random-data nonce-bytes))
         (nonce-hex     (ironclad:byte-array-to-hex-string nonce))
         (expiry        (+ (%unix-time) ttl-seconds))
         (expiry-str    (write-to-string expiry))
         (payload-bytes (if payload (%coerce-octets payload) (%empty-octets)))
         (payload-hex   (ironclad:byte-array-to-hex-string payload-bytes))
         (tag-hex       (hmac-sha256-hex secret-key
                                         (%signing-input-bytes nonce-hex
                                                               expiry-str
                                                               payload-hex))))
    (format nil "~A.~A.~A.~A" nonce-hex expiry-str payload-hex tag-hex)))

(defun verify-token (secret-key encoded)
  "Verify ENCODED token under SECRET-KEY.

   Returns (VALUES VALID-P SECOND THIRD):
     - VALID-P T plus nonce-hex (string) and payload-octets on success.
     - VALID-P NIL plus :BAD-TAG (string-level corruption, wrong secret,
       tampered field, empty nonce/expiry segment, or any token minted
       under a different domain/format version) and NIL on tag failure.
     - VALID-P NIL plus :EXPIRED and NIL when tag is valid but expiry
       has passed.

   The framework does not store nonces — consumers wanting single-use
   semantics keep a spent-nonce table keyed by NONCE-HEX.

   v1 format only. Tokens whose HMAC input lacks the domain+version prefix
   (sibling token types, legacy mints under the same key) fail :BAD-TAG —
   cross-protocol forgery cannot succeed even if the secret is shared."
  (declare (type (vector (unsigned-byte 8)) secret-key)
           (type string encoded))
  (let ((parts (loop with start = 0
                     with end = (length encoded)
                     for dot = (position #\. encoded :start start)
                     collect (subseq encoded start (or dot end))
                     while dot
                     do (setf start (1+ dot)))))
    (if (/= (length parts) 4)
        (values nil :bad-tag nil)
        ;; PARSE-ERROR: non-numeric expiry. SIMPLE-ERROR: ironclad hex
        ;; decode. TYPE-ERROR: helper type mismatches. Anything else
        ;; escapes — a programming bug must not look like a bad token.
        (handler-case
            (let ((nonce-hex   (first parts))
                  (expiry-str  (second parts))
                  (payload-hex (third parts))
                  (tag-hex     (fourth parts)))
              (cond
                ;; Empty nonce or expiry segments cannot be legitimate; reject
                ;; before any HMAC work so timing-side observation cannot tell
                ;; an empty-field rejection from a tag mismatch (both bail in
                ;; constant work). Empty payload is intentionally allowed.
                ((or (zerop (length nonce-hex))
                     (zerop (length expiry-str)))
                 (values nil :bad-tag nil))
                ;; Length cap on every segment, before any charset scan or
                ;; HMAC compute. An attacker submitting a megabyte-scale
                ;; nonce / expiry / payload forces O(n) work through
                ;; %SIGNING-INPUT-BYTES (BABEL:STRING-TO-OCTETS plus the
                ;; HMAC-SHA256 pass) for every forged token. Bounding the
                ;; segment length collapses that work to a small constant.
                ;; Tag length is fixed by HMAC-SHA256 output width; a tag
                ;; of any other length is forged-by-shape and rejected
                ;; without entering CONSTANT-TIME-STRING= (which would
                ;; otherwise run min-length compares against a 64-char
                ;; expected tag, leaking nothing but doing pointless work).
                ((or (> (length nonce-hex)   *token-max-nonce-hex-length*)
                     (> (length expiry-str)  +token-max-expiry-str-length+)
                     (> (length payload-hex) *token-max-payload-hex-length*)
                     (/= (length tag-hex)    +token-tag-hex-length+))
                 (values nil :bad-tag nil))
                ;; Charset gate on every signed segment. The HMAC signing
                ;; input separates fields with #\Soh (ASCII 0x01) and relies
                ;; on the invariant that no segment contains SOH. Enforcing
                ;; the mint-side charset (lowercase hex for nonce / payload
                ;; / tag, decimal for expiry) at verify time closes a class
                ;; of framing-collision attacks where attacker-controlled
                ;; bytes in a signed segment could redistribute the same
                ;; byte stream as a different (nonce, expiry, payload)
                ;; triple under the same tag. The tag charset gate also
                ;; closes the wedge where a tag carrying uppercase or
                ;; non-hex bytes passed CONSTANT-TIME-STRING= against a
                ;; lowercase-hex expected tag (impossible by construction
                ;; today, but the gate makes the invariant explicit).
                ((not (and (%hex-segment-p nonce-hex)
                           (%decimal-segment-p expiry-str)
                           (%hex-segment-p payload-hex)
                           (%hex-segment-p tag-hex)))
                 (values nil :bad-tag nil))
                (t
                 (let* ((expected-tag (hmac-sha256-hex
                                       secret-key
                                       (%signing-input-bytes nonce-hex
                                                             expiry-str
                                                             payload-hex))))
                   (cond
                     ((not (constant-time-string= tag-hex expected-tag))
                      (values nil :bad-tag nil))
                     ((< (parse-integer expiry-str) (%unix-time))
                      (values nil :expired nil))
                     (t
                      (values t
                              nonce-hex
                              (ironclad:hex-string-to-byte-array payload-hex))))))))
          ((or parse-error simple-error type-error) ()
            (values nil :bad-tag nil))))))
