;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/CRYPTO; Base: 10 -*-
;;;; Argon2id-backed password hashing.
;;;;
;;;; Encoded format (v1, self-describing, no extra dep):
;;;;
;;;;   v1:argon2id:<block-count>:<iterations>:<parallelism>:<argon2-version>:<salt-hex>:<key-hex>
;;;;
;;;; NOT PHC ($argon2id$v=19$...) — PHC requires base64 and would force a
;;;; cl-base64 dep on this leaf sub-system. The delimited form is an
;;;; internal storage primitive; consumers don't compare encoded strings
;;;; against other tools, they call HASH-PASSWORD and VERIFY-PASSWORD.
;;;;
;;;; The leading "v1" discriminator reserves space for future schemes
;;;; (e.g., argon2id at a higher parallelism, or a different KDF) without
;;;; ambiguity. Verifiers must reject any string that does not start with
;;;; a known version prefix.
;;;;
;;;; Parallelism and argon2-version are recorded even though ironclad's
;;;; current argon2id implementation fixes them at 1 and 19 respectively:
;;;; if a future ironclad update changes either default, the verifier can
;;;; detect the mismatch and fail loudly (returning NIL) instead of
;;;; silently producing wrong derived keys.
;;;;
;;;; Defaults match the OWASP Argon2id floor: 19 MiB memory, 2 iterations,
;;;; 16-byte salt, 32-byte key. Measured ~180 ms on a typical SBCL host;
;;;; consumers can tune per call.

(in-package :lol-web/crypto)

(defparameter *password-default-block-count* 19456
  "OWASP Argon2id floor — ~19 MiB.")

(defparameter *password-default-iterations* 2
  "OWASP Argon2id floor.")

(defparameter *password-default-salt-length* 16
  "OWASP Argon2id floor.")

(defparameter *password-default-key-length* 32
  "OWASP Argon2id floor.")

(defparameter *password-supported-parallelism* 1
  "The only parallelism value ironclad's argon2id implementation supports
   today. Recorded in the encoded format so a future ironclad update that
   silently changes this is detectable instead of producing wrong keys.")

(defparameter *password-supported-argon2-version* 19
  "Argon2 v1.3 (= decimal 19, the spec's only published version). Recorded
   in the encoded format for the same forward-detection reason as
   *PASSWORD-SUPPORTED-PARALLELISM*.")

(defparameter *password-min-salt-length* 16
  "Salt length floor enforced at HASH-PASSWORD time. RFC 9106 §4 recommends
   16+ bytes for password storage; anything shorter is a misconfiguration.")

(defparameter *password-min-block-count* 1024
  "Lower-bound floor on block-count parsed from an encoded string. Catches
   obvious downgrade-by-DB-write attacks (e.g., :1:1:salt:key) while still
   accommodating fast test parameters. Production deployments should run
   at *PASSWORD-DEFAULT-BLOCK-COUNT*; rehash via NEEDS-REHASH?.")

(defparameter *password-max-block-count* 131072
  "Upper-bound cap on block-count parsed from an encoded string (~128 MiB).
   The floor proves a stored hash is reachable by VERIFY-PASSWORD, which
   feeds block-count straight into IRONCLAD:MAKE-KDF; an attacker with DB
   write could otherwise set an enormous block-count and amplify every
   login attempt into a multi-gigabyte allocation. The cap bounds per-verify
   memory while leaving generous headroom above *PASSWORD-DEFAULT-BLOCK-COUNT*.")

(defparameter *password-min-iterations* *password-default-iterations*
  "Lower-bound floor on iterations parsed from an encoded string. Matches
   the OWASP Argon2id default — sub-floor stored hashes refuse to verify
   so an attacker with DB write cannot drop iterations to 1.")

(defparameter *password-min-key-length* *password-default-key-length*
  "Lower-bound floor on derived-key length (in bytes) parsed from an
   encoded string. Matches the OWASP 32-byte default — a sub-floor stored
   key would give an attacker a small-comparison advantage at verify time.")

(defun %split-encoded (encoded)
  "Parse a v1 encoded password string into its fields. Returns NIL on any
   parse failure (unknown prefix, wrong field count, malformed integers,
   malformed hex, unsupported parallelism / argon2-version, sub-floor
   block-count / iterations / salt-length / key-length). Internal helper."
  (let ((parts (loop with start = 0
                     with end = (length encoded)
                     for colon = (position #\: encoded :start start)
                     collect (subseq encoded start (or colon end))
                     while colon
                     do (setf start (1+ colon)))))
    (when (and (= (length parts) 8)
               (string= (first parts)  "v1")
               (string= (second parts) "argon2id"))
      ;; PARSE-ERROR: PARSE-INTEGER on non-numeric. SIMPLE-ERROR:
      ;; IRONCLAD:HEX-STRING-TO-BYTE-ARRAY on malformed hex.
      (handler-case
          (let ((block-count    (parse-integer (third parts)))
                (iterations     (parse-integer (fourth parts)))
                (parallelism    (parse-integer (fifth parts)))
                (argon2-version (parse-integer (sixth parts)))
                (salt           (ironclad:hex-string-to-byte-array (seventh parts)))
                (key            (ironclad:hex-string-to-byte-array (eighth parts))))
            (when (and (>= block-count *password-min-block-count*)
                       (<= block-count *password-max-block-count*)
                       (>= iterations *password-min-iterations*)
                       (= parallelism *password-supported-parallelism*)
                       (= argon2-version *password-supported-argon2-version*)
                       (>= (length salt) *password-min-salt-length*)
                       (>= (length key)  *password-min-key-length*))
              (list :block-count    block-count
                    :iterations     iterations
                    :parallelism    parallelism
                    :argon2-version argon2-version
                    :salt           salt
                    :key            key)))
        ((or parse-error simple-error type-error) () nil)))))

(defun hash-password (password &key (block-count    *password-default-block-count*)
                                    (iterations     *password-default-iterations*)
                                    (parallelism    *password-supported-parallelism*)
                                    (argon2-version *password-supported-argon2-version*)
                                    (salt-length    *password-default-salt-length*)
                                    (key-length     *password-default-key-length*))
  "Hash PASSWORD (string) with Argon2id and return a self-describing
   v1-encoded string.

   Defaults: block-count=19456 (~19 MiB), iterations=2, parallelism=1,
   argon2-version=19, salt-length=16, key-length=32 — the Argon2id floor
   recommended by OWASP. ~180 ms per call on a typical host.

   PARALLELISM and ARGON2-VERSION are recorded but not currently variable:
   ironclad's argon2id fixes them at 1 and 19. Passing other values
   signals at hash time so misconfiguration is observable rather than
   producing keys that cannot be verified.

   The returned string is opaque to consumers but stable: store it as-is
   and pass it back to VERIFY-PASSWORD on login. NEEDS-REHASH? reports
   whether a stored string is below current defaults."
  (declare (type string password)
           (type (integer 1 *) block-count iterations salt-length key-length))
  (check-type salt-length (integer 16 *)
              "at least 16 (per RFC 9106 §4 password-storage guidance)")
  (assert (= parallelism *password-supported-parallelism*)
          (parallelism)
          "PARALLELISM must equal ~D — ironclad's argon2id implementation ~
           does not currently accept other lane counts (got ~D)."
          *password-supported-parallelism* parallelism)
  (assert (= argon2-version *password-supported-argon2-version*)
          (argon2-version)
          "ARGON2-VERSION must equal ~D (Argon2 v1.3); got ~D."
          *password-supported-argon2-version* argon2-version)
  (let* ((salt (ironclad:random-data salt-length))
         (kdf  (ironclad:make-kdf :argon2id :block-count block-count))
         (key  (ironclad:derive-key kdf
                                    (babel:string-to-octets password)
                                    salt
                                    iterations
                                    key-length)))
    (format nil "v1:argon2id:~D:~D:~D:~D:~A:~A"
            block-count
            iterations
            parallelism
            argon2-version
            (ironclad:byte-array-to-hex-string salt)
            (ironclad:byte-array-to-hex-string key))))

(defun verify-password (password encoded)
  "Verify PASSWORD against ENCODED (the string returned by HASH-PASSWORD).
   Returns T on match, NIL on mismatch OR on any parse failure (unknown
   version, wrong algorithm, malformed fields, sub-floor parameters,
   unsupported parallelism / argon2-version).

   Uses IRONCLAD:CONSTANT-TIME-EQUAL on the raw key octets — comparison
   time is proportional to key length and independent of matching prefix.
   Comparing on the octet vector itself (not its hex encoding) keeps the
   running time uniform over the underlying byte width: a hex-encoded
   compare runs over (simple-array character), where per-element load
   time can vary between base-char and character storage; an octet
   compare runs over (simple-array (unsigned-byte 8)) where every load
   is one machine instruction of fixed cost."
  (declare (type string password encoded))
  (let ((parsed (%split-encoded encoded)))
    (when parsed
      (let* ((block-count (getf parsed :block-count))
             (iterations  (getf parsed :iterations))
             (salt        (getf parsed :salt))
             (stored-key  (getf parsed :key))
             (kdf         (ironclad:make-kdf :argon2id :block-count block-count))
             (candidate   (ironclad:derive-key kdf
                                               (babel:string-to-octets password)
                                               salt
                                               iterations
                                               (length stored-key))))
        (ironclad:constant-time-equal candidate stored-key)))))

(defun needs-rehash? (encoded &key
                                (block-count    *password-default-block-count*)
                                (iterations     *password-default-iterations*)
                                (parallelism    *password-supported-parallelism*)
                                (argon2-version *password-supported-argon2-version*)
                                (salt-length    *password-default-salt-length*)
                                (key-length     *password-default-key-length*))
  "Return T when ENCODED should be re-hashed under the supplied (or
   current default) parameters, NIL otherwise.

   Returns T on:
     - parse failure (cannot trust the hash → rehash)
     - non-v1 format (e.g., a pre-v1 stored string)
     - any recorded parameter below the corresponding floor argument

   Returns NIL only when all recorded parameters meet or exceed the
   supplied thresholds. Call this on every successful login and
   re-hash transparently when the predicate fires."
  (declare (type string encoded))
  (let ((parsed (%split-encoded encoded)))
    (cond
      ((null parsed) t)
      ((< (getf parsed :block-count) block-count) t)
      ((< (getf parsed :iterations) iterations) t)
      ((/= (getf parsed :parallelism) parallelism) t)
      ((/= (getf parsed :argon2-version) argon2-version) t)
      ((< (length (getf parsed :salt)) salt-length) t)
      ((< (length (getf parsed :key)) key-length) t)
      (t nil))))
