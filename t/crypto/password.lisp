(in-package :lol-web/crypto/test)
(in-suite :lol-web/crypto/test)

;;; Argon2id test parameters: low block-count for fast tests. Production
;;; callers omit the keywords and pick up the OWASP-floor defaults.
(defparameter *test-block-count* 4096)
(defparameter *test-iterations* 2)

(defun %test-hash (password)
  (hash-password password
                 :block-count *test-block-count*
                 :iterations  *test-iterations*))

;;; ============================================================================
;;; hash-password / verify-password round-trip
;;; ============================================================================

(test password-round-trip
  "Hash a password, verify the same password matches."
  (is (verify-password "correct horse battery staple"
                       (%test-hash "correct horse battery staple"))))

(test password-rejects-wrong-password
  "verify-password returns NIL for a different password."
  (let ((encoded (%test-hash "right")))
    (is (null (verify-password "wrong" encoded)))
    (is (null (verify-password ""      encoded)))))

(test password-verify-uses-octet-constant-time-equal
  "verify-password compares the derived key against the stored key on raw
   octet vectors via IRONCLAD:CONSTANT-TIME-EQUAL — not via hex strings.
   Hex-string comparison would degrade the constant-time guarantee: each
   octet becomes two characters, and (simple-array character) loads can
   vary by per-element storage class. Disassembly-text inspection pins
   the callee so a future refactor that silently restores hex comparison
   trips this regression guard. FDEFN entries in the disassembly retain
   the symbol name even when the call is inlined."
  (let ((asm (with-output-to-string (s)
               (disassemble 'verify-password :stream s))))
    (is (search "CONSTANT-TIME-EQUAL" asm)
        "verify-password disassembly must reference IRONCLAD:CONSTANT-TIME-EQUAL")
    (is (not (search "CONSTANT-TIME-STRING" asm))
        "verify-password must NOT reference CONSTANT-TIME-STRING= (string compare)")
    (is (not (search "BYTE-ARRAY-TO-HEX-STRING" asm))
        "verify-password must not hex-encode the keys before comparison")))

(test password-rejects-empty-password-when-different
  "Empty hashed password verifies against empty, not against a non-empty guess."
  (let ((encoded (%test-hash "")))
    (is (verify-password "" encoded))
    (is (null (verify-password "anything" encoded)))))

;;; ============================================================================
;;; Encoded format stability
;;; ============================================================================

(test password-encoded-format-prefix
  "Encoded string is v1-discriminated, algorithm-tagged, and records the
   block-count, iterations, parallelism, and argon2-version verbatim."
  (let ((encoded (%test-hash "x")))
    (is (and (>= (length encoded) (length "v1:argon2id:"))
             (string= (subseq encoded 0 (length "v1:argon2id:"))
                      "v1:argon2id:"))
        "starts with v1:argon2id: discriminator")
    (is (search (format nil ":~D:~D:1:19:" *test-block-count* *test-iterations*)
                encoded)
        "records block-count, iterations, parallelism (1), argon2-version (19)")))

(test password-fresh-salt-each-call
  "Two hashes of the same password produce different encoded strings (fresh salts)."
  (let ((a (%test-hash "same"))
        (b (%test-hash "same")))
    (is (not (string= a b))
        "salts differ → encoded strings differ even for identical passwords")
    (is (verify-password "same" a) "first encoding verifies")
    (is (verify-password "same" b) "second encoding verifies")))

;;; ============================================================================
;;; Malformed input rejection
;;; ============================================================================

(test password-rejects-malformed-encoded
  "verify-password returns NIL (not error) on every parse-failure shape."
  (is (null (verify-password "anything" ""))
      "empty encoded string")
  (is (null (verify-password "anything" "v1:bcrypt:4096:2:1:19:00:00"))
      "wrong algorithm tag")
  (is (null (verify-password "anything" "v2:argon2id:4096:2:1:19:00:00"))
      "unknown version prefix")
  (is (null (verify-password "anything" "v1:argon2id:not-an-int:2:1:19:00:00"))
      "non-integer block-count")
  (is (null (verify-password "anything" "v1:argon2id:4096"))
      "too few fields")
  (is (null (verify-password "anything" "v1:argon2id:4096:2:1:19:nothex:00"))
      "non-hex salt"))

;;; ============================================================================
;;; Pre-v1 encoded shape rejected by verify
;;; ============================================================================

(test password-rejects-pre-v1-format
  "Legacy 5-field shape (no v1 prefix, no parallelism/version) fails
   verification under the v1 verifier."
  (is (null (verify-password "anything"
                             "argon2id:4096:2:0102030405060708090a0b0c0d0e0f10:00"))))

;;; ============================================================================
;;; Recorded parameters: parallelism, argon2-version
;;; ============================================================================

(test password-rejects-unsupported-parallelism-in-encoded
  "An encoded string whose recorded parallelism field differs from the
   value ironclad supports must NOT verify — even if the salt and key
   would round-trip, the framework cannot honestly say the recorded
   parameters match what was used."
  (is (null (verify-password "anything"
                             "v1:argon2id:4096:2:4:19:0102030405060708090a0b0c0d0e0f10:00"))
      "parallelism=4 in stored hash is rejected"))

(test password-rejects-unsupported-argon2-version-in-encoded
  "Same property for argon2-version: an encoded string claiming v=0x20
   (or any non-19 value) must fail to parse so verify returns NIL."
  (is (null (verify-password "anything"
                             "v1:argon2id:4096:2:1:32:0102030405060708090a0b0c0d0e0f10:00"))
      "argon2-version=32 in stored hash is rejected"))

(test password-rejects-sub-floor-block-count-in-encoded
  "An attacker with DB write must not be able to downgrade a stored hash
   to argon2id:1:1:... and bypass the work factor. The parser enforces
   block-count >= *PASSWORD-MIN-BLOCK-COUNT*."
  (is (null (verify-password "anything"
                             "v1:argon2id:1:1:1:19:0102030405060708090a0b0c0d0e0f10:00"))
      "block-count below the parser floor is rejected"))

(test password-rejects-sub-floor-iterations-in-encoded
  "A stored hash with iterations=1 (below the OWASP-floor default) must
   not verify, even for the correct password. The parser refuses the
   encoded string at parse time so argon2id never runs."
  (let ((sub-floor (hash-password "x"
                                  :block-count *test-block-count*
                                  :iterations 1)))
    (is (null (verify-password "x" sub-floor))
        "iterations=1 in stored hash must be rejected for the correct password")))

(test password-rejects-sub-floor-key-length-in-encoded
  "A 16-byte derived key (below the 32-byte OWASP floor) must not verify
   even for the correct password. The parser refuses at parse time."
  (let ((sub-floor (hash-password "x"
                                  :block-count *test-block-count*
                                  :iterations  *test-iterations*
                                  :key-length 16)))
    (is (null (verify-password "x" sub-floor))
        "key-length=16 in stored hash must be rejected for the correct password")))

(test password-rejects-sub-floor-salt-length-in-encoded
  "Encoded strings carrying an 8-byte salt (below RFC 9106 §4's 16-byte
   recommendation) must be rejected at parse time. Hand-constructed
   because HASH-PASSWORD's CHECK-TYPE refuses to mint sub-floor salts."
  (is (null (verify-password "anything"
                             "v1:argon2id:4096:2:1:19:0102030405060708:0000000000000000000000000000000000000000000000000000000000000000"))
      "salt-length=8 bytes in stored hash is rejected"))

;;; ============================================================================
;;; HASH-PASSWORD assertions
;;; ============================================================================

(test password-rejects-small-salt-length-at-hash
  "HASH-PASSWORD signals when SALT-LENGTH is below the RFC 9106 floor of
   16 bytes — a misconfiguration we want loud, not silent."
  (signals error
    (hash-password "x" :block-count *test-block-count* :salt-length 8)))

(test password-rejects-unsupported-parallelism-at-hash
  "HASH-PASSWORD signals when PARALLELISM != 1 because ironclad's argon2id
   implementation does not honour it; producing a hash that claims a
   parallelism the verify side cannot reproduce would be a silent fail
   on first login."
  (signals error
    (hash-password "x" :block-count *test-block-count* :parallelism 4)))

(test password-rejects-unsupported-argon2-version-at-hash
  "Same protection for ARGON2-VERSION."
  (signals error
    (hash-password "x" :block-count *test-block-count* :argon2-version 32)))

;;; ============================================================================
;;; NEEDS-REHASH? predicate
;;; ============================================================================

(test needs-rehash-true-on-parse-failure
  "Garbage cannot be trusted — rehash the user's password on next login."
  (is (needs-rehash? ""))
  (is (needs-rehash? "garbage"))
  (is (needs-rehash? "v1:argon2id:not-an-int:2:1:19:00:00")))

(test needs-rehash-true-on-pre-v1-format
  "Non-v1 encoded shape triggers rehash."
  (is (needs-rehash?
       "argon2id:4096:2:0102030405060708090a0b0c0d0e0f10:00")))

(test needs-rehash-true-when-block-count-below-threshold
  "A v1 hash recorded with a block-count below the supplied threshold
   should rehash. Use the test block-count (4096) hash against the
   production default threshold (19456)."
  (let ((encoded (%test-hash "x")))
    (is (needs-rehash? encoded)
        "default thresholds: 4096 < 19456 → rehash")
    (is (needs-rehash? encoded :block-count 8192)
        "explicit threshold: 4096 < 8192 → rehash")))

(test needs-rehash-false-when-at-or-above-threshold
  "A v1 hash whose recorded parameters meet the supplied thresholds
   exactly should NOT rehash."
  (let ((encoded (%test-hash "x")))
    (is (null (needs-rehash? encoded
                             :block-count *test-block-count*
                             :iterations  *test-iterations*))
        "thresholds set to the recorded values → no rehash")))

(test needs-rehash-true-when-recorded-iterations-below-threshold
  "Iterations comparison is independent of block-count. Values stay at or
   above *PASSWORD-MIN-ITERATIONS* on both sides so the test isolates the
   threshold-vs-recorded check rather than the parser-floor rejection."
  (let ((encoded (hash-password "x"
                                :block-count *test-block-count*
                                :iterations  2)))
    (is (needs-rehash? encoded
                       :block-count *test-block-count*
                       :iterations 3))
    (is (null (needs-rehash? encoded
                             :block-count *test-block-count*
                             :iterations  2)))))

(test regression-password-block-count-upper-cap
  "An encoded hash whose recorded block-count exceeds *PASSWORD-MAX-BLOCK-COUNT*
   refuses to verify, so an attacker with DB write cannot amplify every login
   into a multi-gigabyte MAKE-KDF allocation. The same hash at a within-range
   block-count still round-trips, proving the cap is the cause of rejection."
  (labels ((split-colons (s)
             (loop with start = 0
                   for colon = (position #\: s :start start)
                   collect (subseq s start (or colon (length s)))
                   while colon do (setf start (1+ colon)))))
    (let* ((encoded (%test-hash "secret"))
           (over    (format nil "~{~A~^:~}"
                            (loop for p in (split-colons encoded)
                                  for i from 0
                                  collect (if (= i 2)
                                              (princ-to-string
                                               (1+ lol-web/crypto::*password-max-block-count*))
                                              p)))))
      (is (verify-password "secret" encoded)
          "within-range block-count verifies")
      (is (null (verify-password "secret" over))
          "block-count above the upper cap refuses to verify"))))
