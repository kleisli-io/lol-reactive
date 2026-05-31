(in-package :lol-web/crypto/test)
(in-suite :lol-web/crypto/test)

;;; ============================================================================
;;; constant-time-string=
;;; ============================================================================

(test constant-time-string=-equal-strings
  "Equal strings compare T."
  (is (constant-time-string= "" ""))
  (is (constant-time-string= "a" "a"))
  (is (constant-time-string= "abcdefghij" "abcdefghij")))

(test constant-time-string=-differing-strings
  "Differing strings compare NIL — independent of mismatch position."
  (is (null (constant-time-string= "abc" "xbc")) "early differ")
  (is (null (constant-time-string= "abc" "abx")) "late differ"))

(test constant-time-string=-length-mismatch
  "Length mismatch returns NIL without inspecting characters."
  (is (null (constant-time-string= "" "a")))
  (is (null (constant-time-string= "abc" "abcd")))
  (is (null (constant-time-string= "abcd" "abc"))))

;;; ============================================================================
;;; random-bytes-hex
;;; ============================================================================

(test random-bytes-hex-length-and-charset
  "Output length is exactly 2*N and contains only lowercase hex characters."
  (let ((out (random-bytes-hex 16)))
    (is (= 32 (length out)) "16 bytes encode to 32 hex chars")
    (is (every (lambda (c) (or (digit-char-p c)
                               (find c "abcdef")))
               out)
        "all characters are lowercase hex")))

(test random-bytes-hex-distinct-calls
  "Two consecutive calls return distinct values (CSPRNG sanity, not entropy proof)."
  (is (not (string= (random-bytes-hex 16) (random-bytes-hex 16)))))
