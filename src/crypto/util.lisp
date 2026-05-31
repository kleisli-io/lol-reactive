;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/CRYPTO; Base: 10 -*-
;;;; Timing-safe string compare + OS-CSPRNG hex helper.

(in-package :lol-web/crypto)

(defun constant-time-string= (a b)
  "Compare two strings in time proportional to MAX(|a|,|b|), independent of
   matching prefix length. XOR-folds char codes into a single accumulator and
   tests it against zero only after the full pass. Returns NIL when lengths
   differ (length is non-secret) but does not short-circuit on character
   mismatch within equal-length inputs."
  (declare (type string a b)
           (optimize (speed 3) (safety 1)))
  (and (= (length a) (length b))
       (let ((acc 0))
         (declare (type (unsigned-byte 32) acc))
         (loop for ca across a
               for cb across b
               do (setf acc (logior acc
                                    (logxor (char-code ca) (char-code cb)))))
         (zerop acc))))

(defun random-bytes-hex (n-bytes)
  "Return a 2*N-BYTES-long lowercase hex string sourced from the OS CSPRNG.
   Wraps IRONCLAD:RANDOM-DATA + IRONCLAD:BYTE-ARRAY-TO-HEX-STRING."
  (declare (type (integer 1 *) n-bytes))
  (ironclad:byte-array-to-hex-string (ironclad:random-data n-bytes)))

(defun hmac-sha256-hex (secret-key data)
  "Compute HMAC-SHA256 of DATA under SECRET-KEY. Return tag as lowercase hex.
   SECRET-KEY is an octet vector. DATA is either a string (UTF-8 encoded
   before hashing) or an octet vector (hashed verbatim)."
  (declare (type (vector (unsigned-byte 8)) secret-key))
  (let ((mac (ironclad:make-hmac secret-key :sha256))
        (bytes (etypecase data
                 (string (babel:string-to-octets data :encoding :utf-8))
                 ((vector (unsigned-byte 8)) data))))
    (ironclad:update-hmac mac bytes)
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest mac))))

(defun sha256-hex (data)
  "Return the lowercase hex SHA-256 digest of DATA. DATA is a string (UTF-8
   encoded before hashing) or an octet vector (hashed verbatim). A bare
   content digest for content-addressing — not a keyed MAC; use
   hmac-sha256-hex when an authentication tag is required."
  (let ((bytes (etypecase data
                 (string (babel:string-to-octets data :encoding :utf-8))
                 ((vector (unsigned-byte 8)) data))))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha256 bytes))))
