;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/crypto — mechanism-only crypto primitives.
;;;;
;;;; Leaf sub-system. Depends only on ironclad and babel. Zero HTTP/Clack
;;;; dependency, so a CLI consumer signing tokens can load just this package
;;;; without dragging in the Clack stack.
;;;;
;;;; Surface:
;;;;   hash-password / verify-password   — argon2id-backed password hashing
;;;;   needs-rehash?                      — argon2id parameter freshness check
;;;;   mint-token / verify-token          — HMAC-SHA256 signed token
;;;;   hmac-sha256-hex                    — bare HMAC-SHA256 primitive (hex tag)
;;;;   sha256-hex                         — bare SHA-256 digest (content address)
;;;;   constant-time-string=              — timing-safe string compare
;;;;   random-bytes-hex                   — OS CSPRNG → hex string

(in-package :cl-user)

(defpackage :lol-web/crypto
  (:use :cl)
  (:export
   #:hash-password
   #:verify-password
   #:needs-rehash?
   #:mint-token
   #:verify-token
   #:*token-min-secret-key-bytes*
   #:*token-max-nonce-hex-length*
   #:*token-max-payload-hex-length*
   #:hmac-sha256-hex
   #:sha256-hex
   #:random-bytes-hex
   #:constant-time-string=))
