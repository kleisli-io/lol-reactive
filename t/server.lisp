;;;; LOL-REACTIVE Test Suite - Server
;;;;
;;;; Tests for server routing, security headers, and error handling.

(in-package :lol-reactive.tests)
(in-suite :server)

;;; ============================================================================
;;; ROUTE REGISTRATION TESTS
;;; ============================================================================

(test defroute-macro-exists
  "defroute macro exists"
  (is (macro-function 'lol-reactive:defroute)))

(test defapi-macro-exists
  "defapi macro exists"
  (is (macro-function 'lol-reactive:defapi)))

(test routes-hash-table-exists
  "Routes are stored in a hash table"
  (is (hash-table-p lol-reactive:*routes*)))

;;; ============================================================================
;;; SECURITY TESTS
;;; ============================================================================

(test add-security-headers-exists
  "add-security-headers function exists"
  (is (fboundp 'lol-reactive:add-security-headers)))

(test add-csp-header-exists
  "add-csp-header function exists"
  (is (fboundp 'lol-reactive:add-csp-header)))

(test sanitize-html-exists
  "sanitize-html function exists"
  (is (fboundp 'lol-reactive:sanitize-html)))

(test sanitize-url-exists
  "sanitize-url function exists"
  (is (fboundp 'lol-reactive:sanitize-url)))

;;; ============================================================================
;;; CSRF TESTS
;;; ============================================================================

(test generate-csrf-token-exists
  "generate-csrf-token function exists"
  (is (fboundp 'lol-reactive:generate-csrf-token)))

(test validate-csrf-token-exists
  "validate-csrf-token function exists"
  (is (fboundp 'lol-reactive:validate-csrf-token)))

(test csrf-token-input-exists
  "csrf-token-input function exists"
  (is (fboundp 'lol-reactive:csrf-token-input)))

;;; ============================================================================
;;; RATE LIMITING TESTS
;;; ============================================================================

(test check-rate-limit-exists
  "check-rate-limit function exists"
  (is (fboundp 'lol-reactive:check-rate-limit)))

;;; ============================================================================
;;; ERROR HANDLING TESTS
;;; ============================================================================

(test log-error-exists
  "log-error function exists"
  (is (fboundp 'lol-reactive:log-error)))

(test render-error-page-exists
  "render-error-page function exists"
  (is (fboundp 'lol-reactive:render-error-page)))

(test render-404-page-exists
  "render-404-page function exists"
  (is (fboundp 'lol-reactive:render-404-page)))

(test render-500-page-exists
  "render-500-page function exists"
  (is (fboundp 'lol-reactive:render-500-page)))
