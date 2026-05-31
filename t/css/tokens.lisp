(in-package :lol-web/css/test)
(in-suite :lol-web/css/test)

;;; ============================================================================
;;; Token accessors
;;; ============================================================================

(test get-color-returns-default-palette-entry
  "get-color reads from *colors*"
  (is (string= (cdr (assoc :primary *default-colors*))
               (get-color :primary))
      "default :primary value matches *default-colors*"))

(test get-font-typography-keys
  "get-font reads from *typography*"
  (is (string= (cdr (assoc :family *default-typography*))
               (get-font :family))))

(test get-spacing-numeric-keys
  "get-spacing accepts the numeric keyword keys :0 :4 :8 ..."
  (is (string= (cdr (assoc :4 *default-spacing*))
               (get-spacing :4))))

(test get-effect-shadow
  "get-effect reads from *effects*"
  (is (string= (cdr (assoc :shadow-md *default-effects*))
               (get-effect :shadow-md))))

;;; ============================================================================
;;; Validation: keyword shape + Levenshtein "did you mean?"
;;; ============================================================================

(test validate-token-rejects-non-keyword
  "validate-token rejects non-keyword tokens with a type-shaped error"
  (signals error (validate-token "primary" *colors* "color")))

(test validate-token-rejects-unknown-and-suggests
  "Unknown token errors with a suggestion derived from Levenshtein distance"
  (handler-case
      (progn (validate-token :primry *colors* "color")
             (is nil "expected an error for an unknown token"))
    (error (c)
      (let ((msg (princ-to-string c)))
        ;; Symbols print upcased via ~A; do a case-insensitive search.
        (is (search "primry" msg :test #'char-equal)
            "error message includes the bad token")
        (is (search "primary" msg :test #'char-equal)
            "error message suggests :primary as the closest match")))))

(test validate-token-accepts-known-key
  "validate-token returns the token for a known key"
  (is (eq :primary (validate-token :primary *colors* "color"))))

(test regression-validate-token-length-cap
  "Tokens whose symbol-name exceeds *validate-token-max-length* are
   rejected before the O(n*m) Levenshtein suggestion path runs. With
   the cap at the default 256, an attacker-controlled key 257 chars
   long must error with a length message — not a 'did you mean?' one."
  (let ((huge (intern (concatenate 'string
                                    ":"
                                    (make-string (1+ *validate-token-max-length*)
                                                 :initial-element #\A))
                      :keyword)))
    (handler-case (validate-token huge *colors* "color")
      (error (c)
        (let ((msg (princ-to-string c)))
          (is (search "maximum length" msg)
              "long token must error with length message, not suggestion"))))))

(test regression-validate-token-cap-is-tunable
  "Lowering *validate-token-max-length* shrinks the accepted token-name
   bound. Bind to 8 and a 9-char key must be rejected by length."
  (let ((*validate-token-max-length* 8))
    (handler-case (validate-token :nineletter *colors* "color")
      (error (c)
        (let ((msg (princ-to-string c)))
          (is (search "maximum length" msg)))))))

;;; ============================================================================
;;; Levenshtein distance
;;; ============================================================================

(test levenshtein-distance-known-cases
  "levenshtein-distance returns the standard edit-distance values"
  (is (= 0 (levenshtein-distance "kitten" "kitten")))
  (is (= 3 (levenshtein-distance "kitten" "sitting")))
  (is (= 1 (levenshtein-distance "abc" "abcd")))
  (is (= 1 (levenshtein-distance "abcd" "abc"))))

;;; ============================================================================
;;; CSS variable generation iterates the alists directly
;;; ============================================================================

(test generate-css-variables-emits-root-block
  "generate-css-variables emits a :root block with variables for every alist key"
  (let* ((colors    '((:bg . "#000") (:fg . "#fff")))
         (typography '((:family . "monospace")))
         (spacing   '((:4 . "1rem")))
         (effects   '((:shadow-md . "0 0 4px rgba(0,0,0,0.1)")))
         (css (generate-css-variables :colors colors
                                      :typography typography
                                      :spacing spacing
                                      :effects effects)))
    (is (search ":root {" css))
    (is (search "--color-bg: #000" css))
    (is (search "--color-fg: #fff" css))
    (is (search "--font-family: monospace" css))
    (is (search "--space-4: 1rem" css))
    (is (search "--effect-shadow-md: 0 0 4px rgba(0,0,0,0.1)" css))))

(test regression-generate-css-variables-escapes-token-values
  "Design-token values are CSS-escaped before variable emission."
  (let ((css (generate-css-variables
              :colors '((:danger . "red;}body{color:red"))
              :typography '((:family . "Inter<script>"))
              :spacing '()
              :effects '())))
    (is (search "--color-danger: red\\3B \\7D body{color:red" css))
    (is (search "--font-family: Inter\\3C script\\3E " css))
    (is (null (search "red;}body" css)))
    (is (null (search "<script>" css)))))

(test generate-css-variables-uses-dynamic-defaults
  "Calling without keyword args reads the dynamic *colors* / *typography* etc."
  (let* ((*colors*     '((:probe-bg . "#123456")))
         (*typography* '((:family . "probe-font")))
         (*spacing*    '((:probe-4 . "0.4rem")))
         (*effects*    '((:probe-effect . "probe-value")))
         (css (generate-css-variables)))
    (is (search "--color-probe-bg: #123456" css))
    (is (search "--font-family: probe-font" css))
    (is (search "--space-probe-4: 0.4rem" css))
    (is (search "--effect-probe-effect: probe-value" css))))

(test regression-css-token-name-validated
  "generate-css-variables and tailwind-config validate token NAMES via
   safe-css-ident-p on the prefixed `prefix-name` segment, so a numeric
   spacing name (:4 -> --space-4) passes while a metacharacter-bearing
   key that would smuggle a `;`/`}`/`<` boundary into the declaration is
   refused with unsafe-css-ident."
  (let ((css (generate-css-variables :colors nil :typography nil
                                     :spacing '((:|4| . "1rem")) :effects nil)))
    (is (search "--space-4: 1rem;" css)))
  (let ((css (generate-css-variables :colors '((:primary . "#000"))
                                     :typography nil :spacing nil :effects nil)))
    (is (search "--color-primary: #000;" css)))
  (signals unsafe-css-ident
    (generate-css-variables :colors (list (cons (intern "x; color:red" :keyword) "#000"))
                            :typography nil :spacing nil :effects nil))
  (signals unsafe-css-ident
    (generate-css-variables :colors nil :typography nil :spacing nil
                            :effects (list (cons (intern "y}injected" :keyword) "0"))))
  (signals unsafe-css-ident
    (tailwind-config :colors (list (cons (intern "z<x" :keyword) "#000")))))
