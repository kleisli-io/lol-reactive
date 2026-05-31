(in-package :lol-web/css/test)
(in-suite :lol-web/css/test)

;;; ============================================================================
;;; classes — composition with nil/empty filtering and list flattening
;;; ============================================================================

(test classes-filters-nil-and-empty
  "classes drops NIL and empty-string entries"
  (is (string= "p-4 bg-black"
               (classes "p-4" nil "" "bg-black" nil))))

(test classes-flattens-nested-lists
  "classes flattens nested lists for conditional composition"
  (is (string= "a b c"
               (classes (list "a" "b") "c"))))

(test classes-empty-input-yields-empty-string
  "classes with only nil/empty strings returns the empty string"
  (is (string= "" (classes nil "" nil))))

;;; ============================================================================
;;; tw- helpers
;;; ============================================================================

(test tw-color-prefix-and-key
  "tw-color formats prefix-key with the keyword downcased"
  (is (string= "bg-primary"   (tw-color "bg" :primary)))
  (is (string= "text-muted"   (tw-color "text" :muted)))
  (is (string= "border-error" (tw-color "border" :error))))

(test tw-spacing-numeric-keys
  "tw-spacing handles numeric keyword keys"
  (is (string= "p-4"  (tw-spacing "p" :4)))
  (is (string= "mx-8" (tw-spacing "mx" :8))))

(test tw-bg-text-border-shorthands
  "tw-bg / tw-text / tw-border are shorthands for tw-color"
  (is (string= "bg-primary"  (tw-bg :primary)))
  (is (string= "text-muted"  (tw-text :muted)))
  (is (string= "border-error" (tw-border :error))))

(test tw-arbitrary-strips-spaces
  "tw-arbitrary wraps a literal value in [] and removes spaces"
  (is (string= "w-[clamp(1rem,5vw,3rem)]"
               (tw-arbitrary "w" "clamp(1rem, 5vw, 3rem)"))))

(test regression-tw-arbitrary-strips-closing-bracket
  "Closing bracket cannot end the arbitrary-value segment early."
  (is (string= "bg-[red/[hover:blue]"
               (tw-arbitrary "bg" "red]/[hover:blue"))))

(test tw-bg-value-resolves-token
  "tw-bg-value pulls the colour from *colors* and wraps in [...]"
  (let ((*colors* '((:probe . "#abcdef"))))
    (is (string= "bg-[#abcdef]" (tw-bg-value :probe)))))

;;; ============================================================================
;;; tailwind-config — alist iteration through Parenscript
;;; ============================================================================

(test tailwind-config-emits-color-pairs-from-alist
  "tailwind-config maps every alist colour into the generated JS"
  (let* ((colors    '((:primary . "#00FF41") (:secondary . "#FF006E")))
         (typography '((:family . "\"JetBrains Mono\", monospace")))
         (js (tailwind-config :colors colors :typography typography)))
    (is (stringp js))
    (is (search "tailwind.config" js))
    (is (search "#00FF41" js)
        "value from alist appears verbatim in generated JS")
    (is (search "#FF006E" js)
        "second alist value also appears")
    (is (search "JetBrains Mono" js)
        "font family from typography alist is interpolated")))

(test tailwind-config-errors-on-missing-family
  "tailwind-config requires the :family typography token"
  (signals error
    (tailwind-config :colors '((:primary . "#000"))
                     :typography '((:weight . "400")))))

(test regression-tailwind-config-refuses-non-keyword-key
  "TAILWIND-CONFIG refuses to coerce a non-keyword color key — the
   alexandria:make-keyword path that interned downcased variants of every
   key is gone. A caller passing an arbitrary symbol or string as a key
   now signals TAILWIND-CONFIG-TOKEN-INVALID instead of growing the
   keyword pool."
  (signals lol-web/css:tailwind-config-token-invalid
    (tailwind-config
     :colors      '(("primary" . "#000"))
     :typography  '((:family . "\"JetBrains Mono\", monospace"))))
  (signals lol-web/css:tailwind-config-token-invalid
    (tailwind-config
     :colors      `((,(intern "PROBE" :cl-user) . "#000"))
     :typography  '((:family . "\"JetBrains Mono\", monospace")))))

(test regression-tailwind-config-refuses-over-cap
  "TAILWIND-CONFIG refuses a COLORS alist longer than
   *TAILWIND-CONFIG-MAX-TOKENS*. The check happens before any parenscript
   work so a hostile alist cannot force the JS generation pass."
  (let ((lol-web/css:*tailwind-config-max-tokens* 4))
    (signals lol-web/css:tailwind-config-too-many-tokens
      (tailwind-config
       :colors     '((:a . "1") (:b . "2") (:c . "3") (:d . "4") (:e . "5"))
       :typography '((:family . "\"JetBrains Mono\", monospace"))))))

(test regression-tailwind-config-bounded-keyword-pool
  "TAILWIND-CONFIG must not intern new keywords. Compare the keyword
   package's symbol count before and after a series of calls with the
   default keyword-based palette — the count must not grow."
  (let* ((before (let ((n 0))
                   (do-symbols (s :keyword) (declare (ignore s)) (incf n))
                   n)))
    (dotimes (_ 50)
      (tailwind-config
       :colors      '((:primary . "#00FF41")
                      (:secondary . "#FF006E")
                      (:accent . "#FFB000"))
       :typography  '((:family . "\"JetBrains Mono\", monospace"))))
    (let ((after (let ((n 0))
                   (do-symbols (s :keyword) (declare (ignore s)) (incf n))
                   n)))
      (is (= before after)
          (format nil "keyword count grew by ~D across 50 tailwind-config calls"
                  (- after before))))))

(test regression-tw-arbitrary-strips-quotes-and-brackets
  "tw-arbitrary drops characters that could break out of the
   `prefix-[value]` arbitrary-value syntax — spaces, `]`, quotes, and
   angle brackets — so a hostile token value cannot inject a sibling
   class or an attribute/tag boundary. Legitimate parens/commas survive."
  (let ((out (tw-arbitrary "bg" "url('x')\"<y>]z evil")))
    (is (null (find #\" out)))
    (is (null (find #\' out)))
    (is (null (find #\< out)))
    (is (null (find #\> out)))
    (is (null (find #\Space out)))
    (is (string= "bg-[url(x)yzevil]" out)))
  (is (string= "w-[clamp(1rem,5vw,3rem)]"
               (tw-arbitrary "w" "clamp(1rem, 5vw, 3rem)"))))

(test regression-tw-arbitrary-strips-unquoted-attr-breakers
  "A-L02: tw-arbitrary also drops `{ } ; = \\` backtick and control chars so
   the token is inert in an UNQUOTED attribute sink (where space/=/backtick
   would start a new attribute), not only the double-quoted class attribute.
   Legitimate CSS-value characters — parens, commas, `#`, `%` — survive."
  (let ((out (tw-arbitrary "bg" (format nil "a{b}c;d=e\\f`g~Ch~Ci" #\Newline #\Tab))))
    (is (null (find #\{ out)))
    (is (null (find #\} out)))
    (is (null (find #\; out)))
    (is (null (find #\= out)))
    (is (null (find #\\ out)))
    (is (null (find #\` out)))
    (is (null (find #\Newline out)))
    (is (null (find #\Tab out)))
    (is (string= "bg-[abcdefghi]" out)))
  (is (string= "w-[clamp(1rem,5vw,3rem)]"
               (tw-arbitrary "w" "clamp(1rem, 5vw, 3rem)")))
  (is (string= "bg-[#FF0000]" (tw-arbitrary "bg" "#FF0000"))))
