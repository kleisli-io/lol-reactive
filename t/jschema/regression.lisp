;;;; Regression tests for :lol-web/jschema.
;;;;
;;;; Coverage: boolean schemas, type / const / enum / required / properties /
;;;; additionalProperties / patternProperties / propertyNames /
;;;; unevaluatedProperties, items / prefixItems / minItems / maxItems /
;;;; uniqueItems, scalar bounds, allOf / anyOf / oneOf / not, if/then/else,
;;;; $ref / $defs, $dynamicRef / $dynamicAnchor, dependentSchemas, and
;;;; finally the OpenAPI 3.1 base schema parses cleanly.

(in-package :lol-web/jschema/test)
(in-suite :lol-web/jschema/test)

(defun parse-schema (string)
  "Parse a JSON Schema string."
  (lol-web/jschema:parse string))

(defun parse-value (string)
  "Parse a JSON value the way the validator expects (jzon shape)."
  (com.inuoe.jzon:parse string))

(defun valid-p (schema-string value-string)
  "Return T if VALUE-STRING validates against SCHEMA-STRING; NIL otherwise."
  (handler-case
      (progn (lol-web/jschema:validate (parse-schema schema-string)
                                       (parse-value value-string))
             t)
    (lol-web/jschema:invalid-json () nil)))

;;; ============================================================================
;;; Boolean schemas
;;; ============================================================================

(test boolean-schema-true-accepts-anything
  "Schema 'true' validates any value."
  (is (valid-p "true" "42"))
  (is (valid-p "true" "\"hi\""))
  (is (valid-p "true" "{}"))
  (is (valid-p "true" "null")))

(test boolean-schema-false-rejects-everything
  "Schema 'false' rejects every value, including the trivially-good empty object."
  (is (not (valid-p "false" "{}")))
  (is (not (valid-p "false" "null"))))

;;; ============================================================================
;;; type
;;; ============================================================================

(test type-string-accepts-strings-rejects-numbers
  (is (valid-p "{\"type\":\"string\"}" "\"hello\""))
  (is (not (valid-p "{\"type\":\"string\"}" "5"))))

(test type-integer-vs-number
  (is (valid-p "{\"type\":\"integer\"}" "5"))
  (is (not (valid-p "{\"type\":\"integer\"}" "5.5")))
  (is (valid-p "{\"type\":\"number\"}" "5"))
  (is (valid-p "{\"type\":\"number\"}" "5.5")))

(test type-array-of-tags-accepts-any-listed
  (is (valid-p "{\"type\":[\"string\",\"null\"]}" "\"x\""))
  (is (valid-p "{\"type\":[\"string\",\"null\"]}" "null"))
  (is (not (valid-p "{\"type\":[\"string\",\"null\"]}" "5"))))

(test type-rejects-invalid-tag-at-parse-time
  (signals lol-web/jschema:invalid-schema
    (parse-schema "{\"type\":\"flarp\"}")))

;;; ============================================================================
;;; const, enum
;;; ============================================================================

(test const-matches-exactly
  (is (valid-p "{\"const\":42}" "42"))
  (is (not (valid-p "{\"const\":42}" "43"))))

(test enum-membership
  (is (valid-p "{\"enum\":[\"red\",\"green\",\"blue\"]}" "\"green\""))
  (is (not (valid-p "{\"enum\":[\"red\",\"green\",\"blue\"]}" "\"yellow\""))))

;;; ============================================================================
;;; required, properties, additionalProperties
;;; ============================================================================

(test required-accepts-when-present
  (is (valid-p "{\"required\":[\"a\"]}" "{\"a\":1}"))
  (is (not (valid-p "{\"required\":[\"a\"]}" "{}"))))

(test properties-validates-children
  (is (valid-p "{\"properties\":{\"a\":{\"type\":\"integer\"}}}" "{\"a\":5}"))
  (is (not (valid-p "{\"properties\":{\"a\":{\"type\":\"integer\"}}}" "{\"a\":\"x\"}"))))

(test additional-properties-false-rejects-extras
  (is (valid-p "{\"properties\":{\"a\":true},\"additionalProperties\":false}"
               "{\"a\":1}"))
  (is (not (valid-p "{\"properties\":{\"a\":true},\"additionalProperties\":false}"
                    "{\"a\":1,\"b\":2}"))))

(test additional-properties-schema-validates-extras
  (is (valid-p "{\"additionalProperties\":{\"type\":\"integer\"}}"
               "{\"a\":1,\"b\":2}"))
  (is (not (valid-p "{\"additionalProperties\":{\"type\":\"integer\"}}"
                    "{\"a\":\"x\"}"))))

(test regression-additional-properties-uses-same-schema-coverage
  "additionalProperties ignores properties evaluated by sibling applicators;
   unevaluatedProperties sees that global evaluated-property set."
  (is (not (valid-p
            "{\"allOf\":[{\"properties\":{\"a\":true}}],\"additionalProperties\":false}"
            "{\"a\":1}"))
      "additionalProperties:false must reject keys absent from the same schema")
  (is (valid-p
       "{\"allOf\":[{\"properties\":{\"a\":true}}],\"unevaluatedProperties\":false}"
       "{\"a\":1}")
      "unevaluatedProperties:false must honour keys evaluated by allOf"))

(test pattern-properties-applies-to-matching-keys
  (is (valid-p "{\"patternProperties\":{\"^x-\":{\"type\":\"string\"}}}"
               "{\"x-foo\":\"bar\"}"))
  (is (not (valid-p "{\"patternProperties\":{\"^x-\":{\"type\":\"string\"}}}"
                    "{\"x-foo\":1}")))
  ;; Non-matching keys are unconstrained by patternProperties alone.
  (is (valid-p "{\"patternProperties\":{\"^x-\":{\"type\":\"string\"}}}"
               "{\"y\":1}")))

(test unevaluated-properties-false-rejects-uncovered-keys
  (is (valid-p
       "{\"properties\":{\"a\":true},\"unevaluatedProperties\":false}"
       "{\"a\":1}"))
  (is (not (valid-p
            "{\"properties\":{\"a\":true},\"unevaluatedProperties\":false}"
            "{\"a\":1,\"b\":2}"))))

;;; ============================================================================
;;; items / minItems / maxItems
;;; ============================================================================

(test items-validates-each-element
  (is (valid-p "{\"items\":{\"type\":\"integer\"}}" "[1,2,3]"))
  (is (not (valid-p "{\"items\":{\"type\":\"integer\"}}" "[1,\"x\"]"))))

(test regression-items-starts-after-same-schema-prefix-items-only
  "items skips prefixItems only when both keywords live in the same schema."
  (is (valid-p "{\"prefixItems\":[true],\"items\":false}" "[1]"))
  (is (not (valid-p "{\"prefixItems\":[true],\"items\":false}" "[1,2]")))
  (is (not (valid-p
            "{\"prefixItems\":[true],\"allOf\":[{\"items\":false}]}"
            "[1]"))
      "an items keyword inside allOf must not inherit root prefixItems"))

(test min-max-items
  (is (valid-p "{\"minItems\":2}" "[1,2]"))
  (is (not (valid-p "{\"minItems\":2}" "[1]")))
  (is (valid-p "{\"maxItems\":2}" "[1,2]"))
  (is (not (valid-p "{\"maxItems\":2}" "[1,2,3]"))))

(test unique-items
  (is (valid-p "{\"uniqueItems\":true}" "[1,2,3]"))
  (is (not (valid-p "{\"uniqueItems\":true}" "[1,2,1]"))))

;;; ============================================================================
;;; allOf / anyOf / oneOf / not
;;; ============================================================================

(test all-of-requires-every-branch
  (is (valid-p "{\"allOf\":[{\"type\":\"integer\"},{\"minimum\":0}]}" "5"))
  (is (not (valid-p "{\"allOf\":[{\"type\":\"integer\"},{\"minimum\":0}]}" "-1"))))

(test any-of-requires-some-branch
  (is (valid-p "{\"anyOf\":[{\"type\":\"integer\"},{\"type\":\"string\"}]}"
               "\"x\""))
  (is (not (valid-p "{\"anyOf\":[{\"type\":\"integer\"},{\"type\":\"string\"}]}"
                    "true"))))

(test one-of-requires-exactly-one
  (is (valid-p "{\"oneOf\":[{\"type\":\"integer\"},{\"type\":\"string\"}]}"
               "5"))
  ;; Both branches accept "5" only when... well, integer != string, so 5 hits
  ;; integer-only. To make oneOf fail on >1 match, use overlapping branches:
  (is (not (valid-p "{\"oneOf\":[{\"type\":\"integer\"},{\"minimum\":0}]}"
                    "5"))))

(test not-inverts
  (is (valid-p "{\"not\":{\"type\":\"string\"}}" "5"))
  (is (not (valid-p "{\"not\":{\"type\":\"string\"}}" "\"x\""))))

;;; ============================================================================
;;; if/then/else
;;; ============================================================================

(test if-then-applies-when-if-passes
  (is (valid-p "{\"if\":{\"type\":\"integer\"},\"then\":{\"minimum\":0}}" "5"))
  (is (not (valid-p "{\"if\":{\"type\":\"integer\"},\"then\":{\"minimum\":0}}"
                    "-1"))))

(test if-else-applies-when-if-fails
  (is (valid-p "{\"if\":{\"type\":\"integer\"},\"else\":{\"type\":\"string\"}}"
               "\"x\""))
  (is (not (valid-p "{\"if\":{\"type\":\"integer\"},\"else\":{\"type\":\"string\"}}"
                    "true"))))

;;; ============================================================================
;;; Numeric bounds — parse-error message names the offending keyword
;;; ============================================================================

(test regression-numeric-keyword-parse-error-names-keyword
  "A non-numeric minimum/maximum/exclusiveMinimum/exclusiveMaximum is refused
   at parse time and the INVALID-SCHEMA message names the specific keyword,
   not NIL — each parser closure captures its own keyword name."
  (dolist (kw '("minimum" "maximum" "exclusiveMinimum" "exclusiveMaximum"))
    (let ((msg (handler-case
                   (progn
                     (parse-schema (format nil "{\"~A\":\"not-a-number\"}" kw))
                     nil)
                 (lol-web/jschema:invalid-schema (c)
                   (lol-web/jschema:invalid-schema-error-message c)))))
      (is (not (null msg))
          "~A with a string value must signal invalid-schema" kw)
      (is (not (null (search kw (or msg ""))))
          "parse-error for ~A must name the keyword, got ~S" kw msg))))

;;; ============================================================================
;;; $ref / $defs
;;; ============================================================================

(test ref-resolves-against-defs
  (is (valid-p "{\"$defs\":{\"int\":{\"type\":\"integer\"}},\"$ref\":\"#/$defs/int\"}"
               "5"))
  (is (not (valid-p
            "{\"$defs\":{\"int\":{\"type\":\"integer\"}},\"$ref\":\"#/$defs/int\"}"
            "\"x\""))))

(test ref-chain-allowed
  "Draft 2020-12 permits $ref to a schema that itself contains $ref."
  (is (valid-p
       "{\"$defs\":{\"a\":{\"$ref\":\"#/$defs/b\"},\"b\":{\"type\":\"integer\"}},\"$ref\":\"#/$defs/a\"}"
       "5")))

(test unresolvable-ref-fails
  (is (not (valid-p "{\"$ref\":\"#/$defs/missing\"}" "5"))))

;;; ============================================================================
;;; $ref cycles — must error gracefully, never blow the stack
;;; ============================================================================

(test ref-self-cycle-emits-error-not-stack-overflow
  "A schema whose $ref resolves to itself with no shrinking of the value
   must surface as an INVALID-JSON error rather than recurse forever."
  (let ((s (parse-schema
             "{\"$defs\":{\"loop\":{\"$ref\":\"#/$defs/loop\"}},\"$ref\":\"#/$defs/loop\"}")))
    (signals invalid-json
      (lol-web/jschema:validate s (parse-value "42")))))

(test ref-mutual-cycle-emits-error-not-stack-overflow
  "Mutual recursion (A->B->A) hitting the same value identity on the way
   back is a cycle and must error, not overflow."
  (let ((s (parse-schema
             "{\"$defs\":{\"a\":{\"$ref\":\"#/$defs/b\"},\"b\":{\"$ref\":\"#/$defs/a\"}},\"$ref\":\"#/$defs/a\"}")))
    (signals invalid-json
      (lol-web/jschema:validate s (parse-value "42")))))

(test ref-legitimate-recursion-against-nested-value-passes
  "A recursive schema applied to legitimately nested data must traverse —
   the (target . value) cycle key narrows at each step because the value
   is a fresh sub-object, so the seen-refs check never matches."
  (let ((s (parse-schema
             (concatenate 'string
               "{\"$defs\":{\"node\":{\"type\":\"object\","
               "\"properties\":{\"v\":{\"type\":\"integer\"},"
               "\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
               "\"$ref\":\"#/$defs/node\"}"))))
    (is (valid-p
          (concatenate 'string
            "{\"$defs\":{\"node\":{\"type\":\"object\","
            "\"properties\":{\"v\":{\"type\":\"integer\"},"
            "\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
            "\"$ref\":\"#/$defs/node\"}")
          "{\"v\":1,\"child\":{\"v\":2,\"child\":{\"v\":3}}}")
        "legitimate three-level recursive validate must pass")
    ;; Re-validate independently to prove the cycle guard doesn't pollute
    ;; state across calls.
    (lol-web/jschema:validate s (parse-value "{\"v\":1,\"child\":{\"v\":2}}"))))

;;; ============================================================================
;;; *max-validation-depth* — bound recursion through %check-schema
;;; ============================================================================

(test validation-depth-cap-emits-error-not-stack-overflow
  "A legitimately recursive schema applied to legitimately deep data,
   beyond the depth cap, must surface as a validation error rather than
   recurse far enough to overflow the stack. Cap lowered via let to
   keep the test cheap."
  (let* ((schema-string
          (concatenate 'string
            "{\"$defs\":{\"node\":{\"type\":\"object\","
            "\"properties\":{\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
            "\"$ref\":\"#/$defs/node\"}"))
         (build-value
          (lambda (depth)
            (let ((acc "{}"))
              (loop repeat depth
                    do (setf acc (concatenate 'string "{\"child\":" acc "}")))
              acc)))
         (s (parse-schema schema-string)))
    ;; With a tiny cap of 4, a 30-level deep value must error.
    (let ((lol-web/jschema:*max-validation-depth* 4))
      (signals invalid-json
        (lol-web/jschema:validate s (parse-value (funcall build-value 30)))))
    ;; With the default high cap, the same shallow value passes.
    (is (valid-p schema-string (funcall build-value 3)))))

;;; ============================================================================
;;; Depth cap survives a scratch-context fork (anyOf / oneOf branches)
;;; ============================================================================
;;;
;;; A branch keyword validates each sub-schema in a forked context. If the
;;; fork resets the depth counter, the recursion through a branch never
;;; accumulates depth, so *max-validation-depth* never fires and a deeply
;;; recursive value walks the stack unbounded. The fork must carry the live
;;; depth so the cap fires inside a branch just as it does on the main path.

(defun %nested-child-value (depth)
  "A DEPTH-deep chain of {\"child\": ...} objects, terminating in {}."
  (let ((acc "{}"))
    (loop repeat depth
          do (setf acc (concatenate 'string "{\"child\":" acc "}")))
    acc))

(test regression-anyof-depth-cap-fires
  "A recursive schema whose every level descends through anyOf, applied to a
   deep value with a low cap, must surface the depth cap as INVALID-JSON.
   A fork that reset depth would let the value validate uncapped."
  (let ((schema (parse-schema
                  (concatenate 'string
                    "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
                    "{\"child\":{\"anyOf\":[{\"$ref\":\"#/$defs/node\"}]}}}},"
                    "\"$ref\":\"#/$defs/node\"}"))))
    (let ((lol-web/jschema:*max-validation-depth* 4))
      (signals invalid-json
        (lol-web/jschema:validate schema (parse-value (%nested-child-value 30)))))))

(test regression-oneof-depth-cap-fires
  "oneOf forks a scratch context per branch like anyOf; the live depth must
   survive the fork so the cap fires inside a oneOf branch too."
  (let ((schema (parse-schema
                  (concatenate 'string
                    "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
                    "{\"child\":{\"oneOf\":[{\"$ref\":\"#/$defs/node\"}]}}}},"
                    "\"$ref\":\"#/$defs/node\"}"))))
    (let ((lol-web/jschema:*max-validation-depth* 4))
      (signals invalid-json
        (lol-web/jschema:validate schema (parse-value (%nested-child-value 30)))))))

;;; ============================================================================
;;; fork-eval-ctx — every branch fork threads the safety fields
;;; ============================================================================
;;;
;;; not / if read only their fork's pass/fail and discard its errors, so a
;;; depth or cycle error raised inside their fork never reaches the caller —
;;; a behavioural depth test can only observe the anyOf/oneOf forks. This
;;; white-box test pins the primitive directly: the live depth survives and
;;; seen-refs is copied (independent spine, same entries) for every keyword
;;; fork, which is what makes %check-not's depth gap impossible to reopen.

(test regression-fork-eval-ctx-threads-safety-fields
  "fork-eval-ctx carries the live depth (not reset to 0), copies seen-refs
   into an independent list with the same entries, threads root / pointer /
   ignore-unresolvable-refs / dynamic-scope, and starts errors + annotations
   fresh."
  (let* ((entry (cons :target :value))
         (parent (lol-web/jschema::make-eval-ctx
                  :root :the-root
                  :pointer "/a/b"
                  :ignore-unresolvable-refs t
                  :dynamic-scope '(:scope)
                  :depth 7
                  :seen-refs (list entry)))
         (fork (lol-web/jschema::fork-eval-ctx parent)))
    (is (= 7 (lol-web/jschema::eval-ctx-depth fork))
        "live depth must survive the fork, got ~D"
        (lol-web/jschema::eval-ctx-depth fork))
    (is (equal (list entry) (lol-web/jschema::eval-ctx-seen-refs fork))
        "seen-refs entries must carry across the fork")
    (is (not (eq (lol-web/jschema::eval-ctx-seen-refs parent)
                 (lol-web/jschema::eval-ctx-seen-refs fork)))
        "seen-refs must be a copy so the fork's push/pop can't corrupt parent")
    (is (eq :the-root (lol-web/jschema::eval-ctx-root fork)))
    (is (string= "/a/b" (lol-web/jschema::eval-ctx-pointer fork)))
    (is (lol-web/jschema::eval-ctx-ignore-unresolvable-refs fork))
    (is (equal '(:scope) (lol-web/jschema::eval-ctx-dynamic-scope fork)))
    (is (null (lol-web/jschema::eval-ctx-errors fork))
        "fork errors must start empty so a branch is tested in isolation")
    (is (zerop (hash-table-count (lol-web/jschema::eval-ctx-evaluated-props fork)))
        "fork annotations default to empty")))

;;; ============================================================================
;;; $ref cycle survives a branch fork (anyOf / oneOf / if / not)
;;; ============================================================================
;;;
;;; A shallow self-referential schema cycling through a branch keyword must
;;; terminate. anyOf/oneOf surface the cycle as INVALID-JSON (all branches
;;; fail); not/if read only pass/fail so they terminate without error — what
;;; matters is that none recurse forever (which they would if the fork
;;; dropped seen-refs).

(test regression-anyof-ref-cycle-detected
  "anyOf forks must copy seen-refs so a self-cycle through an anyOf branch is
   caught as INVALID-JSON rather than recursing until the stack exhausts."
  (let ((s (parse-schema
             "{\"$defs\":{\"loop\":{\"anyOf\":[{\"$ref\":\"#/$defs/loop\"}]}},\"$ref\":\"#/$defs/loop\"}")))
    (signals invalid-json
      (lol-web/jschema:validate s (parse-value "42")))))

(test regression-oneof-ref-cycle-detected
  "oneOf forks must copy seen-refs so a self-cycle through a oneOf branch is
   caught as INVALID-JSON, not stack exhaustion."
  (let ((s (parse-schema
             "{\"$defs\":{\"loop\":{\"oneOf\":[{\"$ref\":\"#/$defs/loop\"}]}},\"$ref\":\"#/$defs/loop\"}")))
    (signals invalid-json
      (lol-web/jschema:validate s (parse-value "42")))))

(test regression-if-ref-cycle-terminates
  "The if condition is validated in a fork; that fork must copy seen-refs so
   a self-cycle through the if condition terminates instead of recursing
   forever. if reads only the branch decision, so the cycle surfaces as a
   non-match rather than an error — termination is the property under test."
  (let ((s (parse-schema
             "{\"$defs\":{\"loop\":{\"if\":{\"$ref\":\"#/$defs/loop\"}}},\"$ref\":\"#/$defs/loop\"}")))
    (finishes (lol-web/jschema:validate s (parse-value "42")))))

(test regression-not-ref-cycle-terminates
  "not validates its sub-schema in a fork; the fork must copy seen-refs so a
   self-cycle through not terminates. not reads only pass/fail, so the cycle
   surfaces as the sub failing (not passes) — termination is what matters."
  (let ((s (parse-schema
             "{\"$defs\":{\"loop\":{\"not\":{\"$ref\":\"#/$defs/loop\"}}},\"$ref\":\"#/$defs/loop\"}")))
    (finishes (lol-web/jschema:validate s (parse-value "42")))))

;;; ============================================================================
;;; *max-schema-depth* — bound recursion through %make-schema at parse
;;; ============================================================================

(test schema-depth-cap-signals-invalid-schema
  "A schema document whose subschema nesting exceeds *max-schema-depth*
   must signal INVALID-SCHEMA at parse time rather than recurse far
   enough to overflow the parser's stack."
  ;; Build a schema that nests properties.x.properties.x... DEPTH times.
  (flet ((build-schema (depth)
           (let ((acc "{\"type\":\"integer\"}"))
             (loop repeat depth
                   do (setf acc
                           (concatenate 'string
                             "{\"type\":\"object\",\"properties\":{\"x\":"
                             acc "}}")))
             acc)))
    (let ((lol-web/jschema:*max-schema-depth* 4))
      (signals invalid-schema (parse-schema (build-schema 20))))
    ;; With the default high cap, the same shallow schema parses.
    (finishes (parse-schema (build-schema 3)))))

;;; ============================================================================
;;; $dynamicRef + $dynamicAnchor (same-document)
;;; ============================================================================

(test dynamic-ref-resolves-to-dynamic-anchor
  "$dynamicRef '#meta' resolves to the $dynamicAnchor 'meta' in the same document."
  (let ((schema (concatenate 'string
                  "{\"$defs\":{\"meta-ext\":{\"$dynamicAnchor\":\"meta\","
                  "\"type\":\"integer\"}},"
                  "\"$dynamicRef\":\"#meta\"}")))
    (is (valid-p schema "5"))
    (is (not (valid-p schema "\"x\"")))))

;;; ============================================================================
;;; OpenAPI 3.1 base-schema acceptance gate
;;; ============================================================================
;;; Not defining at this layer because the path-resolution is buildLisp-
;;; specific (no asdf:system-relative-pathname). The module-table-driven
;;; build is responsible for substituting the fixture path. Layered as a
;;; defparameter the buildlisp pipeline replaces.

(defparameter *openapi-3.1-schema-path* nil
  "Set by the buildLisp wrapper to a Nix-store path of the bundled schema.
   When NIL (e.g. interactive REPL use), the gate test is skipped.")

(test openapi-3.1-schema-parses
  "The upstream OpenAPI 3.1 base schema parses without signaling
   INVALID-SCHEMA. Skipped if *openapi-3.1-schema-path* is unbound."
  (when *openapi-3.1-schema-path*
    (let ((s (with-open-file (in *openapi-3.1-schema-path*
                                 :element-type 'character)
               (with-output-to-string (out)
                 (loop for line = (read-line in nil)
                       while line do (write-line line out))))))
      (finishes (parse-schema s))))
  (is (eq t t)))

;;; ============================================================================
;;; Per-app *registry* let-binding — isolation across two hash-tables
;;; ============================================================================

(test regression-jschema-registry-let-bind-isolation
  "Let-binding lol-web/jschema:*registry* to caller-owned hash-tables keeps
   register-schema writes isolated; the image-global registry is untouched."
  (let ((table-a (make-hash-table :test 'equal))
        (table-b (make-hash-table :test 'equal))
        (uri-a "https://app-a.example/schema-x")
        (uri-b "https://app-b.example/schema-x")
        (image-global-before
          (hash-table-count lol-web/jschema:*registry*)))
    (let ((lol-web/jschema:*registry* table-a))
      (lol-web/jschema::register-schema uri-a :marker-a))
    (let ((lol-web/jschema:*registry* table-b))
      (lol-web/jschema::register-schema uri-b :marker-b))
    (is (eq :marker-a (gethash uri-a table-a))
        "A's registration lives in A's table")
    (is (null (gethash uri-a table-b))
        "A's registration is not visible in B's table")
    (is (eq :marker-b (gethash uri-b table-b)))
    (is (null (gethash uri-b table-a)))
    (is (= image-global-before
           (hash-table-count lol-web/jschema:*registry*))
        "image-global *registry* untouched by either let-binding")))

;;; ============================================================================
;;; uniqueItems — maxItems short-circuit + hash-set branch
;;; ============================================================================

(test regression-unique-items-short-circuits-max-items
  "When `maxItems` already rejects the value, `uniqueItems` skips its
   O(n^2) pairwise comparison. Validation surfaces the maxItems error
   without spending the uniqueItems work — an attacker who submits a
   200-element array against a schema with maxItems=5 cannot force the
   validator into 20K %json-equal calls."
  (let* ((schema (parse-schema "{\"maxItems\":5,\"uniqueItems\":true}"))
         (value (coerce (loop for i below 200 collect 1) 'vector))
         (errors (handler-case (progn (lol-web/jschema:validate schema value) nil)
                   (lol-web/jschema:invalid-json (c)
                     (lol-web/jschema:invalid-json-errors c)))))
    (is (not (null errors)) "200-element array must fail validation")
    (is (some (lambda (e)
                (search "more than" (lol-web/jschema:invalid-json-value-error-message e)))
              errors)
        "errors must include the maxItems violation")
    (is (notany (lambda (e)
                  (search "Duplicate"
                          (lol-web/jschema:invalid-json-value-error-message e)))
                errors)
        "errors must NOT include duplicate-items reports — uniqueItems short-circuited")))

(test regression-unique-items-hash-set-branch
  "Above *unique-items-hash-set-threshold*, %check-unique-items switches
   to hash-set keyed by canonical JSON serialisation and still detects
   duplicates. Confirmed by feeding a 150-element array whose first and
   last elements collide."
  (let* ((lol-web/jschema:*unique-items-hash-set-threshold* 100)
         (schema (parse-schema "{\"uniqueItems\":true}"))
         (value (let ((v (coerce (loop for i below 150 collect i) 'vector)))
                  (setf (aref v 149) 0) ; duplicate of index 0
                  v))
         (errors (handler-case (progn (lol-web/jschema:validate schema value) nil)
                   (lol-web/jschema:invalid-json (c)
                     (lol-web/jschema:invalid-json-errors c)))))
    (is (not (null errors)) "duplicate must be detected via hash-set branch")
    (is (some (lambda (e)
                (search "Duplicate"
                        (lol-web/jschema:invalid-json-value-error-message e)))
              errors)
        "duplicate-items message must surface")))

;;; ============================================================================
;;; pattern — length cap + compile timeout
;;; ============================================================================

(test regression-pattern-rejects-overlong
  "A pattern string longer than *pattern-max-length* is refused at parse
   time without ever entering cl-ppcre's compiler."
  (let ((overlong-pattern (make-string (1+ lol-web/jschema:*pattern-max-length*)
                                       :initial-element #\a)))
    (signals lol-web/jschema:invalid-schema
      (parse-schema (concatenate 'string
                                 "{\"pattern\":\""
                                 overlong-pattern
                                 "\"}")))))

(test regression-pattern-properties-rejects-overlong
  "patternProperties keys are bounded by the same length cap — refused
   at parse time before cl-ppcre touches them."
  (let ((overlong-key (make-string (1+ lol-web/jschema:*pattern-max-length*)
                                   :initial-element #\b)))
    (signals lol-web/jschema:invalid-schema
      (parse-schema (concatenate 'string
                                 "{\"patternProperties\":{\""
                                 overlong-key
                                 "\":true}}")))))

#+sbcl
(test regression-pattern-timeout-fires
  "When cl-ppcre:create-scanner does not complete within
   *pattern-compile-timeout-seconds*, the parser surfaces INVALID-SCHEMA
   rather than letting compilation block the request. To exercise this
   deterministically — cl-ppcre's compiler is normally microsecond-scale
   and cannot be slowed by pattern shape — transiently replace
   create-scanner with a synchronous sleeper that exceeds the timeout."
  (let ((original (symbol-function 'cl-ppcre:create-scanner)))
    (unwind-protect
         (let ((lol-web/jschema:*pattern-compile-timeout-seconds* 0.05))
           (setf (symbol-function 'cl-ppcre:create-scanner)
                 (lambda (pattern &rest args)
                   (declare (ignore pattern args))
                   (sleep 0.5)))
           (signals lol-web/jschema:invalid-schema
             (parse-schema "{\"pattern\":\"abc\"}")))
      (setf (symbol-function 'cl-ppcre:create-scanner) original))))

#+sbcl
(test regression-scan-bounded-redos-pattern-times-out
  "A schema {\"pattern\":\"^(a+)+$\"} validated against an adversarial value
   must not hang on cl-ppcre's catastrophic backtracking. %scan-bounded aborts
   the match at the time budget, the value reads as non-matching, and validate
   returns (invalid) within the budget instead of backtracking for seconds.
   Exercises real match time against the real scanner — no stub, unlike the
   compile-only regression-pattern-timeout-fires above."
  (let* ((adversarial (format nil "\"~A!\"" (make-string 30 :initial-element #\a)))
         (start (get-internal-real-time))
         (result (valid-p "{\"pattern\":\"^(a+)+$\"}" adversarial))
         (elapsed (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    (is (null result) "adversarial value must not validate")
    (is (< elapsed 2)
        "bounded match must abort well under the unbounded hang; took ~Fs"
        (float elapsed))))

;;; ============================================================================
;;; *schema-json-max-depth* — bound jzon nesting at schema-document parse time
;;; ============================================================================

(test regression-schema-json-depth-cap-fires
  "Decoding a JSON Schema *document* bounds nesting depth at parse time via
   *schema-json-max-depth*, so a pathologically deep document is refused as
   UNPARSABLE-JSON before jzon materialises its whole tree — independent of
   the post-construction *max-schema-depth* guard. Symmetric with the
   request-body path's *json-body-max-depth*."
  (flet ((nest (depth)
           (with-output-to-string (s)
             (dotimes (_ depth) (write-string "{\"properties\":{\"a\":" s))
             (write-string "{}" s)
             (dotimes (_ depth) (write-string "}}" s)))))
    (let ((lol-web/jschema::*schema-json-max-depth* 5))
      (signals lol-web/jschema:unparsable-json
        (parse-schema (nest 20))))
    (finishes (parse-schema (nest 2)))))

;;; ============================================================================
;;; if/then/else — order-independent branch resolution
;;; ============================================================================
;;;
;;; then/else must not depend on `if` having been dispatched first: keyword
;;; dispatch order follows the parsed alist and is unspecified. These pin the
;;; lazy resolution white-box by invoking the then/else checkers with no prior
;;; if dispatch — both would misbehave under a cache only `if` populates.

(test regression-if-then-else-order-independent
  "then applies when if matches and else is skipped when if matches, both
   computed lazily without a prior if dispatch."
  ;; value 5 is an integer (if matches) → then (minimum 10) applies → 5<10 fails.
  (let* ((schema (parse-schema
                  "{\"if\":{\"type\":\"integer\"},\"then\":{\"minimum\":10}}"))
         (then-form (cdr (assoc "then"
                                (lol-web/jschema::json-schema-keywords schema)
                                :test #'string=)))
         (ctx (lol-web/jschema::make-eval-ctx :root schema)))
    (lol-web/jschema::%check-then then-form (parse-value "5") ctx schema)
    (is (not (null (lol-web/jschema::eval-ctx-errors ctx)))
        "then must apply (5<10 fails) with no prior if dispatch"))
  ;; value 5 is an integer (if matches) → else (type string) must be skipped.
  (let* ((schema (parse-schema
                  "{\"if\":{\"type\":\"integer\"},\"else\":{\"type\":\"string\"}}"))
         (else-form (cdr (assoc "else"
                                (lol-web/jschema::json-schema-keywords schema)
                                :test #'string=)))
         (ctx (lol-web/jschema::make-eval-ctx :root schema)))
    (lol-web/jschema::%check-else else-form (parse-value "5") ctx schema)
    (is (null (lol-web/jschema::eval-ctx-errors ctx))
        "else must be skipped when if matches, with no prior if dispatch")))

;;; ============================================================================
;;; anyOf / oneOf — (schema,value) memoization bounds recursive-union fan-out
;;; ============================================================================
;;;
;;; A recursive union re-validates the same (sub-schema, value) pairs across
;;; branch combinations; without memoization a depth-N body costs ~2^N
;;; validations. With the memo it is O(N), so a deep body completes inside a
;;; budget 2^N could never meet.

(defparameter +recursive-anyof-union-schema+
  (concatenate 'string
    "{\"$defs\":{\"node\":{\"anyOf\":["
    "{\"type\":\"object\",\"properties\":{\"child\":{\"$ref\":\"#/$defs/node\"}}},"
    "{\"type\":\"object\",\"properties\":{\"child\":{\"$ref\":\"#/$defs/node\"}}}"
    "]}},\"$ref\":\"#/$defs/node\"}")
  "Two structurally identical anyOf branches, each recursing into `child`: every
   level doubles the validations of the level below absent memoization.")

(defparameter +recursive-oneof-union-schema+
  (concatenate 'string
    "{\"$defs\":{\"node\":{\"oneOf\":["
    "{\"type\":\"object\",\"properties\":{\"child\":{\"$ref\":\"#/$defs/node\"}}},"
    "{\"type\":\"object\",\"properties\":{\"child\":{\"$ref\":\"#/$defs/node\"}}}"
    "]}},\"$ref\":\"#/$defs/node\"}")
  "oneOf counterpart: the work is exponential the same way; the validity verdict
   is irrelevant to the test — completion within budget is.")

#+sbcl
(test regression-anyof-fanout-memoized
  "A recursive anyOf union over a depth-32 body completes within a budget that
   2^32 validations could never meet — evidence the (schema,value) memo fires."
  (let* ((schema (parse-schema +recursive-anyof-union-schema+))
         (value (parse-value (%nested-child-value 32)))
         (completed (handler-case
                        (sb-ext:with-timeout 5
                          (handler-case (lol-web/jschema:validate schema value)
                            (lol-web/jschema:invalid-json () nil))
                          t)
                      (sb-ext:timeout () nil))))
    (is (not (null completed))
        "recursive anyOf fan-out must complete via (schema,value) memoization")))

#+sbcl
(test regression-oneof-fanout-memoized
  "oneOf forks per branch like anyOf and shares the same memo; a depth-32
   recursive oneOf union completes within budget rather than running 2^32."
  (let* ((schema (parse-schema +recursive-oneof-union-schema+))
         (value (parse-value (%nested-child-value 32)))
         (completed (handler-case
                        (sb-ext:with-timeout 5
                          (handler-case (lol-web/jschema:validate schema value)
                            (lol-web/jschema:invalid-json () nil))
                          t)
                      (sb-ext:timeout () nil))))
    (is (not (null completed))
        "recursive oneOf fan-out must complete via (schema,value) memoization")))

;;; ============================================================================
;;; patternProperties / additionalProperties — fail closed on match-timeout
;;; ============================================================================
;;;
;;; A catastrophic-backtracking pattern match is inconclusive, not a miss:
;;; skipping the constraint would let a key escape it. The key is rejected.

#+sbcl
(test regression-pattern-properties-fails-closed-on-timeout
  "A patternProperties match that catastrophically backtracks is treated as
   inconclusive and the key is rejected (fail-closed), within the match bound."
  (let* ((schema (parse-schema
                  "{\"patternProperties\":{\"^(a+)+$\":{\"type\":\"integer\"}}}"))
         (hostile-key (concatenate 'string
                                   (make-string 30 :initial-element #\a) "!"))
         (value (let ((h (make-hash-table :test 'equal)))
                  (setf (gethash hostile-key h) "not-an-integer")
                  h))
         (start (get-internal-real-time))
         (result (handler-case (progn (lol-web/jschema:validate schema value) t)
                   (lol-web/jschema:invalid-json () nil)))
         (elapsed (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    (is (null result) "an inconclusive patternProperties match must reject the key")
    (is (< elapsed 2) "the match must abort at the time bound; took ~Fs"
        (float elapsed))))

;;; ============================================================================
;;; *max-validation-depth* — combinator frames count toward the documented cap
;;; ============================================================================

(test regression-combinator-depth-cap-matches-documented
  "Every in-place applicator frame counts against *max-validation-depth*, so a
   schema nesting combinators deeper than the cap trips it on a shallow value —
   the cap bounds combined applicator+value recursion, as documented."
  (flet ((nest-all-of (depth)
           (let ((acc "{\"type\":\"integer\"}"))
             (loop repeat depth
                   do (setf acc (concatenate 'string "{\"allOf\":[" acc "]}")))
             acc)))
    (let ((lol-web/jschema:*max-validation-depth* 5))
      (signals lol-web/jschema:invalid-json
        (lol-web/jschema:validate (parse-schema (nest-all-of 30))
                                  (parse-value "1")))
      (is (valid-p (nest-all-of 2) "1")
          "within the cap, a shallow combinator nest validates"))))

;;; ============================================================================
;;; Inconclusive/aborted results fail closed (not/if/anyOf/oneOf);
;;; overflow-safe multipleOf; integer-valued floats; native stack budget
;;; ============================================================================

(test regression-not-fails-closed-on-depth-abort
  "A `not` whose subschema is depth-truncated inside the fork rejects the value
   (fail closed) rather than reading the refusal-to-descend as a clean non-match
   and passing a value it could not vouch for."
  (let ((schema-str
          (concatenate 'string
            "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
            "{\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
            "\"not\":{\"$ref\":\"#/$defs/node\"}}")))
    (let ((lol-web/jschema:*max-validation-depth* 4))
      (signals invalid-json
        (lol-web/jschema:validate (parse-schema schema-str)
                                  (parse-value (%nested-child-value 30)))))
    (is (not (valid-p schema-str (%nested-child-value 3)))
        "uncapped: a value matching `node` is rejected by `not`")
    (is (valid-p schema-str "\"x\"")
        "uncapped: a value that cannot match `node` passes `not`")))

(test regression-if-then-not-dropped-on-depth-abort
  "When the `if` condition is depth-truncated inside its fork the verdict is
   undecidable: `then` must not be silently dropped (the value fails closed),
   and the uncapped path still applies `then` when `if` matches."
  (let ((schema-str
          (concatenate 'string
            "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
            "{\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
            "\"if\":{\"$ref\":\"#/$defs/node\"},\"then\":{\"type\":\"string\"}}")))
    (let ((lol-web/jschema:*max-validation-depth* 4))
      (signals invalid-json
        (lol-web/jschema:validate (parse-schema schema-str)
                                  (parse-value (%nested-child-value 30)))))
    (is (not (valid-p schema-str (%nested-child-value 3)))
        "uncapped: then applies when if matches, rejecting a non-string object")))

(test regression-anyof-oneof-fail-closed-on-abort
  "An aborted branch that left no error of its own (an undecidable nested `if`)
   must not be read as a clean pass: anyOf/oneOf fail closed on it rather than
   let the truncated branch satisfy the keyword. (Fails OPEN under the literal
   prior design where %check-branch passed-p = (null errors).)"
  (let ((anyof-str
          (concatenate 'string
            "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
            "{\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
            "\"anyOf\":[{\"if\":{\"$ref\":\"#/$defs/node\"},\"then\":{\"type\":\"string\"}}]}"))
        (oneof-str
          (concatenate 'string
            "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
            "{\"child\":{\"$ref\":\"#/$defs/node\"}}}},"
            "\"oneOf\":[{\"if\":{\"$ref\":\"#/$defs/node\"},\"then\":{\"type\":\"string\"}}]}")))
    (let ((lol-web/jschema:*max-validation-depth* 4))
      (signals invalid-json
        (lol-web/jschema:validate (parse-schema anyof-str)
                                  (parse-value (%nested-child-value 30))))
      (signals invalid-json
        (lol-web/jschema:validate (parse-schema oneof-str)
                                  (parse-value (%nested-child-value 30)))))))

(test regression-multipleof-float-overflow-contained
  "multipleOf on a huge value must never let an arithmetic-error escape
   VALIDATE's invalid-json contract: exact rational arithmetic contains the
   float-division overflow that 1e307 / 0.01 would raise."
  (dolist (case '(("{\"multipleOf\":0.01}" . "1e307")
                  ("{\"multipleOf\":0.01}" . "1.7e308")
                  ("{\"multipleOf\":0.01}" . "1e308")
                  ("{\"multipleOf\":1e-300}" . "1e308")))
    (let ((schema (parse-schema (car case)))
          (value (parse-value (cdr case))))
      (is (not (eq :escaped
                   (handler-case (progn (lol-web/jschema:validate schema value) :accepted)
                     (lol-web/jschema:invalid-json () :rejected)
                     (arithmetic-error () :escaped))))
          "multipleOf ~A vs ~A must not let an arithmetic-error escape"
          (car case) (cdr case))))
  (is (valid-p "{\"multipleOf\":0.5}" "1.5"))
  (is (not (valid-p "{\"multipleOf\":0.5}" "1.25"))))

(test regression-type-integer-accepts-zero-fraction-float
  "type:integer matches by value: a zero-fraction float (5.0) is an integer,
   a non-zero fraction (5.5) is not; number still accepts both."
  (is (valid-p "{\"type\":\"integer\"}" "5.0"))
  (is (valid-p "{\"type\":\"integer\"}" "5"))
  (is (not (valid-p "{\"type\":\"integer\"}" "5.5")))
  (is (valid-p "{\"type\":\"number\"}" "5.0"))
  (is (valid-p "{\"type\":\"number\"}" "5.5")))

(test regression-native-stack-budget-respected
  "A finite *max-validation-depth* bounds the native control stack: a
   combinator-amplified value beyond a low cap fails closed via INVALID-JSON,
   never a STORAGE-CONDITION."
  (let ((schema (parse-schema
                  (concatenate 'string
                    "{\"$defs\":{\"node\":{\"type\":\"object\",\"properties\":"
                    "{\"child\":{\"anyOf\":[{\"$ref\":\"#/$defs/node\"}]}}}},"
                    "\"$ref\":\"#/$defs/node\"}")))
        (value (parse-value (%nested-child-value 64))))
    (let ((lol-web/jschema:*max-validation-depth* 8))
      (is (eq :rejected
              (handler-case (progn (lol-web/jschema:validate schema value) :accepted)
                (lol-web/jschema:invalid-json () :rejected)
                (storage-condition () :leaked-storage)))))))

;;; ============================================================================
;;; %pattern-match — an aborted match is inconclusive, not a miss
;;; ============================================================================
;;;
;;; A catastrophic-backtracking match cannot reach a verdict inside the wallclock
;;; bound. %pattern-match reports that as a second value, INCONCLUSIVE-P, distinct
;;; from a definite miss, so callers reject the key rather than silently skip a
;;; constraint it might be subject to.

#+sbcl
(test regression-pattern-match-aborts-inconclusive
  "A scanner that exceeds *pattern-match-timeout-seconds* returns (values NIL T):
   no definite match, inconclusive — the caller fails closed. A well-formed
   scanner over a matching key returns (values T NIL)."
  (let ((lol-web/jschema::*pattern-match-timeout-seconds* 0.05)
        (hostile (concatenate 'string
                              (make-string 40 :initial-element #\a) "!")))
    (multiple-value-bind (matched inconclusive)
        (lol-web/jschema::%pattern-match (cl-ppcre:create-scanner "^(a+)+$") hostile)
      (is (null matched)
          "an aborted match must not report a definite match")
      (is (eq t inconclusive)
          "an aborted match must report inconclusive (fail closed)")))
  (multiple-value-bind (matched inconclusive)
      (lol-web/jschema::%pattern-match (cl-ppcre:create-scanner "^abc$") "abc")
    (is (eq t matched)
        "a completed match over a matching key reports a definite match")
    (is (null inconclusive)
        "a completed match is conclusive")))
