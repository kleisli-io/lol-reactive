;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/css — CSS infrastructure, tokens, generation, tailwind helpers
;;;;   src/css/{registry,tokens,generation,tailwind}.lisp

(in-package :cl-user)

(defpackage :lol-web/css
  (:use :cl :iterate)
  (:export
   ;; registry.lisp
   #:*component-css-registry*
   #:*css-load-order*
   #:make-css-module
   #:get-css-module
   #:get-component-css
   #:generate-all-component-css
   #:defcss
   #:clear-css-registry
   #:list-registered-css-components
   #:inspect-css-registry
   ;; generation.lisp
   #:css-rule
   #:css-rules
   #:css-section
   #:css-keyframes
   #:css-media
   #:css-var
   #:css-var-definition
   #:safe-css-selector-p
   #:safe-css-value-p
   ;; escape-css-value / escape-css-ident close the declaration/rule/
   ;; <style>-tag boundary (;}<>), NOT the value-internal url()/string context:
   ;; `"` `'` `(` `)` pass through by design (escaping them corrupts legitimate
   ;; framework values — see escape-css-value's RESIDUAL EXPOSURE docstring).
   ;; The `escape-` prefix names a boundary-escape, not a full sanitiser; do
   ;; NOT route attacker-controlled values into url()/content/font-family
   ;; through these alone.
   #:escape-css-value
   #:unsafe-css-selector
   ;; safety.lisp
   #:safe-css-payload-string
   #:safe-css-payload-string-p
   #:safe-css-payload-string-value
   #:make-safe-css-payload-string
   #:safe-css-ident-p
   #:escape-css-ident
   #:unsafe-css-ident
   ;; tokens.lisp
   #:*colors*
   #:*light-colors*
   #:*typography*
   #:*spacing*
   #:*effects*
   #:*default-colors*
   #:*default-typography*
   #:*default-spacing*
   #:*default-effects*
   #:get-color
   #:get-font
   #:get-spacing
   #:get-effect
   #:validate-token
   #:*validate-token-max-length*
   #:levenshtein-distance
   #:find-closest-match
   #:generate-css-variables
   ;; tailwind.lisp
   #:tw-color
   #:tw-spacing
   #:tw-bg
   #:tw-text
   #:tw-border
   #:tw-arbitrary
   #:tw-bg-value
   #:tw-text-value
   #:tw-border-value
   #:classes
   #:null-or-empty-p
   #:tailwind-config
   #:*tailwind-config-max-tokens*
   #:tailwind-config-token-invalid
   #:tailwind-config-token-invalid-key
   #:tailwind-config-too-many-tokens
   #:tailwind-config-too-many-tokens-count
   #:tailwind-config-too-many-tokens-limit))
