;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; Umbrella + shim package definitions.
;;;;
;;;; Each :lol-web/<sub> defpackage lives in src/packages/<name>.lisp and is
;;;; loaded as the head of its sub-system's source list. This file only
;;;; defines the umbrella :lol-web facade and the :lol-reactive deprecation
;;;; shim, so it must load AFTER every sub-system is in the image.

(in-package :cl-user)

(defpackage :lol-web
  (:documentation
   "Umbrella facade for the lol-web framework. Re-exports the external
    symbols of every :lol-web/<sub> sub-package. New code should depend on
    :lol-web (full surface) or a specific :lol-web/<sub> (focused boundary).")
  (:use :cl
        :lol-web/escape
        :lol-web/crypto
        :lol-web/core
        :lol-web/css
        :lol-web/html
        :lol-web/parenscript
        :lol-web/server
        :lol-web/extractors
        :lol-web/jschema
        :lol-web/openapi
        :lol-web/htmx
        :lol-web/realtime
        :lol-web/realtime-htmx
        :lol-web/wizards
        :lol-web/fullstack
        :lol-web/optimization
        :lol-web/forms
        :lol-web/rendering
        :lol-web/resources
        :lol-web/client-runtime)
  (:export
   ;; — :lol-web/escape —
   #:escape-html #:escape-attribute #:safe-attr #:safe-href
   #:neutralize-script-close
   #:safe-url #:safe-url-allowlist
   #:safe-coerce-keyword #:sanitize-hx-on-attrs
   #:%scan-bounded #:*scan-match-timeout-seconds*
   #:*hx-on-attribute-prefix* #:hx-on-attribute-name-p
   ;; — :lol-web/crypto —
   #:hash-password #:verify-password #:needs-rehash?
   #:mint-token #:verify-token
   #:*token-min-secret-key-bytes*
   #:*token-max-nonce-hex-length*
   #:*token-max-payload-hex-length*
   #:hmac-sha256-hex #:sha256-hex
   #:random-bytes-hex #:constant-time-string=
   ;; — :lol-web/core —
   #:*current-effect* #:make-signal #:make-effect #:make-computed #:batch
   #:with-lol-web-thread-safety #:with-reactive-context #:make-pandoric-signal
   #:make-store #:make-evolving-component #:make-component-factory
   #:*factory-registry* #:make-reactive-list
   #:defcomponent #:with-component-state
   #:register-component #:unregister-component #:find-component
   #:component-principal-binding
   #:component-snapshots #:component-undo-stack #:component-redo-stack
   #:component-originals
   #:with-components-lock
   #:generate-component-id #:*components*
   #:defcomponent-with-props #:with-props #:validate-props
   #:defcontext #:defcontext-signal #:list-contexts #:get-context-info
   #:inspect-context #:inspect-all-contexts
   #:bounded-cache #:bounded-cache-p #:make-bounded-cache
   #:bounded-cache-count #:bounded-cache-get #:bounded-cache-set
   #:bounded-cache-remove #:bounded-cache-clear #:bounded-cache-keys
   #:with-cycle-safe-printer
   #:bounded-serialize #:*serialize-truncation-marker*
   ;; — :lol-web/css —
   #:*component-css-registry* #:*css-load-order*
   #:make-css-module #:get-css-module #:get-component-css
   #:generate-all-component-css #:defcss
   #:clear-css-registry #:list-registered-css-components #:inspect-css-registry
   #:css-rule #:css-rules #:css-section #:css-keyframes #:css-media
   #:css-var #:css-var-definition
   #:safe-css-selector-p #:safe-css-value-p #:escape-css-value
   #:unsafe-css-selector
   #:safe-css-payload-string #:safe-css-payload-string-p
   #:safe-css-payload-string-value #:make-safe-css-payload-string
   #:safe-css-ident-p #:escape-css-ident #:unsafe-css-ident
   #:*colors* #:*light-colors* #:*typography* #:*spacing* #:*effects*
   #:*default-colors* #:*default-typography* #:*default-spacing* #:*default-effects*
   #:get-color #:get-font #:get-spacing #:get-effect
   #:validate-token #:*validate-token-max-length*
   #:levenshtein-distance #:find-closest-match
   #:generate-css-variables
   #:tw-color #:tw-spacing #:tw-bg #:tw-text #:tw-border #:tw-arbitrary
   #:tw-bg-value #:tw-text-value #:tw-border-value
   #:classes #:null-or-empty-p #:tailwind-config
   #:*tailwind-config-max-tokens*
   #:tailwind-config-token-invalid #:tailwind-config-token-invalid-key
   #:tailwind-config-too-many-tokens
   #:tailwind-config-too-many-tokens-count
   #:tailwind-config-too-many-tokens-limit
   ;; — :lol-web/html —
   #:htm #:htm-str #:html-attrs #:safe-attribute-name-p #:unsafe-attribute-name
   #:render-component #:component->html
   #:*component-render-hook* #:highlight-sexp
   #:html-page #:reactive-runtime-js #:csp-inline-violations
   #:safe-str #:safe-fmt
   #:safe-html-string #:safe-html-string-p #:safe-html-string-value
   #:make-safe-html-string #:coerce-html-emit
   ;; — :lol-web/parenscript —
   #:reactive-script #:jsonify #:on-click #:on-change #:on-submit
   #:hx-dispatch #:hx-bind
   #:safe-js-string-literal #:safe-js-string-literal-p
   #:safe-js-string-literal-value #:make-safe-js-string-literal
   #:escape-js-string
   ;; — :lol-web/server —
   #:*env* #:*response-headers*
   #:request-path #:request-method #:request-query-string
   #:request-header #:request-content-type #:request-content-length
   #:request-body #:request-body-json
   #:query-param #:query-params #:post-param #:post-params #:param
   #:parse-request-json #:encode-json-string #:decode-json-string
   #:malformed-json-body #:malformed-json-body-input
   #:malformed-json-body-reason #:+json-null+
   #:*json-body-max-depth* #:*json-body-max-string-length*
   #:response #:html-response #:json-response #:text-response
   #:redirect-response #:unsafe-redirect-error #:safe-redirect-path-p #:*canonical-host* #:error-response
   #:with-response-headers #:add-response-header #:get-response-headers
   #:http-status-text
   #:session-get #:session-set #:session-delete #:session-clear #:session-keys
   #:session-rotate #:streaming-session-rotate-error
   #:session-expire #:current-session-id
   #:store #:fetch-session #:store-session #:remove-session
   #:csrf-token
   #:validate-header-value #:validate-origin
   #:add-security-headers #:add-csp-header #:with-security
   #:generate-csrf-token #:get-csrf-token #:validate-csrf-token
   #:csrf-token-input #:with-csrf-validation #:csrf-middleware
   #:request-csrf-valid-p
   #:constant-time-string=
   #:check-rate-limit #:get-client-ip #:client-ip #:*trusted-proxies*
   #:with-rate-limit #:*rate-limit-registry*
   #:rate-limit-entry #:rate-limit-entry-p #:make-rate-limit-entry
   #:rate-limit-entry-count #:rate-limit-entry-window-start
   #:rate-limit-entry-last-seen
   #:rate-limit-namespace #:rate-limit-namespace-p
   #:make-rate-limit-namespace #:rate-limit-namespace-name
   #:rate-limit-namespace-store
   #:rate-limit-namespace-lock #:rate-limit-namespace-max-entries
   #:rate-limit-namespace-inserts-since-eviction
   #:configure-rate-limit-namespace
   #:*rate-limit-min-evict-age*
   #:*rate-limit-eviction-every-n*
   #:*rate-limit-eviction-interval*
   #:configure-rate-limit-eviction-timer
   #:rate-limit-store-full
   #:rate-limit-store-full-namespace
   #:rate-limit-store-full-max-entries
   #:rate-limit-entry-of #:rate-limit-namespace-count
   #:clear-rate-limit-store
   #:current-principal #:current-principal-of-env
   #:session-get-of-env
   #:with-auth
   #:http-error #:http-error-status #:http-error-body
   #:client-error #:server-error
   #:http-bad-request #:http-unauthorized #:http-forbidden
   #:http-not-found #:http-unprocessable-entity
   #:*debug-mode* #:*debug-mode-locked-p*
   #:debug-mode-locked-error #:debug-mode-locked-error-value
   #:set-debug-mode #:lock-debug-mode
   #:*error-log-path*
   #:with-error-handling #:log-error
   #:render-error-page #:render-404-page #:render-500-page
   #:enable-debug-mode #:disable-debug-mode
   #:*max-request-body-bytes*
   #:request-body-too-large
   #:request-body-too-large-limit
   #:request-body-too-large-declared
   #:request-body-too-large-actual
   #:*path-params* #:path-param
   #:unsafe-path-segment #:unsafe-path-segment-segment
   #:safe-path-segment-p #:safe-path-segment
   #:*routes* #:clear-routes #:list-routes #:route-handler #:make-app
   #:app-middleware-order #:middleware-order-error
   #:middleware-order-error-order #:middleware-order-error-reason
   #:*server* #:*before-handler-hook* #:*before-server-start-hook*
   #:start-server #:stop-server
   #:defstreaming-route
   #:streaming-route-entry #:streaming-route-entry-p
   #:make-streaming-route-entry
   #:streaming-route-entry-body #:streaming-route-entry-auth
   #:streaming-route-entry-origin #:streaming-route-entry-bearer-token
   #:streaming-gate
   #:defroute
   #:register-asset #:clear-asset-registry #:*asset-route-prefix*
   #:asset-middleware #:page #:embed-json-data
   ;; — :lol-web/extractors —
   #:extractor-spec #:make-extractor-spec #:extractor-spec-p
   #:extractor-spec-name #:extractor-spec-kind #:extractor-spec-type
   #:extractor-spec-required-p #:extractor-spec-default
   #:extractor-spec-source-string #:extractor-spec-custom-resolver
   #:resolve-extractor #:*handler-metadata* #:handler-metadata
   #:extractor-error #:extractor-error-name #:extractor-error-kind
   #:missing-extractor-input
   #:extractor-coercion-error #:extractor-coercion-error-raw-value
   #:extractor-coercion-error-target-type
   #:extractor-not-registered
   #:defhandler
   ;; — :lol-web/jschema —
   #:parse #:validate #:clear-registry #:get-schema #:*registry*
   #:call-with-registry
   #:*max-schema-depth* #:*max-validation-depth*
   #:*schema-json-max-depth* #:*schema-json-max-string-length*
   #:*pattern-max-length* #:*pattern-compile-timeout-seconds*
   #:*unique-items-hash-set-threshold*
   #:json-schema #:json-schema-p
   #:invalid-schema #:invalid-schema-error-message
   #:invalid-schema-base-uri #:invalid-schema-json-pointer
   #:unparsable-json #:unparsable-json-error #:not-implemented
   #:invalid-json #:invalid-json-errors
   #:invalid-json-value #:invalid-json-value-error-message
   #:invalid-json-value-json-pointer
   ;; — :lol-web/openapi —
   #:lisp-type-to-openapi-schema #:kind-to-openapi-location
   #:build-openapi-spec #:emit-openapi-json
   ;; — :lol-web/htmx —
   #:htmx-runtime-js #:hx-get #:hx-post #:hx-put #:hx-delete
   #:htmx-indicator-css
   #:render-autocomplete #:render-autocomplete-results #:autocomplete-css
   #:oob-swap #:oob-content #:make-oob-swap #:with-oob-swaps
   #:*oob-selector-allowlist* #:*oob-selector-denylist*
   #:*oob-signed-selector-secret*
   #:validate-oob-selector #:mint-oob-selector-token
   #:unsafe-oob-selector #:unsafe-oob-selector-selector
   #:unsafe-oob-selector-reason
   #:htmx-request-p #:htmx-boosted-p #:htmx-history-restore-request-p
   #:htmx-target #:htmx-trigger #:htmx-trigger-name
   #:htmx-current-url #:htmx-prompt
   #:with-htmx-response
   #:set-htmx-trigger #:set-htmx-redirect #:set-htmx-location
   #:render-with-oob #:render-oob-only
   #:htmx-or-redirect #:htmx-or-full-page
   #:*idiomorph-version* #:idiomorph-js-path #:include-idiomorph
   #:htmx-morph-extension-js #:htmx-runtime-with-morph-js
   #:include-htmx-with-morph #:hx-morph #:emit-hx-attrs
   ;; — :lol-web/realtime —
   #:*ws-connections* #:*ws-per-ip-counts*
   #:*ws-per-ip-conn-cap* #:*ws-global-conn-cap* #:*ws-max-frame-size*
   #:ws-cap-exceeded #:ws-cap-exceeded-scope #:ws-cap-exceeded-ip
   #:ws-connection-count #:ws-per-ip-count #:ws-channels
   #:make-ws-handler #:defws
   #:ws-send #:ws-send-text #:ws-send-binary #:ws-send-json #:ws-close
   #:ws-broadcast #:ws-broadcast-json #:ws-broadcast-all
   #:ws-broadcast-text #:ws-broadcast-safe-html
   #:ws-broadcast-oob #:ws-broadcast-trigger
   #:make-oob-update
   #:*sse-connections* #:*sse-per-ip-counts*
   #:*sse-per-ip-conn-cap* #:*sse-global-conn-cap* #:*sse-max-event-bytes*
   #:*sse-default-worker-pool-size* #:*sse-worker-pool-reserve*
   #:sse-cap-exceeded #:sse-cap-exceeded-scope #:sse-cap-exceeded-ip
   #:sse-connection #:sse-connection-p #:make-sse-connection
   #:sse-connection-id #:sse-connection-stream #:sse-connection-channel
   #:sse-connection-ip #:sse-connection-created-at
   #:sse-connection-alive-p #:sse-connection-on-disconnect
   #:sse-connection-count #:sse-remove-connection
   #:sse-per-ip-count #:sse-channels
   #:make-sse-handler #:defsse #:format-sse-event
   #:sse-send #:sse-send-comment #:sse-ping-all
   #:sse-broadcast #:sse-broadcast-all
   #:sse-broadcast-text #:sse-broadcast-safe-html
   #:sse-broadcast-oob #:sse-broadcast-trigger
   ;; — :lol-web/realtime-htmx —
   #:ws-client-js #:sse-client-js #:optimistic-js
   #:*optimistic-originals-cap* #:*optimistic-originals-global-cap*
   #:optimistic-apply-payload #:optimistic-record-original
   #:optimistic-clear-originals
   ;; — :lol-web/wizards —
   #:defwizard #:register-wizard #:get-wizard-spec #:list-wizards
   #:inspect-wizard
   #:start-wizard #:get-wizard-session #:remove-wizard-session
   #:cleanup-stale-sessions #:list-active-wizard-sessions
   #:process-wizard-submission
   #:render-wizard-step #:render-wizard-initial-step #:render-wizard-complete
   #:with-wizard-state
   #:wizard-text-field #:wizard-select-field #:wizard-radio-group
   ;; — :lol-web/fullstack —
   #:sign-hydration-state #:verify-hydration-state
   #:defisomorphic-component #:render-isomorphic #:isomorphic-page
   #:hydration-runtime-js #:include-hydration-runtime #:client-action-attr
   #:serialize-state #:deserialize-state
   #:defcomponent-with-api
   #:register-api-component #:find-api-component
   #:list-api-components #:list-api-routes #:inspect-api-component
   #:generate-api-client-js #:api-client-script-tag
   #:*action-arities* #:register-action-arity #:action-arity
   ;; — :lol-web/optimization —
   #:analyze-dependencies #:reactive-let #:with-reactive-bindings
   #:defvalidated-template #:validate-css-class
   #:*registered-css-classes* #:*registered-css-prefixes*
   #:register-css-class #:register-css-prefix
   #:register-tailwind-classes
   #:lint-hx-on-not-literal
   ;; — :lol-web/forms —
   #:*form-pattern-max-length*
   #:*form-pattern-compile-timeout-seconds*
   #:*form-email-max-length*
   #:*form-url-max-length*
   ;; — :lol-web/client-runtime —
   #:lol-reactive-runtime-js))

(defpackage :lol-reactive
  (:documentation
   "Deprecation shim for the legacy :lol-reactive package. Re-exports every
    external symbol of :lol-web. New code should use :lol-web (umbrella) or
    a specific :lol-web/<sub> sub-package.")
  (:use :cl :lol-web)
  (:export
   #:escape-html #:escape-attribute #:safe-attr #:safe-href
   #:neutralize-script-close
   #:safe-url #:safe-url-allowlist
   #:safe-coerce-keyword #:sanitize-hx-on-attrs
   #:%scan-bounded #:*scan-match-timeout-seconds*
   #:*hx-on-attribute-prefix* #:hx-on-attribute-name-p
   #:hash-password #:verify-password #:needs-rehash?
   #:mint-token #:verify-token
   #:*token-min-secret-key-bytes*
   #:*token-max-nonce-hex-length*
   #:*token-max-payload-hex-length*
   #:hmac-sha256-hex #:sha256-hex
   #:random-bytes-hex #:constant-time-string=
   #:*current-effect* #:make-signal #:make-effect #:make-computed #:batch
   #:with-lol-web-thread-safety #:with-reactive-context #:make-pandoric-signal
   #:make-store #:make-evolving-component #:make-component-factory
   #:*factory-registry* #:make-reactive-list
   #:defcomponent #:with-component-state
   #:register-component #:unregister-component #:find-component
   #:component-principal-binding
   #:component-snapshots #:component-undo-stack #:component-redo-stack
   #:component-originals
   #:with-components-lock
   #:generate-component-id #:*components*
   #:defcomponent-with-props #:with-props #:validate-props
   #:defcontext #:defcontext-signal #:list-contexts #:get-context-info
   #:inspect-context #:inspect-all-contexts
   #:bounded-cache #:bounded-cache-p #:make-bounded-cache
   #:bounded-cache-count #:bounded-cache-get #:bounded-cache-set
   #:bounded-cache-remove #:bounded-cache-clear #:bounded-cache-keys
   #:with-cycle-safe-printer
   #:bounded-serialize #:*serialize-truncation-marker*
   #:*component-css-registry* #:*css-load-order*
   #:make-css-module #:get-css-module #:get-component-css
   #:generate-all-component-css #:defcss
   #:clear-css-registry #:list-registered-css-components #:inspect-css-registry
   #:css-rule #:css-rules #:css-section #:css-keyframes #:css-media
   #:css-var #:css-var-definition
   #:safe-css-selector-p #:safe-css-value-p #:escape-css-value
   #:unsafe-css-selector
   #:safe-css-payload-string #:safe-css-payload-string-p
   #:safe-css-payload-string-value #:make-safe-css-payload-string
   #:safe-css-ident-p #:escape-css-ident #:unsafe-css-ident
   #:*colors* #:*light-colors* #:*typography* #:*spacing* #:*effects*
   #:*default-colors* #:*default-typography* #:*default-spacing* #:*default-effects*
   #:get-color #:get-font #:get-spacing #:get-effect
   #:validate-token #:*validate-token-max-length*
   #:levenshtein-distance #:find-closest-match
   #:generate-css-variables
   #:tw-color #:tw-spacing #:tw-bg #:tw-text #:tw-border #:tw-arbitrary
   #:tw-bg-value #:tw-text-value #:tw-border-value
   #:classes #:null-or-empty-p #:tailwind-config
   #:*tailwind-config-max-tokens*
   #:tailwind-config-token-invalid #:tailwind-config-token-invalid-key
   #:tailwind-config-too-many-tokens
   #:tailwind-config-too-many-tokens-count
   #:tailwind-config-too-many-tokens-limit
   #:htm #:htm-str #:html-attrs #:safe-attribute-name-p #:unsafe-attribute-name
   #:render-component #:component->html
   #:*component-render-hook* #:highlight-sexp
   #:html-page #:reactive-runtime-js #:csp-inline-violations
   #:safe-str #:safe-fmt
   #:safe-html-string #:safe-html-string-p #:safe-html-string-value
   #:make-safe-html-string #:coerce-html-emit
   #:reactive-script #:jsonify #:on-click #:on-change #:on-submit
   #:hx-dispatch #:hx-bind
   #:safe-js-string-literal #:safe-js-string-literal-p
   #:safe-js-string-literal-value #:make-safe-js-string-literal
   #:escape-js-string
   #:*env* #:*response-headers*
   #:request-path #:request-method #:request-query-string
   #:request-header #:request-content-type #:request-content-length
   #:request-body #:request-body-json
   #:query-param #:query-params #:post-param #:post-params #:param
   #:parse-request-json #:encode-json-string #:decode-json-string
   #:malformed-json-body #:malformed-json-body-input
   #:malformed-json-body-reason #:+json-null+
   #:*json-body-max-depth* #:*json-body-max-string-length*
   #:response #:html-response #:json-response #:text-response
   #:redirect-response #:unsafe-redirect-error #:safe-redirect-path-p #:*canonical-host* #:error-response
   #:with-response-headers #:add-response-header #:get-response-headers
   #:http-status-text
   #:session-get #:session-set #:session-delete #:session-clear #:session-keys
   #:session-rotate #:streaming-session-rotate-error
   #:session-expire #:current-session-id
   #:store #:fetch-session #:store-session #:remove-session
   #:csrf-token
   #:validate-header-value #:validate-origin
   #:add-security-headers #:add-csp-header #:with-security
   #:generate-csrf-token #:get-csrf-token #:validate-csrf-token
   #:csrf-token-input #:with-csrf-validation #:csrf-middleware
   #:request-csrf-valid-p
   #:constant-time-string=
   #:check-rate-limit #:get-client-ip #:client-ip #:*trusted-proxies*
   #:with-rate-limit #:*rate-limit-registry*
   #:rate-limit-entry #:rate-limit-entry-p #:make-rate-limit-entry
   #:rate-limit-entry-count #:rate-limit-entry-window-start
   #:rate-limit-entry-last-seen
   #:rate-limit-namespace #:rate-limit-namespace-p
   #:make-rate-limit-namespace #:rate-limit-namespace-name
   #:rate-limit-namespace-store
   #:rate-limit-namespace-lock #:rate-limit-namespace-max-entries
   #:rate-limit-namespace-inserts-since-eviction
   #:configure-rate-limit-namespace
   #:*rate-limit-min-evict-age*
   #:*rate-limit-eviction-every-n*
   #:*rate-limit-eviction-interval*
   #:configure-rate-limit-eviction-timer
   #:rate-limit-store-full
   #:rate-limit-store-full-namespace
   #:rate-limit-store-full-max-entries
   #:rate-limit-entry-of #:rate-limit-namespace-count
   #:clear-rate-limit-store
   #:current-principal #:current-principal-of-env
   #:session-get-of-env
   #:with-auth
   #:http-error #:http-error-status #:http-error-body
   #:client-error #:server-error
   #:http-bad-request #:http-unauthorized #:http-forbidden
   #:http-not-found #:http-unprocessable-entity
   #:*debug-mode* #:*debug-mode-locked-p*
   #:debug-mode-locked-error #:debug-mode-locked-error-value
   #:set-debug-mode #:lock-debug-mode
   #:*error-log-path*
   #:with-error-handling #:log-error
   #:render-error-page #:render-404-page #:render-500-page
   #:enable-debug-mode #:disable-debug-mode
   #:*max-request-body-bytes*
   #:request-body-too-large
   #:request-body-too-large-limit
   #:request-body-too-large-declared
   #:request-body-too-large-actual
   #:*path-params* #:path-param
   #:unsafe-path-segment #:unsafe-path-segment-segment
   #:safe-path-segment-p #:safe-path-segment
   #:*routes* #:clear-routes #:list-routes #:route-handler #:make-app
   #:app-middleware-order #:middleware-order-error
   #:middleware-order-error-order #:middleware-order-error-reason
   #:*server* #:*before-handler-hook* #:*before-server-start-hook*
   #:start-server #:stop-server
   #:defstreaming-route
   #:streaming-route-entry #:streaming-route-entry-p
   #:make-streaming-route-entry
   #:streaming-route-entry-body #:streaming-route-entry-auth
   #:streaming-route-entry-origin #:streaming-route-entry-bearer-token
   #:streaming-gate
   #:defroute
   #:register-asset #:clear-asset-registry #:*asset-route-prefix*
   #:asset-middleware #:page #:embed-json-data
   #:extractor-spec #:make-extractor-spec #:extractor-spec-p
   #:extractor-spec-name #:extractor-spec-kind #:extractor-spec-type
   #:extractor-spec-required-p #:extractor-spec-default
   #:extractor-spec-source-string #:extractor-spec-custom-resolver
   #:resolve-extractor #:*handler-metadata* #:handler-metadata
   #:extractor-error #:extractor-error-name #:extractor-error-kind
   #:missing-extractor-input
   #:extractor-coercion-error #:extractor-coercion-error-raw-value
   #:extractor-coercion-error-target-type
   #:extractor-not-registered
   #:defhandler
   #:parse #:validate #:clear-registry #:get-schema #:*registry*
   #:call-with-registry
   #:*max-schema-depth* #:*max-validation-depth*
   #:*schema-json-max-depth* #:*schema-json-max-string-length*
   #:*pattern-max-length* #:*pattern-compile-timeout-seconds*
   #:*unique-items-hash-set-threshold*
   #:json-schema #:json-schema-p
   #:invalid-schema #:invalid-schema-error-message
   #:invalid-schema-base-uri #:invalid-schema-json-pointer
   #:unparsable-json #:unparsable-json-error #:not-implemented
   #:invalid-json #:invalid-json-errors
   #:invalid-json-value #:invalid-json-value-error-message
   #:invalid-json-value-json-pointer
   #:lisp-type-to-openapi-schema #:kind-to-openapi-location
   #:build-openapi-spec #:emit-openapi-json
   #:htmx-runtime-js #:hx-get #:hx-post #:hx-put #:hx-delete
   #:htmx-indicator-css
   #:render-autocomplete #:render-autocomplete-results #:autocomplete-css
   #:oob-swap #:oob-content #:make-oob-swap #:with-oob-swaps
   #:*oob-selector-allowlist* #:*oob-selector-denylist*
   #:*oob-signed-selector-secret*
   #:validate-oob-selector #:mint-oob-selector-token
   #:unsafe-oob-selector #:unsafe-oob-selector-selector
   #:unsafe-oob-selector-reason
   #:htmx-request-p #:htmx-boosted-p #:htmx-history-restore-request-p
   #:htmx-target #:htmx-trigger #:htmx-trigger-name
   #:htmx-current-url #:htmx-prompt
   #:with-htmx-response
   #:set-htmx-trigger #:set-htmx-redirect #:set-htmx-location
   #:render-with-oob #:render-oob-only
   #:htmx-or-redirect #:htmx-or-full-page
   #:*idiomorph-version* #:idiomorph-js-path #:include-idiomorph
   #:htmx-morph-extension-js #:htmx-runtime-with-morph-js
   #:include-htmx-with-morph #:hx-morph #:emit-hx-attrs
   #:*ws-connections* #:*ws-per-ip-counts*
   #:*ws-per-ip-conn-cap* #:*ws-global-conn-cap* #:*ws-max-frame-size*
   #:ws-cap-exceeded #:ws-cap-exceeded-scope #:ws-cap-exceeded-ip
   #:ws-connection-count #:ws-per-ip-count #:ws-channels
   #:make-ws-handler #:defws
   #:ws-send #:ws-send-text #:ws-send-binary #:ws-send-json #:ws-close
   #:ws-broadcast #:ws-broadcast-json #:ws-broadcast-all
   #:ws-broadcast-text #:ws-broadcast-safe-html
   #:ws-broadcast-oob #:ws-broadcast-trigger
   #:make-oob-update
   #:*sse-connections* #:*sse-per-ip-counts*
   #:*sse-per-ip-conn-cap* #:*sse-global-conn-cap* #:*sse-max-event-bytes*
   #:*sse-default-worker-pool-size* #:*sse-worker-pool-reserve*
   #:sse-cap-exceeded #:sse-cap-exceeded-scope #:sse-cap-exceeded-ip
   #:sse-connection #:sse-connection-p #:make-sse-connection
   #:sse-connection-id #:sse-connection-stream #:sse-connection-channel
   #:sse-connection-ip #:sse-connection-created-at
   #:sse-connection-alive-p #:sse-connection-on-disconnect
   #:sse-connection-count #:sse-remove-connection
   #:sse-per-ip-count #:sse-channels
   #:make-sse-handler #:defsse #:format-sse-event
   #:sse-send #:sse-send-comment #:sse-ping-all
   #:sse-broadcast #:sse-broadcast-all
   #:sse-broadcast-text #:sse-broadcast-safe-html
   #:sse-broadcast-oob #:sse-broadcast-trigger
   #:ws-client-js #:sse-client-js #:optimistic-js
   #:*optimistic-originals-cap* #:*optimistic-originals-global-cap*
   #:optimistic-apply-payload #:optimistic-record-original
   #:optimistic-clear-originals
   #:defwizard #:register-wizard #:get-wizard-spec #:list-wizards
   #:inspect-wizard
   #:start-wizard #:get-wizard-session #:remove-wizard-session
   #:cleanup-stale-sessions #:list-active-wizard-sessions
   #:process-wizard-submission
   #:render-wizard-step #:render-wizard-initial-step #:render-wizard-complete
   #:with-wizard-state
   #:wizard-text-field #:wizard-select-field #:wizard-radio-group
   #:sign-hydration-state #:verify-hydration-state
   #:defisomorphic-component #:render-isomorphic #:isomorphic-page
   #:hydration-runtime-js #:include-hydration-runtime #:client-action-attr
   #:serialize-state #:deserialize-state
   #:defcomponent-with-api
   #:register-api-component #:find-api-component
   #:list-api-components #:list-api-routes #:inspect-api-component
   #:generate-api-client-js #:api-client-script-tag
   #:*action-arities* #:register-action-arity #:action-arity
   #:analyze-dependencies #:reactive-let #:with-reactive-bindings
   #:defvalidated-template #:validate-css-class
   #:*registered-css-classes* #:*registered-css-prefixes*
   #:register-css-class #:register-css-prefix
   #:register-tailwind-classes
   #:lint-hx-on-not-literal
   #:*form-pattern-max-length*
   #:*form-pattern-compile-timeout-seconds*
   #:*form-email-max-length*
   #:*form-url-max-length*
   #:lol-reactive-runtime-js))

;;; Once-per-image deprecation warning. Triggers on first load only; further
;;; loads in the same image (e.g., re-compile during REPL development) are
;;; silent.
(defvar lol-reactive::*shim-warned* nil)
(unless lol-reactive::*shim-warned*
  (cl:warn
   "Package :lol-reactive is a deprecation shim for :lol-web. Update consumers ~
    to use :lol-web (umbrella) or :lol-web/<sub> (focused).")
  (setf lol-reactive::*shim-warned* t))
