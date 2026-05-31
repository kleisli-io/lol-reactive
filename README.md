# lol-web

Server-rendered reactive web framework for Common Lisp. HTML on the server, updates over HTMX, WebSocket, or SSE. Signals, components, and token sets are closures ([Let Over Lambda](https://letoverlambda.com/)).

## Sub-systems

ASDF sub-systems under the umbrella `:lol-web`. Load any leaf via `:lol-web/<name>`:

`escape` · `core` · `css` · `parenscript` · `html` · `server` · `crypto` · `jschema` · `extractors` · `openapi` · `client-runtime` · `rendering` · `resources` · `htmx` · `realtime` · `realtime-htmx` · `forms` · `optimization` · `devtools` · `wizards` · `fullstack`

Signals (`make-signal` / `make-computed` / `make-effect` / `batch`), components (`defcomponent` + `register-component`), HTML (`htm` / `html-page`), scoped CSS + design tokens + Tailwind, HTMX with OOB swaps + idiomorph + WS transport, broadcast over WS/SSE, Clack/Lack server with `defroute` and `defhandler` (typed `:path` / `:query` / `:header` / `:body` / `:json-body` extractors), OpenAPI 3.1 emission with subset JSON Schema 2020-12, surgery-mode devtools, continuation wizards, isomorphic server-render + client hydration.

## Usage

```nix
{
  inputs.lol-web.url = "github:kleisli-io/lol-web";

  outputs = { lol-web, ... }:
    let
      inherit (lol-web.inputs.cl-deps.lib.x86_64-linux) buildLisp;
      lol = lol-web.lib.x86_64-linux.library;
    in {
      packages.default = buildLisp.program {
        name = "my-app";
        deps = [ lol ];
        srcs = [ ./src/app.lisp ];
        main = "my-app:main";
      };
    };
}
```

## Quick start

```lisp
(defpackage :my-app (:use :cl :lol-web))
(in-package :my-app)

(defroute "/" ()
  (html-response
    (html-page (:title "Hello")
      (htm (:h1 "It works")))))

(defhandler get-thing ((id :path :type integer))
  (html-response
    (htm (:p "thing " (princ-to-string id)))))

(defun main ()
  (start-server :port 8080))
```

## Session cookies

`make-app` / `start-server` defaults:

| Keyword | Default | Effect |
|---|---|---|
| `:session-cookie-secure`   | `T`    | HTTPS only |
| `:session-cookie-httponly` | `T`    | No JS access |
| `:session-cookie-samesite` | `:lax` | Top-level navigation only |

Plain-HTTP dev needs `:session-cookie-secure nil`.

## Authentication

Install hook thunks via `:auth`. The principal is opaque — consumer's call.

```lisp
(start-server :port 8080
              :auth (list :authenticated-p (lambda () (session-get "uid"))
                          :current-principal (lambda () (session-get "uid"))))
```

Gate routes with `with-auth`. `:on-unauthorized` accepts integer status, redirect path, or callable returning a Clack triple:

```lisp
(defroute "/account" (:method :get)
  (with-auth (:on-unauthorized "/sign-in")
    (html-response (your-render-account-page (current-principal)))))

(defroute "/api/private" (:method :get)
  (with-auth (:on-unauthorized
              (lambda () (json-response '(:error "auth required") :status 401)))
    (api-payload (current-principal))))
```

Sign-in: per-IP outer rate-limit, CSRF, per-account inner rate-limit, password verify, rotate.

```lisp
(defroute "/sign-in" (:method :post)
  (with-rate-limit (:namespace :ip :max-requests 30 :window-seconds 60)
    (with-csrf-validation
      (let ((email    (post-param "email"))
            (password (post-param "password")))
        (with-rate-limit (:namespace :login
                          :key (format nil "login:~A" email)
                          :max-requests 5 :window-seconds 60)
          (let ((encoded (your-lookup-encoded-password email)))
            (when (and encoded (verify-password password encoded))
              (when (needs-rehash? encoded)
                (your-store-encoded-password
                 email (hash-password password)))
              (session-rotate)
              (redirect-response "/account"))))))))
```

## CSRF

With `:use-csrf t` (default), `make-app` installs `csrf-middleware` — constant-time compare via `constant-time-string=`, 403 on mismatch. Per-route checks:

```lisp
(defroute "/submit" (:method :post)
  (with-csrf-validation
    (process-form)))
```

Emit the hidden field with `(csrf-token-input)`. `:use-csrf t :use-session nil` signals at construction. The session middleware must wrap csrf — `csrf-middleware` reads `:lack.session`, which the session layer populates — so `make-app` installs session outside csrf and asserts the ordering at build time (see Middleware order).

`defform` generated `process-<name>-submission` functions also check CSRF when a Lack session is present. Programmatic calls outside a request/session context still work; request handlers with `:lack.session` must include the submitted `:csrf-token`. Wizards use their own owner-token flow on step transitions and remain covered by `csrf-middleware` when mounted under `make-app`.

JSON requests put the token in `X-CSRF-Token` or a `csrf-token` JSON field. Empty JSON, malformed JSON, and top-level `null` are distinct: empty returns `NIL`, malformed input becomes `http-bad-request`, and top-level `null` returns `+json-null+`.

## Session rotation

Call `session-rotate` after login or privilege escalation. `"csrf-token"` is always cleared; pre-auth state otherwise persists. `:scrub t` wipes everything except a `:preserve` allowlist:

```lisp
(session-rotate :scrub t :preserve '("uid" "intended-path"))
```

`"csrf-token"` is permanently deny-listed (listing it signals). Inside a streaming handler `session-rotate` signals `streaming-session-rotate-error` — Lack's FINALIZE never runs for streamed responses. Rotate before opening the stream.

## Rate limiting

`with-rate-limit` partitions state across namespaces; each owns a bounded LRU store, so a flood in one cannot evict another (fairness guarantee). `:ip` (cap 10000) and `:login` (cap 1000) ship pre-installed.

```lisp
(with-rate-limit (:namespace :ip :max-requests 30 :window-seconds 60)
  (with-rate-limit (:namespace :login
                    :key (format nil "login:~A" email)
                    :max-requests 5 :window-seconds 60)
    body))
```

| Knob | Surface | Effect |
|---|---|---|
| `:rate-limit-namespaces` | `make-app` | Per-namespace `(:max-entries N)` caps |
| `configure-rate-limit-namespace` | runtime | Reconfigure cap of a single namespace |
| `*trusted-proxies*` | parameter | List of proxy `:remote-addr` strings consulted before honoring `X-Forwarded-For` / `X-Real-IP` |

Behind a proxy, set `*trusted-proxies*` — the default ignores `X-Forwarded-For` so an attacker cannot forge their bucket:

```lisp
(setf lol-web/server:*trusted-proxies* '("127.0.0.1"))
```

`client-ip` returns the originating IP.

## CSP overrides

`add-csp-header` defaults to `script-src 'self'` and `style-src 'self' https://fonts.googleapis.com`; inline script/style is not enabled by default. Legacy pages that still need inline handlers must opt in per response:

```lisp
(with-response-headers ()
  (add-csp-header :script-src "'self' 'unsafe-inline'"
                  :style-src "'self' 'unsafe-inline'")
  (html-response (your-sign-in-form)))
```

## Middleware order

`build-clack-env` runs before the Lack app and enforces `*max-request-body-bytes*` before route lookup or middleware dispatch. The request then enters the wrapped app in this order:

```
cors -> static -> accesslog -> session -> csrf -> auth-hooks -> hydration -> jschema-registry -> streaming-gate -> route-handler
```

`app-middleware-order` is the single source of truth for this sequence; `make-app` folds its middleware in exactly that order and calls `%assert-middleware-order` at build time. The assertion enforces two security invariants — `session` wraps `csrf` (so `:lack.session` is populated before `csrf-middleware` reads it) and `streaming-gate` is innermost (immediately wrapping `route-handler`). An inverted order signals `middleware-order-error` at construction, never as a runtime 500 on the first POST.

`streaming-gate` is innermost, immediately before `route-handler`, so WS/SSE decisions see the same auth hooks, session data, and per-app JSON Schema registry as regular routes. The body cap sits outside the chain and returns 413 directly when it fires.

## Request budgets

The request body cap defaults to 8 MiB via `*max-request-body-bytes*`. `NIL` disables it for tests or controlled offline ingestion. JSON decoding adds parser-side limits: `*json-body-max-depth*` bounds array/object nesting and `*json-body-max-string-length*` bounds a single JSON string.

Token verification also rejects overlong nonce, expiry, payload, and tag segments before HMAC work. Form regex patterns and JSON Schema regexes have separate parse-time caps because they are source/configuration inputs, not request bodies.

## Streaming endpoints (WS / SSE)

`make-ws-handler` / `make-sse-handler` (and their macro wrappers `defws` / `defsse`) require both `:auth` and `:origin` — construction signals if either is missing. Fail-closed default; there is no permissive fallback.

```lisp
(defws "/ws/notifications" "notifications"
  :auth   (lambda () (session-get "uid"))
  :origin (list "https://example.com")
  :on-message #'handle-message)

(defsse "/sse/feed" "feed"
  :auth   (lambda () (session-get "uid"))
  :origin (list "https://example.com"))
```

`streaming-gate` is installed innermost in the `make-app` chain. The gate sees a fully populated env (session, auth hooks, CSRF state) and denies forged upgrades before any WebSocket/SSE driver is reached. The auth thunk supplied to `defws`/`defsse` may read `current-principal-of-env` or `session-get-of-env` to inspect that populated env without rebinding `*env*` itself.

Non-browser clients (CLI tools, mobile apps, server-to-server) lack same-origin headers; the gate's verbatim Origin match would otherwise deny them. Construct the entry with `:bearer-token` set to a predicate `(token -> generalised boolean)` that validates an `Authorization: Bearer <token>` value out-of-band, then have `:auth` perform the authorization decision against the same token. Browser traffic carrying `Origin` is unaffected — the bearer escape only fires when the same-origin path is empty:

```lisp
(setf (gethash (cons :get "/sse/api-feed")
               lol-web/server::*streaming-routes*)
      (make-streaming-route-entry
       :body         #'feed-body
       :auth         (lambda (env) (consumer-validates env))
       :origin       '("https://app.example.com")
       :bearer-token (lambda (token) (consumer-valid-token-p token))))
```

Per-IP and global connection caps protect the process. Cap-exceed paths: WS closes after handshake with code 1013 (`ws-cap-exceeded`); SSE refuses with HTTP 503 (`sse-cap-exceeded`) before opening the writer.

| Parameter | Default | Scope |
|---|---|---|
| `*ws-per-ip-conn-cap*`  / `*sse-per-ip-conn-cap*`  | 4    | per-IP slot count |
| `*ws-global-conn-cap*`  / `*sse-global-conn-cap*`  | 1024 | total live connections |
| `*ws-max-frame-size*`   / `*sse-max-event-bytes*`  | 64 KiB | inbound frame / outbound event payload |

Broadcast helpers split by trust:

| Function | Behavior |
|---|---|
| `ws-broadcast-text` / `sse-broadcast-text` | Auto-escapes via `lol-web/escape:escape-html` before emit |
| `ws-broadcast-safe-html` / `sse-broadcast-safe-html` | `check-type` against `lol-web/html:safe-html-string`; raw strings are refused |
| `ws-broadcast-oob` / `sse-broadcast-oob` | HTMX OOB payloads (see HTMX section for selector policy) |

Wrap producer-side trusted HTML with `make-safe-html-string`; never wrap data that came from request input.

## Trust types

Use the trust type for the emission context:

| Type | Context | Constructor |
|---|---|---|
| `safe-html-string` | Raw HTML/script/style payload slots in `html-page`, OOB HTML, optimistic HTML | `make-safe-html-string` |
| `safe-css-payload-string` | Complete trusted CSS payload fragments and CSS section/media/variable payloads | `make-safe-css-payload-string` |
| `safe-js-string-literal` | JavaScript string literal payloads produced by the Parenscript boundary | `make-safe-js-string-literal` |

Do not move a value between contexts by reusing a different trust type. Dynamic CSS selectors, CSS identifiers, URLs, and path segments go through their own validators (`safe-css-selector-p`, `safe-css-ident-p`, `safe-url-allowlist`, `safe-path-segment`) rather than a generic safe-string wrapper.

cl-who does **not** auto-escape runtime values spliced into HTML attribute *values*, so attribute sinks use their own emitters rather than a safe-string trust type:

| Emitter | Context | Behavior |
|---|---|---|
| `safe-attr` | any HTML attribute value interpolating a runtime value | coerce to printed form, then `escape-attribute` so embedded quotes/angle-brackets cannot close the attribute; `NIL` in → no attribute |
| `safe-href` | `href` / `src`-style URL attributes | reject a disallowed or script-bearing scheme via `safe-url-allowlist`, then `escape-attribute` the survivor; `NIL` (no attribute) when the scheme is rejected — fail-closed |

## Components

`defcomponent` defines a pandoric closure; `register-component` interns it with an optional principal-binding gate. IDs come from `generate-component-id` (128 bits of OS CSPRNG entropy — not enumerable from the prefix).

```lisp
(defcomponent dashboard ((count 0))
  (:render ()
    (htm (:div "count: " (princ-to-string count)))))

(register-component (generate-component-id "dashboard")
                    (dashboard)
                    :principal-binding (session-get "uid"))
```

`component-principal-binding` returns the stored binding; downstream lookups compare it `EQUAL` against the current request principal. `NIL` ⇒ no ownership check (opt-in). All per-instance state (snapshots, undo, redo, optimistic originals) is released atomically by `unregister-component`.

## Wizards

`defwizard` declares a multi-step form. Each step's validator runs server-side; `:on-complete` fires once the chain has been walked end-to-end.

```lisp
(defwizard checkout ()
  :steps    (list step-address step-payment step-confirm)
  :internal nil
  :on-complete (lambda (data) (your-place-order data)))
```

Step transitions go through `process-wizard-submission`. The dispatcher refuses with `:forbidden` for: skip-to-complete attempts (`:complete` with no live session), `:back` from no session, owner-token mismatch (CSPRNG token stored in Lack session + compared `constant-time-string=`), and rate-limit denials. Auto-spawn happens only on `:next`. CSRF middleware covers wizard endpoints automatically when `:use-csrf t`.

`:internal` defaults to `T` — wizards drop out of the OpenAPI emission unless opted in.

## Surgery mode (devtools)

`:lol-web/devtools` is an **opt-in** sub-system. The umbrella `:lol-web` does not pull it in; consumers that want the x-ray panel load it explicitly:

```lisp
(asdf:load-system :lol-web/devtools)
```

Even after loading, surgery is disabled by default. **Production must not call `enable-surgery-mode`** — the x-ray render hook exposes per-component closure state to every connected peer.

```lisp
(enable-surgery-mode)   ; REPL / dev only
(disable-surgery-mode)
(surgery-mode-p)        ; T/NIL
```

For per-request gating in a running app, install `lol-web/devtools:surgery-middleware` downstream of session middleware. Its default `:decide` predicate reads `:lol-web/surgery-mode` from the Lack session, so the toggle is thread-local to that request:

```lisp
(setf my-app:*app*
      (lol-web/devtools:surgery-middleware my-app:*app*))
```

Per-component actions (`surgery-get-state` / `surgery-set-state` / `surgery-undo` / `surgery-redo`) are only effective when `surgery-mode-p` returns `T` in the request's dynamic extent.

There is no HTTP-shaped Lisp eval. REPL-into-component is a SLIME concern — attach to the running image directly when you need it. The `/api/surgery/*` routes are scoped to state inspection, surgical state writes, and snapshot/undo/redo; they do not evaluate caller-supplied forms.

## HTMX OOB safety

`make-oob-swap` accepts arbitrary CSS selectors with an optional `:signed-token` escape hatch. The selector denylist refuses `body`, `head`, `html`, and any form selector (including `form[…]`, `form.foo`, `form#bar`) unconditionally.

```lisp
(make-oob-swap "#post-42" rendered-html)
(make-oob-swap "#post-42" rendered-html
               :signed-token (mint-oob-selector-token "#post-42"))
```

| Mode | `*oob-selector-allowlist*` |
|---|---|
| Permissive | `NIL` — denylist is the only gate |
| Tightened  | Non-NIL list — selector must be in the list **or** present a valid signed token |

`mint-oob-selector-token` / `validate-oob-selector` reuse the HMAC v1 framing from `lol-web/crypto`.

The `defvalidated-template` macro runs `lint-hx-on-not-literal` at expansion: any `:hx-on-*` / `:hx-on` attribute with a non-literal value warns (dynamic inline-handler payloads escape static review — compile a server-bound handler instead).

## Optimistic updates

`optimistic-apply-payload` is the producer boundary for client-side optimistic UI. The `:html` slot must be a `safe-html-string` — raw strings are refused. `optimistic-record-original` consults `find-component` to distinguish unregistered IDs from a registered-but-empty store; `*optimistic-originals-cap*` (default 64) bounds the per-component rollback store.

## Mechanism vs. policy

`:lol-web/*` ships gates, not decisions. Not included:

- `User` class or any principal shape — `:current-principal` returns whatever you want
- Sign-in / sign-up / reset / MFA / account HTML
- IdP integration (OAuth, OIDC, SAML)
- Password-policy strings, lockout UI, progressive backoff
- Email delivery

`with-auth`, `current-principal`, `hash-password`, `mint-token`, `csrf-middleware`, `session-rotate`, `with-rate-limit` are mechanism. Decisions, storage, and UI are yours.

## Consumer responsibilities

- **Per-session locking** in custom session stores. The memory default is single-writer.
- **Scrub on rotate** — `(session-rotate :scrub t :preserve '("uid" ...))` drops OAuth state, PKCE verifier, intended-path, captcha-complete.
- **`*trusted-proxies*`** in production behind nginx/HAProxy/Caddy/LB, or per-IP rate-limit collapses to the proxy IP.
- **CSP per-page** only when stricter than the default `script-src 'self'`.
- **Layered rate-limit** — `:ip` outer, `:login` inner. Either alone is bypassable (account spraying or fake-account cycling).
- **Rehash on login** — after `verify-password`, call `(needs-rehash? encoded)` and re-`hash-password` when T.
- **Token replay** — `verify-token` is stateless; the spent-nonce table is yours, keyed on `nonce-hex`.

## Crypto wire formats (v1)

Both encodings carry a leading `v1` discriminator so future schemes coexist.

`hash-password` / `verify-password`:

```
v1:argon2id:<block-count>:<iterations>:<parallelism>:<argon2-version>:<salt-hex>:<key-hex>
```

Parallelism (1) and argon2-version (19) are recorded so an ironclad default change becomes detectable. `verify-password` compares octets under `ironclad:constant-time-equal`.

`mint-token` / `verify-token`:

```
<nonce-hex>.<expiry-unix>.<payload-hex>.<tag-hex>
```

HMAC-SHA256 signing input is `"lolweb-tok" 0x01 "v1" 0x01 <nonce> 0x01 <expiry> 0x01 <payload>` — domain-tagged and version-tagged so a sibling token type under the same key cannot collide. Payload is integrity-only, not encrypted. Parser rejects empty `nonce` / `expiry` segments and any non-hex field before any HMAC work; an empty `payload` is intentionally allowed.

## Running tests

Tests run at build time via FiveAM. `nix build` fails on test failure.

## Tailwind CDN

`html-page` pins the Tailwind Play CDN to a specific version and supplies a
SHA-384 subresource-integrity attribute so the browser refuses any bundle whose
hash does not match. The current pin lives in `src/html/page.lisp`:

- URL: `https://cdn.tailwindcss.com/3.4.16`
- Integrity: `sha384-mS5Uq7sE90lgbBDN8xgf34ibEgbZo4gB3tfLY40ZRle+M188BQw8onzNHg6GUZaA`

To rotate the pin, fetch the new bundle (`wget -qO /tmp/tw.js
https://cdn.tailwindcss.com/<version>`), compute `openssl dgst -sha384 -binary
/tmp/tw.js | openssl base64 -A`, and update both the URL and the
`:integrity` attribute. The `regression-html-page-tailwind-cdn-has-sri`
regression asserts both attributes are present so a half-rotation fails CI.

## License

[MIT](LICENSE)
