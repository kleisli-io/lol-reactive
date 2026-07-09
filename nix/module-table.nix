{
  modules = {
    escape = {
      package-src = "src/packages/escape.lisp";
      srcs = [
        "src/escape/escape.lisp"
      ];
      internal-deps = [ ];
      external-deps = [ "iterate" "cl-ppcre" ];
      test-srcs = [
        "t/escape/package.lisp"
        "t/escape/suite.lisp"
        "t/escape/escape.lisp"
      ];
    };

    crypto = {
      package-src = "src/packages/crypto.lisp";
      srcs = [
        "src/crypto/util.lisp"
        "src/crypto/password.lisp"
        "src/crypto/token.lisp"
      ];
      internal-deps = [ ];
      external-deps = [ "ironclad" "babel" ];
      test-srcs = [
        "t/crypto/package.lisp"
        "t/crypto/suite.lisp"
        "t/crypto/util.lisp"
        "t/crypto/password.lisp"
        "t/crypto/token.lisp"
      ];
    };

    core = {
      package-src = "src/packages/core.lisp";
      srcs = [
        "src/core/components.lisp"
        "src/core/signals.lisp"
        "src/core/state.lisp"
        "src/core/collections.lisp"
        "src/core/bounded-cache.lisp"
        "src/core/cycle-safe-printer.lisp"
        "src/composition/props.lisp"
        "src/composition/context.lisp"
        "src/composition/children.lisp"
      ];
      internal-deps = [ "crypto" ];
      external-deps = [ "iterate" "let-over-lambda" "bordeaux-threads" "alexandria" ];
      test-srcs = [
        "t/core/package.lisp"
        "t/core/suite.lisp"
        "t/core/signals.lisp"
        "t/core/components.lisp"
        "t/core/effects.lisp"
        "t/core/thread-safety.lisp"
      ];
    };

    css = {
      package-src = "src/packages/css.lisp";
      srcs = [
        ## safety.lisp defines safe-css-payload-string and safe-css-ident-p
        ## which generation.lisp references. generation.lisp must load
        ## before registry.lisp so make-css-module's :render closure can
        ## resolve css-rule / css-keyframes.
        "src/css/safety.lisp"
        "src/css/generation.lisp"
        "src/css/registry.lisp"
        "src/css/tokens.lisp"
        "src/css/tailwind.lisp"
      ];
      internal-deps = [ ];
      external-deps = [ "iterate" "alexandria" "parenscript" "bordeaux-threads" ];
      test-srcs = [
        "t/css/package.lisp"
        "t/css/suite.lisp"
        "t/css/registry.lisp"
        "t/css/tokens.lisp"
        "t/css/tailwind.lisp"
        "t/css/generation.lisp"
      ];
    };

    parenscript = {
      package-src = "src/packages/parenscript.lisp";
      srcs = [
        "src/client/safety.lisp"
        "src/client/parenscript.lisp"
      ];
      internal-deps = [ "escape" ];
      external-deps = [ "iterate" "let-over-lambda" "parenscript" "cl-who" ];
      test-srcs = [
        "t/parenscript/package.lisp"
        "t/parenscript/suite.lisp"
        "t/parenscript/parenscript.lisp"
        "t/parenscript/regression.lisp"
      ];
    };

    html = {
      package-src = "src/packages/html.lisp";
      srcs = [
        "src/html/elements.lisp"
        "src/html/page.lisp"
        "src/html/csp-conformance.lisp"
        "src/html/escape.lisp"
      ];
      internal-deps = [ "escape" ];
      external-deps = [ "iterate" "cl-who" "alexandria" "parenscript" "cl-ppcre" ];
      test-srcs = [
        "t/html/package.lisp"
        "t/html/suite.lisp"
        "t/html/runtime.lisp"
        "t/html/regression.lisp"
      ];
    };

    server = {
      package-src = "src/packages/server.lisp";
      srcs = [
        "src/server/clack.lisp"
        "src/server/security.lisp"
        "src/server/http-errors.lisp"
        "src/server/errors.lisp"
        "src/server/assets.lisp"
        "src/server/app.lisp"
        "src/server/streaming-gate.lisp"
        "src/server/routes.lisp"
      ];
      internal-deps = [ "escape" "core" "html" "crypto" "jschema" ];
      external-deps = [
        "iterate"
        "let-over-lambda"
        "alexandria"
        "cl-ppcre"
        "babel"
        "ironclad"
        "bordeaux-threads"
        "jzon"
        "puri"
        "hunchentoot"
        "flexi-streams"
        "clack"
        "lack-core"
        "clack-session"
        "clack-csrf"
        "clack-static"
        "clack-accesslog"
        "clack-cors"
        "clack-handler-hunchentoot"
      ];
      test-external-deps = [ "lack-test" ];
      test-srcs = [
        "t/server/package.lisp"
        "t/server/suite.lisp"
        "t/server/server.lisp"
        "t/server/regression.lisp"
        "t/server/assets.lisp"
        "t/server/csp-invariant.lisp"
        "t/server/http-e2e.lisp"
      ];
    };

    jschema = {
      package-src = "src/packages/jschema.lisp";
      srcs = [
        "src/jschema/registry.lisp"
        "src/jschema/conditions.lisp"
        "src/jschema/parse.lisp"
        "src/jschema/validate.lisp"
        "src/jschema/keywords.lisp"
      ];
      internal-deps = [ "escape" ];
      external-deps = [ "alexandria" "bordeaux-threads" "cl-ppcre" "jzon" "puri" ];
      test-srcs = [
        "t/jschema/package.lisp"
        "t/jschema/suite.lisp"
        "t/jschema/regression.lisp"
      ];
    };

    extractors = {
      package-src = "src/packages/extractors.lisp";
      srcs = [
        "src/extractors/registry.lisp"
        "src/extractors/coercion.lisp"
        "src/extractors/builtin.lisp"
        "src/extractors/defhandler.lisp"
        "src/extractors/sentinel.lisp"
      ];
      internal-deps = [ "escape" "server" ];
      external-deps = [ "let-over-lambda" "bordeaux-threads" "babel" "flexi-streams" ];
      test-srcs = [
        "t/extractors/package.lisp"
        "t/extractors/suite.lisp"
        "t/extractors/regression.lisp"
      ];
    };

    openapi = {
      package-src = "src/packages/openapi.lisp";
      srcs = [
        "src/openapi/schema-mapping.lisp"
        "src/openapi/spec-builder.lisp"
      ];
      internal-deps = [ "server" "extractors" ];
      external-deps = [ "bordeaux-threads" "cl-ppcre" ];
      test-srcs = [
        "t/openapi/package.lisp"
        "t/openapi/suite.lisp"
        "t/openapi/regression.lisp"
      ];
      ## The conformance gate parses and validates against the bundled
      ## OpenAPI 3.1 base schema; that needs jschema's API at test
      ## compile time. The production sub-system stays jschema-free —
      ## emitting a spec doesn't require validating it.
      test-internal-deps = [ "jschema" ];
    };

    htmx = {
      package-src = "src/packages/htmx.lisp";
      srcs = [
        "src/htmx/runtime/config.lisp"
        "src/htmx/runtime/swap.lisp"
        "src/htmx/runtime/ajax.lisp"
        "src/htmx/runtime/triggers.lisp"
        "src/htmx/runtime.lisp"
        "src/htmx/oob.lisp"
        "src/htmx/autocomplete.lisp"
        "src/htmx/server.lisp"
        "src/htmx/morph.lisp"
      ];
      internal-deps = [ "escape" "crypto" "css" "html" "server" "parenscript" ];
      external-deps = [ "iterate" "parenscript" "cl-ppcre" "cl-who" "babel" ];
      test-external-deps = [ "lack-test" "bordeaux-threads" ];
      test-srcs = [
        "t/htmx/package.lisp"
        "t/htmx/suite.lisp"
        "t/htmx/htmx.lisp"
        "t/htmx/regression.lisp"
        "t/htmx/e2e.lisp"
      ];
      ## e2e.lisp's hx-on-strip coverage drives the broadcast-OOB path,
      ## which lives in realtime; production htmx stays realtime-free.
      test-internal-deps = [ "realtime" ];
    };

    realtime = {
      package-src = "src/packages/realtime.lisp";
      srcs = [
        "src/realtime/websocket.lisp"
        "src/realtime/sse.lisp"
      ];
      internal-deps = [ "escape" "html" "server" ];
      external-deps = [ "iterate" "websocket-driver-server" "bordeaux-threads" "hunchentoot" "babel" ];
      test-external-deps = [ "lack-test" ];
      test-srcs = [
        "t/realtime/package.lisp"
        "t/realtime/suite.lisp"
        "t/realtime/regression.lisp"
        "t/realtime/e2e.lisp"
      ];
    };

    realtime-htmx = {
      package-src = "src/packages/realtime-htmx.lisp";
      srcs = [
        "src/realtime/ws-client.lisp"
        "src/realtime/sse-client.lisp"
        "src/realtime/optimistic.lisp"
      ];
      internal-deps = [ "core" "html" ];
      external-deps = [ "iterate" "parenscript" ];
      test-srcs = [
        "t/realtime-htmx/package.lisp"
        "t/realtime-htmx/suite.lisp"
        "t/realtime-htmx/regression.lisp"
      ];
    };

    resources = {
      package-src = "src/packages/resources.lisp";
      srcs = [
        "src/async/resources.lisp"
      ];
      internal-deps = [ "core" "css" "html" ];
      external-deps = [ "iterate" "let-over-lambda" "bordeaux-threads" "cl-who" ];
      test-srcs = [
        "t/resources/package.lisp"
        "t/resources/suite.lisp"
        "t/resources/regression.lisp"
      ];
    };

    forms = {
      package-src = "src/packages/forms.lisp";
      srcs = [
        "src/forms/form-dsl.lisp"
      ];
      internal-deps = [ "escape" "core" "css" "html" "server" ];
      external-deps = [ "iterate" "let-over-lambda" "parenscript" "cl-who" "cl-ppcre" ];
      test-srcs = [
        "t/forms/package.lisp"
        "t/forms/suite.lisp"
        "t/forms/regression.lisp"
      ];
    };

    wizards = {
      package-src = "src/packages/wizards.lisp";
      srcs = [
        "src/wizards/wizards.lisp"
      ];
      internal-deps = [ "escape" "css" "html" "server" "extractors" ];
      external-deps = [ "iterate" "let-over-lambda" "alexandria" "cl-who" "cl-ppcre" "bordeaux-threads" ];
      test-external-deps = [ "lack-test" ];
      test-internal-deps = [ "openapi" ];
      test-srcs = [
        "t/wizards/package.lisp"
        "t/wizards/suite.lisp"
        "t/wizards/wizards.lisp"
        "t/wizards/regression.lisp"
        "t/wizards/e2e.lisp"
      ];
    };

    devtools = {
      package-src = "src/packages/devtools.lisp";
      srcs = [
        "src/devtools/surgery.lisp"
        "src/devtools/surgery-js.lisp"
        "src/devtools/surgery-routes.lisp"
      ];
      internal-deps = [ "escape" "core" "html" "server" "extractors" ];
      external-deps = [ "iterate" "let-over-lambda" "parenscript" "cl-who" ];
      ## Opt-in: not pulled in by the umbrella system. Consumers that want
      ## surgery must (asdf:load-system :lol-web/devtools) explicitly and
      ## pair it with their own auth gating. The surgery panel modifies
      ## live closure state and renders attacker-controllable component
      ## IDs; shipping it by default is an auto-registration footgun.
      opt-in = true;
      test-srcs = [
        "t/devtools/package.lisp"
        "t/devtools/suite.lisp"
        "t/devtools/surgery.lisp"
        "t/devtools/regression.lisp"
      ];
    };

    rendering = {
      package-src = "src/packages/rendering.lisp";
      srcs = [
        "src/rendering/keyed-list.lisp"
      ];
      internal-deps = [ "core" "html" ];
      external-deps = [ "iterate" "let-over-lambda" ];
      test-srcs = [
        "t/rendering/package.lisp"
        "t/rendering/suite.lisp"
        "t/rendering/keyed-list.lisp"
      ];
    };

    fullstack = {
      package-src = "src/packages/fullstack.lisp";
      srcs = [
        "src/fullstack/hydration.lisp"
        "src/fullstack/component-api.lisp"
        "src/fullstack/isomorphic.lisp"
      ];
      internal-deps = [ "escape" "crypto" "core" "html" "server" "extractors" ];
      external-deps = [ "iterate" "let-over-lambda" "parenscript" "alexandria" "cl-who" ];
      test-external-deps = [ "lack-test" ];
      test-srcs = [
        "t/fullstack/package.lisp"
        "t/fullstack/suite.lisp"
        "t/fullstack/regression.lisp"
        "t/fullstack/e2e.lisp"
      ];
    };

    optimization = {
      package-src = "src/packages/optimization.lisp";
      srcs = [
        "src/optimization/reactive-analysis.lisp"
        "src/optimization/template-validation.lisp"
      ];
      internal-deps = [ "core" "css" "html" "server" ];
      external-deps = [ "iterate" "let-over-lambda" "alexandria" "cl-ppcre" "cl-who" ];
      test-srcs = [
        "t/optimization/package.lisp"
        "t/optimization/suite.lisp"
        "t/optimization/regression.lisp"
      ];
    };

    client-runtime = {
      package-src = "src/packages/client-runtime.lisp";
      srcs = [
        "src/client/runtime.lisp"
      ];
      internal-deps = [ "html" "htmx" "realtime-htmx" ];
      external-deps = [ "iterate" "parenscript" ];
      test-srcs = [
        "t/client-runtime/package.lisp"
        "t/client-runtime/suite.lisp"
        "t/client-runtime/regression.lisp"
      ];
    };
  };

  ## Build order at the umbrella layer. Each module's package-src and srcs
  ## are concatenated in this order before the umbrella file (src/package.lisp,
  ## defining :lol-web and :lol-reactive) loads.
  load-order = [
    "escape"
    "crypto"
    "core"
    "css"
    "parenscript"
    "html"
    "server"
    "jschema"
    "extractors"
    "openapi"
    "htmx"
    "realtime"
    "realtime-htmx"
    "resources"
    "forms"
    "wizards"
    "devtools"
    "rendering"
    "fullstack"
    "optimization"
    "client-runtime"
  ];

  umbrella-src = "src/package.lisp";

  ## Umbrella-level test files: shim parity assertions that need both
  ## :lol-web and :lol-reactive loaded, plus the run-all-tests aggregator
  ## that the umbrella build's test phase invokes.
  umbrella-test-srcs = [
    "t/umbrella/package.lisp"
    "t/umbrella/suite.lisp"
    "t/umbrella/regression.lisp"
  ];
}
