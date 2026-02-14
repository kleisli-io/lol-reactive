{
  description = "lol-reactive — reactive web framework using Let Over Lambda patterns";

  inputs = {
    # Pinned to same nixpkgs as core depot — SBCL 2.5.7
    nixpkgs.url = "github:NixOS/nixpkgs/88d3861acdd3d2f0e361767018218e51810df8a1";
    cl-deps.url = "github:kleisli-io/cl-deps";
    cl-deps.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, cl-deps, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in {
      lib = forAllSystems (system:
        let
          inherit (cl-deps.lib.${system}) buildLisp lisp;
        in {
          library = buildLisp.library {
            name = "lol-reactive";

            deps = with lisp; [
              # Core dependencies
              alexandria iterate cl-ppcre babel

              # Let Over Lambda
              let-over-lambda

              # Web stack
              cl-who parenscript cl-json hunchentoot
              clack lack

              # Lack middlewares
              lack-middleware-session
              lack-middleware-csrf
              lack-middleware-static
              lack-middleware-accesslog

              # Clack handler
              clack-handler-hunchentoot

              # WebSocket support
              websocket-driver-server
            ];

            srcs = map (f: ./. + "/${f}") [
              # Package definition
              "src/package.lisp"

              # CSS infrastructure
              "src/css/registry.lisp"
              "src/css/tokens.lisp"
              "src/css/generation.lisp"
              "src/css/tailwind.lisp"

              # Core reactive primitives
              "src/core/components.lisp"
              "src/core/signals.lisp"
              "src/core/state.lisp"
              "src/core/collections.lisp"

              # HTML generation (elements first — defines htm, htm-str, etc.)
              "src/html/elements.lisp"
              "src/html/page.lisp"
              "src/html/escape.lisp"

              # Client-side utilities
              "src/client/parenscript.lisp"

              # HTMX runtime + extensions
              "src/htmx/runtime.lisp"
              "src/htmx/oob.lisp"
              "src/htmx/autocomplete.lisp"
              "src/htmx/server.lisp"
              "src/htmx/morph.lisp"

              # Server infrastructure (Clack-based)
              "src/server/clack.lisp"
              "src/server/security.lisp"
              "src/server/errors.lisp"
              "src/server/app.lisp"
              "src/server/routes.lisp"

              # Composition (props, context, children)
              "src/composition/props.lisp"
              "src/composition/context.lisp"
              "src/composition/children.lisp"

              # Forms and async
              "src/forms/form-dsl.lisp"
              "src/async/resources.lisp"

              # Advanced features
              "src/advanced/wizards.lisp"

              # Real-time features (server-side WebSocket + SSE)
              "src/realtime/websocket.lisp"
              "src/realtime/sse.lisp"

              # Real-time client runtimes (Parenscript)
              "src/realtime/ws-client.lisp"
              "src/realtime/sse-client.lisp"
              "src/realtime/optimistic.lisp"

              # Development tools
              "src/devtools/surgery.lisp"
              "src/devtools/surgery-js.lisp"

              # Rendering infrastructure
              "src/rendering/dom-diff.lisp"
              "src/rendering/keyed-list.lisp"

              # Fullstack (isomorphic components)
              "src/fullstack/component-api.lisp"
              "src/fullstack/isomorphic.lisp"

              # Optimization (compile-time analysis)
              "src/optimization/reactive-analysis.lisp"
              "src/optimization/template-validation.lisp"

              # Combined client runtime (must be last — aggregates all JS)
              "src/client/runtime.lisp"
            ];

            # FiveAM test suite - build fails if tests don't pass
            tests = {
              deps = [ lisp.fiveam ];
              srcs = map (f: ./t + "/${f}") [
                "package.lisp"
                "suite.lisp"
                "signals.lisp"
                "components.lisp"
                "surgery.lisp"
                "dom-diff.lisp"
                "keyed-list.lisp"
                "wizards.lisp"
                "htmx.lisp"
                "server.lisp"
                "parenscript.lisp"
                "regression.lisp"
              ];
              expression = "(lol-reactive.tests:run-all-tests)";
            };
          };
        });

      packages = forAllSystems (system:
        let
          inherit (cl-deps.lib.${system}) buildLisp lisp;
          library = self.lib.${system}.library;
        in {
          default = library;

          demo = buildLisp.program {
            name = "lol-reactive-demo";
            deps = [ library ];
            srcs = [
              ./demo/package.lisp
              ./demo/theme.lisp
              ./demo/app.lisp
              ./demo/showcase.lisp
              ./demo/main.lisp
            ];
            main = "lol-reactive-demo:main";
          };
        });
    };
}
