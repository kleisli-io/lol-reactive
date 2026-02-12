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
              "package.lisp"

              # CSS infrastructure
              "css/registry.lisp"
              "css/tokens.lisp"
              "css/generation.lisp"
              "css/tailwind.lisp"

              # Core reactive primitives
              "components.lisp"
              "signals.lisp"

              # HTML generation
              "html.lisp"
              "html/elements.lisp"
              "html/escape.lisp"

              # Parenscript utilities
              "parenscript-utils.lisp"

              # HTMX-style runtime
              "htmx/runtime.lisp"
              "htmx/server.lisp"
              "htmx/morph.lisp"

              # Server infrastructure (Clack-based)
              "server/clack.lisp"
              "server/security.lisp"
              "server/errors.lisp"
              "server/app.lisp"
              "server/routes.lisp"

              # Composition (props, context, children)
              "composition/props.lisp"
              "composition/context.lisp"
              "composition/children.lisp"

              # Forms and async
              "forms/form-dsl.lisp"
              "async/resources.lisp"

              # Advanced features
              "advanced/wizards.lisp"

              # Real-time features (WebSocket + SSE support)
              "realtime/websocket.lisp"
              "realtime/sse.lisp"

              # Development tools
              "surgery.lisp"
              "surgery-js.lisp"

              # Rendering infrastructure
              "rendering/dom-diff.lisp"
              "rendering/keyed-list.lisp"

              # Fullstack (isomorphic components)
              "fullstack/component-api.lisp"
              "fullstack/isomorphic.lisp"

              # Optimization (compile-time analysis)
              "optimization/reactive-analysis.lisp"
              "optimization/template-validation.lisp"
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
