{
  description = "lol-web — reactive web framework using Let Over Lambda patterns";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/88d3861acdd3d2f0e361767018218e51810df8a1";
    cl-deps.url = "github:kleisli-io/cl-deps";
    cl-deps.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, cl-deps, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      ## Map module-table.nix's external-deps names onto the cl-deps lisp
      ## attribute set. The names lol-web uses internally (clack-session,
      ## clack-csrf, clack-static, clack-accesslog, clack-cors, lack-core)
      ## predate cl-deps's flat upstream-aligned naming, so the mapping
      ## bridges the two vocabularies.
      mkLispDepsByName = lisp: with lisp; {
        inherit alexandria iterate cl-ppcre babel
          ironclad bordeaux-threads
          let-over-lambda
          cl-who parenscript jzon hunchentoot
          flexi-streams puri
          clack clack-handler-hunchentoot
          websocket-driver-server
          lack-test;
        lack-core = lack;
        clack-session = lack-middleware-session;
        clack-csrf = lack-middleware-csrf;
        clack-static = lack-middleware-static;
        clack-accesslog = lack-middleware-accesslog;
        clack-cors = lack-middleware-cors;
      };

      ## Build per system. `built.library` is the umbrella derivation with
      ## tests + passthru.asdfDriftCheck. `built.modules` is an attrset of
      ## per-sub-system standalone derivations keyed by short name (e.g.,
      ## "server", "extractors").
      buildFor = system:
        let
          inherit (cl-deps.lib.${system}) buildLisp lisp;
          pkgs = nixpkgs.legacyPackages.${system};
        in
        import ./nix/build.nix {
          inherit buildLisp pkgs;
          srcDir = ./.;
          lispDepsByName = mkLispDepsByName lisp;
          testDeps = [ lisp.fiveam ];
        };
    in
    {
      lib = forAllSystems (system: {
        library = (buildFor system).library;
        modules = (buildFor system).modules;
      });

      packages = forAllSystems (system: {
        default = (buildFor system).library;
      });

      ## Every per-sub-system derivation, the umbrella, and the ASDF
      ## manifest drift check are exposed as flake checks so
      ## `nix flake check` exercises the complete build matrix in one
      ## invocation (CI's gate). Sub-system check names are prefixed
      ## with `module-` so the umbrella's plain `library` attr stays
      ## discoverable in `nix flake show`.
      checks = forAllSystems (system:
        let
          built = buildFor system;
          moduleChecks = nixpkgs.lib.mapAttrs'
            (name: drv: nixpkgs.lib.nameValuePair "module-${name}" drv)
            built.modules;
        in
        moduleChecks // {
          library = built.library;
          asdf-drift = built.library.asdfDriftCheck;
        });
    };
}
