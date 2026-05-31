{ buildLisp, pkgs, srcDir, lispDepsByName, testDeps }:

let
  table = import ./module-table.nix;

  toPath = s: srcDir + ("/" + s);

  resolveExternal = names: map (n: lispDepsByName.${n}) names;

  ## Some test sources reference an in-tree fixture file that asdf:
  ## system-relative-pathname can't resolve under buildLisp (each source
  ## lands in the Nix store individually with no system root). The
  ## buildLisp wrapper copies the source out and substitutes a literal
  ## Nix-store path for the bundled fixture before compilation. Pattern
  ## mirrors `nix/cl-deps/default.nix`'s quri-etld substitution.
  openapiSchemaFixture = toPath "t/openapi/fixtures/openapi-3.1-schema.json";

  testSrcPath = moduleName: file:
    if moduleName == "openapi" && file == "t/openapi/regression.lisp"
    then
      pkgs.runCommand "lol-web-openapi-regression.lisp" { } ''
        substitute ${toPath file} $out --replace-fail \
          ${pkgs.lib.escapeShellArg "(defparameter *openapi-3.1-schema-path* nil"} \
          ${pkgs.lib.escapeShellArg ''(defparameter *openapi-3.1-schema-path* "${openapiSchemaFixture}"''}
      ''
    else toPath file;

  ## Per-sub-system buildLisp.library derivations. Each compiles its own
  ## package.lisp + sources against only the libraries it actually needs.
  ## internal-deps are resolved against `modules` itself (let-rec).
  ##
  ## Sub-systems with non-empty test-srcs gain a `tests = { ... }` block
  ## that runs ONLY that sub-system's suite — `nix build … "lol-web/<sub>"`
  ## verifies the sub-system in isolation. The umbrella's test phase
  ## (further down) runs every sub-system's tests plus the umbrella suite.
  ## test-internal-deps adds extra in-tree sub-systems to the standalone
  ## test closure (e.g. openapi's tests pull jschema for the conformance
  ## gate, while production openapi keeps a jschema-free dep set).
  ## test-external-deps adds extra external libraries to the test closure
  ## without leaking into the production closure (e.g. server's tests pull
  ## lack-test for synthetic-request E2E coverage).
  modules = builtins.mapAttrs
    (name: cfg:
      let
        baseAttrs = {
          ## Hyphen, not slash — buildLisp uses this verbatim as a fasl file
          ## name and a slash would be interpreted as a directory separator.
          ## The consumer-facing attribute path keeps the slash form.
          name = "lol-web-${name}";
          deps = resolveExternal cfg.external-deps
            ++ map (n: modules.${n}) cfg.internal-deps;
          srcs = map toPath ([ cfg.package-src ] ++ cfg.srcs);
        };
        testInternal = cfg.test-internal-deps or [ ];
        testExternal = cfg.test-external-deps or [ ];
        testsAttr = pkgs.lib.optionalAttrs (cfg.test-srcs != [ ]) {
          tests = {
            deps = testDeps
              ++ map (n: modules.${n}) testInternal
              ++ resolveExternal testExternal;
            srcs = map (testSrcPath name) cfg.test-srcs;
            expression = ''(lol-web/${name}/test:run-tests)'';
          };
        };
      in
      buildLisp.library (baseAttrs // testsAttr))
    table.modules;

  ## All per-module package.lisp files first, in defpackage-topological order.
  ## This makes every :lol-web/<sub> package visible before any module source
  ## loads, so cross-package symbol references in source files (e.g., a forward
  ## reference like `lol-web/htmx:foo`) resolve at READ time even when the
  ## sub-system that exports them hasn't compiled yet.
  packageSrcsInOrder =
    map (name: table.modules.${name}.package-src) table.load-order;

  moduleBodySrcsInOrder =
    builtins.concatLists
      (map (name: table.modules.${name}.srcs) table.load-order);

  ## Umbrella concatenates: every package.lisp first (in topological order),
  ## then every module's body sources (in load-order), then the umbrella file
  ## (which defines :lol-web and the :lol-reactive shim).
  allExternalDeps =
    let
      names = builtins.concatLists
        (map (cfg: cfg.external-deps) (builtins.attrValues table.modules));
    in
    pkgs.lib.unique names;

  ## Test-only externals across all modules. Threaded into the umbrella's
  ## test phase but kept out of the umbrella's production dep closure.
  allTestExternalDeps =
    let
      names = builtins.concatLists
        (map (cfg: cfg.test-external-deps or [ ])
          (builtins.attrValues table.modules));
    in
    pkgs.lib.unique names;

  ## Umbrella's test phase loads every per-sub-system test file (so each
  ## :lol-web/<sub>/test suite is registered) and then the umbrella's own
  ## test files (shim parity + run-all-tests aggregator). After loading,
  ## the expression `(lol-web/test:run-all-tests)` invokes every suite in
  ## sequence — each via its own `(fiveam:run! :lol-web/<sub>/test)` call.
  umbrellaTestSrcs =
    builtins.concatLists (map (name: table.modules.${name}.test-srcs) table.load-order)
    ++ table.umbrella-test-srcs;

  umbrellaLib = buildLisp.library {
    name = "lol-web";
    deps = resolveExternal allExternalDeps;
    srcs = map toPath (packageSrcsInOrder
      ++ moduleBodySrcsInOrder
      ++ [ table.umbrella-src ]);
    tests = {
      deps = testDeps ++ resolveExternal allTestExternalDeps;
      srcs = map toPath umbrellaTestSrcs;
      expression = "(lol-web/test:run-all-tests)";
    };
    passthru = {
      asdfDriftCheck = import ./drift-check.nix { inherit pkgs srcDir; };
    };
  };

in
{
  library = umbrellaLib;
  inherit modules;
}
