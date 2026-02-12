# LOL-REACTIVE Demo - Task Tracker
# Demonstrates composable themes and reactive patterns
{ core, ... }:

let
  lol-reactive = core.third_party.languages.lisp.lol-reactive;

  srcs = [
    ./package.lisp
    ./theme.lisp
    ./app.lisp
    ./main.lisp
  ];

in core.nix.buildLisp.program {
  name = "lol-reactive-demo";

  deps = [ lol-reactive ];

  inherit srcs;

  main = "lol-reactive-demo:main";
}
