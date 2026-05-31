{ pkgs, srcDir }:

let
  generated = import ./asdf.nix { inherit pkgs; };
  static = srcDir + "/lol-web.asd";
in
pkgs.runCommand "lol-web-asd-drift-check"
{
  passthru = { inherit generated; };
} ''
  if ! diff -u ${static} ${generated}; then
    echo
    echo "lol-web.asd is out of sync with module-table.nix."
    echo "Regenerate by copying the generated file over the static one:"
    echo "  cp ${generated} <project-root>/lol-web.asd"
    exit 1
  fi
  touch $out
''
