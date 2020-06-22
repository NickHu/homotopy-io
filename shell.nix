{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = [ nodejs purescript spago ];

  shellHook = ''
    export PATH="$PWD/node_modules/.bin/:$PATH"
  '';
}

