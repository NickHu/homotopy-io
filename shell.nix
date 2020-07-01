{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = [ nodejs purescript spago ] ++ (with nodePackages; [purty purescript-language-server]);

  shellHook = ''
    export PATH="$PWD/node_modules/.bin/:$PATH"
  '';
}

