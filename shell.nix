{ nixpkgs ? import <nixpkgs> }:
let
    pkgs = nixpkgs {  };
in
    pkgs.mkShell {
        buildInputs = [
            pkgs.haskellPackages.threadscope
        ];
    }