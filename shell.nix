{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = (with pkgs; [
    gmp
    mpfr
    inotify-tools
    ocamlformat
  ]) ++ (with pkgs.ocamlPackages; [
    ocaml
    ocaml-lsp
    dune_2
    findlib
    apron
    ppxlib
    ppx_deriving
  ]);
}
