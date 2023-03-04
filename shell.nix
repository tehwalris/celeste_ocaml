{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = (with pkgs; [
    ocamlformat
    opam
  ]) ++ (with pkgs.ocamlPackages; [
    ocaml
    ocaml-lsp
  ]);
}
