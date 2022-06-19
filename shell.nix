{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.kubectl
    pkgs.argo
    (import ./default.nix).emacs-28-1
  ];
}
