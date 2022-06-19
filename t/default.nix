let
  emacs-overlay = import (builtins.fetchTarball { url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz; });
  emacs-ci = import (builtins.fetchTarball { url = https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz; });

  pkgs = import <nixpkgs> {
    overlays = [ emacs-overlay ];
  };
in
builtins.mapAttrs
  (version: emacs:
    (pkgs.emacsPackagesFor emacs).emacsWithPackages
      (emacsPackages: [
      ] ++ (with emacsPackages.melpaPackages; [
        bash-completion
      ])
      ))
  emacs-ci
