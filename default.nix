let
  pkgs = (import <nixpkgs> {});
in pkgs.haskellPackages.callCabal2nix "gtk-sni-tray" ./. { inherit (pkgs) gtk3; }
