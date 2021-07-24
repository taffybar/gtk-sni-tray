{
  description = "gtk-sni-tray";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:IvanMalison/gitignore.nix/master";
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          gtk-sni-tray =
            hself.callCabal2nix "gtk-sni-tray"
            (git-ignore-nix.gitIgnoreSource ./.)
            { inherit (final) gtk3;  };
        });
      });
    };
    overlays = [overlay];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-sni-tray ];
    };
    defaultPackage = pkgs.haskellPackages.gtk-sni-tray;
  }) // { inherit overlay overlays; } ;
}
