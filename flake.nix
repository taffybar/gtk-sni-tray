{
  description = "gtk-sni-tray";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
    gtk-strut = {
      url = "github:taffybar/gtk-strut";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, status-notifier-item, gtk-strut }:
  flake-utils.lib.eachDefaultSystem (system: let
    inherit (nixpkgs) lib;
    pkgs = import nixpkgs {
      inherit system;
      overlays = lib.attrValues self.overlays;
      config.allowBroken = true;
    };
  in
  {
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-sni-tray ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install haskell-language-server
      ];
    };
    packages.default = pkgs.haskellPackages.gtk-sni-tray;
  }) // {
    overlays = {
      default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            gtk-sni-tray =
              hself.callCabal2nix "gtk-sni-tray"
              (git-ignore-nix.lib.gitignoreSource ./.)
              { inherit (final) gtk3;  };
          });
        });
      };
      status-notifier-item = status-notifier-item.overlay or status-notifier-item.overlays.default;
      gtk-strut = gtk-strut.overlay or gtk-strut.overlays.default;
    };
  };
}
