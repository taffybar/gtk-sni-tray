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
    dbus-menu = {
      url = "github:taffybar/dbus-menu";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, status-notifier-item, gtk-strut, dbus-menu }:
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
      buildInputs = with pkgs; [
        gtk-layer-shell
      ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install haskell-language-server
      ];
      shellHook = ''
        # ld.gold has been observed to crash (Bus error) on some systems during
        # GHC links. Prefer the more conservative bfd linker.
        export NIX_LDFLAGS="''${NIX_LDFLAGS:-} -fuse-ld=bfd"
      '';
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
      dbus-menu = dbus-menu.overlay or dbus-menu.overlays.default;
    };
  };
}
