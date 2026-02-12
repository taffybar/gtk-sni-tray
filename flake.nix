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
    # Keep the default dev shell lightweight so `direnv reload` doesn't have to
    # build a full GHC-with-packages environment (which can be slow and/or
    # require building git-based Haskell deps).
    devShells.default = pkgs.mkShell {
      nativeBuildInputs = (with pkgs; [
        pkg-config
        dbus
      ]) ++ (with pkgs.haskellPackages; [
        ghc
        cabal-install
        haskell-language-server
      ]);
      buildInputs = with pkgs; [
        gtk3
        gtk-layer-shell
      ];
      shellHook = ''
        # ld.gold has been observed to crash (Bus error) on some systems during
        # GHC links. Prefer the more conservative bfd linker.
        export NIX_LDFLAGS="''${NIX_LDFLAGS:-} -fuse-ld=bfd"
      '';
    };

    # A heavier shell that wires the project into nixpkgs' Haskell package set.
    # Useful for fully-Nix builds, but intentionally not the default.
    devShells.shellFor = pkgs.haskellPackages.shellFor {
      packages = p: [ p.gtk-sni-tray ];
      buildInputs = with pkgs; [
        gtk-layer-shell
      ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        cabal-install haskell-language-server
      ];
      shellHook = ''
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

      # Keep `direnv reload` / `use flake` working: the devShell ends up building
      # Haskell deps, and status-notifier-item's tests expect `dbus-daemon` to be
      # available on PATH.
      #
      # This overlay name is intentionally lexicographically last so it is
      # applied after the upstream overlays.
      zz-direnv-fixes = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
            (hself: hsuper: {
              status-notifier-item =
                final.haskell.lib.addBuildTool hsuper.status-notifier-item final.dbus;
            });
        });
      };
    };
  };
}
