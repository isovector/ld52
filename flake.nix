{
  description = "Ludum Dare 52";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # This is one of the last commits from https://github.com/NixOS/nixpkgs/pull/169513.
    # This commit is used because it is for the `haskell-updates` branch.
    # Hydra builds a GHC for static linking on this branch, so we can get
    # most dependencies from the NixOS cache.
    nixpkgs.url = "github:NixOS/nixpkgs/999a0e87f9b1792e229c53602e80bda6dd52c105";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }: let
    name = "ld52";
    defaultGhcVersion = "ghc8107";
  in
    {
      overlays = {
        "${name}-haskell-overlay" = final: prev: {
          haskell =
            prev.haskell
            // {
              packageOverrides =
                final.lib.composeExtensions
                prev.haskell.packageOverrides
                (hfinal: _: {
                  ${name} = hfinal.callCabal2nix name ./. {};
                  ldtk-types = hfinal.callPackage ./nix/deps/ldtk-types.nix {};
                });
            };

          ${name} = final.haskell.packages.${defaultGhcVersion}.${name};
          "${name}-pkgset" = final.pkgsStatic.haskell.packages.${defaultGhcVersion};
          "${name}-static" = final.haskell.lib.justStaticExecutables final."${name}-pkgset".${name};
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = builtins.attrValues self.overlays;
        };
      in {
        packages.default = pkgs.${name};
        packages.static = pkgs."${name}-static";

        formatter = pkgs.alejandra;

      devShell = pkgs.haskell.packages.${defaultGhcVersion}.shellFor {
        shellHook = ''
          ${pkgs.hpack}/bin/hpack .
        '';
        # withHoogle = true;
        packages = p: [
          p.${name}
        ];
        buildInputs = with pkgs; [
          haskell.packages.${defaultGhcVersion}.haskell-language-server
          haskellPackages.cabal-install
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
          hpack
        ];
      };
    });
}
