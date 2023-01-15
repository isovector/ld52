let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.macaroniNix {};
in pkgs.haskell-nix.cabalProject { # macaroni.nix is currently only tested with cabal
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "ld52";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc925"; # macaroni.nix is currently only tested on GHC 9.2.5

  # This is necessary for x-compilation
  # See https://github.com/input-output-hk/haskell.nix/issues/1666
  evalPackages = pkgs;
}
