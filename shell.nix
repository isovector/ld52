(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hsc2hs = "latest";
    hpack = "latest";
  };

  # See https://github.com/input-output-hk/haskell.nix/issues/1608 for why
  # we set exactDeps = false
  exactDeps = false;

  withHoogle = false;

  buildInputs = [ ];
}
