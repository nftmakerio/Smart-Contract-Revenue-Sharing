let
  packages = import ./nix;
in {
  inherit (packages) pkgs revenue-sharing plutus cardano-node;

  inherit (packages.revenue-sharing.haskell) project;
}
