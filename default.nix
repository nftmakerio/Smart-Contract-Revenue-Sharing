let
  packages = import ./nix;

  inherit (packages) pkgs revenue-sharing;
  project = revenue-sharing.haskell.project;
in
{
  inherit pkgs revenue-sharing;

  inherit project;
}
