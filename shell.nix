let
  packages = import ./.;
  inherit (packages) pkgs revenue-sharing;
  inherit (revenue-sharing) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with revenue-sharing; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }
