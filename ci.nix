let
  inherit (import ./.) project;
in
  project.revenue-sharing.components.exes.create-revenue-split-sc
