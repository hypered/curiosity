let

  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;

  # Overlays let us override certain packages at a central location.
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };
  nixpkgs = import sources.nixpkgs { };
  hp = nixpkgs-overlayed.haskellPackages;

  inherit (nixpkgs.lib.attrsets) getAttrFromPath;

  contents = import ./nix/contents.nix { inherit nixpkgs; };

  # Brittany, as the formatter, is just here as an example.
  # I personally prefer to have the formatters pinned to a version and then
  # made available via direnv to avoid unnecessary diff pollution across upgrades.

  # Niv is great at pinning dependencies in sources.json and computing SHA's etc.
  nix-tooling = with hp; [ (callCabal2nix "niv" sources.niv { }) ];

  # Haskell tools
  haskell-tooling = with hp; [ cabal-install ghcid hlint hasktags ];

  # Add more as we need them.
  formatters = [ nixpkgs.ormolu nixpkgs.treefmt ] ;

  system-tooling = with nixpkgs; [
    inotify-tools # needed for HotExe.sh (filesystem notifs.)
    psmisc # needed for HotExe.sh: (kill processes by port.)
  ];

in hp.shellFor {
  packages = p: map (contents.getPkg p) (builtins.attrNames contents.pkgList);
  buildInputs = nix-tooling ++ haskell-tooling ++ system-tooling ++ formatters;
}
