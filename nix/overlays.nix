# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages, if they become needed.
let

  sources = import ./sources.nix;
  inherit (sources) commence smart-design-hs;

  getOverlays = pkg : import "${pkg}/nix/overlays.nix";

  # We can overlay haskell packages here.
  haskellOverlays =
          getOverlays commence # we get 2 packages: commence-core and commence-interactive-state
       ++ getOverlays smart-design-hs
       ;

in haskellOverlays ++ [ (import ./overlay.nix) ]
