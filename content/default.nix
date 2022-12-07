{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:

let
  sources = import ../nix/sources.nix {};
  nix-filter = import sources.nix-filter;

in
{
  html.all = nixpkgs.stdenv.mkDerivation {
    name = "content";
    src = nix-filter {
      root = ../.;
      include = [
        "content"
        "man"
        "scripts/doc.Makefile"
        "scripts/stork.toml"
        "scripts/template.html"
      ];
    };
    nativeBuildInputs = [ nixpkgs.mandoc nixpkgs.pandoc nixpkgs.stork ];
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile
      mv _site $out

      cp -r ${(import ../.).static} $out/static
    '';
  };

  # Define this here, instead of creating a .nix file in data/.
  data = nixpkgs.stdenv.mkDerivation {
    name = "data";
    src = nix-filter {
      root = ../.;
      include = [
        "data"
      ];
    };
    installPhase = ''
      cp -r data $out
    '';
  };

  # Define this here, instead of creating a .nix file in scenarios/.
  scenarios = nixpkgs.stdenv.mkDerivation {
    name = "scenarios";
    src = nix-filter {
      root = ../.;
      include = [
        "scenarios"
      ];
    };
    installPhase = ''
      cp -r scenarios $out
    '';
  };
}
