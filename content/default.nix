{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:

let
  sources = import ../nix/sources.nix {};
  nix-filter = import sources.nix-filter;

in rec
{
  indexes = nixpkgs.stdenv.mkDerivation {
    name = "indexes";
    src = nix-filter {
      root = ../.;
      include = [
        "content"
        "scripts/doc.Makefile"
        "scripts/stork.css"
        "scripts/stork.toml"
      ];
    };
    nativeBuildInputs = [ nixpkgs.mandoc nixpkgs.pandoc nixpkgs.stork ];
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile _site/static/indexes/content.st
      make -f scripts/doc.Makefile _site/static/indexes/stork.css
      mv _site/static $out
    '';
  };

  asciinema = nixpkgs.stdenv.mkDerivation {
    name = "asciinema";
    src = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "scripts/doc.Makefile"
        (and (matchExt "cast") (inDirectory "scripts"))
        (and (matchExt "css") (inDirectory "scripts"))
        (and (matchExt "js") (inDirectory "scripts"))
      ];
    };
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile _site/static/asciinema/demo.cast
      make -f scripts/doc.Makefile _site/static/asciinema/asciinema-player-3.0.1.css
      make -f scripts/doc.Makefile _site/static/asciinema/asciinema-player-3.0.1.min.js
      mv _site/static $out
    '';
  };

  html.all = nixpkgs.stdenv.mkDerivation {
    name = "content";
    src = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "content"
        "man"
        "scripts/doc.Makefile"
        "scripts/stork.css"
        "scripts/stork.toml"
        "scripts/template.html"
        "scripts/template-public.html"
        "scripts/*.cast"
        "scripts/*.css"
        "scripts/*.js"
        (and (matchExt "cast") (inDirectory "scripts"))
        (and (matchExt "css") (inDirectory "scripts"))
        (and (matchExt "js") (inDirectory "scripts"))
      ];
    };
    nativeBuildInputs = [ nixpkgs.mandoc nixpkgs.pandoc nixpkgs.stork ];
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile
      mv _site $out

      cp -r ${(import ../.).static}/* $out/static/
    '';
  };

  html.public = nixpkgs.stdenv.mkDerivation {
    name = "public";
    inherit (html.all) src nativeBuildInputs;
    installPhase = ''
      # Use the template with a navigation bar, instead of the SSI feature.
      mv scripts/template-public.html scripts/template.html
    '' + html.all.installPhase;
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
