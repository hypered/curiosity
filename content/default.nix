{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};

in
{
  html.all = pkgs.stdenv.mkDerivation {
    name = "content";
    # TODO We only need content/, man/, scripts/
    src = ../.;
    nativeBuildInputs = [ pkgs.mandoc pkgs.pandoc ];
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile
      mv _site $out
    '';
  };

  # Define this here, instead of creating a .nix file in data/.
  data = pkgs.stdenv.mkDerivation {
    name = "data";
    # TODO We only need data/
    src = ../.;
    installPhase = ''
      cp -r data $out
    '';
  };
}
