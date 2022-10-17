let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
  overlays = import ./nix/overlays.nix;

  os = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
      ./machine/disk-size.nix
      ./machine/https.nix
      ./modules/play.nix
    ];
  };

  qemu = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      ./machine/no-gui.nix
    ];
  };
    # Build with nix-build -A <attr>
    # binaries + haddock are also available as binaries.all.
    binaries = nixpkgs.haskellPackages.curiosity;
    content = (import ./content {}).html.all;
    haddock = nixpkgs.haskellPackages.curiosity.doc;
    run = import ./scripts/integration-tests {
      inherit nixpkgs binaries haddock content;
    };
in rec
  {
    inherit nixpkgs binaries content haddock run;
    data = (import ./content {}).data;
    scenarios = (import ./content {}).scenarios;
    static = (import "${sources.design-hs}").static;
    man-pages = (import ./man {}).man-pages;
    toplevel = os.config.system.build.toplevel;
    image = os.config.system.build.digitalOceanImage;
    runvm = qemu.config.system.build.vm;
    docker = (import ./docker { inherit nixpkgs; });

    # A shell to try out our binaries
    # TODO Can this be defined in shell.nix instead ?
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
        man-pages # This works in environment.systemPackages but here we need
                  # to set MANPATH below.
                  # I guess it would work if it was packaged with the binaries.
      ];
      # Setting the CURIOSITY_STATIC_DIR and CURIOSITY_DATA_DIR is not strictly
      # necessary as this shell is usually run from the source repository, and
      # the default paths will work (provided the _site/ directory has been
      # built. But this makes the shell usable even without those conditions.
      shellHook = ''
        source <(cty             --bash-completion-script `which cty`)
        source <(cty-sock        --bash-completion-script `which cty-sock`)
        export CURIOSITY_STATIC_DIR=${content}
        export CURIOSITY_DATA_DIR=${data}
        export MANPATH=${man-pages}/share/man
      '';
    };
  }
