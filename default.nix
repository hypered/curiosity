let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
  overlays = import ./nix/overlays.nix;

  os = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
      ./machine/disk-size.nix
    ];
  };

  qemu = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      ./machine/no-gui.nix
    ];
  };
in with nixpkgs.haskellPackages;
  {
    # Build with nix-build -A <attr>
    # binaries + haddock are also available as binaries.all.
    binaries = nixpkgs.haskellPackages.curiosity;
    haddock = nixpkgs.haskellPackages.curiosity.doc;
    content = (import ./content {}).html.all;
    data = (import ./content {}).data;
    man-pages = (import ./man {}).man-pages;
    toplevel = os.config.system.build.toplevel;
    image = os.config.system.build.digitalOceanImage;
    runvm = qemu.config.system.build.vm;
  }
