{ config, lib, pkgs, ... }:
let
  brotlify = pkgs.callPackage ./brotlify.nix { };
  static = (import ../.).static;
  indexes = (import ../.).indexes;
  asciinema = (import ../.).asciinema;
  static-layered = pkgs.buildEnv {
    name = "static-layered";
    paths = [
      static
      (brotlify { src = static; })
      indexes
      asciinema
    ];
  };
in
{
  services.nginx = {
    enable = true;
    package = pkgs.nginxMainline;
    additionalModules = [ pkgs.nginxModules.brotli ];
    recommendedGzipSettings = true;
    virtualHosts."cty.hypered.systems" = {
      locations = {
        "/".proxyPass = "http://127.0.0.1:9100";
        "/about" = {
          proxyPass = "http://127.0.0.1:9100";
          extraConfig = "ssi on;";
        };
        "/documentation" = {
          proxyPass = "http://127.0.0.1:9100";
          extraConfig = "ssi on;";
        };
        "/static/" = {
          alias = static-layered + "/";
        };
        # TODO How to avoid hard-coding this 0.1.0.0 path ?
        "/haddock/".alias = (import ../.).haddock + "/share/doc/curiosity-0.1.0.0/html/";
      };

      extraConfig = ''
        brotli_static on;
      '';
    };
  };
}
