{ config, lib, pkgs, ... }:
{
  services.nginx = {
    enable = true;
    virtualHosts."play.cty-1.hypered.systems" = {
      forceSSL = true;
      enableACME = true;
      locations = {
        "/".proxyPass = "http://127.0.0.1:9000";
        "/documentation" = {
          proxyPass = "http://127.0.0.1:9000";
          extraConfig = "ssi on;";
        };
        # TODO How to avoid hard-coding this 0.1.0.0 path ?
        "/haddock/".alias = (import ../.).haddock + "/share/doc/curiosity-0.1.0.0/html/";
      };
    };
  };

  security.acme.certs = {
    "play.cty-1.hypered.systems".email = "noteed@gmail.com";
  };
}
