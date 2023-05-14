{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."cty.hypered.systems" = {
      forceSSL = true;
      enableACME = true;
    };
  };

  security.acme.acceptTerms = true;
  security.acme.certs = {
    "cty.hypered.systems".email = "noteed@gmail.com";
  };
}
