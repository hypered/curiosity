{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."cty-1.hypered.systems" = {
      forceSSL = true;
      enableACME = true;
    };
  };

  security.acme.acceptTerms = true;
  security.acme.certs = {
    "cty-1.hypered.systems".email = "noteed@gmail.com";
  };
}
