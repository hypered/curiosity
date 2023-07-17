{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."cty-2.hypered.systems" = {
      forceSSL = true;
      enableACME = true;
    };
  };

  security.acme.acceptTerms = true;
  security.acme.certs = {
    "cty-2.hypered.systems".email = "noteed@gmail.com";
  };
}
