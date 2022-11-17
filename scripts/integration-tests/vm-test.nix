{
  nixpkgs ? (import ../../.).nixpkgs,
  binaries ? (import ../../.).binaries,
  haddock ? (import ../../.).haddock,
  content ? (import ../../.).content,
  data ? (import ../../.).data,
  scenarios ? (import ../../.).scenarios,
  system ? builtins.currentSystem,
  lib ? nixpkgs.lib
}:

let
  x = x;
  tls-cert = nixpkgs.runCommand "selfSignedCerts" { buildInputs = [ nixpkgs.openssl ]; } ''
    openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -nodes -subj '/CN=${serverFqdn}' -days 36500
    mkdir -p $out
    cp key.pem cert.pem $out
  '';
  serverFqdn = "smartcoop.sh";
  hosts = nodes: ''
    ${nodes.server.config.networking.primaryIPAddress} ${serverFqdn}
  '';
in nixpkgs.nixosTest {
  name = "curiosity-vm/test";
  nodes = {
    server = { nodes, pkgs, config, ...}: {
      imports = [
        ../../machine/configuration.nix
      ];
      security.pki.certificateFiles = [ "${tls-cert}/cert.pem" ];
      networking.extraHosts = hosts nodes;
      networking.firewall.enable = false;
      services.nginx.virtualHosts."${serverFqdn}" = {
        addSSL = true;
        sslCertificate = "${tls-cert}/cert.pem";
        sslCertificateKey = "${tls-cert}/key.pem";
      };
    };
    client = { nodes, pkgs, config, ...}: {
      security.pki.certificateFiles = [ "${tls-cert}/cert.pem" ];
      networking.extraHosts = hosts nodes;
      environment.systemPackages = [
        pkgs.curl
        pkgs.brotli
      ];
    };
  };
  testScript = ''
    start_all()
    server.wait_for_unit("nginx.service")
    server.wait_for_unit("curiosity.service")
    server.wait_for_unit("multi-user.target")
    client.wait_for_unit("multi-user.target")
    client.succeed('CURIOSITY_ENDPOINT="https://${serverFqdn}" ${nixpkgs.bash}/bin/bash -c ${./test-curiosity.sh}')
    exit(0)
  '';
}
