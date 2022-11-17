{
  nixpkgs ? (import ../../.).nixpkgs,
  binaries ? (import ../../.).binaries,
  haddock ? (import ../../.).haddock,
  content ? (import ../../.).content,
  data ? (import ../../.).data,
  scenarios ? (import ../../.).scenarios,
  system ? builtins.currentSystem,
  lib ? nixpkgs.lib
}@attrs:

let
  runCuriosity = nixpkgs.writers.writeBashBin "run-curiosity" ''
    set -euo pipefail
    PATH=${lib.strings.makeBinPath [ binaries ]}:$PATH
    export CURIOSITY_STATIC_DIR=${content}
    export CURIOSITY_DATA_DIR=${data}
    export CURIOSITY_SCENARIOS_DIR=${scenarios}
    ${builtins.readFile ./run-curiosity.sh}
  '';
  nginxConf = nixpkgs.substituteAll {
    src = ./nginx.conf;
    curiosityaddr = "http://127.0.0.1:9000";
    # TODO: find a way to reference this folder without the explicit
    #       0.1.0.0 version
    curiosityhaddock = "${haddock.doc}/share/doc/curiosity-0.1.0.0/html";
  };
  run-nginx = nixpkgs.writers.writeBashBin "run-nginx" ''
    set -euo pipefail
    PATH=${lib.strings.makeBinPath [ nixpkgs.nginx ]}:$PATH
    NGINX_CONF="${nginxConf}"
    ${builtins.readFile ./run-nginx.sh}
  '';
  procFile = nixpkgs.writeText "Procfile" ''
    curiosity: ${runCuriosity}/bin/run-curiosity
    nginx: ${run-nginx}/bin/run-nginx
  '';
  run-full-environment = nixpkgs.writers.writeBashBin "run-full-environment" ''
    set -euo pipefail
    ${nixpkgs.hivemind}/bin/hivemind ${procFile}
  '';
  run-vm-test = import ./vm-test.nix attrs;
in {
  inherit run-full-environment run-nginx run-vm-test;
}
