{ config, lib, pkgs, ... }:
{
  systemd.services.curiosity = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "notify";
      ExecStartPre =
        let preScript = pkgs.writers.writeBashBin "curiosityStartPre" ''
          # Run from a state file, reset to a known state.
          # TODO system is meaningless for now.
          rm -f /tmp/state.json
          ${(import ../.).binaries}/bin/cty \
          --state /tmp/state.json \
          --user system \
          init
          ${(import ../.).binaries}/bin/cty \
          --state /tmp/state.json \
          --user system \
          run ${(import ../.).scenarios}/state-0.txt
          '';
        in "${preScript}/bin/curiosityStartPre";
      ExecStart = ''
        ${(import ../.).binaries}/bin/cty \
        --state /tmp/state.json \
        --user system \
        serve \
        --server-port 9100 \
        --static-dir ${(import ../.).content} \
        --data-dir ${(import ../.).data} \
        --scenarios-dir ${(import ../.).scenarios} \
        --stdout
      '';
    };
  };
}
