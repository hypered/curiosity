{ config, lib, pkgs, ... }:
{
  systemd.services.curiosity-2 = {
    wantedBy = [ "multi-user.target" ];
    # TODO Right way to depends on graphviz in curiosity.
    path = [ pkgs.graphviz ];
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
        --server-port 9102 \
        --static-dir ${(import ../.).content} \
        --data-dir ${(import ../.).data} \
        --scenarios-dir ${(import ../.).scenarios} \
        --stdout
      '';

      # Hardening Options
      CapabilityBoundingSet = [ "" ];
      DevicePolicy = "closed";
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      ProtectClock = true;
      ProtectHome = true;
      ProtectHostname = true;
      ProtectControlGroups = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      RemoveIPC = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
    };
  };
}
