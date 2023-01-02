{ config, lib, pkgs, ... }:
  let
     statsConfig = {
       db = "statsdb";
       user = "stats";
       password = "stats";
     };
  in
{
  services.matomo = {
    enable = true;
    nginx = {
      serverName = "stats.smartcoop.sh";
      # This should be re-enabled in machine/https.nix.
      forceSSL = false;
      enableACME = false;
    };
  };

  environment.systemPackages = [ pkgs.python2 pkgs.python3 ];

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    settings.mysqldb.bind-address = "localhost";
    ensureDatabases = [
      statsConfig.db
    ];
    ensureUsers = [
      {
        name = statsConfig.user;
        ensurePermissions = {
          "${statsConfig.db}.*" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  # The idea of setting the password with a systemd unit comes from
  # https://joseph-long.com/writing/website-analytics-with-nixos/
  systemd.services.setdbpass = {
    description = "Setup a password for MySQL database";
    wants = [ "mysql.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
      ${pkgs.mariadb}/bin/mysql \
        -e "grant all privileges on ${statsConfig.db}.* to ${statsConfig.user}@localhost identified by '${statsConfig.password}';" \
        ${statsConfig.db}
      '';
      User = "root";
      PermissionsStartOnly = true;
      RemainAfterExit = true;
    };
  };
}
