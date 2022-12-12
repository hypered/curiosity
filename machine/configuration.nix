{ config, lib, pkgs,
  ... }:
let
  cty-shell = pkgs.writeShellScriptBin "cty-shell" ''
    #! /bin/bash

    # $1 must be '-c'.
    # $2 must be 'cty --user <username>'.
    # In practice, they are provided by the SSH ForceCommand.
    # $SSH_ORIGINAL_COMMAND is what the user types, e.g.
    # if she types 'ssh curiosity@smartcoop.sh state', then
    # it is 'state'.

    if [[ -n $SSH_ORIGINAL_COMMAND ]]
    then
      if [[ "$1" -ne "-c" ]]
      then
        echo "Expecting '-c' option."
        exit 1
      fi
      regex="^cty --user ([a-z]+)$"
      if [[ $2 =~ $regex ]]
      then
        username="''${BASH_REMATCH[1]}"
        cty --user $username --socket /curiosity.sock $SSH_ORIGINAL_COMMAND
      else
        echo "Expecting 'cty --user <username>' command prefix."
        exit 1
      fi
    else
      echo "Hi <username>, your SSH key is recognized, but"
      echo "Curiosity does not offer an interactive shell."
      exit 1
    fi
  '';
in
{
  services.sshd.enable = true;

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  users.users.root.password = "nixos";
  services.openssh.permitRootLogin = lib.mkDefault "yes";
  services.getty.autologinUser = lib.mkDefault "root";

  imports = [
    ../modules/curiosity.nix
    ../modules/nginx.nix
  ];

  environment.systemPackages = [
    (import ../.).binaries
    (import ../.).man-pages # TODO Man pages should come with .binaries ?
    pkgs.jq
  ];

  # Output to stderr, otherwise this confuses nix-copy-closure during
  # deployment.
  programs.bash.loginShellInit = ''
    >&2 echo "Welcome to the Curiosity environment."
    >&2 echo "Run \`man curiosity\` for the manual."
  '';

  programs.bash.interactiveShellInit = ''
    source <(cty             --bash-completion-script `which cty`)
    export CURIOSITY_STATIC_DIR=${(import ../.).content}
    export CURIOSITY_DATA_DIR=${(import ../.).data}
    export CURIOSITY_SCENARIOS_DIR=${(import ../.).scenarios}
  '';

  users.users.curiosity = {
    description = "Curiosity SSH commands user";
    isSystemUser = true; # TODO What does this do ?
    group = "curiosity";
    shell = "${cty-shell}/bin/cty-shell";
    openssh.authorizedKeys.keys = [
      ''command="cty --user thu",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC9pA/P3A72o7wCs40rPo4kr91c8OokgJhH0LxKBF0EmiLjY++8Nh3t7avo88fJI86dkBR4SkdmAG+elicNwQc/n7iN4zMOs8Cdbye/ZrN4xoI5OHyAz1OjzYY6Lje0tuFYrQa8XxW3GF6cWVOLE/v6ShlIoUL1QPrwygdREVhh+as4DhJ6G+4qcjQMMSWw9IPIwpKV+Q8TycTVfL/rDnzzadkp5aPmPgpUhXo8mjY0CY7hGxOpmuPDmyEej8aOTl5fR4yyuz/12lglNNCm8UDu8zJbMOKvvyVWQiXoxmnNFg7lAUU/FcLla0JbQx+4szPHfUgqJNYKyoxdGktmx0FvKavPK5df70ezwEnBAqhHauHDu52GsrCSH8ZItgxvts2CowP52X+GDaWsVtNgXOsu2+1FODog/wVHjOadKBOsp0w6tXsf5zcfysANeSHgB79zyAg4NaJ8UpD0g9qdbhzX5zOJ3JCeA/J+ulnHdegRZSbeXlhTCsvAJygHF74RWx0Bcdr1SiUgOj51Wl9aTERgM7wIykHOvEv38T3ZYw7ZVVsV2atcWdqCOsT9OhVOdO5nqgS8Yh3maHoP9fwKoxNZGF650KIl927GQ7l2DKH8aWhqxhxMagtj4zKimpCEUMUQNJFzOQbi9jL5ri9yUA1FqWlCnxc65MTVWQ8FdPp0LQ== thu on tank''
    ];

    # If we want to support adding keys without changing this file, we can
    # use this configuration instead:
    #   openssh.authorizedKeys.keyFiles = [
    #     /etc/nixos/ssh/curiosity_authorized_keys
    #   ];
    # Or AuthorizedKeysCommand.
  };

  users.groups.curiosity = {};

  users.users.alice = {
    description = "Linux user to play with Curiosity commands";
    isNormalUser = true;
    group = "alice";
    openssh.authorizedKeys.keys = [
      ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC9pA/P3A72o7wCs40rPo4kr91c8OokgJhH0LxKBF0EmiLjY++8Nh3t7avo88fJI86dkBR4SkdmAG+elicNwQc/n7iN4zMOs8Cdbye/ZrN4xoI5OHyAz1OjzYY6Lje0tuFYrQa8XxW3GF6cWVOLE/v6ShlIoUL1QPrwygdREVhh+as4DhJ6G+4qcjQMMSWw9IPIwpKV+Q8TycTVfL/rDnzzadkp5aPmPgpUhXo8mjY0CY7hGxOpmuPDmyEej8aOTl5fR4yyuz/12lglNNCm8UDu8zJbMOKvvyVWQiXoxmnNFg7lAUU/FcLla0JbQx+4szPHfUgqJNYKyoxdGktmx0FvKavPK5df70ezwEnBAqhHauHDu52GsrCSH8ZItgxvts2CowP52X+GDaWsVtNgXOsu2+1FODog/wVHjOadKBOsp0w6tXsf5zcfysANeSHgB79zyAg4NaJ8UpD0g9qdbhzX5zOJ3JCeA/J+ulnHdegRZSbeXlhTCsvAJygHF74RWx0Bcdr1SiUgOj51Wl9aTERgM7wIykHOvEv38T3ZYw7ZVVsV2atcWdqCOsT9OhVOdO5nqgS8Yh3maHoP9fwKoxNZGF650KIl927GQ7l2DKH8aWhqxhxMagtj4zKimpCEUMUQNJFzOQbi9jL5ri9yUA1FqWlCnxc65MTVWQ8FdPp0LQ== thu on tank''
      ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP+E/KZ/dQdLdBWnsWkp+1KAWoMxCLSDQpwYayIys2KT bur@smart.coop''
      ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILMtgSsi3eFUXHQtpJ0xxhv9AUVSvgmp/c6F7ieJ1u/+ github-deploy-key''
    ];
  };

  users.groups.alice = {};

  nix.settings.substituters = [
    "https://cache.nixos.org/"
    "https://s3.eu-central-003.backblazeb2.com/curiosity-store/"
  ];
  nix.settings.trusted-public-keys = [
    "curiosity-store:W3LXUB+6DjtZkKV0gEfNXGtTjA+hMqjPUoK6mzzco+w="
  ];

  system.stateVersion = "22.05";
}
