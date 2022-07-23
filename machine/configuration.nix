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
      # TODO Replace by cty --user <username>
      cty-parse -c "$SSH_ORIGINAL_COMMAND"
    else
      echo "Hi <username>, your SSH key is recognized, but"
      echo "Curiosity does not offer an interactive shell."
    fi
  '';
in
{
  services.sshd.enable = true;

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  users.users.root.password = "nixos";
  services.openssh.permitRootLogin = lib.mkDefault "yes";
  services.mingetty.autologinUser = lib.mkDefault "root";

  imports = [
    ../modules/app.nix
    ../modules/nginx.nix
  ];

  environment.systemPackages = [
    (import ../.).binaries
    (import ../.).man-pages # TODO Man pages should come with .binaries ?
  ];

  programs.bash.loginShellInit = ''
    echo "Welcome to the Curiosity environment."
    echo "Run \`man curiosity\` for the manual."
  '';

  programs.bash.interactiveShellInit = ''
    source <(cty             --bash-completion-script `which cty`)
    source <(cty-interactive --bash-completion-script `which cty-interactive`)
    source <(cty-parse       --bash-completion-script `which cty-parse`)
    source <(cty-repl        --bash-completion-script `which cty-repl`)
    source <(cty-repl-2      --bash-completion-script `which cty-repl-2`)
    source <(cty-serve       --bash-completion-script `which cty-serve`)
    source <(cty-sock        --bash-completion-script `which cty-sock`)
  '';

  users.users.curiosity = {
    description = "Curiosity SSH commands user";
    isSystemUser = true; # TODO What does this do ?
    # group = "xxx"; # TODO Set something here ?
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
}
