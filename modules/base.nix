# This NixOS module provides a base layer for all nixtodo machines:
#
# * It imports <nixtodo/modules/default.nix> to bring all nixtodo
#   services into scope.
#
# * It installs some handy packages like `htop`.
#
# * It enables a SSH daemon so we can log in to our machine.
#
# * It installs user accounts for all nixtodo engineers.

{ lib, pkgs, ... }:

with lib;

let # `engineers` contains a set of user configurations for all
    # nixtodo engineers. This set can be assigned to the NixOS
    # `users.users` option (like we do below) to install user accounts
    # on the machine. The set is based on the ./users directory which
    # contains a directory for each user.
    engineers = with builtins;
      genAttrs (attrNames (readDir ./users)) (name :
        let userDir = ./users + "/${name}"; in {
          inherit name;
          isNormalUser = true;
          extraGroups = [ "systemd-journal" "nixtodo" ];
          openssh.authorizedKeys.keys =
            let sshPublicKeyPath = userDir + /.ssh/id_rsa.pub;
            in optional (pathExists sshPublicKeyPath)
                        (readFile sshPublicKeyPath);
        }
      );
in {
  imports = [ <nixtodo/modules> ];

  nixpkgs.overlays = [ (import <nixtodo/nix/overlay.nix>) ];

  environment.systemPackages = with pkgs; [ htop ];

  services.openssh.enable = true;

  networking.firewall.allowPing = true;

  security = {
    sudo.wheelNeedsPassword = false;
    initialRootPassword = "!"; # This disables the root password.
  };

  users = {
    # This disables dynamic user account creation via the `useradd`
    # command. The less dynamism and the more static configuration the
    # better.
    mutableUsers = false;

    groups.nixtodo = { name = "nixtodo"; };

    users = engineers // {
      "root" = {
         # In order for engineers to directly log in as root via SSH
         # we assign their authorized keys to the root user.
         # This is probably a security hazard...
         openssh.authorizedKeys.keys =
           concatMap (user : user.openssh.authorizedKeys.keys)
                     (attrValues engineers);
      };
    };
  };
}
