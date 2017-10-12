{ pkgs, config, lib, ... }:

with lib;

let
  cfg = config.services.nixtodo.hydra;

in {
  options.services.nixtodo.hydra = {
    enable = mkEnableOption "the lumi Hydra service";

    hostname = mkOption {
      type = types.str;
      default = "hydra.nixtodo.com";
      description = ''
        The hostname of the hydra machine.
      '';
    };

    devopsPasswordFile = mkOption {
      type = types.str;
      default = toString (pkgs.writeTextFile {
        name = "devops-hydra-password";
        text = fileContents <nixtodo/secrets/devops-hydra-password>;
      });

      description = ''
        A file that contains the password of the 'devops' admin account on hydra.
      '';
    };

    hydraGithubPrivateKey = mkOption {
      type = types.path;
      default = <nixtodo/secrets/hydra-github.id_rsa>;
      description = ''
        The SSH private key of the hydra-github keypair. The public key of this
        pair should be uploaded to GitHub to ensure hydra can clone repos.
      '';
    };
  };

  config = mkIf cfg.enable {

    nix = {
      buildMachines = [
        { hostName = "localhost";
          system = "x86_64-linux";
          supportedFeatures = ["kvm" "nixos-test" "big-parallel" "benchmark"];
          maxJobs = 8;
        }
      ];
    };

    services.hydra = {
      enable = true;
      hydraURL = "https://${cfg.hostname}";
      notificationSender = "hydra@nixtodo.com";
      logo = <nixtodo/hs-pkgs/nixtodo-frontend/static/favicon.ico>;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.nginx = {
      enable = true;
      recommendedTlsSettings   = true;
      recommendedOptimisation  = true;
      recommendedProxySettings = true;
      sslDhparam = <nixtodo/secrets/dhparams.pem>;
      upstreams = {
        "hydra" = {
          servers = { "127.0.0.1:${toString config.services.hydra.port}" = {}; };
        };
      };
      virtualHosts = {
        "hydra.nixtodo.com" = {
          default    = true;
          forceSSL   = true;
          enableACME = true;
          locations = {
            "/" = {
              proxyPass = "http://hydra";
            };
          };
        };
      };
    };

    # Register GitHub's host key as a known host so that hydra can connect to
    # GitHub to clone our repo's without being prompted.
    programs.ssh = {
      knownHosts = [
        { hostNames = ["github.com"];
          publicKeyFile = ./github.com-rsa_key.pub;
        }
      ];
    };

    # Create a hydra admin user named "devops" and copy the GitHub private SSH
    # key to hydra's home directory so that it can connect to GitHub to clone
    # our repo's.
    systemd.services.lumi-hydra-setup = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "hydra-init.service" "postgresql.service" ];
      after    = [ "hydra-init.service" "postgresql.service" ];
      environment = config.systemd.services.hydra-init.environment;
      path = [ config.services.hydra.package ];
      script =
        let hydraHome            = config.users.users.hydra.home;
            hydraQueueRunnerHome = config.users.users.hydra-queue-runner.home;
        in ''
          hydra-create-user devops                \
            --full-name 'DevOps'                  \
            --email-address 'devops@lumiguide.nl' \
            --password "$(cat ${cfg.devopsPasswordFile})" \
            --role admin

          # TODO: Don't store the private SSH keys in the public Nix store!
          mkdir -p "${hydraHome}/.ssh"
          chmod 700 "${hydraHome}/.ssh"
          cp "${cfg.hydraGithubPrivateKey}" "${hydraHome}/.ssh/id_rsa"
          chown -R hydra:hydra "${hydraHome}/.ssh"
          chmod 600 "${hydraHome}/.ssh/id_rsa"
        '';
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };
  };
}
