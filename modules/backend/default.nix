# This module configures a complete nixtodo backend machine.
#
# * It enables the nixtodo database.
#
# * It enables the nixtodo backend service.
#
# * It enables a nginx reverse proxy server for handling TLS
# * termination.

{ config, lib, pkgs, ... }:

with lib;

let backendPort = config.services.nixtodo.server.web-server.port;
in {
  options.services.nixtodo.enable = mkEnableOption "nixtodo";

  config = mkIf config.services.nixtodo.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    services = {
      nginx = {
        enable = true;
        recommendedTlsSettings   = true;
        recommendedOptimisation  = true;
        recommendedProxySettings = true;
        sslDhparam = <nixtodo/secrets/dhparams.pem>;
        upstreams = {
          "nixtodo-backend" = {
            servers = { "127.0.0.1:${toString backendPort}" = {}; };
          };
        };
        virtualHosts = {
          "nixtodo.com" = {
            default    = true;
            forceSSL   = true;
            enableACME = true;
            locations = {
              "/" = {
                proxyPass = "http://nixtodo-backend";
                proxyWebsockets = true;
              };
            };
          };
        };
      };

      nixtodo.server = {
        enable = true;
        db.passwordFile = config.services.nixtodo.db.passwordFile;
      };

      nixtodo.db = {
        enable = true;

        # TODO: don't store the password in the world readable Nix store!
        passwordFile = toString (pkgs.writeTextFile {
          name = "postgresql-nixtodo-role-password";
          text = fileContents <nixtodo/secrets/postgresql-nixtodo-role-password>;
        });
      };
    };
  };
}
