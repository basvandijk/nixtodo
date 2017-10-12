# This module configures the nixtodo backend server.
#
# * It defines the `nixtodo-backend.conf` configuration file.
#
# * It installs a `nixtodo-backend` user account and group which will
# * be running the backend.
#
# * It installs a systemd service for running the backend.
#
# * It configures the systemd service to first run the
#   `migrate-nixtodo-db` script, which migrates the database to the
#   desired state, before running the backend.

{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.nixtodo.server;

  nixtodoBackendConf = pkgs.writeTextFile {
    name = "nixtodo-backend.conf";
    text =  ''
      db
      {
        host         = "127.0.0.1"
        port         = "${toString config.services.postgresql.port}"
        database     = "nixtodo_db"
        user         = "nixtodo"
        passwordFile = "${cfg.db.passwordFile}"

        pool
        {
          numStripes   = 2
          maxResources = 25
          idleTime     = 60
        }
      }

      frontendIndexTemplater
      {
        indexTemplatePath = "${pkgs.nixtodo.frontend-static}/index.html.mustache"
        srcDir            = "${pkgs.nixtodo.frontend-static}/static"
        dstDir            = "hashed-frontend"
        urlPrefix         = "hashed"
        compressLevel     = 9
      }

      web-server
      {
        host = "127.0.0.1"
        port = "${toString cfg.web-server.port}"
      }
    '';
  };

  user  = "nixtodo-backend";
  group = user;

  stateDir = "/var/lib/nixtodo-backend";

  migrateDb =
    let PATH = makeSearchPath "bin" [ pkgs.coreutils pkgs.gnugrep config.services.postgresql.package ];
        db = <nixtodo/db>;
        migrate = ''
          port="${toString config.services.postgresql.port}"
          nixtodoPasswordFile="${cfg.db.passwordFile}"
          ${db + "/create_or_update_db.sh"} "nixtodo_db" "$port" "$nixtodoPasswordFile" "migrations"

          export PGPASSWORD
          PGPASSWORD="$(cat $nixtodoPasswordFile)"
        '';
    in pkgs.writeScript "migrate-nixtodo-db" ''
      #!/bin/sh
      set -o errexit
      PATH="${PATH}"

      if [ "${stateDir}/latest-migration" -ef "${db}" ]; then
        echo "No need to migrate the nixtodo-db because it has already been migrated with ${db}"
        exit
      fi

      cd ${db} || exit 1

      ${migrate}
      ln -sfT "${db}" "${stateDir}/latest-migration"
    '';

in {
  options.services.nixtodo.server = {
    enable = mkEnableOption "nixtodo backend server";

    db.passwordFile = mkOption {
      type = types.str;
      description = ''
        A file that contains the password of the nixtodo PostgreSQL role.
      '';
    };

    web-server.port = mkOption {
      type = types.int;
      default = 8000;
      description = ''
        Port to listen on (for http).
      '';
    };

  };

  config = mkIf cfg.enable {
    systemd.services.nixtodo-backend = {
      description = "nixtodo-backend";
      wants = [ "create-roles-nixtodo-db.service" ];
      after = [ "create-roles-nixtodo-db.service" "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = user;
        Group = group;
        WorkingDirectory = stateDir;
        ExecStartPre = migrateDb;
        ExecStart =
          "${pkgs.haskellPackages.nixtodo-backend}/bin/nixtodo-backend" +
          " --config=${nixtodoBackendConf}";
        Restart = "always";
      };
    };

    users = {
      users."${user}" = {
        name  = user;
        group = group;
        home  = stateDir;
        createHome = true;
      };
      groups."${group}" = {
        name = group;
      };
    };
  };
}
