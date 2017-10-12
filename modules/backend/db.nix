# This module configures the nixtodo database.
#
# * It enables and configures the `postgresql` service.
#
# * It installs a oneshot service for upserting the `nixtodo` database
# * role.

{ config, lib, pkgs, ... } :

with lib;

let
  cfg = config.services.nixtodo.db;

  pgPkg = config.services.postgresql.package;

  createDbRoles =
    let PATH = makeSearchPath "bin" [ pkgs.coreutils pkgs.gnugrep pgPkg ];
    in pkgs.writeScript "create-roles-nixtodo-db" ''
      #!/bin/sh
      set -o errexit
      PATH="${PATH}"

      cd ${<nixtodo/db>} || exit 1

      ./create_or_update_roles.sh ${cfg.passwordFile}
    '';
in {
  options.services.nixtodo.db = {
    enable = mkEnableOption "nixtodo DB";

    passwordFile = mkOption {
      type = types.str;
      description = ''
        A file that contains the password of the nixtodo PostgreSQL role.
      '';
    };
  };

  config = mkIf config.services.nixtodo.db.enable {
    services.postgresql = {
      enable = true;

      # PostgreSQL 10.0 got released last week. Lets use it!
      package = pkgs.postgresql100;

      dataDir = "/var/lib/postgresql/${pgPkg.psqlSchema}";
      extraConfig = ''
        # Handy for debugging
        log_statement all
      '';
    };
    systemd.services."create-roles-nixtodo-db" = {
      description = "Create or update roles for the nixtodo_db";
      wants    = [ "postgresql.service" ];
      after    = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart       = createDbRoles;
        Type            = "oneshot";
        RemainAfterExit = "yes";
        User            = "postgres";
      };
    };
  };
}
