# For testing the nixtodo network each machine can be deployed to a
# container running on a NixOS host. This host is typically your own
# workstation.
#
# After deploying the containers the `nixos-container` command can be
# used to interact with the machines.

let
  # Setting up a working TLS implementation (HTTPS) in a testing
  # environment can be difficult because you have to install
  # self-signed certificates and configure your browsers and other
  # clients to use it.
  #
  # Because of this we disable TLS in our containerized
  # network. Note how we use the `mkForce` function from `lib` to
  # override the options which were already defined elsewhere.
  disableTLS = hostname : { lib, ... } : {
    services.nginx = with lib; {
      recommendedTlsSettings = mkForce false;
      virtualHosts."${hostname}" = {
        forceSSL   = mkForce false;
        enableACME = mkForce false;
      };
    };
  };
in {
  backend = { lib, ... }: {
    deployment.targetEnv = "container";
    imports = [ (disableTLS "nixtodo.com") ];
  };

  support =  {
    deployment.targetEnv = "container";
    imports = [
      (disableTLS "hydra.nixtodo.com")
      (disableTLS "cache.nixtodo.com")
    ];
  };
}
