# This file defines the logical configuration of the nixtodo nixops network. It
# can be deployed to containers by combining it with the `./containerized.nix`
# file or to AWS EC2 by combining it with the `aws.nix` file.
#
# See the `<nixtodo/Makefile>` how to turn this into a nixops network.
{
  network.description = "nixtodo network";

  # This configures a base layer of functionality for each machine.
  defaults = <nixtodo/modules/base.nix>;

  # The network currently consists of a single machine callled `backend` running
  # the nixtodo service.
  backend = {
    services.nixtodo.enable = true;
  };

  support = {
    services.nixtodo.hydra.enable = true;
  };
}
