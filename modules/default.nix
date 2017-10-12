# This module imports all other modules so you only need to import
# this module to get access to all the nixtodo services.
#
# Note that the ./base.nix module, which serves as the base layer for
# all nixtodo machines, imports this file.
{
  imports = [
    ./backend
    ./backend/db.nix
    ./backend/server.nix
    ./hydra.nix
  ];
}
