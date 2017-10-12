This directory contains the configuration of the nixtodo nixops
network of machines (it currently consists of a single machine).

The `./default.nix` file contains the logical configuration.

nixops supports multiple deployment options. You can deploy to
Microsoft Azure, Digital Ocean, AWS EC2, Google Compute Cloud,
Hetzner, Virtualbox and any machine running NixOS (so that you can
deploy to your own physical hardware for example).

The nixtodo network support both deploying to containers for testing
and deploying to AWS EC2 for production. See the `./containerized.nix`
and `aws.nix` files respectively.
