################################################################################
#
# This Makefile is our entry point into the nixtodo project. We use it to:
#
# * get into development environments for working on our Haskell packages,
#
# * provisioning and deploying containers on our workstation for testing the
#   system,
#
# * provisioning and deploying EC2 instances, elastic IPs, key-pairs, security
#   groups and other AWS resources to run our production system.
#
# With this Makefile you almost never have to type in actual nix commands for
# day-to-day develoment and operational work.
#
################################################################################

# Some of our Nix expressions refer to <nixpkgs> and <nixtodo>. In order for
# them to find these directories we have to put them in the NIX_PATH environment
# variable.
export NIX_PATH := nixpkgs=$(shell nix-build --no-out-link ./nixpkgs.nix):nixtodo=$(shell pwd)

help:
	cat Makefile


################################################################################
# Development
################################################################################

# The following are some handy targets for building our Haskell packages and
# getting into shells which are setup with all the dependencies needed to work
# on a package.

.PHONY: nixtodo-frontend.build
nixtodo-frontend.build:
	nix-build -A haskell.packages.ghcjsHEAD.nixtodo-frontend

.PHONY: nixtodo-frontend.shell
nixtodo-frontend.shell:
	cd hs-pkgs/nixtodo-frontend && \
	nix-shell --command 'cabal configure; return'

.PHONY: nixtodo-api.build.ghcjs
nixtodo-api.build.ghcjs:
	nix-build -A haskell.packages.ghcjsHEAD.nixtodo-api

.PHONY: nixtodo-api.build.ghc
nixtodo-api.build.ghc:
	nix-build -A haskellPackages.nixtodo-api

.PHONY: nixtodo-api.shell
nixtodo-api.shell:
	cd hs-pkgs/nixtodo-api && \
	nix-shell --command 'cabal configure; return'

.PHONY: nixtodo-api-client.shell
nixtodo-api-client.shell:
	cd hs-pkgs/nixtodo-api-client && \
	nix-shell --command 'cabal configure; return'

.PHONY: nixtodo-backend.build
nixtodo-backend.build:
	nix-build -A haskellPackages.nixtodo-backend

.PHONY: nixtodo-backend.shell
nixtodo-backend.shell:
	cd hs-pkgs/nixtodo-backend && \
	nix-shell --command 'cabal configure; return'

MK_SECRET := openssl rand -base64 32

secrets/devops-hydra-password:
	$(MK_SECRET) > $@

secrets/postgresql-nixtodo-role-password:
	$(MK_SECRET) > $@

secrets/dhparams.pem:
	openssl dhparam 4096 -out $@

secrets/hydra-github.id_rsa:
	ssh-keygen -C hydra-github -P "" -f $@

secrets/cache.nixtodo.com-secret-key:
	nix-store --generate-binary-cache-key cache.nixtodo.com-1 \
	  $@ modules/cache.nixtodo.com-public-key

################################################################################
# Deploying to containers for testing
################################################################################

# For testing our complete nixtodo system we can take our nixops network
# specification (<nixtodo/modules/nixtodo-net>) and deploy it to containers on
# our workstation. These containers can then be managed by the `nixos-container`
# command.
#
# The following targets can be used to create this containerized network and
# deploy it.

.PHONY: containerized-nixtodo-net.create
containerized-nixtodo-net.create:
	nixops create -d containerized-nixtodo-net \
	  '<nixtodo/modules/nixtodo-net>' \
	  '<nixtodo/modules/nixtodo-net/containerized.nix>'

.PHONY: containerized-nixtodo-net.modify
containerized-nixtodo-net.modify:
	nixops modify -d containerized-nixtodo-net \
	  '<nixtodo/modules/nixtodo-net>' \
	  '<nixtodo/modules/nixtodo-net/containerized.nix>'

.PHONY: containerized-nixtodo-net.info
containerized-nixtodo-net.info:
	nixops info -d containerized-nixtodo-net

.PHONY: containerized-nixtodo-net.build
containerized-nixtodo-net.build:
	nixops deploy -d containerized-nixtodo-net --build-only

.PHONY: containerized-nixtodo-net.copy
containerized-nixtodo-net.copy:
	nixops deploy -d containerized-nixtodo-net --copy-only

.PHONY: containerized-nixtodo-net.deploy
containerized-nixtodo-net.deploy:
	nixops deploy -d containerized-nixtodo-net

.PHONY: containerized-nixtodo-net.destroy
containerized-nixtodo-net.destroy:
	nixops destroy -d containerized-nixtodo-net

.PHONY: containerized-nixtodo-net.delete
containerized-nixtodo-net.delete:
	nixops delete -d containerized-nixtodo-net

.PHONY: containerized-backend.ssh
containerized-backend.ssh:
	nixops ssh -d containerized-nixtodo-net backend

.PHONY: containerized-backend.destroy
containerized-backend.destroy:
	nixops destroy -d containerized-nixtodo-net --include backend

.PHONY: containerized-support.ssh
containerized-support.ssh:
	nixops ssh -d containerized-nixtodo-net support

.PHONY: containerized-support.destroy
containerized-support.destroy:
	nixops destroy -d containerized-nixtodo-net --include support


################################################################################
# Deploying to production
################################################################################

# nixtodo.com is hosted on AWS. The following targets can be used to provision
# all the needed resources (like EC2 instances, elastic IPs, key pairs and
# security groups) and bring them in the configuration as specified by
# <nixtodo/modules/nixtodo-net>.

.PHONY: nixtodo-net.create
nixtodo-net.create:
	nixops create -s secrets/state.nixops -d nixtodo-net \
	  '<nixtodo/modules/nixtodo-net>' \
	  '<nixtodo/modules/nixtodo-net/aws.nix>'

.PHONY: nixtodo-net.modify
nixtodo-net.modify:
	nixops modify -s secrets/state.nixops -d nixtodo-net \
	  '<nixtodo/modules/nixtodo-net>' \
	  '<nixtodo/modules/nixtodo-net/aws.nix>'

.PHONY: nixtodo-net.info
nixtodo-net.info:
	nixops info -s secrets/state.nixops -d nixtodo-net

.PHONY: nixtodo-net.build
nixtodo-net.build:
	nixops deploy -s secrets/state.nixops -d nixtodo-net --build-only

.PHONY: nixtodo-net.copy
nixtodo-net.copy:
	nixops deploy -s secrets/state.nixops -d nixtodo-net --copy-only

.PHONY: nixtodo-net.deploy
nixtodo-net.deploy:
	nixops deploy -s secrets/state.nixops -d nixtodo-net

.PHONY: nixtodo-net.destroy
nixtodo-net.destroy:
	nixops destroy -s secrets/state.nixops -d nixtodo-net

.PHONY: nixtodo-net.delete
nixtodo-net.delete:
	nixops delete -s secrets/state.nixops -d nixtodo-net

.PHONY: backend.ssh
backend.ssh:
	nixops ssh -s secrets/state.nixops -d nixtodo-net backend

.PHONY: backend.destroy
backend.destroy:
	nixops destroy -s secrets/state.nixops -d nixtodo-net --include backend

.PHONY: support.ssh
support.ssh:
	nixops ssh -s secrets/state.nixops -d nixtodo-net support

.PHONY: support.destroy
support.destroy:
	nixops destroy -s secrets/state.nixops -d nixtodo-net --include support
