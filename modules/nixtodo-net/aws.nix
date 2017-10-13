# This nixops network specification defines the production
# configuration of the nixtodo network.
#
# As you can see we deploy machines to EC2 instances on Amazon Web
# Services. Note that we also provision some resources like a SSH
# key-pair and an elastic IP that are both used by the instances.

let
  region = "eu-west-1"; # A data center in Ireland

  accessKeyId = "todo-list-app";

in rec {
  backend = {resources, ... }: {
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region accessKeyId;
        instanceType = "t2.micro";

        # I'm expecting nixtodo.com to be a huge success so I better
        # provision some gigabytes to store the world's TODO entries.
        ebsInitialRootDiskSize = 10; # GB

        keyPair     = resources.ec2KeyPairs.todo-list-app-key-pair;
        elasticIPv4 = resources.elasticIPs.nixtodo-elastic-ip;
      };
    };
  };

  support = {resources, ... }: {
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region accessKeyId;
        instanceType = "t2.xlarge";

        ebsInitialRootDiskSize = 20; # GB

        keyPair     = resources.ec2KeyPairs.todo-list-app-key-pair;
        elasticIPv4 = resources.elasticIPs.nixtodo-support-elastic-ip;
      };
    };
  };

  resources = {
    ec2KeyPairs.todo-list-app-key-pair =
      { inherit region accessKeyId; };

    elasticIPs.nixtodo-elastic-ip =
      { inherit region accessKeyId; };
    elasticIPs.nixtodo-support-elastic-ip =
      { inherit region accessKeyId; };
  };
}
