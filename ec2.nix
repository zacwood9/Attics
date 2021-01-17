let
  region = "us-east-1";
  accessKeyId = "nixops";

in
{ attics =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.instanceType = "t2.micro";
    deployment.ec2.keyPair = resources.ec2KeyPairs.attics-keys;

  };

  resources.ec2KeyPairs.attics-keys =
    { inherit region accessKeyId; };
}
