# {
#   network.description = "attics";

#   attics =
#     { config, pkgs, ... }: let
#       attics = import ./default.nix { inherit pkgs; };
#     in
#     { networking.hostName = "attics";

#       networking.firewall.allowedTCPPorts = [ 22 80 443 ];
#       environment.systemPackages = [ attics pkgs.postgresql ];

#       systemd.services.attics =
#         { description = "attics Webserver";
#           wantedBy = [ "multi-user.target" ];
#           after = [ "network.target" ];
#           serviceConfig =
#             { ExecStart = "${attics}/web/bin/RunProdServer";
#               WorkingDirectory = "${attics}/web";
#             };
#         };

#        services.postgresql = {
#           enable = true;
#           package = pkgs.postgresql_11;
#           initialScript = pkgs.writeText "backend-initScript" ''
#             create extension if not exists "uuid-ossp";
#           '';
#         };
#     };
# }

{
  network.description = "attics";

  attics = { config, pkgs, ... }:
    { networking.hostName = "attics";

      networking.firewall.allowedTCPPorts = [ 22 5432 ];

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_12;
        initialScript = pkgs.writeText "backend-initScript" ''
          create extension if not exists "uuid-ossp";
        '';
      };
    };
}
