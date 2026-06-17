{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.services.adhd;
in {
  options.services.adhd = {
    enable = lib.mkEnableOption "adhd DHCP server";
    configuration = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "adhd configuration.";
    };
    package = lib.mkOption {
      type = lib.types.package;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.adhd = let
      configFile = pkgs.writeText "adhd-config" cfg.configuration;
    in {
      description = "ADHD DHCP server";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];

      serviceConfig = {
        User = "root";

        ExecStart = "${cfg.package}/bin/adhd -c ${configFile}";
        Restart = "always";
      };
      preStart = ''
        mkdir -p /var/lib/adhd && cp ${configFile} /var/lib/adhd/config.dhall
      '';
    };
  };
}
