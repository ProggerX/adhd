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
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "adhd configuration.";
    };
    package = lib.mkOption {
      type = lib.types.package;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.adhd = let
      configFile = pkgs.writeText "adhd-config" (lib.toDhall cfg.settings);
    in {
      description = "ADHD DHCP server";
      wantedBy = ["multi-user.target"];

      serviceConfig = {
        WorkingDirectory = "/var/lib/adhd";

        StateDirectory = "adhd";

        Restart = "always";

        ExecStart = "${cfg.package}/bin/adhd";
      };
      preStart = ''
        mkdir /var/lib/adhd && cp ${configFile} /var/lib/adhd/config.dhall
      '';
    };
  };
}
