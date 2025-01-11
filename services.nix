{ pkgs, ... }:
{
  services = {
    # cryptographic software suite
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableZshIntegration = true;
      pinentryPackage = pkgs.pinentry-gtk2;
    };

    # network management
    network-manager-applet.enable = true;

    # enable automounting
    udiskie.enable = true;
  };
}
