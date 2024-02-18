{
  services = {
    # cryptographic software suite
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gtk2";
      enableZshIntegration = true;
    };

    # network management
    network-manager-applet.enable = true;

    # enable automounting
    udiskie.enable = true;
  };
}
