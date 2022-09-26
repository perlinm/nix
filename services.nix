{
  # cryptographic software suite
  gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };

  # network / wifi management
  network-manager-applet.enable = true;

  # disk automounting
  udiskie.enable = true;
}