{
  # cryptographic software suite
  gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "qt";
  };

  # network / wifi management
  network-manager-applet.enable = true;

  # enable automounting
  udiskie.enable = true;
}
