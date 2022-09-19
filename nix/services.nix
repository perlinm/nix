{
  # cryptographic software suite
  # gnome-keyring.enable = true;
  gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };

  # disk automounting
  udiskie.enable = true;

  # screen-locker = {
  #   enable = true;
  #   inactiveInterval = 5;
  #   lockCmd = "betterlockscreen -l dim";
  # };
}
