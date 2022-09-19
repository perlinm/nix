{
  # cryptographic software suite
  # gnome-keyring.enable = true;
  # gnome-keyring.components = [ "ssh" ];
  gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
  };

  # disk automounting
  udiskie.enable = true;

  # winow compositor (transparency, fading, etc.)
  picom = {
    enable = true;
    fade = true;
    fadeDelta = 2;
  };

  # screen-locker = {
  #   enable = true;
  #   inactiveInterval = 5;
  #   lockCmd = "betterlockscreen -l dim";
  # };
}
