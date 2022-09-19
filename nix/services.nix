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

  # winow compositor (transparency, fading, etc.)
  picom = {
    enable = true;
    fade = true;
    fadeDelta = 2;
    shadow = true;
    shadowOpacity = 0.75;
    shadowExclude = [ "class = '^Rofi$'" ];
  };

  # screen locker
  betterlockscreen = {
    enable = true;
    # inactiveInterval = 5;
  };
}
