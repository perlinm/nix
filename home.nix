# https://github.com/nix-community/home-manager
{ pkgs, ... }: {
  # The state version determines some configuration defaults.
  # This version can be updated, but doing so may require manual intervention.
  # https://nix-community.github.io/home-manager/options.html#opt-home.stateVersion
  home.stateVersion = "22.11";

  home.username = "perlinm";
  home.homeDirectory = "/home/perlinm";

  home.keyboard.layout = "us(colemak),us";
  home.keyboard.options = [
    "caps:backspace"
    "lv3:ralt_alt"
    "shift:both_capslock_cancel"
    "grp:ctrls_toggle"
  ];

  # let Home Manager manager the x session, e.g. to set keyboard settings
  xsession.enable = true;

  imports =
    [ ./dotfiles.nix ./programs.nix ./services.nix ./packages.nix ./shell.nix ];

  # let Home Manager manage fonts
  fonts.fontconfig.enable = true;

  # let Home Manager manage gtk themes
  gtk = {
    enable = true;
    theme.name = "Dracula";
    theme.package = pkgs.dracula-theme;
    iconTheme.name = "Papirus-Dark";
    iconTheme.package = pkgs.papirus-icon-theme;
  };

  # disable networkmanager notifications
  dconf.settings = {
    "org/gnome/nm-applet" = {
      disable-connected-notifications = true;
      disable-disconnected-notifications = true;
    };
  };
}
