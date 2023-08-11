{ config }:
let
  dir = "/home/perlinm/nix/dotfiles";
  link_source = "$DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles";
  symlink = path: config.lib.file.mkOutOfStoreSymlink path;
in {
  home = {
    ".vimrc".source = symlink "${dir}/vimrc";
    ".emacs.d/init.el".source = symlink "${dir}/emacs-init.el";
    ".latexmkrc".source = symlink "${dir}/latexmkrc";
    ".ipython/profile_perlinm/startup/00-libs.py".source =
      "${dir}/ipython-startup-libs.py";
    ".condarc".source = symlink "${dir}/condarc";

    ".indentconfig_settings.yaml".source = symlink "${dir}/indentconfig_settings.yaml";
    ".indentconfig.yaml".source = symlink "${dir}/indentconfig.yaml";

    ".mozilla/firefox/p32pbshe.default/chrome/userChrome.css".source =
      "${dir}/firefox_userChrome.css";
  };

  xdg = {
    "starship.toml".source = symlink "${dir}/starship.toml";
    "alacritty/alacritty.yml".source = symlink "${dir}/alacritty.yml";
    "kitty/kitty.conf".source = symlink "${dir}/kitty/kitty.conf";
    "kitty/current-theme.conf".source = symlink "${dir}/kitty/current-theme.conf";
    "qpdfview/shortcuts.conf".source = symlink "${dir}/qpdfview-shortcuts.conf";
    "black".source = symlink "${dir}/black";
    "flake8".source = symlink "${dir}/flake8";
    "swaylock/config".source = symlink "${dir}/swaylock-config";
  };

  activation = ''
    ${link_source}/bin $HOME/bin
    ${link_source}/scripts $HOME/scripts
    ${link_source}/ssh $HOME/.ssh

    ${link_source}/helix $HOME/.config/helix
    ${link_source}/i3 $HOME/.config/i3
    ${link_source}/polybar $HOME/.config/polybar
    ${link_source}/rofi $HOME/.config/rofi
    ${link_source}/sway $HOME/.config/sway
    ${link_source}/waybar $HOME/.config/waybar
  '';
}
