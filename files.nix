let
  dir = "/home/perlinm/nix/dotfiles";
  link = "$DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles";
  home = "$HOME";
  config = "$HOME/.config";
in {
  home = {
    ".emacs.d/init.el".source = "${dir}/emacs-init.el";
    ".ipython/profile_perlinm/startup/00-libs.py".source =
      "${dir}/ipython-startup-libs.py";
    ".mozilla/firefox/p32pbshe.default/chrome/userChrome.css".source =
      "${dir}/firefox_userChrome.css";
  };

  xdg = {
    "alacritty/alacritty.yml".source = "${dir}/alacritty.yml";
    "kitty/kitty.conf".source = "${dir}/kitty/kitty.conf";
    "kitty/current-theme.conf".source = "${dir}/kitty/current-theme.conf";
    "qpdfview/shortcuts.conf".source = "${dir}/qpdfview-shortcuts.conf";
    "swaylock/config".source = "${dir}/swaylock-config";
  };

  activation = ''
    ${link}/bin ${home}/bin
    ${link}/scripts ${home}/scripts
    ${link}/ssh ${home}/.ssh
    ${link}/vimrc ${home}/.vimrc
    ${link}/starship.toml ${config}/starship.toml

    ${link}/helix ${config}/helix
    ${link}/i3 ${config}/i3
    ${link}/polybar ${config}/polybar
    ${link}/rofi ${config}/rofi
    ${link}/sway ${config}/sway
    ${link}/waybar ${config}/waybar

    ${link}/condarc ${home}/.condarc

    ${link}/black ${config}/black
    ${link}/flake8 ${config}/flake8

    ${link}/latexmkrc ${home}/.latexmkrc
    ${link}/indentconfig.yaml ${home}/.indentconfig.yaml
    ${link}/indentconfig_settings.yaml ${home}/.indentconfig_settings.yaml
  '';
}
