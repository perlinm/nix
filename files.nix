let dir = "/home/perlinm/nix/dotfiles";
in {
  home = {
    ".vimrc".source = "${dir}/vimrc";
    ".emacs.d/init.el".source = "${dir}/emacs-init.el";
    ".latexmkrc".source = "${dir}/latexmkrc";
    ".ipython/profile_perlinm/startup/00-libs.py".source =
      "${dir}/ipython-startup-libs.py";
    ".condarc".source = "${dir}/condarc";

    ".indentconfig_settings.yaml".source = "${dir}/indentconfig_settings.yaml";
    ".indentconfig.yaml".source = "${dir}/indentconfig.yaml";

    ".mozilla/firefox/p32pbshe.default/chrome/userChrome.css".source =
      "${dir}/firefox_userChrome.css";
  };

  xdg = {
    "starship.toml".source = "${dir}/starship.toml";
    "alacritty/alacritty.yml".source = "${dir}/alacritty.yml";
    "kitty/kitty.conf".source = "${dir}/kitty/kitty.conf";
    "kitty/current-theme.conf".source = "${dir}/kitty/current-theme.conf";
    "qpdfview/shortcuts.conf".source = "${dir}/qpdfview-shortcuts.conf";
    "black".source = "${dir}/black";
    "flake8".source = "${dir}/flake8";
    "swaylock/config".source = "${dir}/swaylock-config";
  };

  activation = ''
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/bin $HOME/bin
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/scripts $HOME/scripts
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/ssh $HOME/.ssh

    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/helix $HOME/.config/helix
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/i3 $HOME/.config/i3
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/polybar $HOME/.config/polybar
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/rofi $HOME/.config/rofi
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/sway $HOME/.config/sway
    $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/dotfiles/waybar $HOME/.config/waybar
  '';
}
