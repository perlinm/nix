let dir = "/home/perlinm/nix/dotfiles";
in let
  home = {
    ".ssh/config".source = "${dir}/ssh/config";
    ".ssh/id_rsa.gpg".source = "${dir}/ssh/id_rsa.gpg";
    ".ssh/id_rsa.pub".source = "${dir}/ssh/id_rsa.pub";
    ".ssh/fingerprint".source = "${dir}/ssh/fingerprint";

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
    "helix/config.toml".source = "${dir}/helix/config.toml";
    "helix/languages.toml".source = "${dir}/helix/languages.toml";
    "helix/themes/onedark_perlinm.toml".source =
      "${dir}/helix/themes/onedark_perlinm.toml";

    "starship.toml".source = "${dir}/starship.toml";
    "alacritty/alacritty.yml".source = "${dir}/alacritty.yml";
    "kitty/kitty.conf".source = "${dir}/kitty/kitty.conf";
    "kitty/current-theme.conf".source = "${dir}/kitty/current-theme.conf";
    "qpdfview/shortcuts.conf".source = "${dir}/qpdfview-shortcuts.conf";
    "black".source = "${dir}/black";
    "flake8".source = "${dir}/flake8";

    "i3/config".source = "${dir}/i3/config";
    "i3/keybindings".source = "${dir}/i3/keybindings";
    "i3/rules".source = "${dir}/i3/rules";
    "i3/startup".source = "${dir}/i3/startup";
    "polybar/config.ini".source = "${dir}/polybar/config.ini";
    "polybar/launch.sh".source = "${dir}/polybar/launch.sh";

    "sway/config".source = "${dir}/sway/config";
    "sway/keybindings".source = "${dir}/sway/keybindings";
    "sway/rules".source = "${dir}/sway/rules";
    "sway/startup".source = "${dir}/sway/startup";
    "sway/variables".source = "${dir}/sway/variables";
    "waybar/config".source = "${dir}/waybar/config";
    "waybar/style.css".source = "${dir}/waybar/style.css";
    "swaylock/config".source = "${dir}/swaylock-config";
  };
in {
  xdg = xdg;
  home = home;
}
