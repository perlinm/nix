{ config, lib }:
let
  dir = "/home/perlinm/nix/dotfiles";
  newline-join = lines: lib.strings.concatMapStrings (line: line + "\n") lines;
in let
  store-symlink = path: config.lib.file.mkOutOfStoreSymlink "${dir}/${path}";
  make-symlink = source: dest:
    "$DRY_RUN_CMD ln -sfT $VERBOSE_ARG ${dir}/${source} $HOME/${dest}";
in {
  home = {
    ".vimrc".source = store-symlink "vimrc";
    ".emacs.d/init.el".source = store-symlink "emacs-init.el";
    ".latexmkrc".source = store-symlink "latexmkrc";
    ".ipython/profile_perlinm/startup/00-libs.py".source =
      store-symlink "ipython-startup-libs.py";
    ".condarc".source = store-symlink "condarc";

    ".indentconfig_settings.yaml".source =
      store-symlink "indentconfig_settings.yaml";
    ".indentconfig.yaml".source = store-symlink "indentconfig.yaml";

    ".mozilla/firefox/p32pbshe.default/chrome/userChrome.css".source =
      store-symlink "firefox_userChrome.css";
  };

  xdg = {
    "starship.toml".source = store-symlink "starship.toml";
    "alacritty/alacritty.yml".source = store-symlink "alacritty.yml";
    "kitty/kitty.conf".source = store-symlink "kitty/kitty.conf";
    "kitty/current-theme.conf".source =
      store-symlink "kitty/current-theme.conf";
    "qpdfview/shortcuts.conf".source = store-symlink "qpdfview-shortcuts.conf";
    "black".source = store-symlink "black";
    "flake8".source = store-symlink "flake8";
    "swaylock/config".source = store-symlink "swaylock-config";
  };

  activation = newline-join [
    (make-symlink "bin" "bin")
    (make-symlink "scripts" "scripts")
    (make-symlink "ssh" ".ssh")
    (make-symlink "helix" ".config/helix")
    (make-symlink "i3" ".config/i3")
    (make-symlink "polybar" ".config/polybar")
    (make-symlink "rofi" ".config/rofi")
    (make-symlink "sway" ".config/sway")
    (make-symlink "waybar" ".config/waybar")
  ];
}
