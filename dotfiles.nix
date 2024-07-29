{ config, lib, ... }:
let
  dotfile-dir = "${config.home.homeDirectory}/nix/dotfiles";
  copy = path: config.lib.file.mkOutOfStoreSymlink "${dotfile-dir}/${path}";

  # Define custom symbolic links.
  # These links are "dirty", in the sense that home-manager will add links according to
  # the list below, but removing an entry from the list will not remove a link.
  symlink = source: dest:
    "$DRY_RUN_CMD ln -sfT $VERBOSE_ARG ${dotfile-dir}/${source} $HOME/${dest}";
  newline-join = lines: lib.strings.concatMapStrings (line: line + "\n") lines;
  activation = newline-join [
    (symlink "bin" "bin")
    (symlink "scripts" "scripts")
    (symlink "ssh" ".ssh")
    (symlink "helix" ".config/helix")
    (symlink "i3" ".config/i3")
    (symlink "polybar" ".config/polybar")
    (symlink "rofi" ".config/rofi")
    (symlink "sway" ".config/sway")
    (symlink "waybar" ".config/waybar")
  ];

in {
  home.file = {
    ".vimrc".source = copy "vimrc";
    ".emacs.d/init.el".source = copy "emacs-init.el";
    ".latexmkrc".source = copy "latexmkrc";
    ".ipython/profile_perlinm/startup/00-libs.py".source =
      copy "ipython-startup-libs.py";
    ".condarc".source = copy "condarc";

    ".indentconfig_settings.yaml".source = copy "indentconfig_settings.yaml";
    ".indentconfig.yaml".source = copy "indentconfig.yaml";
  };

  xdg.configFile = {
    "starship.toml".source = copy "starship.toml";
    "alacritty/alacritty.toml".source = copy "alacritty.toml";
    "kitty/kitty.conf".source = copy "kitty/kitty.conf";
    "kitty/current-theme.conf".source = copy "kitty/current-theme.conf";
    "wezterm/wezterm.lua".source = copy "wezterm.lua";
    "qpdfview/shortcuts.conf".source = copy "qpdfview-shortcuts.conf";
    "black".source = copy "black";
    "flake8".source = copy "flake8";
    "swaylock/config".source = copy "swaylock-config";
  };

  home.activation = {
    makeSymbolicLinks = lib.hm.dag.entryAfter [ "writeBoundary" ] activation;
  };
}
