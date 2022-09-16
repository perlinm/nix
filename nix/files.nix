let
  home = {
    ".ssh/config".source = ./files/ssh/config;
    ".ssh/id_rsa.gpg".source = ./files/ssh/id_rsa.gpg;
    ".ssh/id_rsa.pub".source = ./files/ssh/id_rsa.pub;
    ".ssh/fingerprint".source = ./files/ssh/fingerprint;
    
    ".xinitrc".source = ./files/xinitrc;
    ".Xmodmap".source = ./files/Xmodmap;
    ".xmonad/xmonad.hs".source = ./files/xmonad.hs;
    ".vimrc".source = ./files/vimrc;
    ".emacs.d/init.el".source = ./files/emacs-init.el;
    ".latexmkrc".source = ./files/latexmkrc;
    ".ipython/profile_perlinm/startup/00-libs.py".source = ./files/ipython-startup-libs.py;
  };
  xdg = {
    "helix/config.toml".source = ./files/helix/config.toml;
    "helix/languages.toml".source = ./files/helix/languages.toml;
    "helix/themes/onedark_perlinm.toml".source = ./files/helix/themes/onedark_perlinm.toml;
    # TODO: symlink helix/themes/runtime to $HOME/.nix-profile/lib/runtime
    # look into home.activation

    "regolith2/Xresources".source = ./files/regolith2/Xresources;
    "regolith2/i3/config.d/70_bar".source = ./files/regolith2/i3/config.d/70_bar;
    "regolith2/i3/config.d/80_rules".source = ./files/regolith2/i3/config.d/80_rules;
    "regolith2/i3/config.d/90_keybindings".source = ./files/regolith2/i3/config.d/90_keybindings;
    "regolith2/i3/config.d/99_startup".source = ./files/regolith2/i3/config.d/99_startup;
    "regolith2/i3xrocks/conf.d/setup".source = ./files/regolith2/i3xrocks/conf.d/setup;

    "starship.toml".source = ./files/starship.toml;
    "xfce4/terminal/terminalrc".source = ./files/xfce4-terminalrc;
    "qpdfview/shortcuts.conf".source = ./files/qpdfview-shortcuts.conf;
    "flake8".source = ./files/flake8;
  };
in
{
  xdg = xdg;
  home = home;
}
