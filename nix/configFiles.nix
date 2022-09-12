let
  home = {
    ".ssh/config".source = ./configs/ssh/config;
    ".ssh/id_rsa.gpg".source = ./configs/ssh/id_rsa.gpg;
    ".ssh/id_rsa.pub".source = ./configs/ssh/id_rsa.pub;
    ".ssh/fingerprint".source = ./configs/ssh/fingerprint;
    
    ".xinitrc".source = ./configs/xinitrc;
    ".Xmodmap".source = ./configs/Xmodmap;
    ".xmonad/xmonad.hs".source = ./configs/xmonad.hs;
    ".vimrc".source = ./configs/vimrc;
    ".emacs.d/init.el".source = ./configs/emacs-init.el;
    ".latexmkrc".source = ./configs/latexmkrc;
    ".ipython/profile_perlinm/startup/00-libs.py".source = ./configs/ipython-startup-libs.py;
  };
  xdg = {
    "helix/config.toml".source = ./configs/helix/config.toml;
    "helix/languages.toml".source = ./configs/helix/languages.toml;
    "helix/themes/onedark_perlinm.toml".source = ./configs/helix/themes/onedark_perlinm.toml;
    # TODO: symlink helix/themes/runtime to $HOME/.nix-profile/lib/runtime
    # look into home.activation

    "regolith2/Xresources".source = ./configs/regolith2/Xresources;
    "regolith2/i3/config.d/70_bar".source = ./configs/regolith2/i3/config.d/70_bar;
    "regolith2/i3/config.d/80_keybindings".source = ./configs/regolith2/i3/config.d/80_keybindings;
    "regolith2/i3/config.d/90_rules".source = ./configs/regolith2/i3/config.d/90_rules;
    "regolith2/i3/config.d/99_startup".source = ./configs/regolith2/i3/config.d/99_startup;
    "regolith2/i3xrocks/conf.d/setup".source = ./configs/regolith2/i3xrocks/conf.d/setup;

    "starship.toml".source = ./configs/starship.toml;
    "xfce4/terminal/terminalrc".source = ./configs/xfce4-terminalrc;
    "qpdfview/qpdfview.conf".source = ./configs/qpdfview/qpdfview.conf;
    "qpdfview/shortcuts.conf".source = ./configs/qpdfview/shortcuts.conf;
    "flake8".source = ./configs/flake8;
  };
in
{
  xdg = xdg;
  home = home;
}
