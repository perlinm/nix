let
  home = {
    ".ssh/config".source = ./files/ssh/config;
    ".ssh/id_rsa.gpg".source = ./files/ssh/id_rsa.gpg;
    ".ssh/id_rsa.pub".source = ./files/ssh/id_rsa.pub;
    ".ssh/fingerprint".source = ./files/ssh/fingerprint;
    
    ".vimrc".source = ./files/vimrc;
    ".emacs.d/init.el".source = ./files/emacs-init.el;
    ".latexmkrc".source = ./files/latexmkrc;
    ".ipython/profile_perlinm/startup/00-libs.py".source = ./files/ipython-startup-libs.py;
  };

  xdg = {
    "helix/config.toml".source = ./files/helix/config.toml;
    "helix/languages.toml".source = ./files/helix/languages.toml;
    "helix/themes/onedark_perlinm.toml".source = ./files/helix/themes/onedark_perlinm.toml;

    "sway/config".source = ./files/sway/config;
    "sway/keybindings".source = ./files/sway/keybindings;
    "sway/rules".source = ./files/sway/rules;
    "sway/startup".source = ./files/sway/startup;
    "sway/variables".source = ./files/sway/variables;

    "waybar/config".source = ./files/waybar/config;
    "waybar/style.css".source = ./files/waybar/style.css;
    "swaylock/config".source = ./files/swaylock-config;

    "starship.toml".source = ./files/starship.toml;
    "alacritty/alacritty.yml".source = ./files/alacritty.yml;
    "qpdfview/shortcuts.conf".source = ./files/qpdfview-shortcuts.conf;
    "flake8".source = ./files/flake8;
  };
in
{
  xdg = xdg;
  home = home;
}
