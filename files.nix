let
  home = {
    ".ssh/config".source = ./configs/ssh/config;
    ".ssh/id_rsa.gpg".source = ./configs/ssh/id_rsa.gpg;
    ".ssh/id_rsa.pub".source = ./configs/ssh/id_rsa.pub;
    ".ssh/fingerprint".source = ./configs/ssh/fingerprint;
    
    ".vimrc".source = ./configs/vimrc;
    ".emacs.d/init.el".source = ./configs/emacs-init.el;
    ".latexmkrc".source = ./configs/latexmkrc;
    ".ipython/profile_perlinm/startup/00-libs.py".source = ./configs/ipython-startup-libs.py;
  };

  xdg = {
    "helix/config.toml".source = ./configs/helix/config.toml;
    "helix/languages.toml".source = ./configs/helix/languages.toml;
    "helix/themes/onedark_perlinm.toml".source = ./configs/helix/themes/onedark_perlinm.toml;

    "sway/config".source = ./configs/sway/config;
    "sway/keybindings".source = ./configs/sway/keybindings;
    "sway/rules".source = ./configs/sway/rules;
    "sway/startup".source = ./configs/sway/startup;
    "sway/variables".source = ./configs/sway/variables;

    "waybar/config".source = ./configs/waybar/config;
    "waybar/style.css".source = ./configs/waybar/style.css;
    "swaylock/config".source = ./configs/swaylock-config;

    "starship.toml".source = ./configs/starship.toml;
    "alacritty/alacritty.yml".source = ./configs/alacritty.yml;
    "qpdfview/shortcuts.conf".source = ./configs/qpdfview-shortcuts.conf;
    "flake8".source = ./configs/flake8;
  };
in
{
  xdg = xdg;
  home = home;
}
