let
  home = {
    ".ssh/config".source = ./dotfiles/ssh/config;
    ".ssh/id_rsa.gpg".source = ./dotfiles/ssh/id_rsa.gpg;
    ".ssh/id_rsa.pub".source = ./dotfiles/ssh/id_rsa.pub;
    ".ssh/fingerprint".source = ./dotfiles/ssh/fingerprint;
    
    ".vimrc".source = ./dotfiles/vimrc;
    ".emacs.d/init.el".source = ./dotfiles/emacs-init.el;
    ".latexmkrc".source = ./dotfiles/latexmkrc;
    ".ipython/profile_perlinm/startup/00-libs.py".source = ./dotfiles/ipython-startup-libs.py;
  };

  xdg = {
    "helix/config.toml".source = ./dotfiles/helix/config.toml;
    "helix/languages.toml".source = ./dotfiles/helix/languages.toml;
    "helix/themes/onedark_perlinm.toml".source = ./dotfiles/helix/themes/onedark_perlinm.toml;

    "sway/config".source = ./dotfiles/sway/config;
    "sway/keybindings".source = ./dotfiles/sway/keybindings;
    "sway/rules".source = ./dotfiles/sway/rules;
    "sway/startup".source = ./dotfiles/sway/startup;
    "sway/variables".source = ./dotfiles/sway/variables;

    "waybar/config".source = ./dotfiles/waybar/config;
    "waybar/style.css".source = ./dotfiles/waybar/style.css;
    "swaylock/config".source = ./dotfiles/swaylock-config;

    "i3/config".source = ./dotfiles/i3/config;
    "i3/keybindings".source = ./dotfiles/i3/keybindings;
    "i3/rules".source = ./dotfiles/i3/rules;
    "i3/startup".source = ./dotfiles/i3/startup;

    "starship.toml".source = ./dotfiles/starship.toml;
    "alacritty/alacritty.yml".source = ./dotfiles/alacritty.yml;
    "qpdfview/shortcuts.conf".source = ./dotfiles/qpdfview-shortcuts.conf;
    "flake8".source = ./dotfiles/flake8;
  };
in
{
  xdg = xdg;
  home = home;
}
