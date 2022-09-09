{ config, pkgs, lib, ... }:

# TODO:
# add LSPs
# figure out python + packages
# remaining parts of arch-setup
# window manager
# configure firefox

let
  programs = import ./programs.nix;
  packages = import ./packages.nix { inherit pkgs; };
  configFiles = import ./configFiles.nix;
in
{
  # https://nix-community.github.io/home-manager/options.html#opt-home.stateVersion
  home.stateVersion = "22.05";

  home.username = "perlinm";
  home.homeDirectory = "/home/perlinm";

  home.keyboard.options = [ "shift:both_capslock" "caps:backspace" ];
  home.keyboard.variant = "colemak";

  programs = import ./programs.nix;
  home.packages = import ./packages.nix { inherit pkgs; };
  home.file = configFiles.home;
  xdg.configFile = configFiles.xdg;

  nixpkgs.config.allowUnfree = true;  # allow installing unfree packages
  fonts.fontconfig.enable = lib.mkForce true;  # update font cache;  https://github.com/nix-community/home-manager/issues/1118

  home.sessionPath = [
    "/usr/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.pyenv/bin"
    "$HOME/bin"
  ];

  home.sessionVariables = {
    EDITOR = "hx";
    VISUAL = "hx";
    BROWSER = "/usr/bin/firefox";
    TERM = "xterm-256color";

    NIX_PATH = "$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels$\{NIX_PATH:+:$NIX_PATH\}";
    RUST_BACKTRACE = 1;

    MATLAB_LOG_DIR = "/home/perlinm/Workspace/MATLAB/logs";
    QT_LOGGING_RULES = "qt5ct.debug=false";
    TF_CPP_MIN_LOG_LEVEL = 2;
  };

  home.shellAliases = {
    sudo = "sudo ";  # allows using aliases after "sudo"
    pm = "aptitude";  # package manager
    rem = "trash";  # trash management, replacing "rm"
    fire = "firefox";  # web browser

    # python aliases
    py = "python";
    ipy = "ipython";
    python = "python3";
    ipython = "ipython3";
    pyenv-init = ''
      eval "$(pyenv init --path)"
      eval "$(pyenv virtualenv-init -)"
    '';
    jupyter = "$(pyenv which jupyter)";
    pytest = "$(pyenv which pytest)";
    mypy = "$(pyenv which mypy)";
    flake8 = "$(pyenv which flake8)";

    # super.tech
    ss = ''
      pyenv-init
      cd ~/super.tech/SuperstaQ
    '';
    pygrep = "$HOME/super.tech/SuperstaQ/dev_tools/pygrep.sh";

    # telehealth
    tt = ''
      pyenv-init
      export FLASK_APP=app
      export FLASK_ENV=development
      cd ~/telehealth
      if [ "$(systemctl is-active postgresql.service)" != "active" ]; then
        echo "systemctl start postgresql.service"
        sudo systemctl start postgresql.service
      fi
    '';
  };
}
