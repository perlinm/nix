{ config, pkgs, lib, ... }:

# TODO:
# add LSPs
# figure out python + packages
# remaining parts of arch-setup
# window manager

{
  home.username = "perlinm";
  home.homeDirectory = "/home/perlinm";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.keyboard.options = [ "shift:both_capslock" "caps:backspace" ];
  home.keyboard.variant = "colemak";

  nixpkgs.config.allowUnfree = true;
  home.packages = import ./packages.nix { inherit pkgs; };

  fonts.fontconfig.enable = lib.mkForce true;

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
  
  ### package configurations

  programs.bash = {
    enable = true;
    enableCompletion = true;  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bash.enableCompletion
    enableVteIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.zsh.enableCompletion
    enableVteIntegration = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    defaultKeymap = "emacs";
    oh-my-zsh.enable = true;
    plugins = [
      # {
      #   name = "zsh-autocomplete";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "marlonrichert";
      #     repo = "zsh-autocomplete";
      #     rev = "22.01.21";
      #     sha256 = "12y0zg06hqkkz5snzf1gp07fv8ds4fxar99bk6p9i0i3id6y4k7r";
      #   };
      # }
      # {
      #   name = "zsh-autosuggestions";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "zsh-users";
      #     repo = "zsh-autosuggestions";
      #     rev = "v0.7.0";
      #     sha256 = "1g3pij5qn2j7v7jjac2a63lxd97mcsgw6xq6k5p7835q9fjiid98";
      #   };
      # }
    ];
    initExtra = ''
      # prompts
      PROMPT=$(print "[%{$fg[yellow]%}%*%{$reset_color%}]%{$fg[green]%}%~:\n$ %{$reset_color%}")
      SPROMPT='Correct '%R' to '%r' ? ([y]es/[N]o/[e]dit/[a]bort)'

      # fuzzy tab completion: https://superuser.com/a/815317
      zstyle ':completion:*' matcher-list "" \
        'm:{a-z\-}={A-Z\_}' \
        'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
        'r:|?=** m:{a-z\-}={A-Z\_}'
    '';
    shellGlobalAliases = {
        "..." = "../..";
        "...." = "../../..";
        NN = "2>/dev/null";
    };
  };

  programs.exa.enable = true;
  programs.exa.enableAliases = true;

  programs.git = {
    enable = true;
    userName = "Michael A. Perlin";
    userEmail = "mika.perlin@gmail.com";
    extraConfig = {
      core.editor = "hx";
      init.defaultBranch = "main";
      fetch.prune = "true";
      pull.ff = "only";
      push.autoSetupRemote = "true";
    };
    aliases = {
      st = "status";
      br = "branch";
      co = "checkout";
    };
  };

  programs.helix.enable = true;
  xdg.configFile."helix/config.toml".source = ./configs/helix/config.toml;
  xdg.configFile."helix/languages.toml".source = ./configs/helix/languages.toml;
  xdg.configFile."helix/themes/onedark_perlinm.toml".source = ./configs/helix/themes/onedark_perlinm.toml;
  # TODO: symlink helix/themes/runtime to $HOME/.nix-profile/lib/runtime

  home.file.".ssh/config".source = ./configs/ssh/config;
  home.file.".ssh/id_rsa.gpg".source = ./configs/ssh/id_rsa.gpg;
  home.file.".ssh/id_rsa.pub".source = ./configs/ssh/id_rsa.pub;
  home.file.".ssh/fingerprint".source = ./configs/ssh/fingerprint;

  # misc. config files
  home.file.".xinitrc".source = ./configs/xinitrc;
  home.file.".Xmodmap".source = ./configs/Xmodmap;
  home.file.".xmonad/xmonad.hs".source = ./configs/xmonad.hs;
  home.file.".vimrc".source = ./configs/vimrc;
  home.file.".emacs.d/init.el".source = ./configs/emacs-init.el;
  home.file.".latexmkrc".source = ./configs/latexmkrc;
  home.file.".ipython/profile_perlinm/startup/00-libs.py".source = ./configs/ipython-startup-libs.py;
  xdg.configFile."xfce4/terminal/terminalrc".source = ./configs/xfce4-terminalrc;
  xdg.configFile."qpdfview/qpdfview.conf".source = ./configs/qpdfview/qpdfview.conf;
  xdg.configFile."qpdfview/shortcuts.conf".source = ./configs/qpdfview/shortcuts.conf;
  xdg.configFile."flake8".source = ./configs/flake8;
}
