{ lib }:
{
  sessionPath = [
    "$HOME/bin"
    "$HOME/.local/bin"
    "$HOME/.pyenv/bin"
    "$HOME/.cargo/bin"
    "$HOME/.cabal/bin"
  ];

  sessionVariables = {
    EDITOR = "helix";
    VISUAL = "helix";
    BROWSER = "firefox";
    TERM = "xterm-256color";

    RUST_BACKTRACE = 1;

    # make firefox work with wayland
    MOZ_ENABLE_WAYLAND = 1;
    XDG_CURRENT_DESKTOP = "sway";

    # to fix matlab isssues
    MATLAB_LOG_DIR = "$HOME/Workspace/MATLAB/logs";
    QT_LOGGING_RULES = "qt5ct.debug=false";
    TF_CPP_MIN_LOG_LEVEL = 2;
  };

  aliases = {
    sudo = "sudo ";  # allows using aliases after "sudo"
    rem = "trash";  # trash management, replacing "rm"
    hx = "helix";  # text editor
    # pm = "yay";  # package manager
    calc = "ipython3 --profile=perlinm --no-banner";

    # python aliases
    py = "python";
    ipy = "ipython";
    python = "python3";
    ipython = "ipython3";
    # pyenv-init = ''
    #   eval "$(pyenv init --path)"
    #   eval "$(pyenv virtualenv-init -)"
    # '';
    # black = "$(pyenv which black)";
    # flake8 = "$(pyenv which flake8)";
    # jupyter = "$(pyenv which jupyter)";
    # mypy = "$(pyenv which mypy)";
    # pip = "$(pyenv which pip)";
    # pytest = "$(pyenv which pytest)";

    # # super.tech
    # ss = ''
    #   pyenv-init
    #   cd ~/super.tech/SuperstaQ
    # '';
    # qq = ''
    #   pyenv-init
    #   cd ~/super.tech/QFI-Opt
    # '';
    # cc = ''
    #   pyenv-init
    #   cd ~/super.tech/coldquanta-system/modeling/coldquanta/modeling/gates/cz_atomic_sim
    # '';

    # # telehealth
    # tt = ''
    #   pyenv-init
    #   export FLASK_APP=app
    #   export FLASK_ENV=development
    #   cd ~/telehealth
    #   if [ "$(systemctl is-active postgresql.service)" != "active" ]; then
    #     echo "systemctl start postgresql.service"
    #     sudo systemctl start postgresql.service
    #   fi
    # '';
  };

  activation = {
    makeSymbolicLinks = lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/bin $HOME/bin
      $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/scripts $HOME/scripts
    '';
  };
}
