{
  sessionPath = [
    "/usr/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.pyenv/bin"
    "$HOME/bin"
  ];

  sessionVariables = {
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

  aliases = {
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
    black = "$(pyenv which black)";
    flake8 = "$(pyenv which flake8)";
    jupyter = "$(pyenv which jupyter)";
    mypy = "$(pyenv which mypy)";
    pip = "$(pyenv which pip)";
    pytest = "$(pyenv which pytest)";

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
