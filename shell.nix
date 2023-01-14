{ lib }:
let
  conda-setup =
    ''eval "$(~/.conda/bin/conda shell.$(basename $(echo $SHELL)) hook)"'';
in {
  sessionPath = [ "$HOME/bin" ];

  sessionVariables = {
    EDITOR = "hx";
    VISUAL = "hx";
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
    sudo = "sudo "; # allows using aliases after "sudo"
    rem = "trash"; # trash management, replacing "rm"

    py = "python";
    ipy = "ipython";
    calc = "ipython --profile=perlinm --no-banner";

    conda-shell = "conda-shell -c $(echo $SHELL)";
    cs = ''
      ${conda-setup}
      conda activate base
    '';
    nn = ''
      ${conda-setup}
      conda activate base
      jupyter notebook
    '';
    ss = ''
      ${conda-setup}
      conda activate SuperstaQ
      cd ~/super.tech/SuperstaQ
    '';
    qq = ''
      ${conda-setup}
      conda activate QFI-Opt
      cd ~/super.tech/QFI-Opt
    '';
    cc = ''
      ${conda-setup}
      conda activate ColdQuanta
      cd ~/super.tech/coldquanta-system/modeling/coldquanta/modeling/gates/cz_atomic_sim
    '';
    ccs = ''
      ${conda-setup}
      conda activate ColdQuanta
      cd ~/super.tech/coldquanta-system
    '';
  };
}
