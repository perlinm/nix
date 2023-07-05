{ lib }:
let
  conda-setup = "conda-shell -c $(echo $SHELL)";
  conda-init =
    ''eval "$(~/.conda/bin/conda shell.$(basename $(echo $SHELL)) hook)"'';
  conda-activate = conda-env: "${conda-init} && conda activate ${conda-env}";
  conda-go = conda-env: cmd: ''
    if [ "$(env | grep CONDA_EXE)" ]; then
      ${conda-activate conda-env}
    fi
    ${cmd}
  '';
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
  };

  aliases = {
    sudo = "sudo "; # allows using aliases after "sudo"
    rem = "trash-put"; # trash management, replacing "rm"

    # hack for unstable helix to work in stable conda
    hx = "LD_LIBRARY_PATH='' hx";

    py = "python";
    ipy = "ipython";
    calc = "ipython --profile=perlinm --no-banner";

    cs = conda-setup;
    ci = conda-init;
    nn = conda-go "base" "jupyter notebook";
    ss = conda-go "superstaq" "cd ~/super.tech/server-superstaq";
    ssc = conda-go "superstaq" "cd ~/super.tech/client-superstaq";
    ssr = conda-go "superstaq" "cd ~/super.tech/research-superstaq";
    qq = conda-go "QFI-Opt" "cd ~/super.tech/QFI-Opt";
    cc = conda-go "ColdQuanta" "cd ~/super.tech/coldquanta-system";
    ccc = conda-go "ColdQuanta"
      "cd ~/super.tech/coldquanta-system/modeling/coldquanta/modeling/gates/cz_atomic_sim";

    mm = "mathematica 2> /dev/null";
  };
}
