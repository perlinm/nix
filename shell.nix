{ ... }:
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
  home.sessionPath = [ "$HOME/bin" ];

  home.sessionVariables = {
    EDITOR = "hx";
    VISUAL = "hx";
    BROWSER = "firefox";
    TERM = "xterm-256color";

    # # make firefox work with wayland / sway
    # MOZ_ENABLE_WAYLAND = 1;
    # XDG_CURRENT_DESKTOP = "sway";
  };

  home.shellAliases = {
    sudo = "sudo "; # allows using aliases after "sudo"
    res = "systemctl reboot";
    sus = "systemctl suspend";
    hib = "systemctl hibernate";

    rem = "trash-put"; # trash management, replacing "rm"
    cd = "z"; # zoxide

    py = "python";
    ipy = "ipython";
    calc = "ipython --profile=perlinm --no-banner";

    cs = conda-setup;
    ci = conda-init;
    nn = "jupyter notebook";
    ss = conda-go "superstaq" "cd ~/super.tech/server-superstaq";
    ssc = conda-go "superstaq" "cd ~/super.tech/client-superstaq";
    ssr = conda-go "research-superstaq"
      "cd ~/super.tech/research-superstaq/research_superstaq/theory";
    # ssl = conda-go "qldpc" "cd ~/src/qLDPC";
    ssl = conda-go "qldpc"
      "cd ~/super.tech/research-superstaq/research_superstaq/theory/qLDPC";
    qq = conda-go "QFI-Opt" "cd ~/super.tech/QFI-Opt";
    cc = conda-go "ColdQuanta" "cd ~/super.tech/coldquanta-system";
    zz = conda-go "zain" "cd ~/super.tech/zain";

    update-cq = "python -m coldquanta.qc_common_api.cq_authorize";

    mm = "mathematica 2> /dev/null";
  };
}
