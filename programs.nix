{ lib, pkgs, ... }:
let shell = import ./shell.nix { inherit lib; };
in {
  programs = {
    # let Home Manager install and manage itself
    home-manager.enable = true;

    bash = {
      enable = true;
      enableCompletion = true;
      enableVteIntegration = true;
      initExtra = ''
        eval "$(starship init bash)"
        export PATH=$HOME/bin:$PATH
      '';
      sessionVariables = shell.home.sessionVariables;
    };

    zsh = {
      enable = true;
      autocd = false;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      enableVteIntegration = true;
      defaultKeymap = "emacs";
      oh-my-zsh.enable = true;
      plugins = [{
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "Aloxaf";
          repo = "fzf-tab";
          rev = "5a81e13792a1eed4a03d2083771ee6e5b616b9ab";
          sha256 = "sha256-dPe5CLCAuuuLGRdRCt/nNruxMrP9f/oddRxERkgm1FE=";
        };
      }];
      shellGlobalAliases = {
        "..." = "../..";
        "...." = "../../..";
        "....." = "../../../..";
        NN = "2>/dev/null";
      };
      initExtra = ''
        # fuzzy tab completion: https://superuser.com/a/815317
        zstyle ':completion:*' matcher-list "" \
          'm:{a-z\-}={A-Z\_}' \
          'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
          'r:|?=** m:{a-z\-}={A-Z\_}'

        eval "$(starship init zsh)"
        export PATH=$HOME/bin:$PATH
      '';
      sessionVariables = shell.home.sessionVariables;
    };

    exa.enable = true;
    exa.enableAliases = true;

    zoxide.enable = true;
    zoxide.enableZshIntegration = true;

    atuin.enable = true;
    atuin.flags = [ "--disable-up-arrow" ];

    git = {
      enable = true;
      userName = "Michael A. Perlin";
      userEmail = "mika.perlin@gmail.com";
      extraConfig = {
        core.editor = "hx";
        init.defaultBranch = "main";
        fetch.prune = "true";
        pull.ff = "only";
        push.autoSetupRemote = "true";
        core.pager = "less -XF";
      };
      aliases = {
        st = "status";
        br = "branch";
        co = "checkout";
      };
    };
  };
}
