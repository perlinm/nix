{ lib, pkgs, ... }:
let shell = import ./shell.nix { inherit lib; };
in {
  programs = {
    # let home-manager install and manage itself
    home-manager.enable = true;

    # command-line interpreter
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

    # replacement for bash
    zsh = {
      enable = true;
      autocd = false;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
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

        # This speeds up pasting w/ autosuggest
        # https://github.com/zsh-users/zsh-autosuggestions/issues/238
        pasteinit() {
          OLD_SELF_INSERT=''${''${(s.:.)widgets[self-insert]}[2,3]}
          zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
        }
        pastefinish() {
          zle -N self-insert $OLD_SELF_INSERT
        }
        zstyle :bracketed-paste-magic paste-init pasteinit
        zstyle :bracketed-paste-magic paste-finish pastefinish
      '';
      sessionVariables = shell.home.sessionVariables;
    };

    # version control!
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
        core.askpass = "";
        credential = { helper = "store"; };
      };
      aliases = {
        st = "status";
        br = "branch";
        co = "checkout";
      };
    };

    # replacement for 'ls'
    eza.enable = true;
    eza.enableAliases = true;

    # replacement for 'cd'
    zoxide.enable = true;
    zoxide.enableZshIntegration = true;

    # better shell history
    atuin.enable = true;
    atuin.flags = [ "--disable-up-arrow" ];
  };
}
