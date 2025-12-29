{
  config,
  lib,
  pkgs,
  ...
}:
let
  shell = import ./shell.nix { inherit lib; };
in
{
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
      autosuggestion.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      enableVteIntegration = true;
      defaultKeymap = "emacs";
      oh-my-zsh.enable = true;
      plugins = [
        {
          name = "fzf-tab";
          src = pkgs.fetchFromGitHub {
            owner = "Aloxaf";
            repo = "fzf-tab";
            rev = "5a81e13792a1eed4a03d2083771ee6e5b616b9ab";
            sha256 = "sha256-dPe5CLCAuuuLGRdRCt/nNruxMrP9f/oddRxERkgm1FE=";
          };
        }
      ];
      shellGlobalAliases = {
        "..." = "../..";
        "...." = "../../..";
        "....." = "../../../..";
        DN = "2>/dev/null";
      };
      initContent = ''
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

        # disable bracketed paste
        unset zle_bracketed_paste
      '';
      sessionVariables = shell.home.sessionVariables;
      dotDir = "${config.xdg.configHome}/zsh";
    };

    # version control!
    git = {
      enable = true;
      settings = {
        user = {
          name = "Michael A. Perlin";
          email = "mika.perlin@gmail.com";
        };
        init.defaultBranch = "main";
        core = {
          editor = "hx";
          pager = "less -XF";
          askpass = "";
        };
        fetch.prune = "true";
        pull.ff = "only";
        push.autoSetupRemote = "true";
        credential.helper = "store";
        alias = {
          st = "status";
          br = "branch";
          co = "checkout";
          cm = "commit";
          aa = "!git ls-files -m -o --exclude-standard | fzf -m --print0 | xargs -0 git add";
        };
      };
    };

    # replacement for 'ls'
    eza.enable = true;
    eza.enableBashIntegration = true;
    eza.enableZshIntegration = true;

    # replacement for 'cd'
    zoxide.enable = true;
    zoxide.enableZshIntegration = true;

    # better shell history
    atuin.enable = true;
    atuin.flags = [ "--disable-up-arrow" ];
  };
}
