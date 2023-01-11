{ pkgs }:
{
  # let Home Manager install and manage itself
  home-manager.enable = true;

  bash = {
    enable = true;
    enableCompletion = true;  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bash.enableCompletion
    enableVteIntegration = true;
    initExtra = ''
      eval "$(starship init bash)"
    '';
  };

  zsh = {
    enable = true;
    enableCompletion = true;  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.zsh.enableCompletion
    enableVteIntegration = true;
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
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.7.0";
          sha256 = "1g3pij5qn2j7v7jjac2a63lxd97mcsgw6xq6k5p7835q9fjiid98";
        };
      }
    ];
    shellGlobalAliases = {
        "..." = "../..";
        "...." = "../../..";
        NN = "2>/dev/null";
    };
    initExtra = ''
      # fuzzy tab completion: https://superuser.com/a/815317
      zstyle ':completion:*' matcher-list "" \
        'm:{a-z\-}={A-Z\_}' \
        'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
        'r:|?=** m:{a-z\-}={A-Z\_}'

      # command-line prompt
      eval "$(starship init zsh)"
    '';
  };

  exa.enable = true;
  exa.enableAliases = true;

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
}
