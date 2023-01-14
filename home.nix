# https://github.com/nix-community/home-manager
{ pkgs, lib, ... }:
let
  services = import /home/perlinm/nix/services.nix;
  programs = import /home/perlinm/nix/programs.nix { inherit pkgs; };
  packages = import /home/perlinm/nix/packages.nix { inherit pkgs; };
  files = import /home/perlinm/nix/files.nix;
  shell = import /home/perlinm/nix/shell.nix { inherit lib; };
in {
  # The state version determines some configuration defaults.
  # This version can be updated, but doing so may require manual intervention.
  # https://nix-community.github.io/home-manager/options.html#opt-home.stateVersion
  home.stateVersion = "22.11";

  home.username = "perlinm";
  home.homeDirectory = "/home/perlinm";

  home.keyboard.layout = "us(colemak),us";
  home.keyboard.options = [
    "caps:backspace"
    "lv3:ralt_alt"
    "shift:both_capslock_cancel"
    "grp:ctrls_toggle"
  ];

  # add ~/bin and ~/scripts symlinks
  home.activation = {
    makeSymbolicLinks = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/bin $HOME/bin
      $DRY_RUN_CMD ln -sTf $VERBOSE_ARG $HOME/nix/scripts $HOME/scripts
    '';
  };

  # add symlinks inside ~/ and ~/.config
  home.file = files.home;
  xdg.configFile = files.xdg;

  services = services;
  programs = programs;

  home.sessionPath = shell.sessionPath;
  home.sessionVariables = shell.sessionVariables;
  home.shellAliases = shell.aliases;
  home.packages = packages;

  # let Home Manager manage fonts
  fonts.fontconfig.enable = true;

  # let Home Manager manage gtk themes
  gtk = {
    enable = true;
    theme.name = "Dracula";
    theme.package = pkgs.dracula-theme;
    iconTheme.name = "Papirus-Dark";
    iconTheme.package = pkgs.papirus-icon-theme;
  };

  # allow installing unfree packages
  nixpkgs.config.allowUnfree = true;
}
