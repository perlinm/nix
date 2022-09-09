{ config, pkgs, lib, ... }:

# TODO:
# add LSPs
# figure out python + packages
# remaining parts of arch-setup
# window manager
# configure firefox

let
  programs = import ./programs.nix;
  packages = import ./packages.nix { inherit pkgs; };
  configFiles = import ./configFiles.nix;
  shell = import ./shell.nix;
in
{
  # The state version determines some configuration defaults.
  # This version can be updated, but doing so may require manual intervention.
  # https://nix-community.github.io/home-manager/options.html#opt-home.stateVersion
  home.stateVersion = "22.05";

  home.username = "perlinm";
  home.homeDirectory = "/home/perlinm";

  home.keyboard.options = [ "shift:both_capslock_cancel" "caps:backspace" ];
  home.keyboard.variant = "colemak";

  programs = programs;
  home.packages = packages;
  home.file = configFiles.home;
  xdg.configFile = configFiles.xdg;

  home.sessionPath = shell.sessionPath;
  home.sessionVariables = shell.sessionVariables;
  home.shellAliases = shell.aliases;

  nixpkgs.config.allowUnfree = true;  # allow installing unfree packages
  fonts.fontconfig.enable = lib.mkForce true;  # let Home Manager manage fonts;  https://github.com/nix-community/home-manager/issues/1118
}
