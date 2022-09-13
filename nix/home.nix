 # https://github.com/nix-community/home-manager
{ config, pkgs, lib, ... }:

# TODO:
# add LSPs
# python environments: https://github.com/DavHau/mach-nix
# configure firefox
# remaining parts of arch-setup

let
  programs = import ./programs.nix;
  packages = import ./packages.nix { inherit pkgs; };
  files = import ./files.nix;
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
  home.file = files.home;
  xdg.configFile = files.xdg;

  home.sessionPath = shell.sessionPath;
  home.sessionVariables = shell.sessionVariables;
  home.shellAliases = shell.aliases;

  nixpkgs.config.allowUnfree = true;  # allow installing unfree packages
  fonts.fontconfig.enable = lib.mkForce true;  # let Home Manager manage fonts;  https://github.com/nix-community/home-manager/issues/1118
}
