 # https://github.com/nix-community/home-manager
{ config, pkgs, lib, ... }:

# TODO:
# add LSPs
# configure firefox
# python environments: https://github.com/DavHau/mach-nix
# remaining parts of arch-setup
# credit i3-scriatchpad and add it to the repo
# look into https://looking-glass.io/

let
  services = import ./services.nix;
  programs = import ./programs.nix {inherit pkgs; };
  packages = import ./packages.nix { inherit pkgs; };
  files = import ./files.nix;
  shell = import ./shell.nix { inherit lib; };
in
{
  # The state version determines some configuration defaults.
  # This version can be updated, but doing so may require manual intervention.
  # https://nix-community.github.io/home-manager/options.html#opt-home.stateVersion
  home.stateVersion = "22.05";

  home.username = "perlinm";
  home.homeDirectory = "/home/perlinm";

  home.keyboard.layout = "us";
  home.keyboard.variant = "colemak";
  home.keyboard.options = [
    "caps:backspace"
    "lv3:ralt_alt"
    "shift:both_capslock_cancel"
    "grp:shift_caps_toggle"
  ];

  services = services;
  programs = programs;
  home.file = files.home;
  xdg.configFile = files.xdg;

  home.sessionPath = shell.sessionPath;
  home.sessionVariables = shell.sessionVariables;
  home.shellAliases = shell.aliases;
  home.activation = shell.activation;

  home.packages = packages.work;

  # allow installing unfree packages
  nixpkgs.config.allowUnfree = true;

  # let Home Manager manage fonts;  https://github.com/nix-community/home-manager/issues/1118
  fonts.fontconfig.enable = lib.mkForce true;

  # have Home Manager set some helpful environment variables for installed packages.
  targets.genericLinux.enable = true;
}
