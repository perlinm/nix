# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  sway-fixes = import ./sway-fixes.nix { inherit pkgs; };
in
{
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  # system.autoUpgrade.enable = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  imports = [
    ./hardware-configuration.nix  # results of hardware scan
    <nixos-hardware/dell/xps/15-9550>
    <home-manager/nixos>
  ];

  # use the Zen linux kernel (others mignt not work!)
  boot.kernelPackages = pkgs.linuxPackages_zen;

  # bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # setup keyfile for encrypted hard drive
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # enable swap on LUKS
  boot.initrd.luks.devices."luks-f4650d9b-98cf-48a4-b7e2-6b1a6ccc1538".device = "/dev/disk/by-uuid/f4650d9b-98cf-48a4-b7e2-6b1a6ccc1538";
  boot.initrd.luks.devices."luks-f4650d9b-98cf-48a4-b7e2-6b1a6ccc1538".keyFile = "/crypto_keyfile.bin";

  # schedule user processes/threads
  security.rtkit.enable = true;

  # fine-grained authentication agent
  security.polkit.enable = true;

  # networking options
  networking.hostName = "map-work";
  networking.networkmanager.enable = true;

  # internationalisation properties
  # WARNING: these are ignored by some desktop environments (e.g. GNOME)
  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.utf8";
  i18n.extraLocaleSettings.LC_TIME = "en_GB.utf8";

  # X11 services
  services.xserver = {
    enable = true;

    # touchpad
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;

    # keyboard layout
    layout = "us";
    xkbVariant = "colemak";
    autoRepeatDelay = 200;
    autoRepeatInterval = 60;

    # display (login), desktop, and window managers
    displayManager.gdm.enable = true;
    displayManager.gdm.wayland = true;

    # enable automatic login
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "perlinm";
  };

  # enable sway window manager
  programs.sway.enable = true;
  programs.xwayland.enable = true;
  programs.sway.wrapperFeatures.gtk = true;
  xdg.portal = sway-fixes.xdg-portal;

  environment.systemPackages = with pkgs; [
    sway-fixes.dbus-sway-environment
    sway-fixes.configure-gtk
    sway-fixes.qt5-fix
    sway-fixes.qt6-fix
  ];

  # sound and bluetooth control
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;

  # enable CUPS to print documents
  services.printing.enable = true;

  # interprocess communications manager
  services.dbus.enable = true;

  # automounting external drives
  services.udisks2.enable = true;

  # change some power settings
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    HandleLidSwitch=ignore
  '';

  users.users.perlinm = {
    isNormalUser = true;
    description = "Michael A. Perlin";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
    shell = pkgs.zsh;
  };

  # allow unfree software, which may be necessary for some drivers
  nixpkgs.config.allowUnfree = true;

  # make home-manager use global configs and install paths (as opposed to user-specefic ones)
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.users.perlinm = import /home/perlinm/.config/nixpkgs/home.nix;
}
