# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  home-manager-tarball =
    builtins.fetchTarball
      "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    (import "${home-manager-tarball}/nixos")
    ];

  # allow unfree software, which may be necessary for drivers
  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "map-nix";
  networking.networkmanager.enable = true;

  # internationalisation properties
  # WARNING: these are ignored by some desktop environments (e.g. GNOME)
  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.utf8";
  i18n.extraLocaleSettings.LC_TIME = "en_GB.utf8";

  # X11 services
  services.xserver = {
    enable = true;
    libinput.enable = true;  # touchpad support

    # keyboard layout
    layout = "us";
    xkbVariant = "colemak";
    autoRepeatDelay = 200;  # delay before a key repeats, in milliseconds
    autoRepeatInterval = 60;  # delay between repeat strokes, in milliseconds

    # display (login) and desktop managers
    displayManager.gdm.enable = true;
    # displayManager.defaultSession = "xfce+i3";
    # desktopManager = {
    #   xterm.enable = false;
    #   xfce = {
    #     enable = true;
    #     noDesktop = true;
    #     enableXfwm = false;
    #   };
    # };
    windowManager.i3.enable = true;
    windowManager.i3.package = pkgs.i3-gaps;
  };

  # sound with pipewire and pulseaudio
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  nixpkgs.config.pulseaudio = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # enable CUPS to print documents
  services.printing.enable = true;

  environment.systemPackages = with pkgs; [
    firefox  # web browser
    git  # version control system
    killall  # kill processes by name
    pavucontrol  # volume control
    vim  # text editors
    wget  # retrieve files from the web
    xdotool  # simulate keyboard/mouse inputs
    zsh  # better than bash

    gnome.gnome-keyring  # secret/certificate manager

    xorg.xbacklight  # screen brightness
    xorg.xev  # log X events
    xorg.xmodmap  # modify keymaps
    xorg.xprop  # get window properties

    xfce.xfce4-terminal  # terminal emulator
    xfce.xfce4-notifyd  # notification daemon
    xfce.xfce4-panel  # status bar/panel
    xfce.xfce4-panel-profiles  # panel profiles
    xfce.xfce4-i3-workspaces-plugin  # workspace management
    xfce.xfce4-pulseaudio-plugin  # volume management
    xfce.xfce4-netload-plugin  # show upload/download speeds
    xfce.xfce4-cpugraph-plugin  # graph of CPU load
  ];

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };

  users.users.perlinm = {
    isNormalUser = true;
    description = "Michael A. Perlin";
    extraGroups = [ "wheel" "networkmanager" "audio" ];
    shell = pkgs.zsh;
  };

  # make home-manager use global configs and install paths (as opposed to user-specefic ones)
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.users.perlinm = import /home/perlinm/.config/nixpkgs/home.nix;
}
