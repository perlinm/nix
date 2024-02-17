{ lib, pkgs, ... }:
# let sway-fixes = import ./sway-fixes.nix { inherit pkgs; };
{
  imports =
    [ ./hardware-configuration.nix ./luks.nix ]; # results of hardware scan

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  # system.autoUpgrade.enable = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # use the Zen linux kernel (others mignt not work!)
  boot.kernelPackages = pkgs.linuxPackages_zen;

  # bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # internationalization properties
  # WARNING: these are ignored by some desktop environments (such as GNOME)
  time.timeZone = lib.mkForce null; # allow time zone to be set by user
  services.automatic-timezoned.enable = true;
  i18n.defaultLocale = "en_US.utf8";
  i18n.extraLocaleSettings.LC_TIME = "en_GB.utf8";

  # networking options
  networking.hostName = "map-work";
  networking.networkmanager.enable = true;

  # keyboard layout in the console
  console.keyMap = "colemak";

  # define user
  users.users.perlinm = {
    isNormalUser = true;
    description = "Michael A. Perlin";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
    shell = pkgs.zsh;
  };
  programs.zsh.enable = true;

  # X11 services
  services.xserver = {
    enable = true;

    # keyboard layout
    xkb.layout = "us";
    xkb.variant = "colemak";
    autoRepeatDelay = 200;
    autoRepeatInterval = 60;

    # display (login) and window managers
    displayManager = {
      sddm.enable = true;
      autoLogin.enable = true;
      autoLogin.user = "perlinm";
      defaultSession = "none+i3";
    };
    windowManager.i3.enable = true;

    # touchpad
    libinput.enable = true;
    libinput.touchpad = {
      naturalScrolling = true;
      clickMethod = "clickfinger";
      disableWhileTyping = true;
    };
  };

  # gtk configuration tool; expected by gtk applications
  programs.dconf.enable = true;

  # # enable sway window manager
  # programs.sway.enable = true;
  # programs.sway.wrapperFeatures.gtk = true;
  # programs.xwayland.enable = true;
  # xdg.portal = sway-fixes.xdg-portal;

  # basic packages for bootstrapping
  environment.systemPackages = with pkgs; [
    git
    home-manager
  ]; # ++ sway-fixes.packages;

  # sound and bluetooth control
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  hardware.pulseaudio.enable = false;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluez;

  # change some power settings
  services.logind.lidSwitch = "ignore";
  services.logind.extraConfig = "HandlePowerKey=suspend";

  # miscellaneous utilities
  security.rtkit.enable = true; # schedule user processes/threads
  security.polkit.enable = true; # fine-grained authentication agent
  services.dbus.enable = true; # interprocess communications manager
  services.udisks2.enable = true; # automounting external drives
  services.printing.enable = true; # enable CUPS to print documents

  # make home-manager use global install paths and package configurations
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}
