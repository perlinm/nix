{ config, pkgs, ... }:

let sway-fixes = import ./sway-fixes.nix { inherit pkgs; };
in {
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  # system.autoUpgrade.enable = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # allow unfree software, which may be necessary for some drivers
  nixpkgs.config.allowUnfree = true;

  imports = [
    ./hardware-configuration.nix # results of hardware scan
    <home-manager/nixos>
  ];

  # bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # ...because the boot partition on this laptop is too small...
  boot.loader.systemd-boot.configurationLimit = 2;

  # internationalisation properties
  # WARNING: these are ignored by some desktop environments (e.g. GNOME)
  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.utf8";
  i18n.extraLocaleSettings.LC_TIME = "en_GB.utf8";

  # networking options
  networking.hostName = "map-work";
  networking.networkmanager.enable = true;

  # keyboard layout in the console
  console.keyMap = "colemak";

  users.users.perlinm = {
    isNormalUser = true;
    description = "Michael A. Perlin";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
    shell = pkgs.zsh;
  };

  # X11 services
  services.xserver = {
    enable = true;

    # keyboard layout
    layout = "us";
    xkbVariant = "colemak";
    autoRepeatDelay = 200;
    autoRepeatInterval = 60;

    # display (login), desktop, and window managers
    displayManager.gdm.enable = true;
    displayManager.gdm.wayland = true;

    # touchpad
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;
  };

  # enable sway window manager
  programs.sway.enable = true;
  programs.xwayland.enable = true;
  programs.sway.wrapperFeatures.gtk = true;
  xdg.portal = sway-fixes.xdg-portal;

  environment.systemPackages = [
    pkgs.home-manager
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
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
}
