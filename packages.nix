{ pkgs }:
let
  languages = with pkgs; [
    cargo
    gcc
    # mathematica
    texlive.combined.scheme-full
  ];
  python = [(import ./python.nix { inherit pkgs; })];
  console-utilities = with pkgs; [
    cmake  # build system
    git  # version control system
    helix vim emacs  # text editors
    htop  # process viewer
    jq  # json parsing
    killall  # kill processes by name
    mosh  # better than ssh
    pdftk  # pdf editor
    ripgrep  # faster grep
    starship  # customizable shell prompt
    trashy  # trash management, replacing "rm"
    tree  # list directories
    udevil  # sudo-free mounting
    udiskie  # automounting removable media
    watch  # repeat a command and watch output
    wget  # retrieve files from the web
    woof # secure network file sharing
    zip unzip  # zipping/unzipping
  ];
  fonts-icons-themes = with pkgs; [
    dracula-theme
    nerdfonts
    nerd-font-patcher
    papirus-icon-theme
    vistafonts  # provides consolas
  ];
  applications = with pkgs; [
    blueberry  # bluetooth tool
    gnome.eog  # image viewer
    gpick  # color picker
    inkscape  # vector graphics (SVG) editor
    meld  # file comparison tool
    qpdfview zathura okular foxitreader  # pdf viewers
    slack  # work chat
    vlc  # for watching videos
    xfce.thunar  # file browser
    zotero  # bibliography/reference manager
  ];
  nixos-applications = with pkgs; [
    alacritty xfce.xfce4-terminal  # terminal emulators
    pamixer  # command-line volume control
    pavucontrol  # GUI volume control
    spotify  # music
    zoom-us  # video conferencing app
  ];
  sway-utilities = with pkgs; [
    autotiling-rs  # sane tiling defaults
    brightnessctl  # screen brightness
    firefox-wayland  # web browser
    grim slurp  # for screenshots
    swaybg  # set background image
    swaylock-effects  # screen locker
    swayidle  # lock or turn off screen when idling
    swaynotificationcenter libnotify  # notification daemon
    swaytools  # get window properties with swayinfo
    waybar  # info bar / panel
    wdisplays  # display settings
    wev  # event logger
    wl-clipboard  # CLI copy/paste tool
  ];
  i3-utilities = with pkgs; [
    autotiling  # sane tiling defaults
    firefox  # web browser
    feh  # set background image
    gnome.gnome-control-center  # provides display settings
    lxappearance  # set GTK themes
    maim  # screenshots
    notify-osd-customizable  # noitification daemon
    picom  # window compositor
    polybar  # info bar / panel
    xclip  # CLI copy/paste tool
    xdotool  # simulate keyboard/mouse input, manipulate windows
    xidlehook  # lock or turn off screen when idling
    xorg.xbacklight  # screen brightness
    xorg.xev  # event logger
    xorg.xprop  # get window properties
    xss-lock  # lock screen manager
  ];
in
{
  home = console-utilities ++ languages ++ fonts-icons-themes ++ applications ++ sway-utilities ++ nixos-applications ++ python;
  work = console-utilities ++ languages ++ fonts-icons-themes ++ applications;
}
