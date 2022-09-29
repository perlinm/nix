{ pkgs }:
let
  languages = with pkgs; [
    cargo
    gcc
    # mathematica
    texlive.combined.scheme-full
    # (import ./python.nix { inherit pkgs; })
  ];
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
    watch  # repeat a command and watch output
    wget  # retrieve files from the web
    woof # secure network file sharing
    zip unzip  # zipping/unzipping
  ];
  sway-utilities = with pkgs; [
    autotiling-rs  # sane tiling defaults
    brightnessctl  # screen brightness
    grim slurp  # for screenshots
    swaybg  # set background image
    # swaylock-effects  # screen locker
    swayidle  # lock or turn off screen when idling
    # swaynotificationcenter libnotify  # notification daemon
    swaytools  # get window properties with swayinfo
    waybar  # info bar / panel
    wdisplays  # display settings
    wev  # event logger
    wl-clipboard  # CLI copy/paste tool
  ];
  i3-utilities = with pkgs; [
    autotiling  # sane tiling defaults
    picom  # window compositor
    maim  # screenshots
    feh  # set background image
    xclip  # CLI copy/paste tool
    xidlehook  # lock or turn off screen when idling
    xorg.xbacklight  # screen brightness
    xorg.xev  # event logger
    xorg.xprop  # get window properties
    xss-lock  # lock screen manager
  ];
  fonts-icons-themes = with pkgs; [
    dracula-theme
    nerdfonts
    nerd-font-patcher
    papirus-icon-theme
    vistafonts  # provides consolas
  ];
  applications = with pkgs; [
    alacritty  # terminal emulator
    blueberry  # bluetooth tool
    gnome.eog  # image viewer
    firefox-wayland  # web browser
    gpick  # color picker
    inkscape  # vector graphics (SVG) editor
    meld  # file comparison tool
    pavucontrol  # volume control
    qpdfview okular foxitreader  # pdf viewers
    slack  # work chat
    spotify  # music
    vlc  # for watching videos
    xfce.thunar  # file browser
    # zoom-us  # video conferencing app
    zotero  # bibliography/reference manager
  ];
in
languages ++ console-utilities ++ sway-utilities ++ i3-utilities ++ fonts-icons-themes ++ applications
