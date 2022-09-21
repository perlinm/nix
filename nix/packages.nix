{ pkgs }:
let
  languages = with pkgs; [
    cargo
    gcc
    # mathematica
    texlive.combined.scheme-full
    (import ./python.nix { inherit pkgs; })
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
  desktop-utilities = with pkgs; [
    brightnessctl  # screen brightness
    grim slurp  # for screenshots
    nwg-drawer   # application drawer
    swaybg  # set background image
    swaylock-effects swayidle  # screen locker
    swaynotificationcenter libnotify  # notification daemon
    swaytools  # get window properties with swayinfo
    waybar  # info bar / panel
    wdisplays  # display settings
    wev  # event logger
    wl-clipboard  # CLI copy/paste tool
    # wmctrl  # command-line window control (used for scratchpads)
    # workstyle  # add running applications to workspace names
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
    # workstyle  # add proprams to workspace names
    # zoom-us  # video conferencing app
    zotero  # bibliography/reference manager
  ];
in
languages ++ console-utilities ++ desktop-utilities ++ fonts-icons-themes ++ applications
