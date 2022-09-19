{ pkgs }:
let
  core-utilities = with pkgs; [
    cmake  # build system
    gnome.gnome-keyring  # secret/certificate manager
    helix vim emacs  # text editors
    htop  # process viewer
    jq  # json parsing
    killall  # kill processes by name
    mosh  # better than ssh
    ripgrep  # faster grep
    scrot  # screenshots
    starship  # customizable shell prompt
    trashy  # trash management, replacing "rm"
    tree  # list directories
    udevil  # sudo-free mounting
    watch  # repeat a command and watch output
    wget  # retrieve files from the web
    wmctrl  # command-line window control (used for scratchpads)
    woof # secure network file sharing
    xclip  # clipboard
    zip unzip  # zipping/unzipping
  ];
  languages = with pkgs; [
    cargo
    gcc
    # mathematica
    texlive.combined.scheme-full
    (import ./python.nix { inherit pkgs; })
  ];
  applications = with pkgs; [
    blueberry  # bluetooth tool
    firefox  # web browser
    gparted  # graphical disk partitioning utility
    gpick  # color picker
    inkscape  # vector graphics (SVG) editor
    lxappearance  # window themes
    meld  # file comparison tool
    pavucontrol  # volume control
    qpdfview okular foxitreader pdftk  # pdf viewers and editors
    rofi  # application launcher
    slack  # work chat
    spotify  # music
    vistafonts dejavu_fonts  # fonts, including consolas
    vlc  # for watching videos
    # zoom-us  # video conferencing app
    zotero  # bibliography/reference management system
  ];
in
core-utilities ++ languages ++ applications
