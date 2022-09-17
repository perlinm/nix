{ pkgs }:
let
  core-utilities = with pkgs; [
    cmake  # build system
    helix vim emacs  # text editors
    htop  # process viewer
    jq  # json parsing
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
    gcc
    cargo
    (import ./python.nix { inherit pkgs; })
    texlive.combined.scheme-full
    # mathematica
  ];
  applications = with pkgs; [
    firefox  # web browser
    rofi  # application launcher
    vistafonts dejavu_fonts  # fonts, including consolas
    meld  # file comparison tool
    qpdfview okular foxitreader pdftk  # pdf viewers and editors
    zotero  # bibliography/reference management system
    inkscape  # vector graphics (SVG) editor
    gparted  # graphical disk partitioning utility
    vlc  # for watching videos
    spotify  # music
    slack  # work chat
    # zoom-us  # video conferencing app
  ];
in
core-utilities ++ languages ++ applications
