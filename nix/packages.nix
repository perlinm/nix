{ pkgs }:
let
  consoleUtilities = with pkgs; [
    helix vim emacs  # text editors
    htop  # process viewer
    jq  # json parsing
    mosh  # better than ssh
    ripgrep  # faster grep
    starship  # customizable shell prompt
    trashy  # trash management, replacing "rm"
    tree  # list directories
    udevil  # sudo-free mounting
    watch  # repeat a command and watch output
    wget  # retrieve files from the web
    woof onionshare onionshare-gui  # secure network file sharing
    xclip  # command-line clipboard
    zip unzip  # zipping/unzipping
  ];
  languages = with pkgs; [
    gcc
    cargo
    # (import ./python.nix { inherit pkgs; })
    texlive.combined.scheme-full
    # mathematica
  ];
  miscellaneous = with pkgs; [
    xfce.xfce4-terminal  # terminal emulator
    xfce.xfce4-panel  # status bar/panel
    vistafonts dejavu_fonts  # fonts, including consolas
    meld  # file comparison tool
    qpdfview okular foxitreader pdftk  # pdf viewers and editors
    zotero  # bibliography/reference management system
    cmake  # build system
    gparted  # graphical disk partitioning utility
    vlc  # for watching videos
    spotify  # music
    slack  # work chat
  ];
in
consoleUtilities ++ languages ++ miscellaneous
