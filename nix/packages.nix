{ pkgs }:
let
  consoleUtilities = with pkgs; [
    helix vim emacs  # text editors
    htop  # process viewer
    mosh  # better than ssh
    ripgrep  # faster grep
    trashy  # trash management, replacing "rm"
    tree  # list directories
    udevil  # sudo-free mounting
    watch  # repeat a command and watch output
    wget  # retrieve files from the web
    xclip  # command-line clipboard
    zip unzip  # zipping/unzipping
  ];
  languages = with pkgs; [
    gcc
    cargo
    (import ./python.nix { inherit pkgs; })
    texlive.combined.scheme-full
  ];
  miscellaneous = with pkgs; [
    xfce.xfce4-terminal  # terminal emulator
    vistafonts dejavu_fonts  # fonts, including consolas
    meld  # file comparison tool
    qpdfview okular foxitreader pdftk  # pdf viewers and editors
    zotero  # bibliography/reference management system
    cmake  # build system
    vlc  # watching videos
  ];
in
consoleUtilities ++ languages ++ miscellaneous
