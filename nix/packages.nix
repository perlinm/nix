{ pkgs, ... }:

let
  basePackages = with pkgs; [
    htop  # process viewer
    ripgrep  # faster grep
    trashy  # trash management, replacing "rm"
    udevil  # sudo-free mounting
    wget  # retrieve files from the web
    xclip  # command-line clipboard
    zip unzip  # zipping/unzipping

    # text editors
    helix
    vim
    emacs

    # languages
    gcc
    cargo
    python3
    texlive.combined.scheme-full

    # pdf viewers and editors
    qpdfview
    okular
    foxitreader
    pdftk

    xfce.xfce4-terminal  # terminal emulator
    vistafonts  # adds consolas font
    dejavu_fonts  # variety of fonts
    meld  # file comparison tool
    zotero  # bibliography/reference management system
    cmake  # build system
    vlc  # for watching videos
    # shutter  # screenshots
  ];
in
basePackages