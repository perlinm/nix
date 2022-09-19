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
    feh  # setting background image
    git  # version control system
    helix vim emacs  # text editors
    htop  # process viewer
    jq  # json parsing
    killall  # kill processes by name
    mosh  # better than ssh
    picom  # window compositing
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
    xdotool  # simulate keyboard/mouse inputs
    zip unzip  # zipping/unzipping
  ];
  desktop-utilities = with pkgs; [
    betterlockscreen # ... than i3lock
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
    slack  # work chat
    spotify  # music
    vistafonts dejavu_fonts  # fonts, including consolas
    vlc  # for watching videos
    # waybar  # status bar
    # workstyle  # add running applications to workspace names
    # zoom-us  # video conferencing app
    zotero  # bibliography/reference management system
  ];
in
languages ++ console-utilities ++ desktop-utilities ++ applications
