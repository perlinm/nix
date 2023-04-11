{ pkgs }:
let
  # install unstable packages with unstable.<PACKAGE-NAME>
  unstable = import <nixos-unstable> { config.allowUnfree = true; };
in let
  languages = with pkgs; [
    cargo
    gcc
    unstable.mathematica
    nixfmt # nix formatter
    texlive.combined.scheme-full
  ];
  python = import ./python.nix { inherit pkgs; };
  console-utilities = with pkgs; [
    cmake
    gnumake # build system
    git # version control system
    unstable.helix
    vim
    emacs # text editors
    htop # process viewer
    ispell # spell checker
    jq # json parsing
    killall # kill processes by name
    mosh # better than ssh
    pciutils
    dmidecode # inspect hardware devices
    pandoc # converter between markup formats
    pdftk # pdf editor
    pdf2svg # convert 'pdf's to 'svg's
    pulseaudio # provides pactl for audio control
    ripgrep # faster grep
    ripgrep-all # faster grep, now also for pdf, docx, etc. files
    starship # customizable shell prompt
    trashy # trash management, replacing "rm"
    tree # list directories
    udevil # sudo-free mounting
    udiskie # automounting removable media
    watch # repeat a command and watch output
    wget # retrieve files from the web
    woof # secure network file sharing
    zip
    unzip # zipping/unzipping
  ];
  fonts-icons-themes = with pkgs; [
    dracula-theme
    nerdfonts
    nerd-font-patcher
    noto-fonts
    noto-fonts-emoji
    noto-fonts-extra
    papirus-icon-theme
  ];
  applications = with pkgs; [
    alacritty
    xfce.xfce4-terminal # terminal emulators
    blueberry # bluetooth tool
    firefox
    google-chrome # web browsers
    gimp # image editor
    gnome.eog # image viewer
    gparted # graphical disk partitioning
    gpick # color picker
    inkscape # vector graphics (SVG) editor
    maxima
    sage # computer algebra systems
    meld # file comparison tool
    networkmanagerapplet # for 'nm-applet'; NOT NECESSARY IN FUTURE VERSIONS OF NIXOS
    pamixer # command-line volume control
    pavucontrol # GUI volume control
    qpdfview
    zathura
    okular # pdf viewers
    slack # work chat
    spotify # music
    vlc # for watching videos
    xfce.thunar # file browser
    zoom-us # video conferencing app
    zotero # bibliography/reference manager
  ];
  sway-utilities = with pkgs; [
    i3 # parent to sway, incuded for 'i3-msg' command
    autotiling-rs # sane tiling defaults
    brightnessctl # screen brightness
    grim
    slurp # for screenshots
    swaybg # set background image
    swaylock-effects # screen locker
    swayidle # lock or turn off screen when idling
    swaynotificationcenter
    libnotify # notification daemon
    swaytools # get window properties with swayinfo
    waybar # info bar / panel
    wdisplays # display settings
    wev # event logger
    wl-clipboard # CLI copy/paste tool
  ];
  i3-utilities = with pkgs; [
    autotiling # sane tiling defaults
    feh # set background image
    gnome.gnome-control-center # provides display settings
    lxappearance # set GTK themes
    maim # screenshots
    notify-osd-customizable # noitification daemon
    picom # window compositor
    polybar # info bar / panel
    xclip # CLI copy/paste tool
    xdotool # simulate keyboard/mouse input, manipulate windows
    xidlehook # lock or turn off screen when idling
    xorg.xbacklight # screen brightness
    xorg.xev # event logger
    xorg.xprop # get window properties
    xss-lock # lock screen manager
  ];
  misc = with pkgs;
    [
      awscli2 # AWS command line services
    ];
in console-utilities ++ languages ++ python ++ fonts-icons-themes
++ applications ++ sway-utilities ++ misc
