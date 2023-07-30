{ pkgs }:
let
  # install unstable packages with unstable.<PACKAGE-NAME>
  unstable = import <nixos-unstable> { config.allowUnfree = true; };
in let
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/science/math/mathematica/default.nix
  mathematica-13-2-1 = unstable.mathematica.override {
    source = pkgs.requireFile {
      name = "Mathematica_13.2.1_BNDL_LINUX.sh";
      # Get this hash via a command similar to this:
      # nix-store --query --hash $(nix store add-path Mathematica_13.2.1_BNDL_LINUX.sh --name 'Mathematica_13.2.1_BNDL_LINUX.sh')
      sha256 = "070ybhgskk3fw8c6fgqs4lq9252ds6585cqdd5as94hj55vjibmq";
      message = ''
        Your override for Mathematica includes a different src for the installer, and it is missing.
      '';
      hashMode = "recursive";
    };
  };
in let
  languages = with pkgs; [
    cargo
    gcc
    mathematica-13-2-1
    nixfmt # nix formatter
    taplo # TOML toolkit
    texlive.combined.scheme-full
    texlab
  ];
  python = import ./python.nix { inherit pkgs; };
  console-utilities = with pkgs; [
    cmake
    gnumake # build system
    git # version control system
    helix
    vim # text editors
    fzf # command-line fuzzy finder
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
    trash-cli # trash management, replacing "rm"
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
    kitty
    kitty-themes
    xfce.xfce4-terminal # terminal emulators
    blueberry # bluetooth tool
    firefox
    google-chrome # web browsers
    brightnessctl # screen brightness
    gimp # image editor
    gnome.eog # image viewer
    gparted # graphical disk partitioning
    gpick # color picker
    imagemagick # mainpulate images, e.g. with `convert`
    inkscape # vector graphics (SVG) editor
    maxima
    sage # computer algebra systems
    meld # file comparison tool
    pamixer # command-line volume control
    pavucontrol # GUI volume control
    qpdfview
    zathura
    okular # pdf viewers
    slack # work chat
    spotify # music
    vlc # for watching videos
    xfce.thunar # file browser
    wpsoffice # office suite (like Word, Excel, etc.)
    zoom-us # video conferencing app
    zotero # bibliography/reference manager
  ];
  sway-utilities = with pkgs; [
    i3 # parent to sway, incuded for 'i3-msg' command
    autotiling-rs # sane tiling defaults
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
    arandr # for display management
    autotiling # sane tiling defaults
    i3lock-fancy
    xss-lock # lock screen management
    i3-wk-switch # XMonad-like workspace switching
    feh # set background image
    lxappearance # set GTK themes
    maim # screenshots
    notify-osd-customizable # noitification daemon
    picom # window compositor
    polybarFull # info bar / panel
    xclip # CLI copy/paste tool
    xdotool # simulate keyboard/mouse input, manipulate windows
    xidlehook # lock or turn off screen when idling
    xorg.xev # event logger
    xorg.xprop # get window properties
  ];
  misc = with pkgs; [
    awscli2 # AWS command line services
    protobuf # for protoc command
  ];
in console-utilities ++ languages ++ python ++ fonts-icons-themes
++ applications ++ sway-utilities ++ i3-utilities ++ misc
