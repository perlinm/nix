{ pkgs, ... }:
let
  python = import ./python.nix { inherit pkgs; };

  rust = with pkgs; [
    cargo # build system
    rust-analyzer # LSP
    rustfmt # formatter
    clippy # linter
  ];

  languages = with pkgs;
    [
      gcc
      julia-bin
      nil # nix LSP
      nixfmt # nix formatter
      taplo # TOML formatter
      texlive.combined.scheme-full
      texlab
    ] ++ python ++ rust;

  console-utilities = with pkgs; [
    bat # better 'cat': cat with wings
    choose # better awk '{print $...}'
    cmake
    gnumake # build system
    du-dust # better 'du'
    git # version control system
    external.helix
    vim # text editors
    fd # better 'find'
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
    pdf2svg # convert 'pdf' to 'svg'
    poppler_utils # convert 'pdf' to 'png' with 'pdftoppm -png input.pdf output'
    pulseaudio # provides pactl for audio control
    ripgrep # faster grep
    stable.ripgrep-all # faster grep, now also for pdf, docx, etc. files
    sd # better 'sed'
    starship # customizable shell prompt
    external.trashy # trash management, replacing "rm"
    tree # list directories
    udevil # sudo-free mounting
    udiskie # automounting removable media
    watch # repeat a command and watch output
    wget # retrieve files from the web
    woof # secure network file sharing
    # xpdf # pdf manipulation
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
    chromium
    firefox # web browsers
    brightnessctl # screen brightness
    gimp # image editor
    gnome.eog # image viewer
    gparted # graphical disk partitioning
    gpick # color picker
    imagemagick # mainpulate images, e.g. with 'convert'
    inkscape # vector graphics (SVG) editor
    kmag # color blindness filter/simulator
    maxima
    sage # computer algebra systems
    meld # file comparison tool
    pamixer # command-line volume control
    pavucontrol # GUI volume control
    qpdfview
    zathura
    okular # pdf viewers
    rofi # application launcher
    unfree.slack # work chat
    unfree.spotify # music
    vlc # for watching videos
    xfce.thunar # file browser
    unfree.wpsoffice # office suite (like Word, Excel, etc.)
    unfree.zoom-us # video conferencing app
    zotero # bibliography/reference manager
  ];

  sway-utilities = with pkgs; [
    i3 # parent to sway, incuded for 'i3-msg' command
    autotiling-rs # sane tiling defaults
    grim
    slurp # for screenshots
    networkmanagerapplet # apparently needed for nm-applet
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
    i3lock-fancy-rapid # lock screen management
    i3-wk-switch # XMonad-like workspace switching
    feh # set background image
    lxappearance # set GTK themes
    maim
    scrot # screenshots
    notify-osd-customizable # noitification daemon
    picom # window compositor
    polybarFull # info bar / panel
    wmctrl # CLI to interact with windows; needed for i3-scratchpad
    xclip # CLI copy/paste tool
    xdotool # simulate keyboard/mouse input, manipulate windows
    xidlehook # lock or turn off screen when idling
    xorg.xev # event logger
    xorg.xkill # kill applications with the mouse
    xorg.xprop # get window properties
    xss-lock # idle screen manager
  ];

  misc-work = with pkgs; [
    awscli2 # AWS command line services
    lynx # text-based browser
    protobuf # for protoc command
  ];

  mathematica = pkgs.unfree.mathematica.override { version = "13.2.1"; };

  misc-other = with pkgs; [
    mathematica
    external.simple-completion-language-server
  ];

in {
  home.packages = console-utilities ++ languages ++ fonts-icons-themes
    ++ applications ++ sway-utilities ++ i3-utilities ++ misc-work
    ++ misc-other;

  # override refusal to install zotero...
  # https://github.com/NixOS/nixpkgs/commit/9438baa49d527dd7f748e90bdfea576cd1daa0db
  nixpkgs.config.permittedInsecurePackages = [ "zotero-6.0.27" ];
}
