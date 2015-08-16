#!/usr/bin/env sh

# add both free and nonfree package repositories
sudo dnf install --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
sudo dnf install --nogpgcheck http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

# update and upgrade all packages
sudo dnf update
sudo dnf upgrade

# system entropy daemon
sudo dnf install haveged
sudo systemctl enable haveged

# keyboard configuration
sudo localectl set-x11-keymap us,us pc105 colemak, shift:both_capslock,grp:alts_toggle
sudo kbdrate -d 200 -r 60

# install and setup zsh
sudo dnf install zsh zsh-lovers
chsh -s /bin/zsh perlinm

# basic console utilities
sudo dnf install vim htop pmount keychain git-all openssh acpi wget

# c/c++ utilities
sudo dnf install gcc gcc-c++ valgrind

# python packages
sudo dnf install python python3 numpy python3-numpy scipy python3-scipy sympy python3-sympy python-matplotlib python3-matplotlib python-ipython python3-ipython pypy pypy3

# haskell packages
sudo dnf install haskell-platform

# texlive
sudo dnf install texlive-scheme-full

# emacs
sudo dnf install emacs emacs-goodies emacs-auctex emacs-pymacs emacs-haskell-mode emacs-auto-complete

# xmonad
sudo dnf install xmonad xmonad-config

# window compositor for xmonad
sudo dnf install xcompmgr

# X utilities
sudo dnf install xdotool xclip xbacklight pdftk

# basic graphic utilities
sudo dnf install gparted meld pavucontrol arandr lxappearance

# web browser and java plugin
sudo dnf install firefox icedtea-web
# flash plugin?

# google chrome
sudo cat << EOF > /etc/yum.repos.d/google-chrome.repo
[google-chrome]
name=google-chrome - \$basearch
baseurl=http://dl.google.com/linux/chrome/rpm/stable/\$basearch
enabled=1
gpgcheck=1
gpgkey=https://dl-ssl.google.com/linux/linux_signing_key.pub
EOF
sudo dnf update
sudo dnf install google-chrome-stable
# google talk plugin?

# image utilities
sudo dnf install feh geeqie scrot gimp inkscape

# video players
sudo dnf install mplayer smplayer vlc vlc-extras

# camera settings
sudo dnf install v4l2ucp

# xfce and kde utilities
sudo dnf install @xfce @kde

# torrent clients
sudo dnf install delude qbittorrent

# office suite, etc. 
sudo dnf install libreoffice evince

# dictionary
sudo dnf install aspell-en aspell-de

# skype
cd /tmp
skype='skype-4.3.0.37-3.fc22.i686.rpm'
skype_data='skype-data-4.3.0.37-3.fc22.noarch.rpm'
wget "http://negativo17.org/repos/skype/fedora-22/x86_64/$skype"
wget "http://negativo17.org/repos/skype/fedora-22/x86_64/$skype_data"
sudo dnf install $skype $skype_data
# make skype appear in kde5 system tray
sudo dnf install sni-qt.i686

# windows "emulator"
sudo dnf install wine

# fac dependencies
sudo dnf install popt popt-static python-markdown python3-markdown rubygem-sass

# deft dependencies
sudo dnf install scons fftw fftw-static gnuplot ghc-HUnit ghc-HUnit-devel

# make and enable a swapfile
sudo fallocate -l 8GB /swapfile
sudo chmod 0600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
sudo echo "/swapfile none swap defaults 0 0" >> /etc/fstab

# set hostname
sudo hostnamectl set-hostname --static MAPHost.MAPDomain
sudo hostnamectl set-hostname --pretty MAPHost.MAPDomain
sudo hostnamectl set-hostname --transient MAPHost.MAPDomain

# todo: xfce panel, pdftk, deft
