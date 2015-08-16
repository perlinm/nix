# get sudo
su -c 'apt-get install sudo'
su -c 'adduser perlinm sudo'

# get aptitude
sudo apt-get install aptitude

# get zsh
sudo apitude install zsh
chsh -s /bin/zsh perlinm

# set package sources
sudo cat << EOF > /etc/apt/sources.list
deb http://ftp.us.debian.org/debian unstable main contrib non-free
deb-src http://ftp.us.debian.org/debian unstable main contrib non-free
EOF

# add google chrome, earth, and talk plugin repos
cd /tmp
wget -q https://dl.google.com/linux/linux_signing_key.pub
sudo apt-key add linux_signing_key.pub
sudo cat << EOF > /etc/apt/sources.list.d/google-chrome.list
deb http://dl.google.com/linux/chrome/deb/ stable main
EOF
sudo cat << EOF > /etc/apt/sources.list.d/google-talkplugin.list
deb http://dl.google.com/linux/talkplugin/deb/ stable main
EOF

# update system
sudo aptitude update
sudo aptitude upgrade

# system entropy daemon
sudo aptitude install haveged
sudo systemctl enable haveged

# keyboard configuration
sudo localectl set-x11-keymap us,us pc105 colemak, shift:both_capslock,caps:backspace
sudo kbdrate -d 200 -r 60

# basic console utilities
sudo aptitude install vim htop pmount keychain git-all openssh acpi wget mosh autotools-dev

# c/c++ utilities
sudo aptitude install gcc g++ valgrind

# python packages
sudo aptitude install python python3 python-numpy python3-numpy python-scipy python3-scipy python-sympy python3-sympy python-matplotlib python3-matplotlib

# haskell packages
sudo aptitude install haskell-platform

# texlive
sudo aptitude install texlive-full

# emacs
sudo aptitude install emacs-goodies-el emacs-goodies-extra-el auctex pymacs haskell-mode auto-complete-el

# xmonad
sudo aptitude install xmonad

# window compositor for xmonad
sudo aptitude install xcompmgr

# X utilities
sudo aptitude install xdotool xclip xbacklight pdftk

# basic graphic utilities
sudo aptitude install gmrun gparted meld pavucontrol arandr lxappearance

# web browser and plugins
sudo aptitude install firefox icedtea-plugin flashplugin-nonfree

# google packages
sudo aptitude install google-chrome-stable google-talkplugin

# image utilities
sudo aptitude install feh geeqie scrot gimp inkscape

# video players
sudo aptitude install mplayer smplayer vlc

# camera settings
sudo aptitude install v4l2ucp

# xfce and kde utilities
sudo aptitude install kde-full xfce4 xfce4-goodies

# torrent clients
sudo aptitude install deluge qbittorrent

# office suite, etc.
sudo aptitude install libreoffice evince

# skype
sudo dpkg --add-architecture i386
sudo aptitude update
cd /tmp
wget -O skype-install.deb http://www.skype.com/go/getskype-linux-deb
sudo dpkg -i skype-install.deb
sudo apt-get -f install

# windows "emulator"
sudo aptitude install wine

# make and enable a swap file
sudo fallocate -l 8GB /swapfile
sudo chmod 0600 /swapfile 
sudo mkswap /swapfile
sudo swapon /swapfile
sudo echo "/swapfile none swap defaults 0 0" >> /etc/fstab

# set hostname
sudo hostnamectl set-hostname --static maphost
sudo hostnamectl set-hostname --pretty maphost
sudo hostnamectl set-hostname --transient maphost

# themes and icons
cd /tmp
wget 'https://bitbucket.org/2ion/faba-icon-theme/downloads/faba-icon-theme_201409-1-2_all.deb'
wget 'https://bitbucket.org/2ion/moka-icon-theme/downloads/moka-icon-theme_201409-1-2_amd64.deb'
sudo dpkg -i faba-icon-theme_201409-1-2_all.deb moka-icon-theme_201409-1-2_amd64.deb
git clone https://github.com/moka-project/orchis-gtk-theme.git
cd orchis-gtk-theme
./render-gtk-assets.py
./render-metacity-assets.py
./render-unity-assets.py
tar xvf orchis-gtk-theme-3.0.tar.gz
cp -r Orchis Orchis-Dark/ orchis-gtk-theme-3.0/
sudo cp -r orchis-gtk-theme-3.0/Orchis* /usr/share/themes
sudo aptitude install gtk2-engines-murrine

# login manager
sudo aptitude install slim
sudo sed -e 's/#default_user.*/default_user perlinm/' -e 's/#focus_password.*/focus_password yes/' -e 's/#auto_login.*/auto_login yes/' -e 's/login_cmd.*/login_cmd exec \/bin\/bash -login ~\/\.xinitrc/' /etc/slim.conf > /etc/.slim.conf
sudo mv /etc/.slim.conf /etc/slim.conf

# fac dependencies
sudo aptitude install libpopt-dev python-markdown python3-markdown sass

# deft dependencies
sudo aptitude install scons haskell-mode gnuplot emacs-goodies-el fftw3-dev

# todo: deft fixes
