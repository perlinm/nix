#!/usr/bin/sh

# add free and nonfree repositories
su -c 'yum localinstall --nogpgcheck \
       http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
       http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'

# add google-chrome repository
su -c 'sudo cat << EOF > /etc/yum.repos.d/google-chrome.repo
[google-chrome]
name=google-chrome - \$basearch
baseurl=http://dl.google.com/linux/chrome/rpm/stable/\$basearch
enabled=1
gpgcheck=1
gpgkey=https://dl-ssl.google.com/linux/linux_signing_key.pub
EOF'

# upgrade the system
sudo yum upgrade

# install stuff for self
sudo yum install \
  zsh vim-enhanced pmount git-all keychain \
  wicd wicd-curses wicd-kde \
  vpnc openssh \
  google-chrome-stable firefox \
  texlive-scheme-full \
  haskell-platform \
  emacs emacs-goodies emacs-auctex emacs-haskell-mode emacs-auto-complete \
  python python3 \
  numpy python3-numpy \
  python-matplotlib python3-matplotlib \
  scipy python3-scipy \
  @xfce @kde-desktop \
  ghc-xmonad ghc-xmonad-contrib \
  mplayer smplayer vlc vlc-extras \
  feh gimp inkscape geeqie lxappearance

# install xfce panel plugins
sudo yum install \
  xfce4-battery-plugin \
  xfce4-cpugraph-plugin \
  xfce4-mount-plugin \
  xfce4-netload-plugin \
  xfce4-systemload-plugin \
  xfce4-weather-plugin \
  xfce4-xkb-plugin \
  xmonad-log-applet-xfce

# install stuff for deft
sudo yum install \
  gcc gcc-c++ scons \
  fftw fftw-static \
  popt popt-static \
  gnuplot \
  python-markdown python3-markdown

# change user shell
sudo chsh -s /usr/bin/zsh perlinm

# switch wireless interface controllers
sudo systemctl stop NetworkManager
sudo systemctl disable NetworkManager
sudo systemctl stop wpa_supplicant
sudo systemctl disable wpa_supplicant
sudo systemctl enable wicd
sudo systemctl start wicd

# install skype
cd /tmp
wget --no-check-certificate https://raw.githubusercontent.com/mikaperlin/scripts-configs-etc/master/scripts/install-skype.sh
sudo sh install-skype.sh

