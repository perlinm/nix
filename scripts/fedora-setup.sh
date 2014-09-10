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
  zsh vim-enhanced \
  @xfce @kde-desktop xcompmgr \
  xmonad ghc-xmonad ghc-xmonad-contrib \
  htop git-all pmount keychain \
  wicd wicd-curses wicd-kde \
  google-chrome-stable firefox \
  texlive-scheme-full \
  haskell-platform \
  emacs emacs-goodies emacs-auctex emacs-haskell-mode emacs-auto-complete \
  python python3 \
  numpy python3-numpy \
  scipy python3-scipy \
  sympy python3-sympy \
  python-matplotlib python3-matplotlib \
  python-ipython python3-ipython \
  feh geeqie gimp \
  mplayer smplayer vlc vlc-extras \
  pavucontrol xdotool \
  lxappearance xbacklight \
  icedtea-web vpnc openssh \
  qbittorrent

# install xfce panel plugins
sudo yum install \
  xfce4-battery-plugin \
  xfce4-cpugraph-plugin \
  xfce4-mount-plugin \
  xfce4-netload-plugin \
  xfce4-systemload-plugin \
  xfce4-weather-plugin \
  xfce4-xkb-plugin \
  xmonad-log-applet-xfce \
  volumeicon

# install stuff for deft
sudo yum install \
  gcc gcc-c++ scons \
  fftw fftw-static \
  popt popt-static \
  gnuplot \
  python-markdown python3-markdown

# change user shell
sudo chsh -s /usr/bin/zsh perlinm

# fix wicd-curses
# WARNING: only known to be valid for fedora 20 with wicd 1.7.2.4
wget --no-check-certificate https://raw.githubusercontent.com/mikaperlin/scripts-configs-etc/master/scripts/.wicd_curses_misc.py
sudo mv .wicd_curses_fix.py /usr/share/wicd/curses/curses_misc.py

# install skype
cd /tmp
wget --no-check-certificate https://raw.githubusercontent.com/mikaperlin/scripts-configs-etc/master/scripts/install-skype.sh
sudo sh install-skype.sh

# swap network interface controllers
sudo systemctl stop NetworkManager
sudo systemctl stop wpa_supplicant
sudo systemctl disable NetworkManager
sudo systemctl disable wpa_supplicant
sudo systemctl enable wicd
sudo systemctl start wicd
