#!/usr/bin/sh

# only allow running this script on Fedora
if [ -z "$(cat /etc/issue | grep Fedora)" ]; then
  echo "script intended to install Skype on Fedora only"
  exit 0
fi

# only allow running this script as the root user
if [ "$EUID" -ne 0 ]; then
  echo "please run as root"
  exit 0
fi

# install dependencies
yum install alsa-lib.i686 fontconfig.i686 freetype.i686 \
  glib2.i686 libSM.i686 libXScrnSaver.i686 libXi.i686 \
  libXrandr.i686 libXrender.i686 libXv.i686 libstdc++.i686 \
  pulseaudio-libs.i686 qt.i686 qt-x11.i686 zlib.i686 qtwebkit.i686

# download the latest version of skype and extract
cd /tmp
wget --trust-server-names http://www.skype.com/go/getskype-linux-dynamic
tar xvf skype-*.tar* -C /opt/skype --strip-components=1

# create skype links
ln -sf /opt/skype/skype.desktop /usr/share/applications/skype.desktop
ln -sf /opt/skype/icons/SkypeBlue_48x48.png /usr/share/icons/skype.png
ln -sf /opt/skype/icons/SkypeBlue_48x48.png /usr/share/pixmaps/skype.png

# create skype launcher
touch /usr/bin/skype
chmod 755 /usr/bin/skype
cat << EOF > /usr/bin/skype
#!/bin/sh
export SKYPE_HOME="/opt/skype"

\$SKYPE_HOME/skype --resources=\$SKYPE_HOME \$*
EOF
