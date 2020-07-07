#!/usr/bin/env zsh

if [ $DESKTOP_SESSION == xmonad ]; then

  # start keyring daemon
  eval $(/usr/bin/gnome-keyring-daemon --start)
  export SSH_AUTH_SOCK

  # panel and power management
  xfce4-panel &
  xfce4-power-manager &

  # window compositing
  xcompmgr &

  # mouse and keyboard
  xsetroot -cursor_name left_ptr
  xmodmap ~/.Xmodmap
  xset r 66
  xset r rate 200 60
  numlockx off &
  setxkbmap -option 'numpad:microsoft'

  # start some services
  #wicd-client --tray &
  #volnoti -t 1 &
  nm-applet &
  /usr/lib/xfce4/notifyd/xfce4-notifyd &
  emacs --daemon &
  fluxgui &

  # background
  feh --bg-fill /usr/share/backgrounds/background.jpg

fi

# add entries to path
NEW_PATH=$PATH
my_path=(/usr/bin /usr/local/bin /usr/local/sbin $HOME/bin)
for p in $my_path; do
  if ! [ $(echo $PATH | grep $p) ]; then
    NEW_PATH=$NEW_PATH:$p
  fi
done
export PATH=$NEW_PATH

# start keyring daemon
if [[ -n "$DESKTOP_SESSION" ]]; then
  eval $(gnome-keyring-daemon --start)
  export SSH_AUTH_SOCK
fi
