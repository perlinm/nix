#!/usr/bin/env zsh

if [ $DESKTOP_SESSION == xmonad ] || [ $DESKTOP_SESSION == bspwm ]; then

  # panel
  xfce4-panel &

  # window compositing
  xcompmgr &

  # mouse and keyboard
  xsetroot -cursor_name left_ptr
  xmodmap ~/.Xmodmap
  xset r 66
  xset r rate 200 60
  #syndaemon -R -k -i 0.5 &

  # start some services
  nm-applet &
  #wicd-client -t &
  volnoti -t 1 &
  emacs --daemon &

  # background
  feh --bg-fill /usr/share/backgrounds/background.jpg

fi

if [ $DESKTOP_SESSION == bspwm ]; then

  # hotkey daemon for bspwm
  sxhkd &

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
