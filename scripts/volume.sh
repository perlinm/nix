#!/usr/bin/env sh

delta=5
medium=50

if [ $1 = dec ]; then
  pamixer --unmute
  pamixer --decrease $delta
elif [ $1 = inc ]; then
  pamixer --unmute
  pamixer --increase $delta
elif [ $1 = min ]; then
  pamixer --set-volume 0
elif [ $1 = max ]; then
  pamixer --set-volume 100
elif [ $1 = med ]; then
  pamixer --set-volume $medium
elif [ $1 = on ]; then
  pamixer --unmute
elif [ $1 = off ]; then
  pamixer --mute
elif [ $1 = toggle ]; then
  if [ `pamixer --get-mute` = true ]; then
    $0 on
  else
    $0 off
  fi
fi

if [ `pamixer --get-mute` = false ]; then
  volnoti-show `pamixer --get-volume`
else
  volnoti-show -m
fi
