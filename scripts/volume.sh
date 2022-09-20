#!/usr/bin/env sh

if [ "$#" -lt 1 ]; then
  exit 1
fi

delta=5
medium=40

device=sink
device_tag=@DEFAULT_SINK@
cmd=$1

if [ $1 = mic ]; then
  device=source
  device_tag=@DEFAULT_SOURCE@
  cmd=$2
fi

if [ $cmd = inc ]; then
  pactl set-$device-volume $device_tag +$delta%
elif [ $cmd = dec ]; then
  pactl set-$device-volume $device_tag -$delta%
elif [ $cmd = max ]; then
  pactl set-$device-volume $device_tag 100%
elif [ $cmd = min ]; then
  pactl set-$device-volume $device_tag 0
elif [ $cmd = med ]; then
  pactl set-$device-volume $device_tag $medium%
elif [ $cmd = toggle ]; then
  pactl set-$device-mute $device_tag toggle
elif [ $cmd = on ]; then
  pactl set-$device-mute $device_tag 0
elif [ $cmd = off ]; then
  pactl set-$device-mute $device_tag 1
fi

if [ $device = source ]; then
  if [ $cmd = toggle ] || [ $cmd = on ] || [ $cmd = off ]; then
    mute=$(pactl get-source-mute @DEFAULT_SOURCE@ | awk '{print $2}')
    if [ $mute = yes ]; then
      notify-send -t 1000 "mic off"
    elif [ $mute = no ]; then
      notify-send -t 1000 "mic on"
    else
      notify-send -t 1000 "mic status unknown"
    fi
  fi
fi
