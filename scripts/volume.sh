#!/usr/bin/env sh

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

if [ $device = sink ]; then
  if [ $cmd = dec ]; then
    pactl set-sink-mute $device_tag 0
    pactl set-sink-volume $device_tag -$delta%
  elif [ $cmd = inc ]; then
    pactl set-sink-mute $device_tag 0
    pactl set-sink-volume $device_tag +$delta%
  elif [ $cmd = min ]; then
    pactl set-sink-volume $device_tag 0
  elif [ $cmd = max ]; then
    pactl set-sink-volume $device_tag 100%
  elif [ $cmd = med ]; then
    pactl set-sink-volume $device_tag $medium%
  elif [ $cmd = on ]; then
    pactl set-sink-mute $device_tag 0
  elif [ $cmd = off ]; then
    pactl set-sink-mute $device_tag 1
  fi
fi

if [ $cmd = toggle ]; then
  pactl set-$device-mute $device_tag toggle
fi

if [ $device = source ]; then
  if [ $cmd = toggle ] || [ $cmd = on ] || [ $cmd = off ]; then
    default_source=$(pactl info | grep 'Default Source' | awk '{print $3}')
    mute=$(pactl list sources | grep -A 10 $default_source | grep Mute | awk '{print $2}')
    if [ $mute = yes ]; then
      notify-send -t 1 "mic off"
    elif [ $mute = no ]; then
      notify-send -t 1 "mic on"
    else
      notify-send -t 1 "mic status unknown"
    fi
  fi
fi
