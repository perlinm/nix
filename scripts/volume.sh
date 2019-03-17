#!/usr/bin/env sh

delta=5
medium=40

device=sink
device_num=0 # this should be the starred index in $(pacmd list-sinks | grep index)
cmd=$1

if [ $1 = mic ]; then
  device=source
  device_num=1 # this should be the starred index in $(pacmd list-sources | grep index)
  cmd=$2
fi

if [ $device = sink ]; then
  if [ $cmd = dec ]; then
    pactl set-$device-mute $device_num 0
    pactl set-$device-volume $device_num -$delta%
  elif [ $cmd = inc ]; then
    pactl set-$device-mute $device_num 0
    pactl set-$device-volume $device_num +$delta%
  elif [ $cmd = min ]; then
    pactl set-$device-volume $device_num 0
  elif [ $cmd = max ]; then
    pactl set-$device-volume $device_num 100%
  elif [ $cmd = med ]; then
    pactl set-$device-volume $device_num $medium%
  elif [ $cmd = on ]; then
    pactl set-$device-mute $device_num 0
  elif [ $cmd = off ]; then
    pactl set-$device-mute $device_num 1
  fi
fi

if [ $cmd = toggle ]; then
  pactl set-$device-mute $device_num toggle
fi

if [ $device = source ]; then
  if [ $cmd = toggle ] || [ $cmd = on ] || [ $cmd = off ]; then
    mute=`pactl list sources | grep Mute | tail -n 1 | awk '{print $2}'`
    if [ $mute = yes ]; then
      notify-send -t 1 "mic off"
    elif [ $mute = no ]; then
      notify-send -t 1 "mic on"
    else
      notify-send -t 1 "mic status unknown"
    fi
  fi
fi
