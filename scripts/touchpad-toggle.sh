#!/usr/bin/env sh

idle_time=0.5 # seconds

state=`synclient -l | grep TouchpadOff | sed 's/^.*= //'`
if [ $state -eq 1 ]
then
  notify-send -t 1 "touchpad on"
  synclient TouchpadOff=0
  syndaemon -d -K -i $idle_time
else
  notify-send -t 1 "touchpad off"
  synclient TouchpadOff=1
  killall syndaemon 2>/dev/null
fi
