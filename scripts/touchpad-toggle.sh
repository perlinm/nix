#!/usr/bin/env sh

device="ELAN1300:00 04F3:3057 Touchpad"

state=`xinput list-props "$device" | grep "Device Enabled" | awk '{print $4}'`
if [ $state -eq 1 ]
then
  notify-send -t 1 "touchpad off"
  xinput --disable "$device"
else
  notify-send -t 1 "touchpad on"
  xinput --enable "$device"
fi
