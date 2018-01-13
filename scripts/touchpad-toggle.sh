#!/usr/bin/env sh

device="Synaptics TM3072-002"

state=`xinput list-props "$device" | grep "Device Enabled" | awk '{print $4}'`
if [ $state -eq 1 ]
then
  notify-send -t 1 "touchpad off"
  xinput --disable "$device"
else
  notify-send -t 1 "touchpad on"
  xinput --enable "$device"
fi
