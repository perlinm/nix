#!/usr/bin/env sh

if [ "$#" -ne 1 ]; then
  exit 1
fi

if [ $1 = inc ]; then
  brightnessctl set 10%+
elif [ $1 = dec ]; then
  brightnessctl set 10%-
elif [ $1 = max ]; then
  brightnessctl set 100%
elif [ $1 = min ]; then
  brightnessctl set 10%
elif [ $1 = mid ]; then
  brightnessctl set 50%
fi
