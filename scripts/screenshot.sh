#!/usr/bin/env sh

if [ "$#" -ne 1 ]; then
  exit 1
fi

screenshot_dir=~/Pictures/screenshots
output=$screenshot_dir/$(date +"%Y-%m-%d-%H-%M-%S").png

if [ $1 = screen ]; then
  grim -o $(swaymsg -t get_outputs | jq -r '.[] | select(.focused) | .name') $output

elif [ $1 = window ]; then
  grim -g "$(swaymsg -t get_tree | jq -j '.. | select(.type?) | select(.focused).rect | "\(.x),\(.y) \(.width)x\(.height)"')" $output

elif [ $1 = selection ]; then
  grim -g "$(slurp)" $output

fi

wl-copy < $output
