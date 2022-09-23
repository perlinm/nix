#!/usr/bin/env zsh

if [ $# -ne 1 ]; then
  echo "must provide exactly one argument: the workspace to swap places with"
  exit
fi

current=$(swaymsg -t get_workspaces \
          | jq '.[] | select(.focused==true).name' \
          | cut -d "\"" -f2)

swaymsg "rename workspace $current to $1"
if [ $? -ne 0 ]; then
  swaymsg "rename workspace $1 to ' ', \
           rename workspace $current to $1, \
           rename workspace ' ' to $current"
fi
