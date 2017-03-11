#!/usr/bin/env sh

xkb-switch -n
notify-send -t 1 "layout: `xkb-switch`"
