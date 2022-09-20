#!/usr/bin/env sh

# colors taken from https://github.com/Big-B/swaylock-fancy/blob/master/swaylock-fancy

swaylock --ignore-empty-password --daemonize \
         --inside-color=0000001c --ring-color=0000003e \
         --line-color=00000000 --key-hl-color=ffffff80 --ring-ver-color=ffffff00 \
         --separator-color=22222260 --inside-ver-color=ff99441c \
         --ring-clear-color=ff994430 --inside-clear-color=ff994400 \
         --ring-wrong-color=ffffff55 --inside-wrong-color=ffffff1c --text-ver-color=00000000 \
         --text-wrong-color=00000000 --text-caps-lock-color=00000000 --text-clear-color=00000000 \
         --line-clear-color=00000000 --line-wrong-color=00000000 --line-ver-color=00000000 --text-color=DB3300FF \
         --screenshots --effect-blur 5x5
