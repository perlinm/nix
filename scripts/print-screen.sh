#!/usr/bin/env sh

cd ~/Pictures/screenshots

for ii in $*; do
  if [ $ii = -s ] || [ $ii = --select ]; then    
    sleep 0.2
  fi
done

scrot $* --freeze '%Y-%m-%d-%H-%M-%S-$wx$h.png'
xclip -selection clipboard -t image/png -i `ls -t | head -n 1`
