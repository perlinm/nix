#!/usr/bin/env zsh

if ! [ $1 ]; then
  exit 0
else
  arg=$1
fi

shell(){
  if [ $1 ]; then
    cmd="--command=$1"
  else
    cmd=
  fi
  xfce4-terminal --title="pad-$arg" $cmd
}

if [ $arg = term ] || [ $arg = alt-term ]; then
  shell
elif [ $arg = calc ]; then
  shell "ipython3 --profile=perlinm --no-banner"
else
  shell $arg
fi
