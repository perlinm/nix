#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

GREEN="$(tput setaf 2)"
RESET="$(tput sgr0)"
PS1='[${GREEN}\t${RESET}]${GREEN} \w:\n$ ${RESET}'

# add entries to path
NEW_PATH=$PATH
my_path=(/usr/bin /usr/local/bin /usr/local/sbin $HOME/bin)
for p in $my_path; do
  if ! [ $(echo $PATH | grep $p) ]; then
    NEW_PATH=$NEW_PATH:$p
  fi
done
export PATH=$NEW_PATH

