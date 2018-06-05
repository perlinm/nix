# add entries to path
NEW_PATH=$PATH
my_path=(/usr/bin /usr/local/bin /usr/local/sbin $HOME/.cargo/bin $HOME/bin)
for p in $my_path; do
  if ! [ $(echo $PATH | grep $p) ]; then
    NEW_PATH=$NEW_PATH:$p
  fi
done
export PATH=$NEW_PATH

# make pacmatic call pacaur -> powerpill -> pacman
export pacman_program="yay"

# other options
export HISTFILE=~/.zhist
export HISTSIZE=10000
export SAVEHIST=10000
export EDITOR=vim
export VISUAL=vim
export BROWSER=/usr/bin/firefox
export XDG_CONFIG_HOME=$HOME/.config
export ZLS_COLORS=$LS_COLORS
export ALTERNATE_EDITOR=""
export TERM=xterm

setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt extendedglob

# Command completion
autoload -U colors && colors
autoload -U compinit && compinit
autoload -U autocomplete
zstyle -e ':completion:*:default' list-colors \
  'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==00=00}:${(s.:.)LS_COLORS}")'
zstyle ':completion:*' menu select
bindkey "^I" expand-or-complete-prefix # complete from the middle of a word
compdef mt=ls # use same completion rules for mt as for ls

# Autocorrection
setopt correct

# Prompt
_lineup=$'\e[1A'
_linedown=$'\e[1B'
autoload -U promptinit && promptinit
PROMPT=$(print "[%{$fg[yellow]%}%*%{$reset_color%}]%{$fg[green]%}%~:\n$ %{$reset_color%}")
SPROMPT='Correct '%R' to '%r' ? ([y]es/[N]o/[e]dit/[a]bort)'

# use aliases
. ~/.aliases

# emacs-like copy/paste commands
x-copy-region-as-kill() {
  zle copy-region-as-kill
  print -rn $CUTBUFFER | xclip -selection c -i
}
zle -N x-copy-region-as-kill

x-kill-region() {
  zle kill-region
  print -rn $CUTBUFFER | xclip -selection c -i
}
zle -N x-kill-region

x-kill-line() {
  zle kill-line
  print -rn $CUTBUFFER | xclip -selection c -i
}
zle -N x-kill-line

x-yank() {
  CUTBUFFER=`xclip -selection c -o`
  zle yank
}
zle -N x-yank

if [[ -n $DISPLAY ]]; then
  bindkey -e '^[w' x-copy-region-as-kill
  bindkey -e '^W' x-kill-region
  bindkey -e '^K' x-kill-line
  bindkey -e '^Y' x-yank
fi

# get other special key bindings working
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}

key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup keys accordingly
[[ -n "${key[Home]}"     ]]  && bindkey  "${key[Home]}"     beginning-of-line
[[ -n "${key[End]}"      ]]  && bindkey  "${key[End]}"      end-of-line
[[ -n "${key[Insert]}"   ]]  && bindkey  "${key[Insert]}"   overwrite-mode
[[ -n "${key[Delete]}"   ]]  && bindkey  "${key[Delete]}"   delete-char
[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
[[ -n "${key[Left]}"     ]]  && bindkey  "${key[Left]}"     backward-char
[[ -n "${key[Right]}"    ]]  && bindkey  "${key[Right]}"    forward-char
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"   history-beginning-search-backward
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}" history-beginning-search-forward

# setup some more keys
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^[u' beginning-of-line
bindkey '^[o' end-of-line
bindkey '^[i' up-line-or-history
bindkey '^[k' down-line-or-history
bindkey '^[j' backward-char
bindkey '^[l' forward-char

# make sure the terminal is in application mode when zle is active
# only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        printf '%s' "${terminfo[smkx]}"
    }
    function zle-line-finish () {
        printf '%s' "${terminfo[rmkx]}"
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

# start keyring daemon
if [[ -n "$DESKTOP_SESSION" ]]; then
  eval $(gnome-keyring-daemon --start)
  export SSH_AUTH_SOCK
fi

