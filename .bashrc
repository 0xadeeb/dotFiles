#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

eval "$(starship init bash)"
export EDITOR=vim
PATH="$HOME/.emacs.d/bin:$PATH"
set -o vi
#bind '"fj":vi-movement-mode'
alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME' 
