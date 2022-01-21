#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
PS1='[\u@\h \W]\$ '

#All aliases
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias ll='ls -l --color=auto'
alias lla='ls -al --color=auto'
alias g='grep --color=auto'
alias v='vim'
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME' 

#Vim and emacs settings
export EDITOR=/usr/bin/vim
set -o vi
bind '"fj":vi-movement-mode'

#Set paths

#Add user's private bin to PATH
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Add Doom emacs to PATH 
if [ -d "$HOME/.emacs.d/bin" ] ; then
	PATH="$PATH:$HOME/.emacs.d/bin"
fi

# add Haskell cabal
if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$PATH:$HOME/.cabal/bin"
fi

# add Ruby gems to PATH
if [ -d "$HOME/.gem/ruby/1.9.1/bin" ] ; then
    PATH="$PATH:$HOME/.gem/ruby/1.9.1/bin"
elif [ -d "$HOME/.gem/ruby/2.0.0/bin" ]; then
    PATH="$PATH:$HOME/.gem/ruby/2.0.0/bin"
fi

#Evaluate starship
eval "$(starship init bash)"
[ -f "/home/adeeb/.ghcup/env" ] && source "/home/adeeb/.ghcup/env" # ghcup-env
