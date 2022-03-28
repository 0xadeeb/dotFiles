#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
PS1='[\u@\h \W]\$ '

#Ignore duplicate commands in history
export HISTCONTROL=ignoreboth:erasedups

#All aliases
alias ls='exa'
alias la='exa -a'
alias ll='exa -l'
alias lla='exa -al'
alias lt='exa -aT --color=always --group-directories-first'
alias grep='grep --color=auto'
#alias dotf='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME' 		#for dotfiles
#alias reboot='sudo reboot'
#alias shutdown='shutdown now'
alias df='df -h'
alias du='du -h'
alias cp="cp -i"
alias mv='mv -i'
alias ln='ln -i'
alias eb='earbuds'
alias brc='vim ~/.bashrc'
alias prg='g++ -o test'
alias ps='procs'

#Function for jumping up directories
up () {
  local d=""
  local limit="$1"

  # Default to limit of 1
  if [ -z "$limit" ] || [ "$limit" -le 0 ]; then
    limit=1
  fi

  for ((i=1;i<=limit;i++)); do
    d="../$d"
  done
  d="$d$2"

  # perform cd. Show error if cd fails
  if ! cd "$d"; then
    echo "Couldn't go up $limit dirs.";
  fi
}

#Vim and emacs settings
export EDITOR=/usr/bin/vim
set -o vi
bind '"jk":vi-movement-mode'
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

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

# add Javascript LSP
export DENO_INSTALL="/home/adeeb/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"


#Evaluate starship
eval "$(starship init bash)"
[ -f "/home/adeeb/.ghcup/env" ] && source "/home/adeeb/.ghcup/env" # ghcup-env
eval "$(zoxide init bash)"

# Pretty-print man(1) pages.
export LESS_TERMCAP_mb=$'\E[1;31m'
export LESS_TERMCAP_md=$'\E[1;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_so=$'\E[1;33m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_us=$'\E[1;32m'

