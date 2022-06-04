##
# Dotfiles management with GNU Stow
#
# @file
# @version 0.1

all: Audio Bash BetterLockScreen Clang Conky Dunst Emacs Kitty Picom Polybar Rofi Starship Vim Wallpaper Xmonad Zathura Zsh

Audio: audio
	stow -v -t ~/.local -R audio

Bash: bash
	stow -v -t ~ -R bash

BetterLockScreen: betterlockscreen
	stow -v -t ~/.config -R betterlockscreen

Clang: clang
	stow -v -t ~ -R clang

Conky: conky
	stow -v -t ~/.config -R conky

Dunst: dunst
	stow -v -t ~/.config -R dunst

Emacs: emacs
	stow -v -t ~ -R emacs

Kitty: kitty
	stow -v -t ~/.config -R kitty

Picom: picom
	stow -v -t ~/.config -R picom

Polybar: polybar
	stow -v -t ~/.config -R polybar

Rofi: rofi
	stow -v -t ~/.config -R rofi

Starship: starship
	stow -v -t ~/.config -R starship

Vim: vim
	stow -v -t ~ -R vim

Wallpaper: wallpaper
	stow -v -t ~ -R wallpaper

X: x
	stow -v -t ~R x

Xmonad: xmonad
	stow -v -t ~/.config -R xmonad

Zathura: zathura
	stow -v -t ~/.config -R zathura

Zsh: zsh
	stow -v -t ~ -R zsh

clean:
	stow --verbose --target=$$HOME --delete */

# end
