#!/bin/bash

function installAurHelper() {
	clear
	if ! command -v $1 &> /dev/null
	then
		echo "Installing $1..."
		sleep 2
		[ ! -d "/$HOME/Downloads" ] && mkdir ~/Downloads
		cd ~/Downloads
		if [ $1 -eq "paru" ]
		then
			sudo pacman -S --noconfirm --needed rust
		fi
		git clone "https://aur.archlinux.org/$1.git"
		cd $1
		makepkg --noconfirm -si
	else
		echo "$1 has already been installed!"
		sleep 2
	fi
}

function installNativePackages() {
	clear
	echo "Installing native packages in packages-list-native.txt..."
	sleep 2
	cd $1
	sudo pacman -S --noconfirm --needed - < packages-list-native.txt
}

function installForeignPackages() {
	clear
	echo "Installing foreign packages in packages-list-foreign.txt..."
	sleep 2
	cd $1
	$2 -S --noconfirm --needed - < packages-list-foreign.txt
}

function simlinkDotfiles() {
	clear
	echo "Creating directories and simlinking dotfile..."
	sleep 2
	[ ! -d "/$HOME/.config" ] && mkdir /$HOME/.config
	mkdir -p  /$HOME/.local/{bin,share/{audio,fonts}}
	cd $1
	sudo pacman -S --noconfirm --needed stow
	make
}

function installXmonad () {
	clear
	echo "Installing xmonad as the window manager..."
	sleep 2
	cd ~/.config/xmonad
	sudo pacman -S --noconfirm --needed stack
	stack upgrade
	[ -f "stack.yaml" ] && rm stack.yaml
	stack init
	stack install
	sudo ln -s /$HOME/.local/bin/{xmonad,xmonad-dbus} /usr/bin
	xmonad --recompile
}

function main() {
	clear
	echo "Welcome!" && sleep 1
	cfg=$(pwd)
	helper="paru"
	echo "Doing a system update..."
	sudo pacman --noconfirm -Syu
	clear
	echo "Which AUR helper would you like to install?"
	echo "1) paru 2)yay"
	read -r -p "Default is paru:" num
	if [ $num -eq 2 ]
	then
		helper="yay"
	fi
	installAurHelper $helper
	installNativePackages $cfg
	installForeignPackages $cfg $helper
	simlinkDotfiles $cfg
	installXmonad
	clear
	echo "Installation process done!"
	echo "Enjoy your new setup ;-)"
	sleep 2
}

main
