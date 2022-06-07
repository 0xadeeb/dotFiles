##
# Dotfiles management with GNU Stow
#
# @file
# @version 0.1

all:
	stow --verbose --target=$$HOME -S --dotfiles dotfiles

clean:
	stow --verbose --target=$$HOME --delete --dotfiles dotfiles

# end
