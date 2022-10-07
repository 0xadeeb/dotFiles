#!/usr/bin/env bash

killall emacs
/usr/bin/emacs --daemon
if [[ $? -eq 0 ]]
then
	notify-send Emacs "Server ready!"
	exit 0
else
	notify-send Emacs "Error! Couldn't start server"
	exit 1
fi


