#!/usr/bin/env bash

killall emacs
/usr/bin/emacs --daemon
notify-send Emacs "Server ready!"
