#!/usr/bin/env bash

BAT=$(cat /sys/class/power_supply/BAT0/capacity)

function batteryPrompt() {
    statusId=$1
    text=$2
    icon=$3

    if [ -f "/tmp/battery_script_status" ]
    then
        currentStatus=$(cat /tmp/battery_script_status 2>&1)
    else
        currentStatus=0
    fi

    if [ $statusId -eq $currentStatus ]
    then
        return
    fi

    export DISPLAY=:0.0
    export XDG_RUNTIME_DIR=/run/user/1000
    if [[ $statusId == 1 ]]
    then
        paplay $HOME/.local/share/audio/Oxygen-Sys-App-Error-Critical.ogg &
    elif [[ $statusId == 2 ]]
    then
        paplay $HOME/.local/share/audio/Oxygen-Sys-App-Error-Serious.ogg &
    else
        paplay $HOME/.local/share/audio/Oxygen-Sys-App-Positive.ogg &
        notify-send "Power Manager" "$text"
        return
    fi

    zenity --warning --text="$text" --icon-name=$icon --display=:0.0
}

if [[ "$(cat /sys/class/power_supply/BAT0/status)" == "Charging" ]]
then
    if [ $BAT -eq 100 ]
    then
        batteryPrompt 3 'Battery is fully charged' null
        echo "3" > /tmp/battery_script_status
    else
        echo "0" > /tmp/battery_script_status
    fi
else
    if [ $BAT -le 7 ]
    then
        batteryPrompt 2 'Battery is critically low!' battery-empty
        echo "2" > /tmp/battery_script_status
    elif [ $BAT -le 20 ]
    then
        batteryPrompt 1 'Battery is at 20%.' battery-low
        echo "1" > /tmp/battery_script_status
    else
        echo "0" > /tmp/battery_script_status
    fi
fi
