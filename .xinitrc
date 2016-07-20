#!/bin/sh
#
# ~/.xinitrc

xset +fp ~/.fonts/

setxkbmap -layout de

xrandr --output HDMI-0 --auto --output DVI-D-1 --auto --left-of HDMI-0 --output VGA-0 --auto --right-of HDMI-0
xcompmgr &

~/.feh.sh &

firefox & xchat & thunderbird & steam &

exec xmonad
