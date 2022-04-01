#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
# xrdb ~/.emacs.d/exwm/Xresources

xautolock -time 120 -locker 'betterlockscreen -l dim' &
picom -b --config ~/.doom.d/exwm/picom.conf

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.doom.d/exwm/desktop.el
