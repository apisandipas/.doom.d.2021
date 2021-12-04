#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
# xrdb ~/.emacs.d/exwm/Xresources

xautolock -time 120 -locker 'betterlockscreen -l dim' &
fix_xcursor &
picom -b --config ~/.config/picom/picom.conf
# Enable screen locking on suspend

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.doom.d/desktop.el
