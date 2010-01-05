#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

xrdb -merge .Xresources
xsetroot -cursor_name left_ptr
xsetroot -solid midnightblue

xscreensaver -no-splash &

xmodmap ~/.Xmodmap

scrobby

exec xmonad
