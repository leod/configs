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
nitrogen --restore

xscreensaver -no-splash &

xmodmap ~/.Xmodmap
setxkbmap us -variant colemak

scrobby

exec xmonad
