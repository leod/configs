#!/bin/sh
xrdb -merge .Xresources
xsetroot -cursor_name left_ptr
xsetroot -solid midnightblue
xmodmap ~/.Xmodmap
#setxkbmap us -variant colemak

exec xmonad
