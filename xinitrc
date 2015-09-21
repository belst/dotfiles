#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
export GTK_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE="ibus"
export XMODIFIER="@im=ibus"
export DefaultIMModule="ibus"
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
ibus-daemon -d --xim
# redshift&
setxkbmap -option caps:escape
feh --bg-scale /home/belst/Pictures/Wandpapier/sgacitywp.jpg
urxvtd -q -o -f
dunst &
dropbox &
xsetroot -cursor_name left_ptr
[ ! -s ~/.config/mpd/pid ] && mpd
exec xmonad
