#!/bin/sh

#settings
xset -b
xrdb -merge ~/.i3/xresources

#background
[ -e ~/.fehbg ] && source ~/.fehbg

#hide mouse coursor when it isn't used
unclutter -root -visible &

#dbus instance
eval `dbus-launch --sh-syntax --exit-with-session`

#apps
start-pulseaudio-x11
urxvtd &

#keyboard layout
setxkbmap -layout pl
setxkbmap -option ctrl:nocaps
xmodmap ~/.i3/xmodmap

#notification
dunst -config ~/.i3/dunstrc &

#disks automounting
udiskie &

#other
#[ -e /usr/bin/dropboxd ] && /usr/bin/dropboxd &

#creating gpg agent
GPG_ENVFILE="$HOME/.gnupg/gpg-agent.env"
if [ -x $(which gpg-agent) ]; then
    eval "$(gpg-agent --enable-ssh-support --daemon --write-env-file "$GPG_ENVFILE")"
    export GPG_AGENT_INFO
fi

#composition manager
compton --config ~/.i3/compton.conf &

# color temperature adjustment, set for Lexington
# redshift -l 38.0297:-84.4947 

i3

#getting rid of gpg agent
[ -x $(which gpg-agent) ] && kill $(grep GPG_AGENT_INFO "$GPG_ENVFILE" | cut -d: -f 2) 2>/dev/null

true
