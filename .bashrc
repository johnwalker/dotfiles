# There are 3 different types of shells in bash: the login shell, normal shell
# and interactive shell. Login shells read ~/.profile and interactive shells
# read ~/.bashrc; in our setup, /etc/profile sources ~/.bashrc - thus all
# settings made here will also take effect in a login shell.
#
# NOTE: It is recommended to make language settings in ~/.profile rather than
# here, since multilingual X sessions would not work properly if LANG is over-
# ridden in every subshell.

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
#export EDITOR=/usr/bin/mcedit

# For some news readers it makes sense to specify the NEWSSERVER variable here
#export NEWSSERVER=your.news.server

# If you want to use a Palm device with Linux, uncomment the two lines below.
# For some (older) Palm Pilots, you might need to set a lower baud rate
# e.g. 57600 or 38400; lowest is 9600 (very slow!)
#
#export PILOTPORT=/dev/pilot
#export PILOTRATE=115200

test -s ~/.alias && . ~/.alias || true

alias cabal=$HOME/.cabal/bin/cabal
PATH=$PATH:$HOME/toolbox/phantomjs/bin:$HOME/toolbox/node-webkit:$HOME/.cabal/bin:$HOME/.xmonad/bin:$HOME/toolbox/bin:$HOME/toolbox/LightTable:$HOME/.cask/bin:$HOME/Development/racket/racket/bin:$HOME/toolbox/bin/clicbs/src:$HOME/toolbox/minisat/build/release/bin:$HOME/.gem/ruby/2.1.0/bin

export PATH
alias ls='ls --color'
alias sl=ls
setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift

export EDITOR=/usr/bin/vim
