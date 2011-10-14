# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '
PS1="\`if [ \$? = 0 ]; then echo "\\[\\033[32m\\]"; else echo \[\e[31m\]; fi\`[\u@\h:\W]\\$\e[0m\] "

export PATH="$PATH:/usr/local/bin/:/home/leod/bin:/home/leod/.cabal/bin:/home/leod/.wine/bin"

export TERM=xterm-256color
export EDITOR=vim

alias lsofnames="lsof | awk '!/^\$/ && /\// { print \$9 }' | sort -u"

function cd
{
	builtin cd "$@" && ls
}

function mkcd
{
	mkdir -p "$@" && builtin cd "$@"
}

if [ -z "$DISPLAY" ] && [ $(tty) = /dev/tty1 ]; then
       startx
fi

