# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '
#PS1="\`if [ \$? = 0 ]; then echo \[\e[33m\]^_^\[\e[0m\]; else echo \[\e[31m\]O_O\[\e[0m\]; fi\`[\u@\h:\w]\\$ "
PS1="\`if [ \$? = 0 ]; then echo "\\[\\033[32m\\]"; else echo \[\e[31m\]; fi\`[\u@\h:\W]\\$\e[0m\] "

export PATH="$PATH:/usr/local/bin/:/home/leod/dev/dmd/bin:/home/leod/bin:/home/leod/dev/ldc/bin:/home/leod/dev/purebasic/compilers:/home/leod/.cabal/bin:/home/leod/.wine/bin:/home/leod/dev/dmd2/linux/bin"
export D_COMPILER="dmd"
export DC="dmd"

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

export PATH="$(cope_path):${PATH}"
