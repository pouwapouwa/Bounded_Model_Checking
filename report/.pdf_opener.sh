#!/bin/sh

OS="`uname`"

if [ "$(uname)" = "Darwin" ]; then
        open $1
else
        DESKTOPFILE=`xdg-mime query default application/pdf`
        DESKTOPPATH='~/.local/share/applications/'
        if [ ! -f $DESKTOPPATH$DESKTOPFILE ];
        then
                DESKTOPPATH='/usr/share/applications/'
        fi

        if [ ! -f $DESKTOPPATH$DESKTOPFILE ];
        then
                DESKTOPPATH='/usr/local/share/applications/'
        fi
        EXECNAME=`cat $DESKTOPPATH$DESKTOPFILE | grep ^Exec | cut -d '=' -f2 | cut -d ' ' -f 1`
        EXECPATH=`which $EXECNAME`
        $EXECPATH $1 &
fi
