#!/bin/sh
lock() {
    i3lock
}

case "$1" in
    lock)
        lock
        ;;
    suspend)
				lock && dbus-send --print-reply --system --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true
        ;;
esac

exit 0
