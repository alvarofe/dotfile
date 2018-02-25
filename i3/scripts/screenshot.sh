#!/bin/bash

SCREENDIR="/home/alvaro/screenshots/"
SCREENFILE="${SCREENDIR}/screenshot-$(date +'%F-%T').png"
LINKLATEST="${SCREENDIR}/latest.png"

[ ! -d "$SCREENDIR" ] && mkdir "$SCREENDIR"

sleep 0.5
scrot -s "$SCREENFILE"
ln -sf "$SCREENFILE" "$LINKLATEST"
