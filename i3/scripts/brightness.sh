#!/bin/sh
CURRBRIGHT=$(xrandr --current --verbose | grep -m 1 'Brightness:' | cut -f2- -d:)
DISPL=$(xrandr |grep connected|grep -v disconnected| awk '{print $1}')

for disp in $DISPL; do 
	if [ "$1" = "inc" ] && [ $(echo "$CURRBRIGHT < 1" | bc) -eq 1 ] 
	then
		xrandr --output $disp --brightness $(echo "$CURRBRIGHT + 0.1" | bc)
	elif [ "$1" = "dec" ] && [ $(echo "$CURRBRIGHT > 0" | bc) -eq 1 ] 
	then
		xrandr --output $disp --brightness $(echo "$CURRBRIGHT - 0.1" | bc)
	fi
done
