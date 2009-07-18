#!/bin/sh
gcc tray_daemon.c -o tray_daemon $(pkg-config --cflags gtk+-2.0 --cflags glib-2.0) $(pkg-config --libs gtk+-2.0 --libs glib-2.0) -Wall
