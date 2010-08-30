all: tray_daemon

tray_daemon: tray_daemon.c
	gcc tray_daemon.c `pkg-config --cflags --libs gtk+-2.0` -o tray_daemon

install: tray_daemon
	cp tray_daemon ~/bin/
clean:
	rm tray_daemon
