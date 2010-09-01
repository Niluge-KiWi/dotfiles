OBJS = libwindowswitchs.o
LIBS =
CFLAGS = -Wall -Wextra -Wno-unused -ggdb `pkg-config --cflags --libs libwnck-1.0` `pkg-config --cflags --libs gtk+-2.0`

CC = gcc


all: tray_daemon

$(OBJS): %.o: %.c %.h
	$(CC) -c $<  $(CFLAGS)

tray_daemon: tray_daemon.c $(OBJS)
	$(CC) $^ $(CFLAGS) -o tray_daemon

install: tray_daemon
	cp tray_daemon ~/bin
	cp tray_daemon.sh ~/bin
	cp emacs.png ~/bin
	cp emacs-red.png ~/bin

clean:
	rm -f *.o tray_daemon *~
