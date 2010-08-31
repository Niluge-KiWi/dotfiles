// http://smeuuh.free.fr/tray_daemon
// compile with
// gcc tray_daemon.c `pkg-config --cflags --libs gtk+-2.0` -o tray_daemon
// To use it, create a fifo with mkfifo(1) in FIFO_PATH, and write on it

// Reads a fifo on FIFO_PATH every FIFO_TIMEOUT ms for instructions, and change a tray icon accordingly
// Supported commands :
// - b/B : toggle between NORMAL_ICON and BEEPING_ICON
// - v/V : toggle between invisible/visible
// - Q : exit

// BUGS : timeout is ugly. Should find a way to use select(1) in gtk_main instead

#define FIFO_PATH "/tmp/tray_daemon_control"
#define NORMAL_ICON "emacs.png"
#define BEEPING_ICON "emacs-red.png"
#define FIFO_TIMEOUT 200
//if 1, fork and return
#define DAEMONIZE 0

#include <glib.h>
#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>

GtkStatusIcon *status;
GdkPixbuf *pixbuf_normal;
GdkPixbuf *pixbuf_beeping;

char *fifo = FIFO_PATH;
int fifo_fd;
//called every FIFO_TIMEOUT. Processes available commands in the fifo and returns
gint timeout_callback( gpointer data ) {
	char command = 0;
	while(read(fifo_fd, &command, 1) >= 1) {
		switch(command)
		{
		case 'b':
			gtk_status_icon_set_from_pixbuf(status, pixbuf_normal);
			break;
		case 'B':
			gtk_status_icon_set_from_pixbuf(status, pixbuf_beeping);
			break;
		case 'v':
			gtk_status_icon_set_visible(status, FALSE);
			break;
		case 'V':
			gtk_status_icon_set_visible(status, TRUE);
			break;
		case 'Q':
			gtk_main_quit();
		}
	}
	return TRUE;
}

void sigquit_handler(int sig)
{
	gtk_main_quit();
}

int main(int argc, char **argv)
{
	//rm existing fifo, if any
	remove(fifo);

	//create fifo
	if(mkfifo(fifo, 0600) == 0) {
		fifo_fd = open(fifo, O_RDONLY | O_NONBLOCK);
	}
	else {
		printf("Unable to create fifo, aborting\n");
		exit(1);
	}

	//register signal handlers, to properly quit
	(void) signal(SIGINT, sigquit_handler);
	(void) signal(SIGQUIT, sigquit_handler);
	(void) signal(SIGTERM, sigquit_handler);

	//daemonize
#if DAEMONIZE
	if(fork() != 0)
		exit(0);
#endif

	//init GTK
	gtk_init(&argc, &argv);

	//init icons
	GError *error = NULL;
	pixbuf_normal = gdk_pixbuf_new_from_file(NORMAL_ICON, &error);
	if(error) {printf("Unable to open %s, aborting\n", NORMAL_ICON); exit(1);}
	pixbuf_beeping = gdk_pixbuf_new_from_file(BEEPING_ICON, &error);
	if(error) {printf("Unable to open %s, aborting\n", BEEPING_ICON); exit(1);}
	status = gtk_status_icon_new_from_pixbuf (pixbuf_normal);
	gtk_status_icon_set_visible(status, TRUE);

	//register the timeout
	gtk_timeout_add(FIFO_TIMEOUT, timeout_callback, NULL);

	//main loop
	gtk_main();

	//and clean
	remove(fifo);

	return 0;
}
