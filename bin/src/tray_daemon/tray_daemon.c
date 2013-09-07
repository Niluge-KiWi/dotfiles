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
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <assert.h>

#include "libwindowswitchs.h"

GtkStatusIcon *status;
GdkPixbuf *pixbuf_normal;
GdkPixbuf *pixbuf_beeping;


/* Command part */
char *fifo = FIFO_PATH;
int fifo_fd;
//called every FIFO_TIMEOUT. Processes available commands in the fifo and returns
gint timeout_callback( gpointer data ) {
	char command = 0;
	while(read(fifo_fd, &command, 1) >= 1) {
		switch(command)
		{
		case 'D':
			gtk_status_icon_set_from_pixbuf(status, pixbuf_normal);
			gtk_status_icon_set_blinking(status, FALSE);
			break;
		case 'R':
			gtk_status_icon_set_from_pixbuf(status, pixbuf_beeping);
			gtk_status_icon_set_blinking(status, FALSE);
			break;
		case 'B':
			gtk_status_icon_set_from_pixbuf(status, pixbuf_beeping);
			gtk_status_icon_set_blinking(status, TRUE);
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


/* Monitor part */

char* get_buffer_name_emacs(const char* window_name)
{
	// the emacs title is "<buffer-name> - emacs@hostname"
	char* buffer_name = NULL;

	char hostname[256];
	gethostname(hostname, 256);

	char* emacs_fix = " - emacs@";

	size_t suffix_size = strlen(emacs_fix) + strlen(hostname);
	char* suffix = (char*) malloc (sizeof(char)*(suffix_size+1));
	strcpy(suffix, emacs_fix);
	strcat(suffix, hostname);

	int prefix_size = strlen(window_name) - suffix_size;
	if(prefix_size <= 0) {
		// no match: window_name is too short
		goto ret;
	}

	const char* suffix_window_name = window_name + prefix_size;

	if(!strcmp(suffix, suffix_window_name)) {
		// we got a match
		buffer_name = (char*) malloc(sizeof(char)*(prefix_size+1));
		strncpy(buffer_name, window_name, prefix_size);
		buffer_name[prefix_size] = '\0';
	}

ret:
	free(suffix);
	return buffer_name;
}

// monitor windows switchs with libwindowswitch, and call emacsclient --eval
void enter_window(const char* window_name)
{
	// TODO: Implement emacs hook (erc-channel-?)window-got-focus
	// currently just call what we need:
	// only when this is an erc channel buffer

	char* buffer_name = get_buffer_name_emacs(window_name);
	if(!buffer_name) {
		// no match
		return;
	}

	//if(buffer_name[0]=='#') {
	// private messages buffers don't start with #
	{
		// TODO lowercase on buffername
		char* format = "emacsclient --eval '(with-current-buffer \"%s\" (erc-modified-channels-update))'";
		size_t cmd_size = strlen(format) + strlen(buffer_name);
		char* cmd = (char*) malloc(sizeof(char)*(cmd_size+1));
		int ret = snprintf(cmd, (cmd_size+1), format, buffer_name);

		assert(0 <= ret && (size_t)ret <= cmd_size); // snprintf should fully work

		system(cmd);

		free(cmd);
	}
	free(buffer_name);
}


/* Main part */
void sigquit_handler(int sig)
{
	windowswitchs_stop();
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

	// init libwindowswitchs and GTK
	windowswitchs_init(&enter_window, NULL);

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
	windowswitchs_start();

	//and clean
	remove(fifo);

	return 0;
}
