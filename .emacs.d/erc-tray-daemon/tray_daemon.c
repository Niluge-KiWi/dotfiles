#include <gtk/gtk.h>
#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

GtkStatusIcon *status;
char *icon = "icon.png";
char *fifo = "/tmp/tray_daemon_control";
int fifo_fd;
gint timeout_callback( gpointer data ) {
	char command = 0;
	while(read(fifo_fd, &command, 1) >= 1) {
		switch(command)
		{
		case 'b':
			gtk_status_icon_set_blinking(status, FALSE);
			break;
		case 'B':
			gtk_status_icon_set_blinking(status, TRUE);
			break;
		case 'v':
			gtk_status_icon_set_visible(status, FALSE);
			break;
		case 'V':
			gtk_status_icon_set_visible(status, TRUE);
			break;
		case 'Q':
			close(fifo_fd);
			exit(0);
		}
	}
	return TRUE;
}
int main(int argc, char **argv)
{
	//init fifo
	if((fifo_fd = open(fifo, O_RDONLY | O_NONBLOCK)) == -1) {
		if(mkfifo(fifo, 0600) == 0) {
			fifo_fd = open(fifo, O_RDONLY | O_NONBLOCK);
		}
		else {
			printf("Unable to create fifo, aborting");
			exit(1);
		}
	}
	//daemonize
	/* if(fork() != 0) */
	/* 	exit(0); */

	//init GTK
	gtk_init(&argc, &argv);
	status = gtk_status_icon_new_from_file (icon);
	gtk_status_icon_set_visible(status, TRUE);
	gtk_timeout_add(500, timeout_callback, NULL);
	gtk_main();
	close(fifo_fd);
	return 0;
}
