// Copyright (C) 2010 Thomas Riccardi

// This program is free software. It comes without any warranty, to
// the extent permitted by applicable law. You can redistribute it
// and/or modify it under the terms of the Do What The Fuck You Want
// To Public License, Version 2, as published by Sam Hocevar. See
// http://sam.zoy.org/wtfpl/COPYING for more details.

#define WNCK_I_KNOW_THIS_IS_UNSTABLE
#include <libwnck/libwnck.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	gtk_init(&argc, &argv);

	WnckScreen *screen = wnck_screen_get_default();
	assert(screen);

	wnck_screen_force_update(screen);
	WnckWindow *window = wnck_screen_get_active_window(screen);

	if (window) {
		const char* window_name = wnck_window_get_name(window);
		printf("%s\n", window_name);
	}
}
