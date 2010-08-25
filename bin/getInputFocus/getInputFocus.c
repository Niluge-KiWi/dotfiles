// Copyright (C) 2010 Thomas Riccardi
		 
// This program is free software. It comes without any warranty, to
// the extent permitted by applicable law. You can redistribute it
// and/or modify it under the terms of the Do What The Fuck You Want
// To Public License, Version 2, as published by Sam Hocevar. See
// http://sam.zoy.org/wtfpl/COPYING for more details.

#include <X11/Xlib.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, const char* argv[])
{
	Display *display = XOpenDisplay(0);
	Window window = 0;
	int revert = 0;

	assert(display);
	
	//sleep(2);
	XGetInputFocus(display, &window, &revert);
	printf("%u\n", window);
}
