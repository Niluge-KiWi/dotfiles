// libwindowswitch.h -- Get window switchs

// Copyright (C) 2010 Antoine Levitt
// Copyright (C) 2010 Thomas Riccardi

// Author: Antoine Levitt
//         Thomas Riccardi <riccardi.thomas@gmail.com>
// URL: http://github.com/antoine-levitt/prodmonitor/tree/sqlite

// This program is free software. It comes without any warranty, to
// the extent permitted by applicable law. You can redistribute it
// and/or modify it under the terms of the Do What The Fuck You Want
// To Public License, Version 2, as published by Sam Hocevar. See
// http://sam.zoy.org/wtfpl/COPYING for more details.


/*
 * Callback functions prototype:
 * Called with the window's name
 */
typedef void (*callback_function)(const char* window_name);

/*
 * Init WindowSwitchs with the two callback functions:
 *  called respectively at enter and when leaving a window.
 * Callbacks can be NULL, to deactivate
 * Also init gtk
 */
void windowswitchs_init(callback_function enter_window_callback, callback_function leave_window_callback);

/*
 * Start WindowSwitchs
 * Returns only when windowswitchs_stop() called
 */
void windowswitchs_start();

/*
 * Stop WindowSwitchs
 */
void windowswitchs_stop();
