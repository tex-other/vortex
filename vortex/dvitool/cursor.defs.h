/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 * define the default cursors
 */
static short cursor_image[] = {
#	include "circle.h"
};
mpr_static(cursor_pr, 16, 16, 1, cursor_image);
struct cursor main_cursor = {
	0, 0, PIX_SRC | PIX_DST, &cursor_pr
};

static short hourglass_image[] = {
#	include "hourglass.h"
};
mpr_static(hour_pr, 16, 16, 1, hourglass_image);
struct cursor hourglass_cursor = {
	0, 0, PIX_SRC | PIX_DST, &hour_pr
};

static short mouse_image[] = {
#	include "mouse.h"
};
mpr_static(mouse_pr, 16, 16, 1, mouse_image);
struct cursor mouse_cursor = {
	0, 0, PIX_SRC | PIX_DST, &mouse_pr
};

DEFINE_CURSOR(dummy, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

/*
 * define the default icon
 */
#define TOOL_ICONWIDTH  (64)
#define TOOL_ICONHEIGHT (64)

static short icon_image[] = {
#include "icon.h"
};
mpr_static(icon_pr, 64, 64, 1, icon_image);
struct icon	 dvi_icon = {
	TOOL_ICONWIDTH, TOOL_ICONHEIGHT, NULL,
	{ 0, 0, TOOL_ICONWIDTH, TOOL_ICONHEIGHT },
	&icon_pr, { 0, 0, 0, 0 }, NULL, NULL, ICON_BKGRDGRY
};

