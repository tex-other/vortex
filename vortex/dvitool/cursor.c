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

static char copyright_notice[] = "Copyright (c) 1986 - 1991 Regents of the University of California.\nAll rights reserved.";

#ifndef lint
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/cursor.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include "sun.h"
#include "cursor.defs.h"

extern sun_d	*sunp;

static struct cursor	*cur_stack[] = {
	&main_cursor,
	0, 0, 0, 0, 0, 0, 0, 0, 0
};
static int		cur_sp = 0;

/* restore things to a normal state after some error */
restore_cursor()
{
	cur_sp = 0;
	set_cursor(cur_stack[0]);	
}

/*
 * this routine doesn't work.  the window_get returns a static cursor.
 * the image return by get_cursor may be different, but the address it
 * returns will always be the same.  The only way to ``tell'' what cursor
 * is being displayed is to compare the bits.
 */
struct cursor *
get_cursor()
{
	return((struct cursor *) window_get(sunp->image, WIN_CURSOR, 0));
}

set_cursor(c)
	struct cursor	*c;
{
	window_set(sunp->image, WIN_CURSOR, c, 0);
	window_set(sunp->msg, WIN_CURSOR, c, 0);
}

do_push_cursor(c)
	struct cursor	*c;
{
	if (cur_sp > 9) {
		return;
	}
	cur_stack[++cur_sp] = c;
	set_cursor(c);
}

pop_cursor()
{
	if (cur_sp != 0) {
		cur_sp--;
	}
	set_cursor(cur_stack[cur_sp]);
}

push_cursor(which)
{
	struct cursor	*c;

	if (which == MOUSE_CUR) {
		c = &mouse_cursor;
	} else if (which == HOUR_CUR) {
		c = &hourglass_cursor;
	} else {
		c = &main_cursor;
	}
	do_push_cursor(c);
}

