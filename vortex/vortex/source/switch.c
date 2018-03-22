/* 
 * Copyright (c) 1987 The Regents of the University of California.
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
 *
 * The above licensing information supersedes all licensing information
 * below.
 */

/*
 *  RCS Info: $Header: switch.c,v 0.1 87/05/01 12:28:15 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the source editor/user interface written
 *  by John Coker for the VorTeX project under the direction of
 *  Prof. Michael A. Harrison of the University of California at
 *  Berkeley.
 *
 *  Copyright (c) 1987 John L. Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  John Coker and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  switch.c - the switch-to-buffer and pop-to-buffer commands
 */
static char _ID[] = "@(#)switch.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: switch-to-buffer
 *  Call: (switch-to-buffer 'buffer [ 'no-display ] [ 'no-record ])
 *  Retu: buffer
 *  Desc: This function switches the current window to begin
 *	displaying/editing another buffer.  Also, the new
 *	buffer is usually moved to the top of the list (which
 *	affects the default when a buffer is prompted for).  The
 *	first argument must evaluate to a string buffer name.
 *
 *	If the optional second argument is given and evaluates non-nil,
 *	it means don't bring the buffer up on the screen, just set
 *	the internal current ``buffer indication'' to this buffer.
 *	This is usually used programatically when a quick change is
 *	to be made to a buffer and then return to a previous buffer.
 *
 *	If the optional third argument is given and evaluates
 *	non-nil, it means that the switch should not be recorded
 *	by modifying the order of the internal buffer list.  The top
 *	element of the buffer list is used as the default when a
 *	buffer is prompted for.  Normally the buffer being switched
 *	to is placed on top of the list, keeping the list sorted
 *	by most recently active buffers.
 *  Side: If the requested buffer does not exist, it is created
 *	as an empty source buffer with no file attached.
 *  SeeA: pop-to-buffer get-buffer create-buffer
 */

DEFUN(doswitchbuf, "switch-to-buffer", FLAG_NONE, "bSwitch to buffer: ")
{
	struct value	arg, ret;
	struct string	*bname;
	struct buffer	*bufp;
	int		argc;
	int		nodisplay = FALSE, norecord = FALSE;

	CHECKAC(1, 3);
	argc = GETACOUNT();

	/* get buffer name argument */
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a buffer name string");
	bname = gstring(arg.vl_data);

	if (argc > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			nodisplay = TRUE;
	}
	if (argc > 2) {
		arg = EVALARGN(3);
		if (truep(arg))
			norecord = TRUE;
	}

	/* create buffer if it doesn't exist */
	bufp = buffer_get(bname, FALSE);
	if (bufp == NULL)
		bufp = buffer_create(bname, BUFF_SOURCE, FLAG_NONE);
	ASSERT(bufp != NULL);

	/* switch and return the name */
	switch_buffer(bufp, nodisplay, norecord);
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, bufp->bu_name);
	return (ret);
}

switch_buffer(new, nodisplay, norecord)
	struct buffer	*new;
{
	struct buffer	*next, *last;

	ASSERT(new != NULL);

	/* assign the global current_buffer */
	current_buffer = new;

	if (!initialized || !havedisplay)
		return (0);

	PROTECT();
	/* switch current window to that buffer now */
	if (!nodisplay && current_window->wi_buffer != new) {
		/* switch by changing current window's buffer */
		if (current_window->wi_type != WIN_BUFFER)
			error("Current window wrong type for buffer editing.");
		switchbuffer(current_window, new);
	}

	/* rearrange list of buffers to put this one on top */
	if (!norecord && buffer_list != new &&
	    (new->bu_flags & BUFF_MINIBUF) == 0) {
		last = NULL;
		for (next = buffer_list; next != NULL; next = next->bu_next) {
			if (next == new)
				break;
			last = next;
		}
		if (next != NULL) {
			/* unlink this buffer from the list */
			next = next->bu_next;
			if (last == NULL)
				buffer_list = next;
			else
				last->bu_next = next;
		}
		/* insert it at the head */
		new->bu_next = buffer_list;
		buffer_list = new;
	}
	UNPROTECT();

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: pop-to-buffer
 *  Call: (pop-to-buffer 'buffer [ 'again ])
 *  Retu: nil
 *  Desc: This function finds the specified buffer in a window somewhere.
 *	If a window is already displaying this buffer, that window
 *	is made the current one, the mouse is moved into it and it
 *	is raised if specified by \sym{auto-raise-windows}.
 *
 *	If the optional argument is given and evaluates non-nil it
 *	means make a new window for the buffer even if one already
 *	exists.
 *  Side: If the given buffer doesn't exist, a empty buffer is created
 *	with that name to pop to.
 *  SeeA: switch-to-buffer auto-raise-windows
 */

DEFUN(dopopbuffer, "pop-to-buffer", FUNC_VISUAL, "bPop to buffer: ")
{
	struct window	*popto_buffer();
	struct value	arg;
	struct buffer	*bufp;
	struct string	*name;
	int		again = FALSE;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string buffer name");
	name = gstring(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			again = TRUE;
	}

	/* make sure we have a valid buffer handle */
	bufp = buffer_get(name, FALSE);
	if (bufp == NULL)
		bufp = buffer_create(name, BUFF_SOURCE, FLAG_NONE);
	ASSERT(bufp != NULL);

	/* find this buffer and return */
	popto_buffer(bufp, again);
	return (v_nil);
}

struct window *
popto_buffer(new, again)
	struct buffer	*new;
{
	struct buffer	*next, *last;
	struct window	*win;

	ASSERT(new != NULL);
	debug(DBUFFER, "Popping to buffer %Y%s.",
	      new->bu_name, again ? " again" : "");

	/* first of all, this is the current buffer now */
	current_buffer = new;

	if (!initialized || !havedisplay)
		return (NULL);

	PROTECT();
	/* rearrange list of buffers to put this one on top */
	if (buffer_list != new) {
		last = NULL;
		for (next = buffer_list; next != NULL; next = next->bu_next) {
			if (next == new)
				break;
			last = next;
		}
		if (next != NULL) {
			/* unlink this buffer from the list */
			next = next->bu_next;
			if (last == NULL)
				buffer_list = next;
			else
				last->bu_next = next;
		}
		/* insert it at the head */
		new->bu_next = buffer_list;
		buffer_list = new;
	}
	UNPROTECT();

	/* check if we need do anything */
	if (current_window->wi_buffer == new)
		return (current_window);

	if (!again) {
		/* try to find the buffer in an active window */
		for (win = window_list; win != NULL; win = win->wi_next)
			if ((win->wi_flags & WIN_ACTIVE) != 0 &&
			    win->wi_buffer == new) {
				/* change this to the current window */
				PROTECT();
				switchwindow(win, TRUE, TRUE);
				UNPROTECT();
				return (win);
			}
	}

	/* need to make a new window */
	PROTECT();
	win = makewindow(NULL, current_window->wi_fontname, new);
	UNPROTECT();

	return (win);
}
