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
 *  RCS Info: $Header: wincmds.c,v 0.1 87/05/01 12:32:34 john Locked $
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
 *  wincmds.c - basic window commands (vLisp functions)
 */
static char _ID[] = "@(#)wincmds.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: windowp
 *  Call: (windowp 'fixnum)
 *  Retu: t or nil
 *  Desc: This predicate function determines if its argument (which
 *	should be a fixnum) is an editor window handle.  If so, t
 *	is returned, otherwise nil.
 *  SeeA: current-window
 */

DEFUN(dowindowp, "windowp", FLAG_NONE, NULL)
{
	struct value	arg;
	struct window	*win;
	register int	index;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum");
	index = gfixnum(arg.vl_data);
	for (win = window_list; win != NULL; win = win->wi_next)
		if (win->wi_index == index)
			return (v_t);
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: current-window
 *  Call: (current-window)
 *  Retu: fixnum
 *  Desc: This function returns the window number of the currently
 *	selected window (as a fixnum).  The current window is generally
 *	the one under the mouse, although it can be changed programatically.
 *	The border of the current window is always a solid color, while
 *	all others have a 50% gray border.
 *
 *	Normally, the current window changes as the mouse moves into
 *	another window.  However, if a window has been designated the
 *	keyboard focus, it remains current no matter where the mouse
 *	is on the screen.
 *
 *	The current window changes as the mouse moves into a new X
 *	editor window on the display.  Normally, the window (or other
 *	X client) irectly underneath the mouse will recieve input events.
 *	However, if one of the editor windows has obtained the keyboard
 *	focus, input goes to that window, regardless of where the
 *	mouse is on the screen.  In this case, moving the mouse does
 *	not change the current window.
 *
 *	Winwow numbers are small fixnums, each unique to that window.
 *	The first window, created at startup by the system has the
 *	number zero, and windows created later count up from one.
 *  SeeA: switch-to-window x-get-focus current-buffer
 */

DEFUN(docurwindow, "current-window", FUNC_VISUAL, NULL)
{
	struct value	ret;

	CHECKAC(0, 0);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, current_window->wi_index);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: current-source-window
 *  Call: (current-source-window)
 *  Retu: fixnum
 *  Desc: This function returns the last source window used (the
 *	last which became the current window.  Nil is returned
 *	if there is no source window.
 *
 *	Note that the minibuffer will normally not be returned
 *	by this call since it is not maintained in the normal
 *	most-recently-used list as the others are.
 *  SeeA: current-window current-proof-window
 */

DEFUN(docursrcwin, "current-source-window", FUNC_VISUAL, NULL)
{
	struct window	*cursrcwindow();
	struct value	ret;
	struct window	*winp;
	
	CHECKAC(0, 0);
	if ((winp = cursrcwindow()) == NULL)
		return (v_nil);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, winp->wi_index);
	return (ret);
}

struct window *
cursrcwindow()
{
	struct window	*winp = NULL;
	struct buffer	*bufp;
	
	/* search in list of current windows for last source window */
	for (winp = window_list; winp != NULL; winp = winp->wi_next) {
		if (winp->wi_type != WIN_BUFFER)
			continue;
		bufp = winp->wi_buffer;
		if (bufp != NULL && bufp->bu_type == BUFF_SOURCE)
			break;
	}
	return (winp);
}

/*
 *  DOCUMENTATION
 *
 *  Name: current-proof-window
 *  Call: (current-proof-window)
 *  Retu: fixnum
 *  Desc: This function returns the last proof editor window used
 *	(the last which became the current window.  Nil is returned
 *	if there is no proof window.
 *  SeeA: current-window current-source-window
 */

DEFUN(docurprfwin, "current-proof-window", FUNC_VISUAL, NULL)
{
	struct window	*curprfwindow();
	struct value	ret;
	struct window	*winp;

	CHECKAC(0, 0);
	if ((winp = curprfwindow()) == NULL)
		return (v_nil);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, winp->wi_index);
	return (ret);
}

struct window *
curprfwindow()
{
	struct window	*winp = NULL;
	struct buffer	*bufp;

	/* search in list of current windows for last proof window */
	for (winp = window_list; winp != NULL; winp = winp->wi_next) {
		if (winp->wi_type != WIN_BUFFER)
			continue;
		bufp = winp->wi_buffer;
		if (bufp != NULL && bufp->bu_type == BUFF_PROOF)
			break;
	}
	return (winp);
}

/*
 *  DOCUMENTATION
 *
 *  Name: switch-to-window
 *  Call: (switch-to-window 'window)
 *  Retu: fixnum
 *  Desc: This function switches the current window indication to the
 *	specified window.  This is done even if another window has the
 *	current keyboard focus.  The window number switched to 
 *	(the original argument) is returned.
 *  Side: Another window will become the current one as soon as the
 *	mouse moves into it or an input event occurs in a window other
 *	than the one newly made current. See \sym{x-set-focus} for a
 *	description of the rules when a window has the keyboard focus.
 *  SeeA: current-window x-set-focus switch-to-buffer
 */

DEFUN(doswitchwindow, "switch-to-window", FUNC_VISUAL, "nWhich window? ")
{
	struct value	arg, ret;
	struct window	*win;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a window number");
	win = getwindow(gfixnum(arg.vl_data));

	switchwindow(win, TRUE, TRUE);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_index);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: kill-window
 *  Call: (kill-window [ 'window ])
 *  Retu: t
 *  Desc: This function deletes the specified window.
 *	If the optional argument is given, it should evaluate
 *	to the window number to kill rather than the current one.
 *
 *	It is an error to attempt to kill the only existing window
 *	of the editor.  To exit, you should use \sym{exit-vortex}.
 *  SeeA: current-window split-window kill-buffer
 */

DEFUN(dokillwindow, "kill-window", FUNC_VISUAL, "")
{
	struct value	arg;
	struct window	*win = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a window number");
		win = getwindow(gfixnum(arg.vl_data));
	}
	killwindow(win, FALSE);

	return (v_t);
}

killwindow(win, force)
	struct window	*win;
{
	extern struct window	*minibuf_window;
	struct window		*next, *last;
	register int		others = 0;
	struct buffer		*bufp;

	ASSERT(win != NULL);
	debug(DWINDOW, "Trying to kill editor window %d%s.",
	      win->wi_index, force ? " (and forcing it)" : "");

	/* check to make sure we can kill this window */
	if ((win->wi_flags & WIN_MINIBUF) != 0) {
		if (force)
			minibuf_window = NULL;
		else
			error("You can't kill the minibuffer window!");
	}

	/* count number of other active windows */
	for (next = window_list; next != NULL; next = next->wi_next)
		if (next != win && (next->wi_flags & WIN_ACTIVE) != 0)
			others++;

	/* remove this window from the list if necessary */
	last = NULL;
	for (next = window_list; next != NULL; next = next->wi_next) {
		if (next == win)
			break;
		last = next;
	}
	if (next != NULL) {
		/* check if this is the last active window */
		if (!force && (win->wi_flags & WIN_ACTIVE) != 0 && others < 1)
			error("Can't kill only remaining active window!");
		/* remove this window */
		if (last == NULL)
			window_list = next->wi_next;
		else
			last->wi_next = next->wi_next;
	}

	PROTECT();
	/* make sure it's not current one */
	if (current_window == win) {
		current_window = NULL;
		if (window_list != NULL)
			switchwindow(window_list, TRUE, TRUE);
	}

	/* release the keyboard focus if it was the focus */
	if ((win->wi_flags & WIN_FOCUSED) != 0)
		XFocusKeyboard(RootWindow);

	/* call the special buffer close hook if necessary */
	if (win->wi_type == WIN_BUFFER && (bufp = win->wi_buffer) != NULL) {
		if (bufp->bu_close != NULL)
			(*bufp->bu_close)(win);
		win->wi_buffer = NULL;
	}

	/* close down the window to save resources */
	if (win->wi_fontinfo != NULL)
		XCloseFont(win->wi_fontinfo);
	if (win->wi_xwindow != NULL)
		XDestroyWindow(win->wi_xwindow);
	if (win->wi_iconwin != NULL)
		XDestroyWindow(win->wi_iconwin);
	vfree(win);
	UNPROTECT();

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: get-internal-border
 *  Call: (get-internal-border [ 'window ])
 *  Retu: width
 *  Desc: This function returns the width of the internal border
 *	for the specified window in pixels as a fixnum.  If the
 *	optional argument is given, it should specify the window
 *	to query.
 *  SeeA: set-internal-border x-get-border-width current-window
 */

DEFUN(dogetintborder, "get-internal-border", FUNC_VISUAL, NULL)
{
	struct value	arg, ret;
	struct window	*win = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a window number");
		win = getwindow(gfixnum(arg.vl_data));
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_border);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: set-internal-border
 *  Call: (set internal-border 'width [ 'window ])
 *  Retu: width
 *  Desc: This function changes the value of the internal border
 *	width for the current window to the number of pixels
 *	specified by its first argument.  If the second argument
 *	is given, it should specify the window to alter.
 *  Side: To implement this, the size of the X window (the window)
 *	must be changed, so the window will appear to clear and
 *	repaint.
 *  SeeA: get-internal-border x-set-border-width
 */

DEFUN(dosetintborder, "set-internal-border", FUNC_VISUAL,
      "nInternal border width: ")
{
	struct value	arg, ret;
	int		width;
	struct window	*win = current_window;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a width fixnum");
	width = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a window number");
		win = getwindow(gfixnum(arg.vl_data));
	}

	if (width != win->wi_border) {
		PROTECT();
		win->wi_border = width;
		win->wi_xsize = GETWIDTH(win, win->wi_cols);
		win->wi_ysize = GETHEIGHT(win, win->wi_rows);
		XChangeWindow(win->wi_xwindow, win->wi_xsize, win->wi_ysize);
		UNPROTECT();
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_border);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: deactivate-window
 *  Call: (deactivate-window [ 'window ])
 *  Retu: fixnum
 *  Desc: This function removes the specified window from the screen,
 *	but keeps its associated information around for later
 *	re-activation with \sym{reactivate-window}.  If the optional
 *	argument is given, it specifies the window other than the
 *	current window to deactivate.
 *
 *	No input will be directed at this window while it is deactivated
 *	and no commands other than \sym{reactivate-window} can be
 *	performed on it.  The last editor window cannot be deactivated.
 *  Side: If the specified window holds the input focus, the focus
 *	will be removed; no window will be the focus.
 *  SeeA: reactivate-window current-window
 */

DEFUN(dodeactivatewin, "deactivate-window", FUNC_VISUAL, "")
{
	struct value	arg, ret;
	struct window	*win = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a window number");
		win = getwindow(gfixnum(arg.vl_data));
	}

	deactivatewin(win);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_index);
	return (ret);
}

deactivatewin(win)
	struct window	*win;
{
	WindowInfo	info;

	ASSERT(win != NULL);
	debug(DWINDOW, "Deactivating window %d.", win->wi_index);

	if (win->wi_type != WIN_BUFFER)
		error("Can't deactivate special window types.");
	if ((win->wi_flags & WIN_ACTIVE) == 0)
		return (1);

	PROTECT();
	if (win->wi_flags & WIN_FOCUSED) {
		win->wi_flags &= ~WIN_FOCUSED;
		XFocusKeyboard(RootWindow);
	}
	/* map associated window, if one exists */
	if (XQueryWindow(win->wi_xwindow, &info) == 0) {
		ierror("Can't query X about window %d's X window!",
		       win->wi_index);
	}
	if (info.assoc_wind != NULL)
		XMapWindow(info.assoc_wind);

	/* upmap the main window if necessary */
	if (info.mapped == IsMapped)
		XUnmapWindow(win->wi_xwindow);

	win->wi_flags &= ~WIN_ACTIVE;
	UNPROTECT();

	return (0);
}

reactivatewin(win)
	struct window	*win;
{
	WindowInfo	info;

	ASSERT(win != NULL);
	debug(DWINDOW, "Reactivating window %d.", win->wi_index);

	/* do we have anything to do? */
	if ((win->wi_flags & WIN_ACTIVE) != 0)
		return (1);

	PROTECT();
	/* unmap associated window, if one exists */
	if (XQueryWindow(win->wi_xwindow, &info) == 0) {
		ierror("Can't query X about window %d's X window!",
		       win->wi_index);
	}
	if (info.assoc_wind != NULL)
		XUnmapWindow(info.assoc_wind);

	/* map our new X window if necessary */
	if (info.mapped == IsUnmapped)
		XMapWindow(win->wi_xwindow);

	win->wi_flags |= WIN_ACTIVE;
	UNPROTECT();

	return (0);
}
