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
 *  RCS Info: $Header: xcmds.c,v 0.1 87/05/01 12:35:32 john Locked $
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
 *  xcmds.c - basic commands for X windows (vLisp functions)
 */
static char _ID[] = "@(#)xcmds.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: x-get-display
 *  Call: (x-get-display)
 *  Retu: string
 *  Desc: This function returns a string describing the X display to
 *	which the user is connected.  The user must be connected to
 *	an X display for this to work, of course.  Thus, this function
 *	only works with the visual editor.
 */

DEFUN(doXgetdisp, "x-get-display", FUNC_VISUAL, NULL)
{
	struct string	*str;
	struct value	ret;

	CHECKAC(0, 0);

	/* create string with the display name */
	str = save_string(Xdispname, strlen(Xdispname));
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-get-font
 *  Call: (x-get-font)
 *  Retu: fontname
 *  Desc: This function returns the name of the font currently being
 *	used for text display in the current window.  Presumably, this name
 *	was established, either as the default, or by a call to
 *	\sym{x-set-font}.
 *
 *	Initially, the font used is that specified by the \em{BodyFont}
 *	X default, and/or the \lit{-f} command line option.  If no font
 *	is specified in either of these ways, the font \lit{vtsingle} is
 *	used as the default.
 *  SeeA: x-set-font current-window
 */

DEFUN(doXgetfont, "x-get-font", FUNC_VISUAL, NULL)
{
	struct string	*str;
	struct value	ret;

	CHECKAC(0, 0);

	/* create string with the font name */
	str = save_string(current_window->wi_fontname,
			  strlen(current_window->wi_fontname));
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-set-font
 *  Call: (x-set-font 'fontname)
 *  Retu: nil
 *  Desc: This function causes the current window to use a different
 *	X font for output than the one previously in use.  The argument
 *	must be a string, and should be the name of the new font
 *	to use.
 *
 *	Fonts used by a window to display its textual contents must
 *	obey several restrictions.  They must be of fixed width,
 *	have a width and a height greater than zero, and contain
 *	entries for all the printable \sc{ASCII} characters.  Also,
 *	the painting scheme of the editor requires that the space
 *	character, \sc{ASCII} 32, be a solid block of the background
 *	color.  These requirements are satisfied by the standard X
 *	``fixed width'' fonts.
 *  SeeA: x-get-font current-window
 */

DEFUN(doXsetfont, "x-set-font", FUNC_VISUAL, "sUse which X font? ")
{
	extern FontInfo	*fixedfont();
	struct value	arg;
	struct string	*str;
	char		font[64];
	FontInfo	*new;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string font name");
	str = gstring(arg.vl_data);
	if (str->st_length < 1)
		error("An empty string specifies no font!");
	makecstring(str, font, sizeof (font));

	/* make sure this font is of use to us */
	new = fixedfont(font);

	/* switch fonts now */
	if (current_window->wi_fontinfo != NULL)
		XCloseFont(current_window->wi_fontinfo);
	current_window->wi_fontinfo = new;
	strncpy(current_window->wi_fontname, font,
		sizeof (current_window->wi_fontname) - 1);
	current_window->wi_fontname[sizeof (current_window->wi_fontname) - 1] =
	    '\0';

	/* change window size and trigger exposure events */
	XChangeWindow(current_window->wi_xwindow,
		      GETWIDTH(current_window, current_window->wi_cols),
		      GETHEIGHT(current_window, current_window->wi_rows));
	XSetResizeHint(current_window->wi_xwindow,
		       MINWIDTH(current_window), MINHEIGHT(current_window),
		       current_window->wi_fontinfo->width,
		       current_window->wi_fontinfo->height);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-store-buffer
 *  Call: (x-store-buffer 'string [ 'buffer ])
 *  Retu: t
 *  Desc: This function stores the text containted in the given
 *	string into the X cut buffer.  If the optional second
 *	argument is given, the cut buffer it specifies is used,
 *	rather than the default cut buffer, zero. Only buffers
 *	numbered 0 through 7 are valid.
 *  Side: The cut buffers are global to the X server (otherwise they
 *	wouldn't be useful) and so text stored into the cut buffer
 *	will be retrieved by the next X application which fetches
 *	bytes from that buffer, including the editor.
 *  SeeA: x-fetch-buffer
 */

DEFUN(doXstore, "x-store-buffer", FUNC_VISUAL, NULL)
{
	struct value	arg;
	struct string	*str;
	int		buf = 0;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a text string");
	str = gstring(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "an X cut buffer number");
		buf = gfixnum(arg.vl_data);
		if (buf < 0 || buf > 7)
			error("X cut buffers are numbered 0 through 7.");
	}
	XStoreBuffer(str->st_buffer, str->st_length, buf);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-fetch-buffer
 *  Call: (x-fetch-buffer [ 'buffer ])
 *  Retu: string
 *  Desc: This function returns the text stored in one of the X
 *	cut buffers.  If the optional argument is given, that
 *	particular buffer is used, instead of buffer zero.
 *	Only buffers numbered 0 through 7 are valid.
 *  SeeA: x-store-buffer
 */

DEFUN(doXfetch, "x-fetch-buffer", FUNC_VISUAL, NULL)
{
	struct value	arg, ret;
	char		*text;
	struct string	*str;
	int		nbytes, buf = 0;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "an X cut buffer number");
		buf = gfixnum(arg.vl_data);
		if (buf < 0 || buf > 7)
			error("X cut buffers are numbered 0 through 7.");
	}
	text = XFetchBuffer(&nbytes, buf);
	str = save_string(text, nbytes);
	free(text);

	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-get-focus
 *  Call: (x-get-focus)
 *  Retu: fixnum
 *  Desc: This function returns the X keyboard focus by returning
 *	the window number it is assigned to or nil if no window of the
 *	editor is the focus.
 *
 *	When an X window (here an editor window) is designated as the
 *	keyboard focus, keyboard input is sent to it regardless of
 *	where the mouse is on the display.
 *
 *	Even \sym{x-get-focus} returning nil, does not necessarily
 *	mean that no X client window holds the input focus, it simply
 *	means that no window of the VorTeX editor holds it.  Another
 *	client may have it and, unfortunately, it is not possible to
 *	discover this information, only to change it.
 *  SeeA: x-set-focus current-window
 */

DEFUN(doXgetfocus, "x-get-focus", FUNC_VISUAL, NULL)
{
	struct value	ret;
	struct window	*win;

	for (win = window_list; win != NULL; win = win->wi_next)
		if ((win->wi_flags & WIN_FOCUSED) != 0)
			break;
	if (win == NULL) {
		/* none of our wins is the focus */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, win->wi_index);
		return (ret);
	}
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-set-focus
 *  Call: (x-set-focus 'window)
 *  Retu: window
 *  Desc: This function sets the X keyboard focus to the specified window
 *	of the editor.  The specified window must be a currently active
 *	one or nil, specifying that the keyboard focus is to be released.
 *	The function returns its argument window number.
 *
 *	When an X window (here an editor window) is designated as the
 *	keyboard focus, keyboard input is sent to it regardless of
 *	where the mouse is on the display.
 *  Side: If the focus is given as nil, the input focus is changed to
 *	the X root window, no matter where it was.  Thus, calling
 *	\sym{x-set-focus} removes any existing input focus, whether
 *	held by a window of the editor or by another program.  This
 *	potentially useful side-effect is actually unavoidable since
 *	there is no way to tell what X window holds the current
 *	input focus.
 *  SeeA: x-get-focus current-window
 */

DEFUN(doXsetfocus, "x-set-focus", FUNC_VISUAL, NULL)
{
	struct value	arg, ret;
	struct window	*old, *new;
	int		index;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_FIXNUM:
		index = gfixnum(arg.vl_data);
		new = getwindow(index);
		break;
	case LISP_NIL:
		new = NULL;
		break;
	default:
		BADARGN(1, "a window number");
	}

	for (old = window_list; old != NULL; old = old->wi_next)
		if ((old->wi_flags & WIN_FOCUSED) != 0)
			break;

	if (new == NULL) {
		/* none of our windows is the focus */
		XFocusKeyboard(RootWindow);
		return (v_nil);
	} else if (new != old) {
		/* this window is no longer the focus */
		old->wi_flags &= ~WIN_FOCUSED;

		/* focus the given window */
		XFocusKeyboard(new->wi_xwindow);
		new->wi_flags |= WIN_FOCUSED;
	}

	/* return the window number */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, new->wi_index);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-get-geometry
 *  Call: (x-get-geometry)
 *  Retu: string
 *  Desc: This function returns the size and placement of the current
 *	window as an X geometry string.
 *
 *	The format of the string will be \em{=cols}x\em{rows+x+y} where
 *	\em{rows} and \em{cols} refer to the size of the entire current
 *	window and \em{xpos} and \em{ypos} are the position of the upper
 *	left corner of the window on the X display.
 *  SeeA: x-set-geometry current-window
 */

DEFUN(doXgetgeom, "x-get-geometry", FUNC_VISUAL, NULL)
{
	struct value	ret;
	struct string	*str;
	unsigned char	buf[64];

	CHECKAC(0, 0);

	sprintf(buf, "=%dx%d+%d+%d",
		current_window->wi_cols, current_window->wi_cols,
		current_window->wi_xpos, current_window->wi_ypos);
	str = save_string(buf, strlen(buf));
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-set-geometry
 *  Call: (x-set-geometry 'geometry)
 *  Retu: string
 *  Desc: This function changes the size and placement of the current
 *	window to conform to the geometry specification as given by
 *	the argument.
 *
 *	The geometry is made up of a string containing two or four
 *	numbers separated by single characters.  The string must
 *	start with an equal sign, then either part is optional.
 *	If present, the size portion must be next, made up of
 *	two numbers separated by an \lit{x} character.  The first
 *	represents the number of columns to make the window, and the
 *	seconds represents the number of rows.
 *
 *	The second portion specifies the placement on the X display of
 *	the upper left corner of the window.  Both numbers refer to the
 *	number of pixels as distance and may be prefixed by either a
 *	\lit{+} or a \lit{-}, meaning respectively ``positive from the
 *	upper left corner'' and ``negative from the lower right corner.''
 *	The X distance is specified first, then the Y distance.
 *
 *	The parts of the geometry string not specified are assumed to
 *	not be changed from the existing values.
 *  Side: If the geometry specified is not the same as the current
 *	geometry, the window will change size and/or placement and redraw
 *	itself.  However, only the current window will change.
 *  SeeA: x-get-geometry current-window
 */
static char	valid_chars[] = "=+-x01234567890";

DEFUN(doXsetgeom, "x-set-geometry", FUNC_VISUAL, "sNew geometry: ")
{
	extern char	*index();
	struct value	arg, ret;
	struct string	*str;
	unsigned char	buf[64];
	unsigned char	*cp;
	int		xpos, ypos, rows, cols;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a geometry string");
	makecstring(gstring(arg.vl_data), buf, sizeof (buf));
	for (cp = buf; *cp != '\0'; cp++)
		if (index(valid_chars, *cp) == NULL)
			break;
	if (*buf != '=' || *cp != '\0')
		error("Garbled geometry string; example: \"=80x24+100+100\".");

	/* get current values into four numbers */
	xpos = current_window->wi_xpos;
	ypos = current_window->wi_ypos;
	rows = current_window->wi_rows;
	cols = current_window->wi_cols;

	/* parse geometry string into four numbers */
	parse_geom(buf, &xpos, &ypos, &rows, &cols);

	/* check that the size in in bounds */
	if (rows < MINROWS)
		rows = MINROWS;
	if (cols < MINCOLS)
		cols = MINCOLS;

	/* set the window to this size and position */
	XConfigureWindow(current_window->wi_xwindow,
			 xpos, ypos,
			 GETWIDTH(current_window, cols),
			 GETHEIGHT(current_window, rows));

	sprintf(buf, "=%dx%d+%d+%d", cols, rows, xpos, ypos);
	str = save_string(buf, strlen(buf));
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-raise-window
 *  Call: (x-raise-window [ 'window ])
 *  Retu: nil
 *  Desc: This function causes the specified window to ``pop above''
 *	all other X clients (and other windows of the editor) on the
 *	X display.  That is, it will not be obscured by any other
 *	client window immediately after this action.  If no window number
 *	is given, the current window is used.
 *  Side: This may cause parts of the window to be repainted as they
 *	are exposed and may also obscure portions of other X windows.
 *	However, the action should be intuitively obvious.
 *  SeeA: x-lower-window current-window
 */

DEFUN(doXraise, "x-raise-window", FUNC_VISUAL, "")
{
	struct value	arg;
	int		wnum;
	struct window	*win = window_list;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a window number");
		wnum = gfixnum(arg.vl_data);
		win = getwindow(wnum);
	}
	XRaiseWindow(win->wi_xwindow);
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-lower-window
 *  Call: (x-lower-window [ 'window ])
 *  Retu: nil
 *  Desc: This function moves the current window ``below'' all other
 *	X clients (and other exitor windows) so that it obscures no
 *	other X window.  If no window number is given, the current
 *	window is assumed.
 *  Side: This may cause other X client windows to become exposed and
 *	may also cause all or part of the affected window to be obscured.
 *	However, the action should be intuitively obvious.
 *  SeeA: x-raise-window current-window
 */

DEFUN(doXlower, "x-lower-window", FUNC_VISUAL, "")
{
	struct value	arg;
	int		wnum;
	struct window	*win = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a window number");
		wnum = gfixnum(arg.vl_data);
		win = getwindow(wnum);
	}
	XLowerWindow(win->wi_xwindow);
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-get-border-width
 *  Call: (x-get-border-width [ 'win ])
 *  Retu: width
 *  Desc: This function returns the width of the border of the given
 *	window (or the current window of none is specified).
 *	The width is specified in pixels.
 *  SeeA: x-set-border-width get-internal-border
 */

DEFUN(doXgetbdwidth, "x-get-border-width", FUNC_VISUAL, NULL)
{
	struct value	arg, ret;
	int		wnum;
	struct window	*win = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a win number");
		wnum = gfixnum(arg.vl_data);
		win = getwindow(wnum);
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_bdwidth);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: x-set-border-width
 *  Call: (x-set-border-width 'width [ 'win ])
 *  Retu: width
 *  Desc: This function changes the width of the X window border for
 *	the specified win (or the current win if none is given).
 *	The first argument should evaluate to a fixnum which specifies
 *	the width of the X window border in pixels.
 *  Side: To implement this, the X window needs to be destroyed and
 *	created with the new border width.  Thus, the win will
 *	appear to clear and repaint.
 *  SeeA: x-get-border-width set-internal-border
 */

DEFUN(doXsetbdwidth, "x-set-border-width", FUNC_VISUAL, "nNew border width: ")
{
	extern Pixmap	GrayPixmap;
	extern int	WINDOW_EVENTS;
	struct value	arg, ret;
	int		wnum, width;
	struct window	*win;
	Pixmap		border;
	Window		new;

	CHECKAC(1, 2);
	if (!fixnump(arg))
		BADARGN(1, "a positive fixnum");
	width = gfixnum(arg.vl_data);
	if (width < 0)
		width = 0;
	arg = EVALARGN(1);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a window number");
		wnum = gfixnum(arg.vl_data);
		win = getwindow(wnum);
	}

	if (width != win->wi_bdwidth) {
		PROTECT();
		/* change the border width */
		if (win == current_window)
			border = win->wi_bdpixmap;
		else
			border = GrayPixmap;
		new = XCreateWindow(RootWindow,
				    win->wi_xpos, win->wi_ypos,
				    win->wi_width, win->wi_height,
				    width, border,
				    win->wi_bgpixmap);
		if (new == NULL) {
			error(
		    "Can't create new X window to change border width!");
		}
		win->wi_bdwidth = width;
		XSelectInput(new, WINDOW_EVENTS);
		XMapWindow(new);
		XSync(FALSE);
		XDestroyWindow(win->wi_xwindow);
		win->wi_xwindow = new;
		XClear(win->wi_xwindow);
		UNPROTECT();
	}

	/* return current border width */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_bdwidth);
	return (ret);
}
