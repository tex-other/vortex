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
 *  RCS Info: $Header: move.c,v 0.1 87/05/01 12:22:07 john Locked $
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
 *  move.c - manipulate point and mark in buffers
 */
static char _ID[] = "@(#)move.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: point
 *  Call: (point [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function reports the position of point in the given
 *	buffer, or in the current buffer if no argument is given.
 *	Point is represented on the screen as the place (between
 *	characters) at the left edge of the text cursor.  This
 *	position is reported as a fixnum, which an offset into
 *	the buffer.
 *
 *	All insertions and (most) deletions occur relative to point.
 *	Point is separate in each buffer, moving point in one
 *	buffer doesn't change it in any other.  What the movement
 *	commands do is move point, since it is synonymous with
 *	the ``cursor position.''
 *  SeeA: set-point window-point mark
 */

DEFUN(dopointpos, "point", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	int		offset;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}
	offset = getpoint(bufp);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

getpoint(bufp)
	struct buffer	*bufp;
{
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("There is no point in this type of buffer.");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	return (srcp->sb_point);
}

/*
 *  DOCUMENTATION
 *
 *  Name: set-point
 *  Call: (set-point 'position [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function sets the position of point in the given
 *	buffer, or in the current buffer if no argument is given.
 *	Point is represented on the screen as the place (between
 *	characters) at the left edge of the text cursor.  This
 *	position is a fixnum, which an offset into the buffer.
 *
 *	The position is in a range from zero to the length of the
 *	buffer.  An error occurs if a negative position is given,
 *	and if a position larger than the length of the buffer is
 *	given, the position is truncated silently.  The actual
 *	position set is returned.
 *  SeeA: point set-window-point
 */

DEFUN(dosetpoint, "set-point", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp;
	int		offset;

	CHECKAC(1, 2);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		/* just use current buffer */
		bufp = current_buffer;
	}
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum position");
	offset = gfixnum(arg.vl_data);

	setpoint(bufp, offset);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

setpoint(bufp, offset)
	struct buffer	*bufp;
{
	struct source	*srcp;

	ASSERT(bufp != NULL);
	if (bufp->bu_type != BUFF_SOURCE)
		error("There is no point in this type of buffer.");

	/* get source buffer information */
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* make sure the given offset is valid */
	if (offset < srcp->sb_start)
		error("Can't set point before beginning of buffer!");
	if (offset > srcp->sb_length)
		offset = srcp->sb_length;
	srcp->sb_point = offset;

	return (offset);
}

/*
 *  DOCUMENTATION
 *
 *  Name: mark
 *  Call: (mark [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function reports the position of mark in the given
 *	buffer, or in the current buffer if no argument is given.
 *	Mark is not represented visually on the screen, but is
 *	always set by the user, so he should know where it is.
 *	The position is reported as a fixnum, an offset into
 *	the buffer, which is returned.
 *
 *	The mark is usually used to specify a region along with
 *	point (the current cursor position).  Usually, the mark
 *	is set to point in a specific buffer with \sym{set-mark}.
 *	The mark is separate for each buffer, setting it in one
 *	buffer doesn't affect any other buffer.
 *
 *	If the mark is unset in this buffer, nil is returned.
 *  SeeA: set-mark window-mark point
 */

DEFUN(domarkpos, "mark", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	int		offset;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}

	if ((offset = getmark(bufp)) < 0) {
		/* no mark set */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, getmark(bufp));
		return (ret);
	}
}

getmark(bufp)
	struct buffer	*bufp;
{
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("There is no mark in this type of buffer.");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	return (srcp->sb_mark);
}

/*
 *  DOCUMENTATION
 *
 *  Name: set-mark
 *  Call: (set-mark 'position [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function sets the position of the mark in the given
 *	buffer, or in the current buffer if no argument is given.
 *	Mark is not represented visually on the screen, but is
 *	usually set by the user.  This position is a fixnum, which
 *	an offset into the buffer.
 *
 *	The position is in a range from zero to the length of the
 *	buffer.  An error occurs if a negative position is given,
 *	and if a position larger than the length of the buffer is
 *	given, the position is truncated silently.  The actual
 *	position set is returned.
 *
 *	If nil or a negative number is given for the position, the
 *	mark is unset.  The function returns nil in this case.
 *  SeeA: mark set-window-mark
 */

DEFUN(dosetmark, "set-mark", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp;
	struct source	*srcp;
	int		offset;

	CHECKAC(1, 2);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		/* just use current buffer */
		bufp = current_buffer;
	}
	arg = EVALARGN(1);
	if (nullp(arg))
		offset = -1;
	else if (fixnump(arg))
		offset = gfixnum(arg.vl_data);
	else
		BADARGN(1, "a fixnum position");

	/* try setting the mark now */
	offset = setmark(bufp, offset);
	if (offset < 0) {
		/* no mark set in the buffer */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, offset);
		return (ret);
	}
}

setmark(bufp, offset)
	struct buffer	*bufp;
{
	struct source	*srcp;

	/* get source buffer information */
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	if (offset >= 0 && offset < srcp->sb_start)
		offset = srcp->sb_start;
	if (offset > srcp->sb_length)
		offset = srcp->sb_length;
	srcp->sb_mark = offset;

	return (offset);
}

/*
 *  DOCUMENTATION
 *
 *  Name: window-point
 *  Call: (window-point [ 'window ])
 *  Retu: fixnum
 *  Desc: Returns the point value for the given window.  That is, the
 *	value point would have were this window the selected window.
 *  SeeA: set-window-point point window-mark
 */

DEFUN(dowindowpoint, "window-point", FLAG_NONE, NULL)
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
	(void)windowbuffer(win, BUFF_SOURCE);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_point);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: set-window-point
 *  Call: (set-window-point 'offset [ 'window ])
 *  Retu: fixnum
 *  Desc: Sets the value of point in the selected window to the offset
 *	given.  Note that this value should be valid for the given
 *	buffer's size.
 *  SeeA: window-point point window-mark
 */

DEFUN(dosetwindowpoint, "set-window-point", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct window	*win = current_window;
	struct buffer	*bufp;
	struct source	*srcp;
	int		offset;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "an offset");
	offset = gfixnum(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a window number");
		win = getwindow(gfixnum(arg.vl_data));
	}

	/* set the offset is given */
	offset = setwindowpoint(win, offset);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, win->wi_point);
	return (ret);
}

setwindowpoint(winp, offset)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;
	register int	over;
	struct tblock	*last, *next;
	unsigned long	*beg, *txp;

	ASSERT(winp != NULL);
	bufp = windowbuffer(winp, BUFF_SOURCE);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* make sure the value is okay for the buffer */
	if (offset < srcp->sb_start)
		offset = srcp->sb_start;
	if (offset > srcp->sb_length)
		offset = srcp->sb_length;

	/* set it in the window only if different */
	if (winp->wi_point != offset) {
		winp->wi_point = offset;
		winp->wi_flags |= WIN_REFRESH;
	}
	if (srcp->sb_point != offset) {
		srcp->sb_point = offset;
		bufp->bu_flags |= BUFF_CHANGED;
	}

	/* find block containing offset in buffer */
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
		last = next;
	}
	if (next == NULL)
		next = last;

	/* find current column position */
	if (next == NULL) {
		/* at the beginning of the buffer */
		winp->wi_curcol = 0;
	} else {
		/* find distance past beginning of line */
		over = 0;
		last = next;
		beg = last->tb_text;
		txp = last->tb_text + (offset - last->tb_offset) - 1;
		while (last != NULL && charof(*txp) != '\n') {
			if (--txp < beg) {
				last = last->tb_prev;
				if (last == NULL)
					break;
				beg = last->tb_text;
				txp = last->tb_text + last->tb_length - 1;
			}
			over++;
		}
		winp->wi_curcol = over;
	}

	return (offset);
}

/*
 *  DOCUMENTATION
 *
 *  Name: window-mark
 *  Call: (window-mark [ 'window ])
 *  Retu: fixnum
 *  Desc: Returns the mark value for the given window.  That is, the
 *	value point would have were this window the selected window.
 *	If no mark is set, nil is returned.
 *  SeeA: point set-window-mark
 */

DEFUN(dowindowmark, "window-mark", FLAG_NONE, NULL)
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
	(void)windowbuffer(win, BUFF_SOURCE);

	if (win->wi_mark < 0) {
		/* no mark set */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, win->wi_mark);
		return (ret);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: set-window-mark
 *  Call: (set-window-mark 'offset [ 'window ])
 *  Retu: fixnum
 *  Desc: Sets the value of mark in the selected window to the offset
 *	given.  Note that this value should be valid for the given
 *	buffer's size.
 *
 *	If a nil or a negative number is given for the position, the
 *	mark is unset in this window.  Nil is returned in this case.
 *  SeeA: mark window-mark window-point
 */

DEFUN(dosetwindowmark, "set-window-mark", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct window	*win = current_window;
	struct buffer	*bufp;
	struct source	*srcp;
	int		offset;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (nullp(arg))
		offset = -1;
	else if (fixnump(arg))
		offset = gfixnum(arg.vl_data);
	else
		BADARGN(1, "an offset");
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a window number");
		win = getwindow(gfixnum(arg.vl_data));
	}

	/* set the offset given */
	offset = setwindowmark(win, offset);
	if (offset < 0) {
		/* window's mark is unset */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, win->wi_point);
		return (ret);
	}
}

setwindowmark(winp, offset)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;

	ASSERT(winp != NULL);
	bufp = windowbuffer(winp, BUFF_SOURCE);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* make sure the value is okay for the buffer */
	if (offset >= 0 && offset < srcp->sb_start)
		offset = srcp->sb_start;
	if (offset > srcp->sb_length)
		offset = srcp->sb_length;

	/* set it in the window and buffer */
	winp->wi_mark = offset;

	return (offset);
}
