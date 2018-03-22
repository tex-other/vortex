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
 *  RCS Info: $Header: scroll.c,v 0.1 87/05/01 12:26:49 john Locked $
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
 *  scroll.c - the scrolling commands (vLisp functions)
 */
static char _ID[] = "@(#)scroll.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: scroll-forward
 *  Call: (scroll-forward [ 'lines ])
 *  Retu: offset
 *  Desc: This function scrolls the window up (going forward
 *	in the buffer) by the specified number of lines.  If
 *	no argument is given, the height of the window, minus
 *	one, is assumed.  If returns the new offset in the buffer
 *	of the top left corner of the window.
 *  SeeA: scroll-backward recenter
 */

DEFUN(doscrollfwd, "scroll-forward", FLAG_NONE, "P")
{
	struct value	arg, ret;
	int		lines = -1, offset;
	struct window	*winp = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (fixnump(arg))
			lines = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(1, "a line count");
	}
	if (lines <= 0)
		lines = winp->wi_rows - 1;

	offset = scrollfwd(winp, lines);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: scroll-backward
 *  Call: (scroll-backward [ 'lines ])
 *  Retu: offset
 *  Desc: This function scrolls the window up (going backward
 *	in the buffer) by the specified number of lines.  If
 *	no argument is given, the height of the window, minus
 *	one, is assumed.  If returns the new offset in the buffer
 *	of the top left corner of the window.
 *  SeeA: scroll-forward recenter
 */

DEFUN(doscrollback, "scroll-backward", FLAG_NONE, "P")
{
	struct value	arg, ret;
	int		lines = -1, offset;
	struct window	*winp = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (fixnump(arg))
			lines = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(1, "a line count");
	}
	if (lines <= 0)
		lines = winp->wi_rows - 1;

	offset = scrollback(winp, lines);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

/*
 *  Scroll the window forward by lines.  This really means moving the
 *  offset of the buffer in the window forward to the next newline
 *  or the end of the buffer.  The count argument is the number of
 *  lines by which to scroll forward.
 */

scrollfwd(wind, count)
	struct window	*wind;
{
	struct buffer	*bufp;
	struct source	*srcp;
	struct tblock	*next;
	unsigned long	*txp, *end;
	register int	offset;

	/* get associated buffer */
	if (wind->wi_type != WIN_BUFFER)
		return (-1);
	bufp = wind->wi_buffer;
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	if (count < 0)
		return scrollback(wind, -count);
	if (count <= 0)
		return (wind->wi_cstart);

	debug(DPAINT, "Scrolling buffer %Y forward %d lines.",
	      bufp->bu_name, count);

	/* make sure its at least in the buffer */
	if (wind->wi_cstart > srcp->sb_length)
		wind->wi_cstart = srcp->sb_length - 1;

	/* find block containing bufxpos in buffer */
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > wind->wi_cstart)
			break;
	}
	offset = wind->wi_cstart;
	if (next == NULL)
		return (offset);

	PROTECT();
	end = next->tb_text + next->tb_length;
	txp = next->tb_text + (offset - next->tb_offset);
	while (count-- > 0 && next != NULL) {
		/* search forwards for a newline */
		do {
			if (++txp >= end) {
				next = next->tb_next;
				if (next == NULL)
					break;
				end = next->tb_text + next->tb_length;
				txp = next->tb_text - 1;
			}
			offset++;
		} while (charof(*txp) != '\n');
	}
	wind->wi_cstart = offset;
	wind->wi_flags |= WIN_REFRESH;
	UNPROTECT();

	debug(DPAINT, "Moved window %d offset forward to %d.",
	      wind->wi_index, offset);
	return (offset);
}

/*
 *  Scroll the window back by lines.  This really means moving the
 *  offset of the window in the buffer back to the previous newline
 *  or the beginning of the buffer.  The count argument is the number
 *  of lines by which to scroll back.
 */

scrollback(wind, count)
	struct window	*wind;
{
	struct buffer	*bufp;
	struct source	*srcp;
	struct tblock	*next;
	unsigned long	*txp, *beg;
	register int	offset;

	/* get associated buffer */
	if (wind->wi_type != WIN_BUFFER)
		return (-1);
	bufp = wind->wi_buffer;
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	if (count < 0)
		return scrollfwd(wind, -count);
	if (count <= 0)
		return (wind->wi_cstart);

	debug(DPAINT, "Scrolling window %d of buffer %Y back %d lines.",
	      wind->wi_index, bufp->bu_name, count);

	/* make sure its at least in the buffer */
	if (wind->wi_cstart > srcp->sb_length)
		wind->wi_cstart = srcp->sb_length;

	/* find block containing bufxpos in buffer */
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > wind->wi_cstart)
			break;
	}
	offset = wind->wi_cstart;
	if (next == NULL)
		return (offset);

	PROTECT();
	beg = next->tb_text;
	txp = next->tb_text + (offset - next->tb_offset) - 1;
	while (count-- > 0 && next != NULL) {
		/* search backwards for a newline */
		do {
			if (--txp < beg) {
				next = next->tb_prev;
				if (next == NULL)
					break;
				beg = next->tb_text;
				txp = next->tb_text + next->tb_length;
			}
			offset--;
		} while (charof(*txp) != '\n');
	}
	wind->wi_cstart = offset;
	wind->wi_flags |= WIN_REFRESH;
	UNPROTECT();

	debug(DPAINT, "Moved window %d offset back to %d.",
	      wind->wi_index, offset);
	return (offset);
}
