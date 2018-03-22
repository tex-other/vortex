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
 *  RCS Info: $Header: recenter.c,v 0.1 87/05/01 12:26:35 john Locked $
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
 *  recenter.c - the recenter command function and routine
 */
static char _ID[] = "@(#)recenter.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: recenter
 *  Call: (recenter [ 'line ])
 *  Retu: offset
 *  Desc: This function moves the current window's offset (it's
 *	view of the source buffer) so that the buffer's point is
 *	visible on the specified line.  If the argument is not
 *	given, the center line of the screen is assumed.
 *  SeeA: scroll-forward scroll-backward next-line previous-line
 */

DEFUN(dorecenter, "recenter", FLAG_NONE, "p")
{
	struct value	arg, ret;
	int		line, offset;
	struct window	*winp = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a line count");
		line = gfixnum(arg.vl_data);
	} else {
		/* just use half the height */
		line = winp->wi_rows / 2;
	}

	offset = centerpoint(winp, line);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

/*
 *  This function implements the general case for recenter, it
 *  puts the line containing point at the line on the current
 *  window's screen specified by the argument.
 */
	
centerpoint(winp, count)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;
	struct tblock	*next, *last;
	unsigned long	*txp, *beg;
	register int	offset;

	/* get associated buffer */
	if (winp->wi_type != WIN_BUFFER)
		return (-1);
	bufp = winp->wi_buffer;
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* make sure the count is reasonable */
	if (count < 1)
		count = 1;
	if (count > winp->wi_rows)
		count = winp->wi_rows;

	debug(DPAINT, "Centering window %d of buffer %Y on line %d.",
	      winp->wi_index, bufp->bu_name, count);

	/* place the buffer X position at point */
	offset = winp->wi_point;

	/* find block containing the offset in buffer */
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > winp->wi_cstart)
			break;
		last = next;
	}
	if (next == NULL)
		next = last;
	if (next == NULL)
		return (offset);

	beg = next->tb_text;
	txp = next->tb_text + (offset - next->tb_offset);
	while (count-- > 0 && next != NULL) {
		/* search backwards for a newline */
		do {
			if (--txp < beg) {
				next = next->tb_prev;
				if (next != NULL) {
					beg = next->tb_text;
					txp = next->tb_text +
					    next->tb_length - 1;
				}
			}
			offset--;
		} while (next != NULL && charof(*txp) != '\n');
	}
	offset++;
	if (offset < 0)
		offset = 0;
	if (offset >= srcp->sb_length)
		offset = srcp->sb_length ;

	/* set to this offset */
	winp->wi_cstart = offset;
	winp->wi_flags |= WIN_REFRESH;

	debug(DPAINT, "Moved window %d's text offset to %d.",
	      winp->wi_index, offset);
	return (offset);
}
