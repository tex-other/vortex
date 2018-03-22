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
 *  RCS Info: $Header: nextline.c,v 0.1 87/05/01 12:22:25 john Locked $
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
 *  nextline.c - next-line and previous-line functions
 */
static char _ID[] = "@(#)nextline.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: next-line
 *  Call: (next-line [ 'count ])
 *  Retu: offset
 *  Desc: This function moves point down to the next line, keeping it
 *	in the same relative place on the line.  If the argument is
 *	given, it specifies the number of lines to move down.  It
 *	returns the new offset from the beginning of the buffer of
 *	the point.
 *  SeeA: previous-line scroll-forward
 */

DEFUN(donextline, "next-line", FLAG_NONE, "p")
{
	struct value	arg, ret;
	int		count = 1, offset;
	struct window	*winp = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (fixnump(arg))
			count = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(1, "a line count");
	}

	offset = nextline(winp, count);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

nextline(winp, count)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;
	struct tblock	*next, *last;
	unsigned long	*txp, *beg, *end;
	register int	offset;
	register int	over = 0;

	/* call prevline if we're to go up */
	if (count < 0)
		return prevline(winp, -count);

	bufp = windowbuffer(winp, BUFF_SOURCE);
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* find block containing offset in buffer */
	offset = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
		last = next;
	}
	if (next == NULL)
		next = last;
	if (next == NULL)
		return (offset);

	/* skip down count lines */
	end = next->tb_text + next->tb_length;
	txp = next->tb_text + (offset - next->tb_offset) - 1;
	while (count-- > 0 && next != NULL) {
		/* search forwards for a newline */
		do {
			if (++txp >= end) {
				next = next->tb_next;
				if (next != NULL) {
					end = next->tb_text + next->tb_length;
					txp = next->tb_text;
				}
			}
			offset++;
		} while (next != NULL && charof(*txp) != '\n');
	}

	over = winp->wi_curcol;
	if (next != NULL && over > 0) {
		/* skip over from beginning of line */
		do {
			if (++txp >= end) {
				next = next->tb_next;
				if (next != NULL) {
					end = next->tb_text + next->tb_length;
					txp = next->tb_text;
				}
			}
			over--;
			offset++;
		} while (over > 0 && next != NULL && charof(*txp) != '\n');
		if (charof(*txp) == '\n')
			offset--;
	}

	/* make sure the given offset is in the buffer */
	if (offset < srcp->sb_start)
		offset = srcp->sb_start;
	else if (offset > srcp->sb_length)
		offset = srcp->sb_length;

	/* set the window's point to this offset */
	if (winp->wi_point != offset) {
		winp->wi_point = offset;
		winp->wi_flags |= WIN_REFRESH;
	}
	srcp->sb_point = offset;
	return (offset);
}

/*
 *  DOCUMENTATION
 *
 *  Name: previous-line
 *  Call: (previous-line [ 'count ])
 *  Retu: offset
 *  Desc: This function moves point up to the previous line, keeping it
 *	in the same relative place on the line.  If the argument is
 *	given, it specifies the number of lines to move up.  It
 *	returns the new offset from the beginning of the buffer of
 *	the point.
 *  SeeA: next-line scroll-backward
 */

DEFUN(doprevline, "previous-line", FLAG_NONE, "p")
{
	struct value	arg, ret;
	int		count = 1, offset;
	struct window	*winp = current_window;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (fixnump(arg))
			count = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(1, "a line count");
	}

	offset = prevline(winp, count);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, offset);
	return (ret);
}

prevline(winp, count)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;
	struct tblock	*last, *next;
	unsigned long	*txp, *beg, *end;
	register int	offset;
	register int	over = 0;

	/* call nextline if we're to go down */
	if (count < 0)
		return nextline(winp, -count);

	bufp = windowbuffer(winp, BUFF_SOURCE);
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* find block containing offset in buffer */
	offset = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
		last = next;
	}
	if (next == NULL)
		next = last;
	if (next == NULL)
		return (offset);

	/* we are examining the character before the cursor */
	beg = next->tb_text;
	txp = next->tb_text + (offset - next->tb_offset);
	while (count-- >= 0 && next != NULL) {
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

	over = winp->wi_curcol;
	if (over > 0 && offset >= 0) {
		if (next == NULL) {
			/* we're at the beginning of the buffer */
			next = srcp->sb_text;
			txp = next->tb_text;
		}

		/* skip over from beginning of line */
		end = next->tb_text + next->tb_length;
		if (++txp >= end) {
			next = next->tb_next;
			if (next != NULL) {
				end = next->tb_text + next->tb_length;
				txp = next->tb_text;
			}
		}
		while (over > 0 && next != NULL && charof(*txp) != '\n') {
			if (++txp >= end) {
				next = next->tb_next;
				if (next != NULL) {
					end = next->tb_text + next->tb_length;
					txp = next->tb_text;
				}
			}
			over--;
			offset++;
		}
	}

	/* make sure the given offset is in the buffer */
	if (offset < srcp->sb_start)
		offset = srcp->sb_start;
	else if (offset > srcp->sb_length)
		offset = srcp->sb_length;

	/* set the window's point to this offset */
	if (winp->wi_point != offset) {
		winp->wi_point = offset;
		winp->wi_flags |= WIN_REFRESH;
	}
	srcp->sb_point = offset;
	return (offset);
}
