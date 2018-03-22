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
 *  RCS Info: $Header: copy.c,v 0.1 87/05/01 11:28:35 john Locked $
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
 *  copy.c - functions to copy text from a buffer
 */
static char _ID[] = "@(#)copy.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: copy-chars
 *  Call: (copy-chars 'count [ 'buffer ])
 *  Retu: string
 *  Desc: This function collects the given number of characters from
 *	point in the buffer specified, or the current buffer if no
 *	second argument is present and returns a string containing
 *	them.
 *
 *	The count specifies not only the number of characters to
 *	be copied, but also the direction relative to point.  If
 *	the count is negative, characters are copied from before point,
 *	if positive, characters are copied from after point.  And,
 *	if the count is zero, nothing is done and an empty string
 *	is returned.
 *  SeeA: copy-region
 */

DEFUN(docopychars, "copy-chars", FLAG_NONE, NULL)
{
	struct string	*copy_chars();
	struct value	arg, ret;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	struct string	*text;
	int		count;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum count");
	count = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (stringp(arg)) {
			/* just copy from the buffer */
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
			winp = NULL;
		} else if (fixnump(arg)) {
			/* copy from the window/buffer */
			winp = getwindow(gfixnum(arg.vl_data));
			bufp = NULL;
		} else {
			/* not a buffer or window */
			BADARGN(2, "a buffer or window");
		}
	}
	if (bufp == NULL) {
		bufp = windowbuffer(winp, BUFF_SOURCE);

		/* set the buffer's point and mark */
		setpoint(bufp, winp->wi_point);
		setmark(bufp, winp->wi_mark);
	}

	text = copy_chars(bufp, count);
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, text);
	return (ret);
}

struct string *
copy_chars(bufp, nchars)
	struct buffer	*bufp;
{
	extern struct string	*copy_region();
	struct source		*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't copy characters from this type of buffer.");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	return copy_region(bufp, srcp->sb_point, srcp->sb_point + nchars);
}

/*
 *  DOCUMENTATION
 *
 *  Name: copy-region
 *  Call: (copy-region 'minpos 'maxpos [ 'buffer ])
 *  Retu: t
 *  Desc: This function collects the region of characters given
 *	from the buffer specified, or from the current buffer if no
 *	second argument is present and returns them.
 *
 *	When invoked interactively, the region formed by point
 *	and mark in the current buffer is passed automatically
 *	as the region argument.  This means that all the text
 *	between point and mark will be copied.
 *  SeeA: copy-chars
 */

DEFUN(docopyregion, "copy-region", FLAG_NONE, NULL)
{
	struct string	*copy_region();
	struct value	arg, ret;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	struct string	*text;
	int		minpos, maxpos;

	CHECKAC(2, 3);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum position");
	minpos = gfixnum(arg.vl_data);
	arg = EVALARGN(2);
	if (!fixnump(arg))
		BADARGN(2, "a fixnum position");
	maxpos = gfixnum(arg.vl_data);

	if (GETACOUNT() > 2) {
		arg = EVALARGN(3);
		if (stringp(arg)) {
			/* just copy from the buffer */
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
			winp = NULL;
		} else if (fixnump(arg)) {
			/* copy from the window/buffer */
			winp = getwindow(gfixnum(arg.vl_data));
			bufp = NULL;
		} else {
			/* not a buffer or window */
			BADARGN(3, "a buffer or window");
		}
	}
	if (bufp == NULL) {
		bufp = windowbuffer(winp, BUFF_SOURCE);

		/* set the buffer's point and mark */
		setpoint(bufp, winp->wi_point);
		setmark(bufp, winp->wi_mark);
	}

	text = copy_region(bufp, minpos, maxpos);
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, text);
	return (ret);
}

struct string *
copy_region(bufp, min, max)
	struct buffer	*bufp;
{
	struct string		*str;
	register unsigned char	*sptr, *send;
	register struct tblock	*tbp;
	register unsigned long	*txp, *tend;
	struct source		*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't copy text from this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* put the arguments in order */
	if (max < min) {
		int	tmp;

		tmp = min;
		min = max;
		max = tmp;
	}

	/* make sure min and max are reasonable */
	if (min < srcp->sb_start)
		min = srcp->sb_start;
	if (max > srcp->sb_length)
		max = srcp->sb_length;
	if (min >= max) {
		/* no characters in this range, even */
		return gstring(v_null.vl_data);
	}

	/* make a string long enough for the buffer */
	str = save_string(NULL, max - min);
	sptr = str->st_buffer;
	send = sptr + str->st_length;

	/* find the block containing the start */
	for (tbp = srcp->sb_text; tbp != NULL; tbp = tbp->tb_next) {
		if (min < tbp->tb_offset + tbp->tb_length)
			break;
	}
	ASSERT(tbp != NULL);

	/* start copying here */
	tend = tbp->tb_text + tbp->tb_length;
	txp = tbp->tb_text + (min - tbp->tb_offset);
	do {
		while (sptr < send && txp < tend)
			*sptr++ = charof(*txp++);
		tbp = tbp->tb_next;
		if (tbp != NULL) {
			tend = tbp->tb_text + tbp->tb_length;
			txp = tbp->tb_text;
		}
	} while (tbp != NULL && sptr < send);

	return (str);
}
