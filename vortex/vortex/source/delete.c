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
 *  RCS Info: $Header: delete.c,v 0.1 87/05/01 11:31:37 john Locked $
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
 *  delete.c - routines and functions to delete from a buffer
 */
static char _ID[] = "@(#)delete.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"
#include "gl_comm.h"
#include "ts_comm.h"

/*
 *  DOCUMENTATION
 *
 *  Name: delete-forward-character
 *  Call: (delete-forward-character [ 'count [ 'buffer | 'window ] ])
 *  Retu: count
 *  Desc: This funtion deletes the specified number of characters
 *	after point in the current buffer.  It returns the number
 *	of characters delted, one by default.  If the option second
 *	argument is given, it specifies the buffer in which to delete
 *	rather than the current buffer.
 *  SeeA: delete-backward-character delete-region
 */

DEFUN(dodelforward, "delete-forward-character", FLAG_NONE, "p")
{
	struct value	arg, ret;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	int		count = 1;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a fixnum count");
		count = gfixnum(arg.vl_data);
	}
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (stringp(arg)) {
			/* just insert into the buffer */
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
			winp = NULL;
		} else if (fixnump(arg)) {
			/* insert into the window/buffer */
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

	if (count == 0)
		return (v_zero);

	/* perform the deletion */
	delete_chars(bufp, count);

	/* update the window's point and mark */
	if (winp != NULL) {
		winp->wi_point = getpoint(bufp);
		winp->wi_mark = getmark(bufp);
		winp->wi_flags |= WIN_REFRESH;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: delete-backward-character
 *  Call: (delete-backward-character [ 'count [ 'buffer | 'window ] ])
 *  Retu: count
 *  Desc: This funtion deletes the specified number of characters
 *	before point in the current buffer.  It returns the number
 *	of characters delted, one by default.  If the option second
 *	argument is given, it specifies the buffer in which to delete
 *	rather than the current buffer.
 *  SeeA: delete-forward-character delete-region
 */

DEFUN(dodelbackward, "delete-backward-character", FLAG_NONE, "p")
{
	struct value	arg, ret;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	int		count = 1;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a fixnum count");
		count = gfixnum(arg.vl_data);
	}
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (stringp(arg)) {
			/* just insert into the buffer */
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
			winp = NULL;
		} else if (fixnump(arg)) {
			/* insert into the window/buffer */
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

	if (count == 0)
		return (v_zero);

	/* perform the deletion */
	delete_chars(bufp, -count);

	/* update the window's point and mark */
	if (winp != NULL) {
		winp->wi_point = getpoint(bufp);
		winp->wi_mark = getmark(bufp);
		winp->wi_flags |= WIN_REFRESH;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

delete_chars(bufp, count)
	struct buffer	*bufp;
{
	struct source	*srcp;
	int		min, max;

	ASSERT(bufp != NULL);
	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't delete characters from this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* calculate region to delete */
	if (count == 0) {
		/* there's nothing for us to do */
		return (0);
	} else if (count < 0) {
		min = srcp->sb_point + count;
		max = srcp->sb_point;
	} else {
		min = srcp->sb_point;
		max = srcp->sb_point + count;
	}

	if (min < srcp->sb_start) {
		message("Can't delete before start of buffer!");
		return (0);
	}
	if (max > srcp->sb_length) {
		message("Can't delete past end of buffer!");
		return (0);
	}

	return delete_region(bufp, min, max);
}

/*
 *  DOCUMENTATION
 *
 *  Name: delete-region
 *  Call: (delete-region 'minpos 'maxpos [ 'buffer | 'window ])
 *  Retu: fixnum
 *  Desc: This function deletes the region of characters given
 *	from the buffer specified, or from the current buffer if no
 *	second argument is present.  The function returns the
 *	number of characters deleted.
 *
 *	When invoked interactively, the region formed by point
 *	and mark in the current buffer is passed automatically
 *	as the region argument.  This means that all the text
 *	between point and mark will be deleted.
 *
 *	Deleted characters are gone forever, they are not saved
 *	anywhere.  Use the kill functions to delete characters
 *	in a manner such that they may be later retrieved.  The
 *	``kill'' analogue to \sym{delete-region} is \sym{kill-region}.
 *  Side: The buffer becomes modified, the deleted characters
 *	are removed from the buffer and are gone forever.
 *  SeeA: delete-chars kill-chars
 */

DEFUN(dodelregion, "delete-region", FLAG_NONE, "r")
{
	struct value	arg, ret;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	int		minpos, maxpos, nchars;

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
			/* just insert into the buffer */
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
			winp = NULL;
		} else if (fixnump(arg)) {
			/* insert into the window/buffer */
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

	/* perform the deletion */
	nchars = delete_region(bufp, minpos, maxpos);

	/* update the window's point and mark */
	if (winp != NULL) {
		winp->wi_point = getpoint(bufp);
		winp->wi_mark = getmark(bufp);
		winp->wi_flags |= WIN_REFRESH;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, nchars);
	return (ret);
}

delete_region(bufp, minpos, maxpos)
	struct buffer	*bufp;
{
	struct tblock	*next, *last;
	register int	nchars, deleted;
	unsigned long	*fp, *tp, *end, *begin;
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't delete text from this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* quick check to make sure there is something to do */
	if (minpos == maxpos)
		return (0);

	/* can't do it if buffer is read-only */
	if (bufp->bu_flags & BUFF_READONLY) {
		/* cause an error here, no chaing this buffer */
		error("Can't delete from %Y; buffer is read-only.",
		      bufp->bu_name);
	}

	/* check that arguments are in bounds */
	if (minpos > maxpos) {
		int	temp;

		temp = minpos;
		minpos = maxpos;
		maxpos = temp;
	}
	if (minpos < srcp->sb_start || maxpos > srcp->sb_length)
		error("Region to delete (%d to %d) is outside buffer!",
		      minpos, maxpos);

	/* inform the formatter */
	send_delete(bufp, minpos, maxpos - minpos);

	/* can we simplify the operation? */
	if (minpos == 0 && maxpos == srcp->sb_length) {
		nchars = srcp->sb_length;
		buffer_erase(bufp, TRUE);
		return (nchars);
	}

	/* find the block containing the start */
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (minpos < next->tb_offset + next->tb_length)
			break;
		last = next;
	}
	ASSERT(next != NULL);

	PROTECT();
	nchars = maxpos - minpos;
	deleted = 0;
	if (minpos > next->tb_offset &&
	    nchars >= (next->tb_offset + next->tb_length) - minpos) {
		/* we can just throw out rest of current block */
		deleted = (next->tb_offset + next->tb_length) - minpos;
		nchars -= deleted;
		next->tb_length -= deleted;
		last = next;
		next = next->tb_next;
	} else if (minpos >= next->tb_offset && nchars < next->tb_length) {
		/* need to delete some from the middle of this block */
		deleted = nchars;
		nchars -= deleted;
		end = next->tb_text + next->tb_length;
		begin = tp = next->tb_text + (minpos - next->tb_offset);

		/* actually delete the characters */
		fp = tp + deleted;
		while (fp < end)
			*tp++ = *fp++;
		next->tb_length -= deleted;
		last = next;
		next = next->tb_next;
	}

	/* now delete blocks starting with next */
	while (next != NULL && nchars >= next->tb_length) {
		deleted += next->tb_length;
		nchars -= next->tb_length;
		next = next->tb_next;
	}

	if (nchars > 0) {
		ASSERT(nchars < next->tb_length);
		/* we have to copy down from where deletion ends */
		tp = next->tb_text;
		end = tp + next->tb_length;
		fp = tp + nchars;
		while (fp < end)
			*tp++ = *fp++;
		next->tb_length -= nchars;
	}
	if (last == NULL)
		srcp->sb_text = next;
	else
		last->tb_next = next;
	if (next != NULL)
		next->tb_prev = last;

	/* decrement buffer length and succeeding offsets */
	srcp->sb_length -= deleted;
	if (srcp->sb_mark >= srcp->sb_point)
		srcp->sb_mark -= deleted;
	if (srcp->sb_point > maxpos)
		srcp->sb_point -= deleted;
	else if (srcp->sb_point > minpos)
		srcp->sb_point = minpos;
	while (next != NULL) {
		if (last == NULL)
			next->tb_offset = 0;
		else
			next->tb_offset = last->tb_offset + last->tb_length;
		last = next;
		next = next->tb_next;
	}
	bufp->bu_flags |= BUFF_MODIFIED|BUFF_CHANGED;
	UNPROTECT();

	return (deleted);
}


int	pending_deletes = FALSE;

send_delete(bufp, offset, length)
	struct buffer	*bufp;
{
	extern int	pending_inserts;
	extern int	format_socket;
	struct source	*srcp;
	unsigned long	data[3];

	/* make sure we're connected to the formatter */
	if (format_socket <= 0)
		return (1);

	/* flush pending insertions so they don't get mingled with deletions */
	if (pending_inserts)
		flush_inserts();

	/* make sure this is a TeX file buffer */
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	if ((bufp->bu_flags & BUFF_TEXFILE) == 0 || srcp->sb_fileID < 0)
		return (1);

	/* send the deleted text off to the formatter */
	data[0] = htonl(IRS_BOF);
	data[1] = htonl(offset);
	data[2] = htonl(length);
	sendformat(TSC_DELETE, srcp->sb_fileID, sizeof (data), (char *)data);

	return (0);
}

flush_deletes()
{
	if (!pending_deletes)
		return (0);

	pending_deletes = FALSE;
	return (0);
}
