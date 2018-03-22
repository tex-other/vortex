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
 *  RCS Info: $Header: doseek.c,v 0.1 87/05/01 12:01:25 john Locked $
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
 *  doseek.c - move the read or write pointer in a channel
 */
static char _ID[] = "@(#)doseek.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <errno.h>
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: seek
 *  Call: (seek 'channel 'offset 'position)
 *  Retu: position
 *  Desc: This function moves the read/write pointer in an I/O
 *	channel, which changes where the next read or write will
 *	occur.  It takes three arguments, all of which must
 *	evaluate to fixnums.  The first argument specifies the
 *	channel in which to seek, the second the signed offset
 *	in bytes, this distance to seek, and the third the position
 *	from which to begin seeking.  This position should be
 *	given as 0, 1 or 2 which mean the beginning, current
 *	position and end respectively.
 *
 *	The function returns the new offset in bytes from the
 *	beginning of the input.  This offset may not be the
 *	same as that specified, since \sym{seek} will refuse
 *	to move past the end of a channel or to before the
 *	beginning (except that one may seek past the end of
 *	data in a \sym{UNIX} file channel, increasing the file
 *	size and leaving a ``hole'').
 *
 *	Of course, one can only \sym{seek} in channels that
 *	support such a notion.  Currently, only file channels
 *	are \sym{seek}able.
 *  Side: Buffered output is flushed before the seek is done,
 *	otherwise data would later be written in the wrong location
 *	when the output buffer overflows.  Buffered input data is
 *	discarded.
 *  Xref: fseek lseek
 *  SeeA: flush channel
 */

DEFUN(doseek, "seek", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, arg3;
	struct channel	*chan;
	int		pos, off;
	struct value	ret;

	CHECKAC(3, 3);
	arg1 = EVALARGN(1);
	if (!fixnump(arg1))
		BADARGN(1, "a channel number");
	arg2 = EVALARGN(2);
	if (!fixnump(arg2))
		BADARGN(2, "an offset fixnum");
	arg3 = EVALARGN(3);
	if (!fixnump(arg3))
		BADARGN(3, "a position fixnum");

	/* get values from arguments */
	chan = get_channel(gfixnum(arg1.vl_data));
	off = gfixnum(arg2.vl_data);
	pos = gfixnum(arg3.vl_data);

	/* now actually do the seek */
	pos = cseek(chan, off, pos);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, pos);
	return (ret);
}

cseek(chan, off, pos)
	struct channel	*chan;
{
	extern int	errno;
	int		newpos = -1;

	/* remove end-of-file condition */
	cclreof(chan);

	/* flushed saved output and empty buffers */
	if (chan->ch_flags & CHAN_WRITE)
		cflush(chan);
	chan->ch_inptr = chan->ch_inend = chan->ch_input;
	chan->ch_outptr = chan->ch_output;

	/* make sure pos is 0, 1 or 2 */
	if (pos < 0 || pos > 2)
		error("Bad seek offset, should be 0, 1 or 2.");

	switch (chan->ch_type) {
	case CHAN_FILE:
		newpos = lseek(chan, off, pos);
		break;
	default:
		error("Can't seek on that type of channel!");
		/* NOTREACHED */
	}
	return (newpos);
}
