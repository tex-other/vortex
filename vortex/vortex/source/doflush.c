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
 *  RCS Info: $Header: doflush.c,v 0.1 87/05/01 11:45:52 john Locked $
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
 *  doflush.c - routine and function to flush a channel
 */
static char _ID[] = "@(#)doflush.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"
#include "buffer.h"
#include "function.h"
#include "process.h"

/*
 *  DOCUMENTATION
 *
 *  Name: flush
 *  Call: (flush 'channel)
 *  Retu: t
 *  Desc: This function flushes pending input/output on an I/O
 *	channel of any type.  Its single argument must evaluate
 *	to a fixnum which corresponds to an active channel.
 *  Side: Since different channel types have very different
 *	actions, several things may happen when an I/O channel
 *	is flushed.  Lisp functions may be called, lisp symbols
 *	may be modified, etc.
 *  Xref: fflush
 *  SeeA: channel
 */

DEFUN(doflush, "flush", FLAG_NONE, NULL)
{
	struct value	arg1;
	struct channel	*chan;

	CHECKAC(1, 1);
	arg1 = EVALARGN(1);
	if (!fixnump(arg1))
		BADARGN(1, "a channel number");

	chan = get_channel(gfixnum(arg1.vl_data));
	if (cflush(chan) >= 0)
		return (v_t);
	else
		return (v_nil);
}

cflush(chan)
	struct channel	*chan;
{
	register int	bytes;
	struct string	fake;
	struct value	ret, arg;

	if (chan == NULL)
		ierror("cflush: Channel argument is null!");
	if ((chan->ch_flags & CHAN_WRITE) == 0)
		return (1);

	bytes = chan->ch_outptr - chan->ch_output;
	chan->ch_outptr = chan->ch_output;
	if (bytes <= 0)
		return (1);

	switch (chan->ch_type) {
	case CHAN_FILE:
		if (write(chan->ch_iofd, chan->ch_output, bytes) != bytes)
			perror("Write error to file %s", chan->ch_path);
		break;
	case CHAN_BUFFER:
		if (chan->ch_buffer == NULL) {
			ierror("cflush: Null buffer pointer in channel %d!",
			    chan->ch_number);
			return (-1);
		}
		fake.st_buffer = chan->ch_output;
		fake.st_length = bytes;
		insert_string(chan->ch_buffer, &fake);
		break;
	case CHAN_FILTER:
		if (chan->ch_filter == NULL) {
			ierror("cflush: Null filter name in channel %d!",
			    chan->ch_number);
		}
		arg.vl_type = LISP_STRING;
		sstring(arg.vl_data, save_string(chan->ch_output, bytes));
		ret = call_function(v_nil, chan->ch_filter, arg);
		break;
	case CHAN_SYMBOL:
		return (-1);
	case CHAN_PIPE:
		if (chan->ch_pipe == NULL || (chan->ch_flags & CHAN_CLOSED)) {
			error("Read end of pipe channel %d isn't open!",
			    chan->ch_number);
		}
		if (write(chan->ch_iofd, chan->ch_output, bytes) != bytes)
			perror("Write error on pipe %d", chan->ch_iofd);
		break;
	default:
		ierror("cflush: Bad channel type %d to flush!", chan->ch_type);
		return (-1);
	}

	return (0);
}
