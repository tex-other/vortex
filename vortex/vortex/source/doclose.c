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
 *  RCS Info: $Header: doclose.c,v 0.1 87/05/01 11:37:39 john Locked $
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
 *  doclose.c - close a generic channel
 */
static char _ID[] = "@(#)doclose.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "process.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: close
 *  Call: (close 'channel)
 *  Retu: t
 *  Desc: This function disables an I/O channel previously
 *	created by \sym{bopen}, \sym{copen}, \sym{fopen},
 *	\sym{sopen}, or \sym{pipe}.  After this call, the
 *	channel is no longer valid.
 *  Side: Closing a channel frees internal and possibly
 *	also system resources.  Also, if this channel is the
 *	input to a process, this will cause and end-of-file
 *	condition to be seen by that process.
 *  SeeA: channel
 */

DEFUN(doclose, "close", FLAG_NONE, NULL)
{
	struct value	arg;
	struct channel	*chan;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a channel number");

	chan = get_channel(gfixnum(arg.vl_data));
	if ((chan->ch_flags & CHAN_PROCESS) != 0) {
		error("Channel %d is connected to process %d at the moment!",
		    chan->ch_number, chan->ch_process->pr_pid);
		/* NOTREACHED */
	}
	if ((chan->ch_flags & CHAN_NOCLOSE) != 0)
		error("Can't close that channel, it's protected.");

	if (cclose(chan) == 0) {
		/* all is well */
		return (v_t);
	} else {
		/* this shouldn't happen */
		return (v_nil);
	}
}

cclose(chan)
	struct channel	*chan;
{
	register int	channel;
	struct channel	*last, *next;

	if (chan == NULL)
		ierror("cclose: Channel argument is null!");

	/* we search for the channel number */
	channel = chan->ch_number;

	last = NULL;
	for (next = open_channels; next != NULL; next = next->ch_next) {
		if (next->ch_number == channel)
			break;
		last = next;
	}
	if (next == NULL)
		ierror("cclose: Can't find channel %d in list!", channel);

	/* unlink this channel from the list */
	if (last == NULL)
		open_channels = next->ch_next;
	else
		last->ch_next = next->ch_next;

	cflush(next);

	switch (next->ch_type) {
	case CHAN_FILE:
		close(next->ch_iofd);
		break;
	case CHAN_BUFFER:
		break;
	case CHAN_FILTER:
		break;
	case CHAN_SYMBOL:
		break;
	case CHAN_PIPE:
		if (next->ch_pipe != NULL) {
			/* don't want this to get too recursive */
			next->ch_pipe->ch_pipe = NULL;
			next->ch_pipe->ch_flags |= CHAN_CLOSED;
		}
		close(next->ch_iofd);
		break;
	}

	/* free up the space, we've already unlinked it */
	vfree(next);

	return (0);
}
