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
 *  RCS Info: $Header: dobopen.c,v 0.1 87/05/01 11:34:48 john Locked $
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
 *  dobopen.c - functions to handle ``buffer channels''
 */
static char _ID[] = "@(#)dobopen.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: bopen
 *  Call: (bopen 'buffer 'mode)
 *  Retu: channel
 *  Desc: This function opens a channel into a buffer.  The first
 *	argument should evaluate to an existing buffer name, the
 *	second must evaluate to a string mode, either \lit{"r"},
 *	\lit{"w"} or \lit{"a"}.  If the mode string contains the
 *	letter \lit{"u"}, the normal buffering will not be done.
 *	A small fixnum, the channel, is returned to be used as a
 *	handle to reference this channel when reading and writing.
 *	If one is reading from a buffer channel, it must exist at
 *	the time this founction is called, otherwise, a new buffer
 *	will be created (if necessary or the \lit{"w"} mode is used)
 *	for writing.
 *
 *	A buffer channel (one created by \sym{bopen}) is a
 *	uni-directional pipe into a buffer.  Characters written
 *	on a buffer channel are inserted before point (or appended
 *	to the end of the buffer, if mode was given as \lit{"a"}).
 *	Characters read from a buffer channel are gathered from
 *	before point in that buffer as point is advanced.
 *
 *	All the reading and writing functions work on all kinds
 *	of channels (assuming the channel has been opened appropriately
 *	for that operation).  All channels are deactivated with
 *	\sym{close}.  For more information about channels in
 *	general, see \em{channel}.
 *  Side: Buffer channels modify buffers asynchronously from the
 *	normal editing commands.  If point is moved carelessly
 *	in a buffer which has an active buffer channel, unexpected
 *	behaviour is likely to occur since moving point in this
 *	way is equivalent to \sym{seek}ing in a \sc{UNIX} file.
 *	Also, unless the channel is marked as unbuffered (by specifying
 *	\lit{"u"} in the mode string to \sym{bopen}), text that one
 *	might think has been inserted may not yet have been actually
 *	inserted into the buffer.  One may call \sym{flush} to handle
 *	any buffered characters.
 *
 *	The \sym{seek} function works with buffer channels, it
 *	actually moves point in the buffer.  This may also be an
 *	unexpected side effect of using a buffer channel.
 *  SeeA: read print seek flush close channel
 *
 *  END
 *
 *  Note that buffer channels only work when we're not working
 *  in stand alone mode.  We need the full editor to do them.
 */

DEFUN(dobopen, "bopen", FLAG_NONE, NULL)
{
	struct buffer	*bufp;
	struct channel	*new;
	struct value	arg1, arg2, ret;
	struct string	*str;
	unsigned char	*sp, *send;

	CHECKAC(2, 2);

	arg1 = EVALARGN(1);
	if (stringp(arg1))
		str = gstring(arg1.vl_data);
	else
		BADARGN(1, "a string buffer name");
	if ((bufp = buffer_get(str, FALSE)) == NULL)
		bufp = buffer_create(str, BUFF_SOURCE, FLAG_NONE);

	arg2 = EVALARGN(2);
	if (stringp(arg2))
		str = gstring(arg2.vl_data);
	else
		BADARGN(2, "a string mode");

	/* make up flags and channel struct */
	new = (struct channel *)valloc(sizeof (struct channel));
	new->ch_type = CHAN_BUFFER;
	new->ch_flags = FLAG_NONE;
	send = str->st_buffer + str->st_length;
	for (sp = str->st_buffer; sp < send; sp++) {
		switch (*sp) {
		case 'r':
			new->ch_flags |= CHAN_READ;
			break;
		case 'w':
			new->ch_flags |= CHAN_WRITE;
			break;
		case 'a':
			new->ch_flags |= CHAN_WRITE|CHAN_APPEND;
			break;
		case '+':
			if (new->ch_flags & CHAN_READ)
				new->ch_flags |= CHAN_WRITE;
			else if (new->ch_flags & CHAN_WRITE)
				new->ch_flags |= CHAN_READ;
			break;
		case 'u':
			new->ch_flags |= CHAN_UNBUF;
			break;
		default:
			error("Bad characters in mode string to bopen.");
		}
	}
	if ((new->ch_flags & (CHAN_READ|CHAN_WRITE)) == 0)
		error("Must specify one of r w or a in mode string.");
	if ((new->ch_flags & CHAN_READ) != 0 &&
	    (new->ch_flags & CHAN_WRITE) != 0)
		error("Can't read and write at the same time with bopen.");

	/* finish up channel struct now that we have a mode */
	new->ch_line = 1;
	new->ch_number = newchan();
	new->ch_buffer = bufp;
	new->ch_inptr = new->ch_inend = new->ch_input;
	new->ch_outptr = new->ch_output;
	set_channel(new);
	bufp->bu_flags |= BUFF_HOLD;

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, new->ch_number);
	return (ret);
}
