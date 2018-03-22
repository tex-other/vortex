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
 *  RCS Info: $Header: docopen.c,v 0.1 87/05/01 11:40:55 john Locked $
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
 *  docopen.c - handle ``lisp function'' (command) channels
 */
static char _ID[] = "@(#)docopen.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: copen
 *  Call: (copen 'function 'mode)
 *  Retu: channel
 *  Desc: This function opens an I/O channel through a lisp
 *	function specified by the user.  The first argument must
 *	evaluate to a functional value, which which will be called
 *	to handle I/O as needed, and the second should
 *	evaluate to a string mode, one of \lit{"r"} or \lit{"w"}.
 *	If the mode string contains the letter \lit{"u"}, the
 *	channel will be unbuffered.  A small fixnum is returned
 *	which is the handle to this channel to be used when
 *	reading or writing.
 *
 *	Command channels (those created with \sym{copen}) allow
 *	one to use the standard reading and writing functions to
 *	talk to user defined lisp functions.  A commane channel
 *	may only be open either for reading or writing, not both.
 *	When a read is done on a command channel, the specified
 *	function is called with the channel number as its argument
 *	and is expected to return a string which is interpreter
 *	as the characters to return as the result of the read.
 *	When this function returns nil, and end-of-file is seen.
 *	When writing on a command channel, the users function
 *	is called with two arguments, the channel number and the
 *	buffered string of characters being written out.
 *
 *	The standard reading and writing function work similarly
 *	on all types of channels.  See \em{channel} for more
 *	information on I/O channels in general.
 *  Side: Opening a channel of any type creates overhead, sometimes
 *	operating system overhead.  For this reason, it is always
 *	important to close channels when they are no longer useful.
 *
 *	Command channels break up the normal flow of control of
 *	lisp.  The user function will be called at unpredictable
 *	times because of I/O buffering (unless disabled with the
 *	\lit{"u"} mode option).  The channel may be explicitly
 *	\sym{flush}ed at any time to dispose of buffered characters.
 *  SeeA: read print close channel
 */

DEFUN(docopen, "copen", FLAG_NONE, NULL)
{
	struct channel	*new;
	struct value	arg1, arg2, ret;
	struct string	*str;
	unsigned char	*cp, *cend;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a function name");
	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(1, "a string mode");

	/* make up flags and channel struct */
	new = (struct channel *)valloc(sizeof (struct channel));
	new->ch_type = CHAN_FILTER;
	new->ch_flags = FLAG_NONE;
	str = gstring(arg2.vl_data);
	cend = str->st_buffer + str->st_length;
	for (cp = str->st_buffer; cp < cend; cp++) {
		switch (*cp) {
		case 'r':
			new->ch_flags |= CHAN_READ;
			break;
		case 'w':
		case 'a':
			new->ch_flags |= CHAN_WRITE;
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
			error("Bad characters in mode string to copen.");
		}
	}
	if ((new->ch_flags & (CHAN_READ|CHAN_WRITE)) == 0)
		error("Must specify r or w in mode string.");

	/* finish up channel struct now that we have a mode */
	new->ch_filter = gsymbol(arg1.vl_data)->sy_pname;
	new->ch_number = newchan();
	new->ch_inptr = new->ch_inend = new->ch_input;
	new->ch_outptr = new->ch_output;
	set_channel(new);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, new->ch_number);
	return (ret);
}
