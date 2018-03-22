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
 *  RCS Info: $Header: docat.c,v 0.1 87/05/01 11:36:03 john Locked $
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
 *  docat.c - copy characters from one channel to another
 */
static char _ID[] = "@(#)docat.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: cat
 *  Call: (cat 'input 'output)
 *  Retu: fixnum
 *  Desc: This function reads all it can from the input channel and
 *	writes it to the output channel.  Both arguments must evaluate
 *	to valid open channels, the first argument to a channel open
 *	for reading and the second to a channel open for writing.
 *	A count of the number of bytes transfered is returned.
 *
 *	This is useful when one wants to copy data from a ``passive''
 *	source to another channel.  For example, one might want to
 *	insert the contents of a file into a buffer; this could be
 *	done with \sym{cat} as follows:
 *
 *	\tab{\lit{(setq cin (fopen "foo" "r"))}
 *	\lit{(setq cout (bopen "*scratch*" "w"))}
 *	\lit{(cat cin cout)}
 *	\lit{(close cin)(close cout)}}
 *
 *	The code above would copy the file \lit{foo} into the buffer
 *	\lit{*scratch*} before point.  Note that we were polite and
 *	closed the channels when finished with them.
 *
 *	This command is named after the \sc{UNIX} program \em{cat(1)},
 *	which copies its input to its output.  In fact, we could have
 *	invoked this program (using \sym{exec}) to do the same thing
 *	as the vlisp \sym{cat} function.
 *  Side: This call will continue until EOF is read on the input
 *	channel.  This may mean that it will block forever if input
 *	to the channel is not being automatically provided.  Since
 *	\sym{cat} reads on the input until end-of-file, once it returns
 *	sucessfully, the input channel will present an EOF to further
 *	\sym{read} calls.  (Once can, of course, \sym{seek} in the channel
 *	to remove this condition, if the channel is of a seek-able type).
 *  SeeA: fopen bopen close channel
 */

DEFUN(docat, "cat", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct channel	*cin, *cout;
	register int	nbytes;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a channel number");
	cin = get_channel(gfixnum(arg.vl_data));
	if ((cin->ch_flags & CHAN_READ) == 0)
		error("I can't read from channel %d!", cin->ch_number);

	arg = EVALARGN(2);
	if (!fixnump(arg))
		BADARGN(2, "a channel number");
	cout = get_channel(gfixnum(arg.vl_data));
	if ((cout->ch_flags & CHAN_WRITE) == 0)
		error("I can't write on channel %d!", cout->ch_number);

	/* read until EOF on cin, writing to cout */
	nbytes = 0;

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, nbytes);
	return (ret);
}
