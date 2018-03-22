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
 *  RCS Info: $Header: dopipe.c,v 0.1 87/05/01 11:57:30 john Locked $
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
 *  dopipe.c - function to create a ``pipe channel''
 */
static char _ID[] = "@(#)dopipe.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: pipe
 *  Call: (pipe)
 *  Retu: dotted pair
 *  Desc: This function creates a uni-directional I/O pipe;
 *	it returns a dotted-pair containing two connected channels.
 *	When the second channel is written on, the characters
 *	can be read on the first one.  This function takes no
 *	arguments, so the only errors that can occur are system
 *	errors.
 *  Side: This uses the \sc{UNIX} \em{pipe(2)} system call
 *	internally, and so has the restrictions imposed by the
 *	operating system.  Only 4096 characters are buffered,
 *	if more text than that is written beofre any of it is read,
 *	further writes to the pipe will block.
 *
 *	Also, becuase of this use of \em{pipe(2)}, two \sc{UNIX}
 *	\em{file descriptors} are created, which take up limited
 *	system file slots.  If too many \sym{fopen} and \sym{pipe}
 *	channels are left open, one may run out of new file descriptor
 *	slots, making further file access impossible.
 *
 *	When one side of a pipe is closed, the other becomes invalid
 *	automatically for further writing.  When the write side is closed,
 *	eof will be seen on the read side.  If the pipe is written to
 *	after the read side has been closed, a system error will occur.
 *	Both sides of the pipe should be eventually be explicitly
 *	\sym{close}d.
 *  SeeA: read print close channel
 */

DEFUN(dopipe, "pipe", FLAG_NONE, NULL)
{
	struct channel	*rchan, *wchan;
	struct value	ret;
	struct ccell	*cell;
	int		fpipe[2];

	/* function takes no arguments */
	CHECKAC(0, 0);

	/* make a UNIX pipe to pass data through */
	if (pipe(fpipe) != 0)
		perror("dopipe: Can't make a UNIX pipe");

	/* allocate channel structs, will keep this storage */
	rchan = (struct channel *)valloc(sizeof (struct channel));
	wchan = (struct channel *)valloc(sizeof (struct channel));

	/* finish up read channel struct */
	rchan->ch_type = CHAN_PIPE;
	rchan->ch_flags = CHAN_READ;
	rchan->ch_line = 1;
	rchan->ch_number = newchan();
	rchan->ch_pipe = wchan;
	rchan->ch_iofd = fpipe[0];
	rchan->ch_inptr = rchan->ch_inend = rchan->ch_input;
	rchan->ch_outptr = rchan->ch_output;
	set_channel(rchan);

	/* finish up read channel struct */
	wchan->ch_type = CHAN_PIPE;
	wchan->ch_flags = CHAN_WRITE;
	wchan->ch_line = 1;
	wchan->ch_number = newchan();
	wchan->ch_pipe = rchan;
	wchan->ch_iofd = fpipe[1];
	wchan->ch_inptr = wchan->ch_inend = wchan->ch_input;
	wchan->ch_outptr = wchan->ch_output;
	set_channel(wchan);

	/* make up returned value */
	cell = save_ccell();
	cell->cc_tcar = LISP_FIXNUM;
	sfixnum(cell->cc_car, rchan->ch_number);
	cell->cc_tcdr = LISP_FIXNUM;
	sfixnum(cell->cc_cdr, wchan->ch_number);
	ret.vl_type = LISP_CONS;
	slist(ret.vl_data, cell);
	return (ret);
}
