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
 *  RCS Info: $Header: iochan.c,v 0.1 87/05/01 12:19:33 john Locked $
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
 *  iochan.c - basic channel handling routines
 */
static char _ID[] = "@(#)iochan.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"
#include "function.h"
#include "buffer.h"
#include "process.h"

struct channel	*open_channels = NULL;

/*
 *  DOCUMENTATION
 *
 *  Name: stdin
 *  Desc: This is a permently open channel that is created
 *	at the time VorTeX is started, connected to the standard
 *	input (usually a terminal) of the process.  This is
 *	used as the default channel for \sym{read}, although
 *	when lisp is being used from inside the VorTeX editor,
 *	it is not too useful.
 *  Side: Since this channel is a file channel connected to the
 *	standard input of the current process, reading from it
 *	steals characters from the terminal input, without
 *	complete synchronization with the editor.  Thus, this
 *	channel should be used with care.
 *  SeeA: read channel
 *
 *  END
 *
 *  We call the C variable cstdin rather than stdin so as not
 *  to get confused with stdio.
 */
struct channel	*cstdin;

/*
 *  DOCUMENTATION
 *
 *  Name: stdout
 *  Desc: This is a permently open channel that is created
 *	at the time VorTeX is started, connected to the standard
 *	input (usually a terminal) of the process.  This is
 *	used as the default channel for \sym{read}, although
 *	when lisp is being used from inside the VorTeX editor,
 *	it is not too useful.
 *  Side: Since this channel is a file channel connected to the
 *	standard output of the current process, writing to it
 *	sends characters to the terminal output, without any
 *	synchronization with the editor.  Thus, this channel
 *	should be used with care.
 *  SeeA: print channel
 *
 *  END
 *
 *  We call the C variable cstdout rather than stdout so as not
 *  to get confused with stdio.
 */
struct channel	*cstdout;

MKSTRING(STDIN_NAME, "stdin");
MKSTRING(STDOUT_NAME, "stdout");

initchannels()
{
	extern char	*ttyname();
	char		*tty;
	struct value	val;

	if ((tty = ttyname(STDIN)) == NULL)
		tty = "/dev/tty";

	/* standard input */
	cstdin = (struct channel *)valloc(sizeof (struct channel));
	cstdin->ch_type = CHAN_FILE;
	cstdin->ch_number = STDIN;
	cstdin->ch_flags = CHAN_READ|CHAN_NOCLOSE|CHAN_TTY;
	strcpy(cstdin->ch_path, tty);
	cstdin->ch_line = 1;
	cstdin->ch_iofd = STDIN;
	cstdin->ch_inptr = cstdin->ch_inend = cstdin->ch_input;
	cstdin->ch_process = NULL;
	set_channel(cstdin);
	val.vl_type = LISP_FIXNUM;
	sfixnum(val.vl_data, cstdin->ch_iofd);
	setglobal(STDIN_NAME, val, FLAG_NONE);

	/* standard output */
	cstdout = (struct channel *)valloc(sizeof (struct channel));
	cstdout->ch_type = CHAN_FILE;
	cstdout->ch_number = STDOUT;
	cstdout->ch_flags = CHAN_WRITE|CHAN_NOCLOSE|CHAN_TTY;
	strcpy(cstdout->ch_path, tty);
	cstdout->ch_line = 1;
	cstdout->ch_iofd = STDOUT;
	cstdout->ch_outptr = cstdout->ch_output;
	cstdout->ch_process = NULL;
	set_channel(cstdout);
	val.vl_type = LISP_FIXNUM;
	sfixnum(val.vl_data, cstdout->ch_iofd);
	setglobal(STDOUT_NAME, val, FLAG_NONE);

	return (0);
}

set_channel(new)
	struct channel	*new;
{
	struct channel	*next, *last;

	if (new == NULL || new->ch_number < 0)
		ierror("set_channel(0x%x): Bad argument!");

	last = NULL;
	for (next = open_channels; next != NULL; next = next->ch_next) {
		if (next->ch_number >= new->ch_number)
			break;
		last = next;
	}
	if (next != NULL && next->ch_number == new->ch_number)
		ierror("setfile: Duplicate channel number %d!",
		    new->ch_number);

	if (last == NULL)
		open_channels = new;
	else
		last->ch_next = new;
	new->ch_next = next;

	return (0);
}

struct channel *
get_channel(number)
{
	struct channel	*next;

	for (next = open_channels; next != NULL; next = next->ch_next)
		if (next->ch_number == number)
			return (next);

	error("Channel number %d is not open currently.", number);
	/* NOTREACHED */
}

newchan()
{
	register int	number;
	struct channel	*chan;

	number = 0;
	for (chan = open_channels; chan != NULL; chan = chan->ch_next)
		if (chan->ch_number >= number)
			number++;

	return (number);
}

fillchannel(chan)
	struct channel	*chan;
{
	int	bytes;

	if (chan == NULL)
		ierror("fillchannel: Channel argument is null!");

	switch (chan->ch_type) {
	case CHAN_FILE:
		if (chan->ch_iofd < 0) {
			/* this file has already been closed! */
			chan->ch_flags |= CHAN_EOF;
			return (EOF);
		}

		/* read as many bytes as we can from the file */
		bytes = read(chan->ch_iofd, chan->ch_input, CHANBUF);
		if (bytes < 0) {
			chan->ch_flags |= CHAN_EOF;
			perror("Read error from file %s!", chan->ch_path);
		} else if (bytes == 0) {
			chan->ch_flags |= CHAN_EOF;
			return (EOF);
		} else {
			chan->ch_inptr = chan->ch_input + 1;
			chan->ch_inend = chan->ch_input + bytes;
			return (*chan->ch_input);
		}
		break;
	case CHAN_BUFFER:
		chan->ch_flags |= CHAN_EOF;
		return (EOF);
		break;
	case CHAN_FILTER:
		chan->ch_flags |= CHAN_EOF;
		return (EOF);
		break;
	case CHAN_SYMBOL:
		chan->ch_flags |= CHAN_EOF;
		return (EOF);
		break;
	case CHAN_PIPE:
		bytes = read(chan->ch_iofd, chan->ch_input, CHANBUF);
		if (bytes < 0) {
			chan->ch_flags |= CHAN_EOF;
			perror("Read error on pipe %d!", chan->ch_iofd);
		} else if (bytes == 0) {
			chan->ch_flags |= CHAN_EOF;
			return (EOF);
		} else {
			chan->ch_inptr = chan->ch_input + 1;
			chan->ch_inend = chan->ch_input + bytes;
			return (*chan->ch_input);
		}
		break;
	default:
		chan->ch_flags |= CHAN_EOF;
		ierror("fillchannel: Bad channel type %d to fill!",
		    chan->ch_type);
	}
	/* NOTREACHED */
}
