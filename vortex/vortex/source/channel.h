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
 *  RCS Info: $Header: channel.h,v 0.1 87/04/30 20:51:45 john Locked $
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
 *  channel.h - input to and output from different devices
 */
 
#ifndef _CHANNEL_
#define _CHANNEL_

/*
 *  The channel struct contains the information about a single
 *  open I/O channel.  All exchanged I/O is done through these
 *  channels, which are abstract in terms of what will acutally
 *  be done with the characters written onto a write channel
 *  or where changes read from a read channel come from.
 *
 *  A channel has one end which the user manipulates (read from
 *  or writes to) and another end that vlisp manipulates (which
 *  is connected to a file, a pipe, a filter, a buffer, or a symbol.
 *  Channels of different types can all be manipulated by the same
 *  set of functions.
 */
#define CHANBUF		512		/* size of I/O buffer queue */
#undef EOF
#define EOF		-1		/* channel end-of-file marker */

struct channel {
	short		ch_number;	/* the port number */
	short		ch_type;	/* channel type */
	unsigned int	ch_flags;	/* flags (see below) */
	unsigned char	ch_input[512],	/* input character queue */
			*ch_inptr,	/* input queue pointer */
			*ch_inend;	/* input queue end marker */
	unsigned char	ch_output[512],	/* output character queue */
			*ch_outptr;	/* output queue pointer */
	char		ch_path[1024];	/* the pathname of file */
	short		ch_line;	/* current line in file */
	short		ch_iofd;	/* the file descriptor */
	struct channel	*ch_pipe;	/* connected channel for pipe */
	struct buffer	*ch_buffer;	/* buffer pointer for bopen */
	struct string	*ch_filter;	/* filter function for copen */
	struct string	*ch_symbol;	/* symbol name for sopen */
	struct process	*ch_process;	/* pointer to process if connected */
	struct channel	*ch_next;	/* next in list */
};

#define CHAN_FILE	1		/* a UNIX file */
#define CHAN_BUFFER	2		/* read/write to a buffer */
#define CHAN_FILTER	3		/* calls a filter command */
#define CHAN_SYMBOL	4		/* read from a symbol */
#define CHAN_PIPE	5		/* pipe to a process */
#define CHAN_SOCKET	6		/* IPC communications */

#define CHAN_READ	(1 << 0)	/* open for reading */
#define CHAN_WRITE	(1 << 1)	/* open for writing */
#define CHAN_APPEND	(1 << 2)	/* always append writes */
#define CHAN_NOCLOSE	(1 << 3)	/* don't allow user close */
#define CHAN_TTY	(1 << 4)	/* this file is a tty */
#define CHAN_UNBUF	(1 << 5)	/* I/O is completely unbuffered */
#define CHAN_EOF	(1 << 6)	/* have read EOF here */
#define CHAN_LOCK_EX	(1 << 7)	/* vlisp has an exclusive lock */
#define CHAN_LOCK_SH	(1 << 8)	/* vlisp has an shared lock */
#define CHAN_LOCK_NB	(1 << 9)	/* don't block for a lock */
#define CHAN_PROCESS	(1 << 10)	/* channel connected to a process */
#define CHAN_CLOSED	(1 << 11)	/* channel just pending close */

#define cclreof(c)	((c)->ch_flags &= ~CHAN_EOF)
#define ceof(c)		((c)->ch_flags & CHAN_EOF)

#define STDIN	0
#define STDOUT	1
#define STDERR	2

extern struct channel	*open_channels;
extern struct channel	*get_channel();
extern struct channel	*cstdin, *cstdout;
extern struct channel	*fcopen();

extern struct value	cread(), cratom();

#endif _CHANNEL_
