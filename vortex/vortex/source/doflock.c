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
 *  RCS Info: $Header: doflock.c,v 0.1 87/05/01 11:45:28 john Locked $
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
 *  doflock.c - UNIX file locking functions
 */
static char _ID[] = "@(#)doflock.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: flock
 *  Call: (flock 'channel 'mode)
 *  Retu: t or nil
 *  Desc: This function allows one to aquire an advisory lock
 *	on a \sc{UNIX} file, preventing others from aquiring
 *	a similar lock.  However, this lock is only advisory,
 *	so other processes can open the file if they don't go
 *	through this locking mechanism.  This function returns
 *	t (or nil if the lock is non-blocking and obtaining the
 *	lock would cause the process to block).
 *
 *	Two arguments must be given, the first must evaluate
 *	to a file channel, the second to a string mode, specifying
 *	the lock parameters.  Locks normally block if the request
 *	cannot be granted due to another active lock; the process
 *	holding the lock must release it before this function will
 *	return.  This can be overridden by making the lock non-blocking.
 *	In that case, \sym{flock} will return nil, meaning the lock
 *	could not be obtained without blocking.
 *
 *	There are two levels of locks available, \em{read} and
 *	\em{write} locks.  A read lock is shared in the sense that
 *	other processes can also lock the file fo reading, but
 *	not gain a write lock.  A write lock is exclusive, that
 *	is, no other process can obtain a read or write lock on
 *	that file.  One specifies the mode of the lock by specifying
 *	letters in the mode string argument.  The possible mode
 *	letters are:
 *
 *	\tab{r	obtain a read (shared) lock
 *	w	obtain a write (exclusive) lock
 *	u	release the previously obtained lock
 *	b	don't block if lock is not immediately obtainable}
 *
 *	This function uses the \sc{UNIX} \em{flock(2)} system call
 *	internally so that cooperating processes not written in
 *	vlisp will understand this locking.  The lock is released
 *	if the channel is closed.
 *  SeeA: fopen close
 */

DEFUN(doflock, "flock", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct channel	*chan;
	struct string	*str;
	unsigned char	*sp, *send;
	int		lock_mode;

	CHECKAC(2, 2);

	/* get the channel argument */
	arg1 = EVALARGN(1);
	if (!fixnump(arg1))
		BADARGN(1, "a channel number");
	chan = get_channel(gfixnum(arg1.vl_data));
	if (chan->ch_type != CHAN_FILE)
		error("Sorry, we can only flock file channels.");

	/* get the mode string argument */
	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(2, "a string mode");
	str = gstring(arg2.vl_data);
	lock_mode = 0;
	send = str->st_buffer + str->st_length;
	for (sp = str->st_buffer; sp < send; sp++)
		switch (*sp) {
		case 'r':
			lock_mode |= LOCK_SH;
			break;
		case 'w':
			lock_mode |= LOCK_EX;
			break;
		case 'b':
			lock_mode |= LOCK_NB;
			break;
		case 'u':
			lock_mode = LOCK_UN;
			break;
		default:
			error("Bad characters in mode string to flock");
		}

	/* make sure that given modes make sense */
	if ((lock_mode & (LOCK_SH|LOCK_EX|LOCK_UN)) == 0)
		error("Must specify one of r w or u in mode string.");
	if ((lock_mode & LOCK_UN) && (lock_mode & (LOCK_EX|LOCK_SH)))
		error("Can't both lock and unlock at the same time!");
	if ((lock_mode & LOCK_UN) != 0)
		lock_mode = LOCK_UN;

	/* try to obtain the file lock */
	if (flock(chan->ch_iofd, lock_mode) < 0)
		return (v_nil);

	/* update mode flags to reflect changes */
	if (lock_mode & LOCK_SH)
		chan->ch_flags |= CHAN_LOCK_SH;
	if (lock_mode & LOCK_EX)
		chan->ch_flags |= CHAN_LOCK_EX;
	if (lock_mode & LOCK_NB)
		chan->ch_flags |= CHAN_LOCK_NB;
	if (lock_mode & LOCK_UN)
		chan->ch_flags &= ~(CHAN_LOCK_SH|CHAN_LOCK_EX|CHAN_LOCK_NB);

	return (v_t);
}
