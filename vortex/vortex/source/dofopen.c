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
 *  RCS Info: $Header: dofopen.c,v 0.1 87/05/01 11:46:15 john Locked $
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
 *  dofopen.c - function to open a UNIX file channel
 */
static char _ID[] = "@(#)dofopen.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <errno.h>
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: channel
 *  Desc: A \em{channel} is a handle for input and output,
 *	abstracted such that its use is largely independant of the
 *	action source and destination of the text being transferred.
 *	Channels can be thought of as one-way pipes, one end of which
 *	is ``open'', on which the user can read and write.  The other
 *	end is connected to some internal object which routes character
 *	according to the way the channel was created.  The functions
 *	to open a channel of each type are distinct; the rest of
 *	the functions work on any channel type.
 *
 *	Channels can be used to automatically transfer text (actually,
 *	any kind of data) to or from various kinds of objects.  For
 *	example, one can open a channel into a buffer, make a pipe
 *	or read characters from a string.  One can also set up a
 *	channel so that input/output causes a user defined function
 *	to be called to handle the characters.
 *
 *	Channels are very inportant when communicating with processes.
 *	Normally, one would only be able to comminucate with a \sc{UNIX}
 *	process through its argument list and its exit status.  But,
 *	by specifying the process standard input or standard output
 *	as any type of channel, one can feed and collect output from
 *	that process and handle it appropriately.  For example, one
 *	could have a process read on its standard input from a lisp
 *	variable, and send its standard output to a buffer.  This
 *	would only require opening two channels and starting the
 *	process up.  Below we show an example of this type of code.
 *
 *	First we create the two channels; \sym{sopen} opens a channel
 *	(here for reading) into a lisp object we've just created and
 *	\sym{bopen} opens a channel (here for writing) into a buffer.
 *
 *	\tab{\lit{(setq cin (sopen "text string." "r"))}
 *	\lit{(setq cout (bopen "*output*" "w"))}}
 *
 *	the variables \sym{cin} and \sym{cout} should be set to
 *	two successive channel numbers, small fixnums.  Next we
 *	start the process using \sym{exec}.  Here we'll wait for
 *	\em{cat(1)} to finish, although we could do more interesting
 *	things in the meantime.
 *
 *	\tab{\lit{(setq pid (exec "/bin/cat" cin cout "cat" "-u"))}
 *	\lit{(wait pid)}}
 *
 *	Now, the string \lit{"text string."} should have been
 *	inserted into buffer \lit{*output*} before point.  Now,
 *	to be nice and tidy, we need to close our input channels.
 *
 *	\tab{\lit{(close cin)}
 *	\lit{(close cout)}}
 *
 *	You've already seen two functions that create channels, and
 *	\sym{close} which deactivates any type of channel.  Here is
 *	a list of the channel functions, all of which have separate
 *	documentation.
 *
 *	\tab{\sym{bopen}	Open a buffer channel
 *	\sym{copen}	Open a ``command'' channel
 *	\sym{fopen}	Open a \sc{UNIX} file channel
 *	\sym{sopen}	Open a ``string'' channel
 *	\sym{pipe}	Create a pair of connected channels
 *	\sym{close}	Deactivate a channel
 *	\sym{flush}	Empty I/O buffers immediately
 *	\sym{read}	Get characters froma channel
 *	\sym{print}	Send characters to a channel
 *	\sym{seek}	Moves read/write pointer in a channel}
 *
 *	Note that by default all channels are buffered.  This means
 *	that characters are saved up in a string and actual
 *	physical I/O is not done until the buffer fills up, usually
 *	in large chunks.  This is done for the sake of efficiency.
 *	One can empty these buffers at any time, forcing the system
 *	to handle the output, by calling the function \sym{flush}.
 *	Each \em{open} function can be told to make the channel
 *	\em{unbuffered}, which simply means that this saving of
 *	characters is not done; each print call does an implicit
 *	\sym{flush}.
 *
 *	The open functions all take two arguments, the object to
 *	read from/write to and a \em{mode string}, which specifies
 *	whether to read or write, and possibly also options.  For
 *	example, the mode \lit{"rau"} to \sym{fopen} would mean
 *	that the file is to be opened for reading and writing
 *	(``appending''--file isn't truncated), and that all I/O
 *	is to be unbuffered.
 *  Xref: I/O i/o io input output
 *  SeeA: bopen copen fopen sopen pipe read print close flush seek
 */

/*
 *  DOCUMENTATION
 *
 *  Name: fopen
 *  Call: (fopen 'file 'mode)
 *  Retu: channel
 *  Desc: This function opens an I/O channel into a \sc{UNIX} file.
 *	The function takes two arguments, both of which should evaluate
 *	to strings, specifying the file name to open and the mode with
 *	which to access it.  The file name should be a valid \sc{UNIX}
 *	pathname (although the tilde character, meaning a home directory
 *	reference as in \em{csh(1)} will be properly expanded).  The
 *	file mode should be made up of the following letters:
 *
 *	\tab{r	file to be opened for reading (it must exist)
 *	w	file to be opened for writing and is truncated
 *	a	file to be opened for writing but not truncated
 *	u	file I/O channel will be unbuffered
 *	b	don't block on open because of a lock
 *	l	don't check for or obtain a lock}
 *
 *	At least one of \lit{"r"}, \lit{"w"} or \lit{"a"} must
 *	be specified, otherwise an error will occur.  Note that
 *	permissions not specified when the file is opened are not
 *	available, and permissions cannot be added later.  The modes
 *	\lit{"w"} and \lit{"a"} both mean ``open for writing'', but
 *	\lit{"w"} means also truncate the file to zero length.
 *
 *	Note that normally I/O to files is buffered for efficiency, so
 *	that the actual physical file will not necessarily change when
 *	\sym{print} is called.  Buffering may be disabled by specifying
 *	\lit{"u"} in the mode string, or by explicitly \sym{flush}ing
 *	the channel when the file is to be brought up to date.
 *
 *	All reading and writing in a file is done relative to a
 *	pointer maintained across \sym{read}/\sym{print} calls.
 *	A file opened with mode \lit{"r"} has this pointer at the
 *	beginning of the file, when opened with \lit{"w"} at the
 *	beginning of an empty file, and when opened with \lit{"a"}
 *	at the end of a non-empty file.  This pointer may be moved
 *	within the file using the \sym{seek} function.
 *
 *	Normally, files opened from vlisp (through \sym{fopen}) are
 *	locked to assure consistency.  That means that a \sc{UNIX}
 *	file open for writing cannot be opened for reading or writing
 *	by another call to \sym{fopen} and a file opened for reading
 *	can only be opened for reading later on.  If the requested
 *	lock is not obtainable, the \sym{fopen} blocks, unless the
 *	\lit{"b"} mode is given.  If the \lit{"l"} mode is given,
 *	no lock is ched for or obtained.  The \sym{flock} function
 *	can change an existing lock or add a new one later.
 *  Side: Opening a file channel creates internal as well as system
 *	overhead, and so these channels should be closed when no
 *	longer needed.  The operating system limits the number of
 *	open files a process may have, so it is possible for a later
 *	\sym{fopen} to fail if there are already too many open channels.
 *
 *	It is an error to try to open for reading a file that doesn't
 *	exist, or isn't readable.  It is also an error to write a file
 *	when one doesn't have the appropriate permissions.  To check
 *	in a ``safe'' way for these permissions, the \sym{access}
 *	function is provided.
 *
 *	If a lock is present on the file to be opened, \sym{fopen}
 *	will normally block until it can lock the file appropriately.
 *	However, if the mode \lit{"b"} is given, the operation will
 *	not block waiting for a lock; the file will not be opened and
 *	\sym{fopen} will encounter an error.  If the \lit{"l"} mode
 *	is given, existing locks are not checked and no new lock
 *	is obtained for this file.  Note that these locks are just
 *	advisory for normal \sc{UNIX} processes, if they don't observe
 *	these locking conventions, they can ignore existing locks.
 *  Xref: open
 *  SeeA: read print seek flush close flock access channel
 */

DEFUN(dofopen, "fopen", FLAG_NONE, NULL)
{
	struct channel	*chan;
	struct value	arg1, arg2, ret;
	struct string	*str;
	unsigned char	*sp, *send;
	char		pbuf[STRBUF], *filename;
	int		chan_mode = 0;
	int		dontlock = 0;

	CHECKAC(2, 2);

	arg1 = EVALARGN(1);
	if (!stringp(arg1))
		BADARGN(1, "a string file name");
	str = gstring(arg1.vl_data);
	if (str->st_length == 0)
		error("Null filename, can't open that file!");
	makecstring(str, pbuf, sizeof (pbuf));

	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(2, "a string mode");
	str = gstring(arg2.vl_data);
	send = str->st_buffer + str->st_length;
	for (sp = str->st_buffer; sp < send; sp++) {
		switch (*sp) {
		case 'r':
			chan_mode |= CHAN_READ;
			break;
		case 'w':
			chan_mode |= CHAN_WRITE;
			break;
		case 'a':
			chan_mode |= CHAN_WRITE|CHAN_APPEND;
			break;
		case '+':
			if (chan_mode & CHAN_READ)
				chan_mode |= CHAN_WRITE;
			else if (chan_mode & CHAN_WRITE)
				chan_mode |= CHAN_READ;
			break;
		case 'u':
			chan_mode |= CHAN_UNBUF;
			break;
		case 'l':
			dontlock = 1;
			break;
		case 'b':
			chan_mode |= CHAN_LOCK_NB;
			break;
		default:
			error("Bad characters in mode string to fopen.");
		}
	}
	if ((chan_mode & (CHAN_READ|CHAN_WRITE)) == 0)
		error("Must specify one of r w or a in mode string.");
	if (!dontlock && (chan_mode & CHAN_WRITE) != 0)
		chan_mode |= CHAN_LOCK_EX;
	else if (!dontlock)
		chan_mode |= CHAN_LOCK_SH;

	/* actually open up the file channel */
	filename = fixpath(pbuf);
	if ((chan = fcopen(filename, chan_mode)) == NULL) {
		perror("Can't open file %s, mode %S", filename, str);
	}

	/* return the channel number as file handle */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, chan->ch_number);
	return (ret);
}

/*
 *  Here we actually open a UNIX file, given the name and the
 *  channel flags which specify the access mode and locking
 *  information.  The sequence of operations here is tricky.
 *  We want to pretend we didn't open the file, if we can't get
 *  a lock on it, so we can't modify it before we check for
 *  the lock.  This means we can't open with the O_CREAT flag
 *  unless the file doesn't exist (in which case, it couldn't
 *  be locked anyway).
 *
 *  Once we've got an open descriptor, we lock as appropriate,
 *  based on the flags handed to us.  The channel flags
 *  CHAN_LOCK_{EX,SH,NB} correspond to the flock(2) flags
 *  LOCK_{EX,SH,NB} which we turn them into for the argument
 *  to flock(2).  If this lock call fails, we assume that the
 *  user didn't want the open to block and we pretend the
 *  open failed, with the error EWOULDBLOCK.
 *
 *  Once we've obtained the lock (if we needed to), we proceed
 *  to set up the file.  If we've opened the file for writing,
 *  we truncate it, if it's open for appending, we seek to the
 *  end.  Otherwise, we just intern this new chann struct and
 *  return it.  A NULL return value always means some error.
 */

struct channel *
fcopen(filename, chan_mode)
	char	*filename;
	int	chan_mode;
{
	extern int	errno;
	struct channel	*new;
	int		lock_mode, open_mode;

	/* make up flags and channel struct */
	new = (struct channel *)valloc(sizeof (struct channel));
	new->ch_type = CHAN_FILE;
	new->ch_flags = chan_mode;

	/* figure out modes to open file */
	open_mode = 0;
	if (new->ch_flags & CHAN_READ)
		open_mode |= O_RDONLY;
	if (new->ch_flags & CHAN_WRITE)
		open_mode |= O_WRONLY;

	/* finish up channel struct now that we have a mode */
	strncpy(new->ch_path, fixpath(filename), sizeof (new->ch_path));
	new->ch_path[sizeof (new->ch_path) - 1] = '\0';
	new->ch_line = 1;
	new->ch_iofd = open(new->ch_path, open_mode, 0644);
	if (new->ch_iofd < 0 && errno == ENOENT &&
	    (new->ch_flags & CHAN_WRITE) != 0) {
		/* try creating the file */
		new->ch_iofd = open(new->ch_path, open_mode|O_CREAT, 0644);
	}
	if (new->ch_iofd < 0) {
		vfree(new);
		return (NULL);
	}

	/* obtain lock if so requested */
	lock_mode = 0;
	if ((new->ch_flags & CHAN_LOCK_EX) != 0)
		lock_mode |= LOCK_EX;
	if ((new->ch_flags & CHAN_LOCK_EX) != 0)
		lock_mode |= LOCK_EX;
	if (lock_mode != 0) {
		if ((new->ch_flags & CHAN_LOCK_NB) != 0)
			lock_mode |= LOCK_NB;
		if (flock(new->ch_iofd, lock_mode) < 0) {
			close(new->ch_iofd);
			vfree(new);
			errno = EWOULDBLOCK;
			return (NULL);
		}
	} else {
		/* just for paranoia's sake */
		new->ch_flags &= ~CHAN_LOCK_NB;
	}

	/* if writing, truncate file or move to the end */
	if ((new->ch_flags & CHAN_WRITE) != 0) {
		if ((new->ch_flags & CHAN_APPEND) != 0)
			lseek(new->ch_iofd, 0L, 2);
		else
			ftruncate(new->ch_iofd, 0);
	}

	/* install this in list of channels */
	new->ch_number = newchan();
	new->ch_inptr = new->ch_inend = new->ch_input;
	new->ch_outptr = new->ch_output;
	set_channel(new);

	return (new);
}
