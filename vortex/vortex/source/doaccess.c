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
 *  RCS Info: $Header: doaccess.c,v 0.1 87/05/01 11:32:56 john Locked $
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
 *  doaccess.c - UNIX file access testing functions
 */
static char _ID[] = "@(#)doaccess.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: access
 *  Call: (access 'file 'mode)
 *  Retu: t or nil
 *  Desc: This predicate function reports whether the named file
 *	is accessible to the current user in the manner specified
 *	by the given mode.  The first argument should evaluate to
 *	a string which contains a \sc{UNIX} file name, the second
 *	to a string which specifies the access mode(s) to check.
 *	This access mode is a list of letters, each of which specifies
 *	something to check for in the file.  If the file cannot be
 *	accesed in any of the ways specified in the mode string,
 *	the function returns nil, otherwise it returns t.
 *
 *	\tab{Access mode specifiers are as follows:
 *	e	file/directory exists
 *	f	file exists and is not a directory
 *	d	the directory exists
 *	r	file/directory exists and is readable
 *	w	writeable file/directory exists or can be created
 *	x	file is executable or directory is searchable
 *	s	file/directory exists and is not of zero size
 *	l	file is not locked, using \em{flock(2)} (doesn't block)
 *	L	file is not locked (blocks until unlocked by someone else)}
 *
 *	A file may be locked by lisp using the \sym{flock} function,
 *	and \sym{access} may be used to check this.  However locks of
 *	this type are advisory, and the file may still be opened if
 *	the lock is not checked for.
 *  Side: If the \lit{"l"} or \lit{"L"} modes are given, the \em{advisory}
 *	\sc{UNIX} locking mechanism, \em{flock(2)} is used internally to
 *	check.  The \lit{"l"} mode is relatively safe, because it will
 *	return right away if the file is locked, but \lit{"L"} may block
 *	for an arbitray amount of time if another process is doing something
 *	incorrectly.
 *  SeeA: flock fopen
 */

DEFUN(doaccess, "access", FLAG_NONE, NULL)
{
	struct value	arg;
	char		*path, file[MAXPATHLEN+1];
	struct stat	stbuf;
	struct string	*str;
	unsigned char	*sptr, *send;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");
	makecstring(gstring(arg.vl_data), file, sizeof (file));

	arg = EVALARGN(2);
	if (!stringp(arg))
		BADARGN(1, "a mode string");
	str = gstring(arg.vl_data);

	if (*file == '\0')
		error("Invalid null file name given.");
	path = fixpath(file);

	sptr = str->st_buffer;
	send = sptr + str->st_length;
	if (send <= sptr)
		error("Invalid null mode string given.");

	while (sptr < send) {
		switch (*sptr) {
		case 'e':
			if (access(path, F_OK) != 0)
				return (v_nil);
			break;
		case 'r':
			if (access(path, R_OK) != 0)
				return (v_nil);
			break;
		case 'w':
			if (access(path, W_OK) != 0)
				return (v_nil);
			break;
		case 'x':
			if (access(path, X_OK) != 0)
				return (v_nil);
			break;
		case 'f':
			if (stat(path, &stbuf) < 0)
				return (v_nil);
			if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
				return (v_nil);
			break;
		case 'd':
			if (stat(path, &stbuf) < 0)
				return (v_nil);
			if ((stbuf.st_mode & S_IFMT) != S_IFDIR)
				return (v_nil);
			break;
		case 's':
			if (stat(path, &stbuf) < 0)
				return (v_nil);
			if (stbuf.st_size <= 0)
				return (v_nil);
			break;
		default:
			/* don't know what to do with this mode spec. */
			error("Invalid access mode specification %C!", *sptr);
		}
		sptr++;
	}
	
	return (v_t);
}
