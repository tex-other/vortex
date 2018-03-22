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
 *  RCS Info: $Header: findfile.c,v 0.1 87/05/01 12:12:21 john Locked $
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
 *  findfile.c - file visiting functions
 */
static char _ID[] = "@(#)findfile.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: find-file
 *  Call: (find-file 'filename [ 'noswitch ])
 *  Retu: nil
 *  Desc: This function edits the specified file in a source buffer.
 *	If the file is already being edited in a buffer, that buffer
 *	is used, otherwise a new one is created.  The current window
 *	will then show the buffer for editing.
 *
 *	If the optional second argument is given and evaluates non-nil,
 *	it specifies that the new file-visiting buffer is not to be
 *	switched to on the screen, however the current buffer indication
 *	is changed to it in any case.
 *  SeeA: find-file-other-window find-file-read-only
 */
struct buffer	*findfile();

DEFUN(dofindfile, "find-file", FLAG_NONE, "fFind file: ")
{
	struct value	arg;
	struct buffer	*bufp;
	struct string	*file;
	int		noswitch = FALSE;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");
	file = gstring(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			noswitch = TRUE;
	}

	/* find the given file into an appropriate buffer */
	bufp = findfile(file, TRUE);

	if (!noswitch && current_window != NULL) {
		/* put the new buffer into the current window */
		if ((current_window->wi_flags & WIN_ACTIVE) == 0)
			error("No current window to display file in!");
		if (current_window->wi_type != WIN_BUFFER)
			error("Current window can't display file buffers!");
		if (current_window->wi_buffer != bufp)
			switchbuffer(current_window, bufp);
	}
	current_buffer = bufp;
	
	return (v_nil);
}

struct buffer *
findfile(name)
	struct string	*name;
{
	struct buffer	*bufp;
	struct source	*srcp;
	unsigned char	path[1025], *file, *last;
	struct string	*full, *base;
	int		exist = FALSE, readonly = FALSE;

	/* get absolute path name to compare */
	makecstring(name, path, sizeof (path));
	if (*path == '\0')
		error("Null file name to find!");
	file = (unsigned char *)fixpath(path);
	full = save_string(file, strlen(file));

	/* search for a buffer editing this file */
	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
		if (bufp->bu_type != BUFF_SOURCE)
			continue;
		srcp = bufp->bu_sdata;
		ASSERT(srcp != NULL);
		if (srcp->sb_file != NULL && sequal(srcp->sb_file, full))
			break;
	}
	if (bufp != NULL)
		return (bufp);

	/* we have to create a new buffer */
	last = (unsigned char *)basename(path);
	base = save_string(last, strlen(last));
	bufp = buffer_create(base, BUFF_SOURCE, FLAG_NONE);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	srcp->sb_file = full;

	/* now read the file into the buffer */
	if (access(file, F_OK) == 0) {
		insert_file(bufp, file);
		exist = TRUE;
		if (access(file, W_OK) != 0) {
			bufp->bu_flags |= BUFF_READONLY;
			readonly = TRUE;
		}
	}
	bufp->bu_flags |= BUFF_CHANGED;
	bufp->bu_flags &= ~BUFF_MODIFIED;

	/* buffer point at beginning, no mark set */
	srcp->sb_point = 0;
	srcp->sb_mark = -1;

	/* print an informative message */
	if (!exist)
		message("(new file \"%s\")", file);
	else if (readonly)
		message("(\"%s\" is read only)", file);
	else
		message("(opened \"%s\")", file);

	return (bufp);
}
