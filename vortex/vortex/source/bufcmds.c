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
 *  RCS Info: $Header: bufcmds.c,v 0.1 87/05/01 11:25:09 john Locked $
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
 *  bufcmds.c - low-level buffer manipulation functions
 */
static char _ID[] = "@(#)bufcmds.c for VorTeX, Copyright (c) 1987 John Coker";

#include <stdio.h>
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-name
 *  Call: (buffer-name)
 *  Retu: string
 *  Desc: This function returns the string name of the current
 *	buffer.  This string name is the handle by which a
 *	buffer is accessed.
 *  Xref: current-buffer
 *  SeeA: buffer-length buffer-file-name
 */

DEFUN(dobufname, "buffer-name", FLAG_NONE, NULL)
{
	struct value	ret;
	struct buffer	*bufp = current_buffer;

	CHECKAC(0, 0);

	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, bufp->bu_name);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: kill-buffer
 *  Call: (kill-buffer 'buffer [ 'force ])
 *  Retu: t or nil
 *  Desc: This function removes the indicated buffer from the list
 *	of buffers currently active.  If the buffer has been
 *	modified (since the last write), the user is prompted for
 *	confirmation unless the optional second argument is given.
 *  Side: This buffer is deleted from the list of active buffers
 *	and all storage for it is deallocated.  This means that
 *	it is not recoverable and the text which made it up is
 *	lost.
 *
 *	If the buffer being deleted was the current buffer, the
 *	current buffer becomes the most recent current buffer
 *	(see switch-to-buffer).
 *  SeeA: get-buffer switch-to-buffer
 */

DEFUN(doremovebuf, "kill-buffer", FLAG_NONE, "bKill buffer: ")
{
	struct value	arg;
	struct buffer	*bufp;
	int		force = FALSE;

	CHECKAC(1, 2);

	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a buffer name string");
	bufp = buffer_get(gstring(arg.vl_data), FALSE);
	if (bufp == NULL)
		return (v_t);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			force = TRUE;
	}

	if (buffer_remove(bufp, force) == 0)
		return (v_t);
	else
		return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-modified
 *  Call: (buffer-modified [ 'buffer [ 'cond ] ])
 *  Retu: t or nil
 *  Desc: This predicate function returns the state of the modified
 *	flag for the current buffer.  If the buffer has been modified
 *	since last being written, t is returned, otherwise nil.  If
 *	the first argument is given and evaluates non-nil, it
 *	specifies a buffer other than the current one to check.
 *
 *	If the second argument is given, it specifies a state to
 *	set the modified flag to in the specified buffer.  A nil
 *	value means clear the flag, and any other values will set
 *	it.
 *  SeeA: bufferp buffer-read-only
 */

DEFUN(dobufmodified, "buffer-modified", FLAG_NONE, NULL)
{
	struct value	arg;
	struct buffer	*bufp = current_buffer;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (stringp(arg))
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
		else if (!nullp(arg))
			BADARGN(1, "a string buffer name");
	}
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg) && (bufp->bu_flags & BUFF_MODIFIED) == 0) {
			bufp->bu_flags |= BUFF_MODIFIED;
			bufp->bu_flags |= BUFF_NEWMODE;
		}
		if (!truep(arg) && (bufp->bu_flags & BUFF_MODIFIED) != 0) {
			bufp->bu_flags &= ~BUFF_MODIFIED;
			bufp->bu_flags |= BUFF_NEWMODE;
		}
	}
	if (bufp->bu_flags & BUFF_MODIFIED)
		return (v_t);
	else
		return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-read-only
 *  Call: (buffer-read-only [ 'buffer [ 'cond ] ])
 *  Retu: t or nil
 *  Desc: This predicate function returns the state of the read-only
 *	flag for the current buffer.  If the buffer has been marked
 *	as read-only, t is returned, otherwise nil.  If the first
 *	argument is given and evaluates non-nil, it
 *	specifies a buffer other than the current one to check.
 *
 *	If the second argument is given, it specifies a state to
 *	set the read-only flag to in the specified buffer.  A nil
 *	value means clear the flag, and any other values will set
 *	it.
 *  SeeA: bufferp buffer-modified
 */

DEFUN(dobufreadonly, "buffer-read-only", FLAG_NONE, NULL)
{
	struct value	arg;
	struct buffer	*bufp = current_buffer;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (stringp(arg))
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
		else if (!nullp(arg))
			BADARGN(1, "a string buffer name");
	}
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg) && (bufp->bu_flags & BUFF_READONLY) == 0) {
			bufp->bu_flags |= BUFF_READONLY;
			bufp->bu_flags |= BUFF_NEWMODE;
		}
		if (!truep(arg) && (bufp->bu_flags & BUFF_READONLY) != 0) {
			bufp->bu_flags &= ~BUFF_READONLY;
			bufp->bu_flags |= BUFF_NEWMODE;
		}
	}
	if (bufp->bu_flags & BUFF_READONLY)
		return (v_t);
	else
		return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: erase-buffer
 *  Call: (erase-buffer [ 'buffer [ 'force ] ])
 *  Retu: t
 *  Desc: This function deletes completely the contents of the
 *	specified buffer, or the current buffer if no arguments
 *	are given.  If the buffer has been modified since last
 *	written, and the optional second argument is not present
 *	or doesn't evaluate non-nil, the user is prompted for
 *	confirmation before the buffer is erased.
 *
 *	Erasing a buffer frees all the text block storage it
 *	previously required and creates an absolutely empty
 *	buffer.  All of the erased text is irretrievably lost.
 *
 *	If the buffer has a non-zero start position (usually this
 *	is not the case, see \sym{buffer-start}) and the force
 *	option is not given, an error will occur.
 *
 *	If the buffer doesn't already exist, a new one is created.
 *  Side: The buffer modified condition is raised if the buffer
 *	was not already empty.
 *  SeeA: delete-region buffer-start
 */

DEFUN(doerasebuf, "erase-buffer", FLAG_NONE, "bErase buffer: ")
{
	struct value	arg;
	struct buffer	*bufp;
	int		force = FALSE;
	struct string	*name = NULL;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		name = gstring(arg.vl_data);
		bufp = buffer_get(name, FALSE);
	} else {
		/* just use the current buffer */
		bufp = current_buffer;

	}
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			force = TRUE;
	}
	if (bufp == NULL) {
		/* make one */
		ASSERT(name != NULL);
		bufp = buffer_create(name, BUFF_SOURCE,
				     force ? BUFF_KILLOK : FLAG_NONE);
	}

	if (buffer_erase(bufp, force) == 0)
		return (v_t);
	else
		return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: list-buffers
 *  Call: (list-buffers)
 *  Retu: count
 *  Desc: This function prints information about each existing
 *	buffer into a buffer called \sym{*buffer-list*}.
 *
 *	The information is columnated in the output.  The different
 *	information categories are \em{flags}, \em{type}, \em{size}
 *	(where applicable), \em{mode} and \em{file} (where applicable).
 *
 *	The \em{flags} field occupies the first six columns.  Each
 *	possible flag is represented by the presence or absense of a
 *	representation character; one of:
 *
 *	\tab{.	this is the current buffer for whole editor
 *	*	this buffer has been modified since being written
 *	%	this buffer is read-only (can't be modified)
 *	<	this buffer is sending process input
 *	>	this buffer is receiving process output
 *	M	this buffer is the minibuffer}
 *
 *	The \em{type} field contains a short word identifying the
 *	buffer type.  Of course, the \em{name} field is the name of
 *	the buffer, its handle.  The \em{size} field specifies the
 *	size in bytes of source type buffers.  The mode contains the
 *	local value of \sym{local-mode-string} for that buffer.
 *	The \em{file} file, also only for source buffers, contains
 *	the file name being edited.
 *  Side: This list is composed at the time the command is executed,
 *	and is not (of course) kept up-to-date.  It must be called
 *	each time the buffer attributes or arrangement may have
 *	changed.
 *
 *	Every time this function is called, the contents of the
 *	buffer \lit{*buffer-list} are blindly over-written.
 *  SeeA: buffer-type buffer-name buffer-length buffer-file-name
 */
MKSTRING(LISTBUF_NAME, "*buffer-list*");

static char	BFLAG_CHARS[] = ".*%<>M";

#define ISFLAG(s,i,c)	((s)[i] = ((c) ? BFLAG_CHARS[i] : ' '))

DEFUN(dolistbufs, "list-buffers", FLAG_NONE, "")
{
	extern char	*gettype();
	struct value	ret;
	struct buffer	*out, *bufp;
	char		bflags[7];
	register int	count;
	struct source	*srcp;

	/* no arguments allowed */
	CHECKAC(0, 0);

	/* get the output buffer */
	out = buffer_create(LISTBUF_NAME, BUFF_SOURCE, BUFF_KILLOK);

	/* write header lines */
	bappend(out, " FLAGS  TYPE    NAME              SIZE  SOURCE FILE\n");
	bappend(out, " -----  ----    ----              ----  -----------\n");

	count = 0;
	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
		/* make up flags strings */
		ISFLAG(bflags, 0, bufp == current_buffer);
		ISFLAG(bflags, 1, bufp->bu_flags & BUFF_MODIFIED);
		ISFLAG(bflags, 2, bufp->bu_flags & BUFF_READONLY);
		ISFLAG(bflags, 3, bufp->bu_flags & BUFF_INPUT);
		ISFLAG(bflags, 4, bufp->bu_flags & BUFF_OUTPUT);
		ISFLAG(bflags, 5, bufp->bu_flags & BUFF_MINIBUF);
		bflags[6] = '\0';

		bappend(out, "%-6.6s  %-6.6s  %-16.16Y %5.5d",
			bflags,
			gettype(bufp),
			bufp->bu_name,
			getlength(bufp));
		if (bufp->bu_type == BUFF_SOURCE) {
			srcp = bufp->bu_sdata;
			ASSERT(srcp != NULL);
			if (srcp->sb_file != NULL)
				bappend(out, "  %S", srcp->sb_file);
		}
		bappend(out, "\n");

		count++;
	}
	out->bu_flags &= ~BUFF_MODIFIED;
	out->bu_flags |= BUFF_CHANGED|BUFF_NEWMODE|BUFF_READONLY;

	popto_buffer(out, FALSE);

	/* return the buffer count */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: dump-buffer
 *  Call: (dump-buffer 'buffer [ 'file ])
 *  Retu: nil
 *  Desc: This function writes information about the source buffer
 *	specified by its first argument onto the file specified by
 *	the second argument (or to stdout if the second argument is
 *	not given).  This routine is used for debugging and is not
 *	much use to the normal user.
 *  SeeA: bufferp
 */

DEFUN(dodumpbuffer, "dump-buffer", FLAG_NONE, "BDump buffer: \nfTo file: ")
{
	extern char	*pstring(), *psymbol();
	struct value	arg;
	char		file[1024];
	struct buffer	*bufp = current_buffer;
	FILE		*outfp = stdout;
	struct source	*srcp;
	struct tblock	*next, *last;
	unsigned long	*txp, *end;
	register int	offset;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (stringp(arg))
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	else if (!nullp(arg))
		BADARGN(1, "a string buffer name");
	ASSERT(bufp != NULL);
	if (bufp->bu_type != BUFF_SOURCE)
		error("Don't know how to dump non-source buffers.");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (stringp(arg)) {
			makecstring(gstring(arg.vl_data), file, sizeof (file));
			if (*file == '\0')
				error("Null file name to dump buffer on!");
			if ((outfp = fopen(file, "w")) == NULL)
				perror("Can't write on file \"%s\"", file);
		} else if (!nullp(arg)) {
			/* not a file name or nil */
			BADARGN(1, "a string file name");
		}
	}

	PROTECT();
	fprintf(outfp, "Buffer \"%s\" dump.\n", psymbol(bufp->bu_name));
	if (srcp->sb_mark < srcp->sb_start) {
		fprintf(outfp, "Start %d, length %d, point %d (no mark)\n",
			srcp->sb_start, srcp->sb_length, srcp->sb_point);
	} else {
		fprintf(outfp, "start %d, length %d, point %d, mark %d\n",
			srcp->sb_start, srcp->sb_length,
			srcp->sb_point, srcp->sb_mark);
	}
	if (srcp->sb_file != NULL)
		fprintf(outfp, "file \"%s\"\n", pstring(srcp->sb_file));
	if (srcp->sb_text != NULL)
		fprintf(outfp, "text blocks:\n");

	last = NULL;
	offset = 0;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		fprintf(outfp, "\toffset %d, length %d, text \"",
			next->tb_offset, next->tb_length);
		end = next->tb_text + next->tb_length;
		for (txp = next->tb_text; txp < end; txp++) {
			if (charof(*txp) == '"' || charof(*txp) == '\\')
				putc('\\', outfp), putc(charof(*txp), outfp);
			else if (charof(*txp) < ' ' || charof(*txp) > '~')
				fprintf(outfp, "%s", pchar(charof(*txp)));
			else
				putc(charof(*txp), outfp);
		}
		putc('"', outfp);

		if (next->tb_offset != offset)
			fprintf(outfp, " (offset wrong)");
		if (next->tb_prev != last)
			fprintf(outfp, " (prev wrong)");
		putc('\n', outfp);

		offset += next->tb_length;
		last = next;
	}
	if (offset != srcp->sb_length) {
		fprintf(outfp, "true length of buffer text is %d (not %d).\n",
			offset, srcp->sb_length);
	}

	fflush(outfp);
	if (outfp != stdout)
		fclose(outfp);
	UNPROTECT();

	return (v_nil);
}
