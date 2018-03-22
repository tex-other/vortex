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
 *  RCS Info: $Header: bufstat.c,v 0.1 87/05/01 11:25:53 john Locked $
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
 *  bufstat.c - buffer state manipulation functions
 */
static char _ID[] = "@(#)bufstat.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-type
 *  Call: (buffer-type [ 'buffer ])
 *  Retu: symbol
 *  Desc: This function returns a symbol identifying the type of the
 *	specified buffer.  This symbol will represent the editing
 *	function of the buffer in the editor.  If the argument is
 *	given, it must evaluate to the name of an existing buffer,
 *	otherwise the current buffer's type is returned.
 *
 *	The current buffer types known to the editorare:
 *
 *	\tab{source	a source (text) editing bufer
 *	proof	a proof editor buffer (formatted output)
 *	graphic	a PostScript graphics display buffer
 *	table	a table editor buffer
 *	math	a math editing buffer}
 *  SeeA: buffer-list
 */

char *
gettype(bufp)
	struct buffer	*bufp;
{
	switch (bufp->bu_type) {
	case BUFF_SOURCE:
		return ("source");
	case BUFF_PROOF:
		return ("proof");
	case BUFF_GRAPHIC:
		return ("graphic");
	default:
		return ("unknown");
	}
}

DEFUN(dobuftype, "buffer-type", FLAG_NONE, "")
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	struct symbol	*sym;
	struct string	*str;
	char		*name;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}

	/* get the C string for the name */
	name = gettype(bufp);

	/* return the lisp symbol of the name */
	str = save_string(name, strlen(name));
	sym = save_symbol(str);
	ret.vl_type = LISP_SYMBOL;
	ssymbol(ret.vl_data, sym);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-length
 *  Call: (buffer-length [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function returns the length of the buffer specified
 *	by the argument, of of the current buffer if no argument
 *	is given.  This length is given as a number of characters.
 *  SeeA: buffer-name buffer-file-name
 */

DEFUN(dobuflen, "buffer-length", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, getlength(bufp));
	return (ret);
}

getlength(bufp)
	struct buffer	*bufp;
{
	struct source	*srcp;
	long		len;

	switch (bufp->bu_type) {
	case BUFF_SOURCE:
		srcp = bufp->bu_sdata;
		ASSERT(srcp != NULL);
		len = srcp->sb_length;
		break;
	default:
		len = 0;
	}
	return (len);
}

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-start
 *  Call: (buffer-start [ 'buffer [ 'position ] ])
 *  Retu: fixnum
 *  Desc: This function returns the start of the buffer specified
 *	by the argument, of of the current buffer if no argument
 *	is given.  This position is given as a character offset.
 *	If the second argument is given, and it must evaluate to
 *	a fixnum, it specifies the position to use as the start.
 *
 *	The start position of a buffer is almost always zero.  It
 *	can be used to make part of the beginning of the buffer
 *	``untouchable''.  One cannot edit or even move the point
 *	before the start position in a buffer.  Erasing the buffer
 *	will cause an error if there is a non-zero start position.
 *  Side: If point in the buffer is before the start position when
 *	a new start position is set, it is moved to (just after)
 *	the start position.  One cannot set the start position
 *	past the end of the buffer.
 *  SeeA: buffer-name buffer-length
 */

DEFUN(dobufstart, "buffer-start", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	struct source	*srcp;
	int		start;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}
	if (GETACOUNT() > 1) {
		if (bufp->bu_type != BUFF_SOURCE)
			error(
		    "There is no start position in this type of buffer.");
		srcp = bufp->bu_sdata;
		ASSERT(srcp != NULL);
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a fixnum offset");
		start = gfixnum(arg.vl_data);
		if (start < 0 || start > srcp->sb_length)
			error("That start position is outside the buffer!");
		if (srcp->sb_point < start)
			srcp->sb_point = start;
		srcp->sb_start = start;
	} else {
		/* just get the start position */
		start = getstart(bufp);
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, start);
	return (ret);
}

getstart(bufp)
	struct buffer	*bufp;
{
	struct source	*srcp;
	long		start;

	switch (bufp->bu_type) {
	case BUFF_SOURCE:
		srcp = bufp->bu_sdata;
		ASSERT(srcp != NULL);
		if (srcp->sb_start < 0)
			srcp->sb_start = 0;
		start = srcp->sb_start;
		break;
	default:
		start = 0;
	}
	return (start);
}

/*
 *  DOCUMENTATION
 *
 *  Name: buffer-file-name
 *  Call: (buffer-file-name [ 'buffer [ 'filename ] ])
 *  Retu: string
 *  Desc: This function returns the full path name of the file
 *	attached to the buffer specified by the argument, or to
 *	the current buffer if no argument is given.  If the buffer
 *	is not connected to any file, nil is returned instead of
 *	any string.
 *
 *	If the optional second argument is given, it must
 *	evaluate to a non-empty string or to nil.  The name of
 *	the file to which the buffer is attached is then set to
 *	this argument.
 *  SeeA: buffer-name buffer-length
 */

DEFUN(dobuffile, "buffer-file-name", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct string	*file;
	struct buffer	*bufp;
	struct source	*srcp;

	CHECKAC(0, 2);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		/* just use the current buffer */
		bufp = current_buffer;
	}

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't edit files in this type of buffer.");

	/* get existing file name */
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	file = srcp->sb_file;

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (nullp(arg)) {
			/* no file to be used */
			srcp->sb_file = NULL;
		} else if (stringp(arg)) {
			if (file->st_length < 1)
				BADARGN(2, "a non-zero length file name");
			srcp->sb_file = file;
		} else {
			/* just an error */
			BADARGN(2, "a string file name");
		}
	}

	if (file == NULL) {
		/* not connected to a file */
		return (v_nil);
	} else {
		ret.vl_type = LISP_STRING;
		sstring(ret.vl_data, file);
		return (ret);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: bufferp
 *  Call: (bufferp 'buffer [ 'error ])
 *  Retu: t or nil
 *  Desc: This predicate function returns t if the string its
 *	first argument must evaluate to is the name of a
 *	currently existing buffer and nil otherwise.
 *
 *	If the second argument is given and evaluates non-nil
 *	and the named buffer does not exist, an error occurs
 *	complaining that the specified buffer does not exist.
 *  Xref: buffer-p
 *  SeeA: buffer-name buffer-modified-p
 */

DEFUN(dobufferp, "bufferp", FLAG_NONE, NULL)
{
	struct value	arg;
	struct string	*name;
	int		makerr = FALSE;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string name");
	name = gstring(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!symbolp(arg))
			makerr = TRUE;
	}

	if (buffer_get(name, makerr) == NULL)
		return (v_nil);
	else
		return (v_t);
}
