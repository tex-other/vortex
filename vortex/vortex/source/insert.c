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
 *  RCS Info: $Header: insert.c,v 0.1 87/05/01 12:16:59 john Locked $
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
 *  insert.c - insert text and characters into a buffer
 */
static char _ID[] = "@(#)insert.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include <signal.h>
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"
#include "format.h"
#include "gl_comm.h"
#include "ts_comm.h"

/*
 *  DOCUMENTATION
 *
 *  Name: self-insert
 *  Call: (self-insert [ 'count ])
 *  Retu: fixnum
 *  Desc: This function inserts the key sequence that invoked it
 *	when invoked interactively into the current buffer before
 *	point.  If the optional argument is given, it specifies
 *	a count of the number of times the sequence is to be inserted
 *	(and so must be positive to be of use).  The function returns
 *	the string key sequence inserted.
 *
 *	This is usually used as the binding for all the printable
 *	\sc{ASCII} characters that are ``just text''.  That is,
 *	when those keys are typed, they appear in the buffer rather
 *	than causing some other editing action to occur.
 *  Side: The current buffer becomes modified and one or more
 *	characters are inserted before point.
 *  Xref: self-insert-command
 *  SeeA: newline quoted-insert
 */
extern struct string	*typed_keys;

DEFUN(doselfinsert, "self-insert", FLAG_NONE, "p")
{
	struct value	arg, ret;
	register int	i, count = 1;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	int		code;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a positive fixnum");
		count = gfixnum(arg.vl_data);
	}
	bufp = windowbuffer(winp, BUFF_SOURCE);

	/* get the character code to insert */
	if (typed_keys == NULL)
		return (v_nil);
	code = typed_keys->st_buffer[typed_keys->st_length - 1];

	for (i = 0; i < count; i++) {
		insert_char(bufp, code);
		setwindowpoint(winp, winp->wi_point + 1);
	}

	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, typed_keys);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: newline
 *  Call: (newline [ 'count ])
 *  Retu: fixnum
 *  Desc: This function always inserts a newline character into the
 *	current buffer before point.  If the optional argument is given,
 *	it specifies a count of the number of newlines to be inserted
 *	(and so must be positive to be of use).  The function returns
 *	the number of newlines inserted.
 *
 *	This is usually used as the binding for the \sc{ASCII}
 *	\em{line feed} and \em{carriage return} keys.
 *  Side: The current buffer becomes modified and one or more
 *	characters are inserted before point.
 *  SeeA: self-insert quoted-insert
 */

DEFUN(donewline, "newline", FLAG_NONE, "p")
{
	struct value	arg, ret;
	register int	i, count = 1;
	struct window	*winp = current_window;
	struct buffer	*bufp;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a positive fixnum");
		count = gfixnum(arg.vl_data);
	}
	bufp = windowbuffer(winp, BUFF_SOURCE);

	/* insert that many newlines */
	for (i = 0; i < count; i++) {
		insert_char(bufp, '\n');
		winp->wi_point++;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: quoted-insert
 *  Call: (quoted-insert [ 'count ])
 *  Retu: fixnum
 *  Desc: This function reads the next character typed and
 *	inserts it into the current buffer before point no
 *	matter what character it is.  If the optional argument
 *	is given, it should evaluate to a positive fixnum, it
 *	specifies the number of times to insert the character.
 *	The \sc{ASCII} character code of the character inserted
 *	is returned.
 *
 *	This is usually used to insert a character which is bound
 *	to something other than \sym{self-insert} into a buffer.
 *  Side: The current buffer becomes modified and one or more
 *	characters are inserted before point.
 *  SeeA: self-insert
 */

DEFUN(doquotedinsert, "quoted-insert", FLAG_NONE, "p")
{
	struct value	arg, ret;
	register int	i, count = 1;
	unsigned int	code;
	struct window	*winp = current_window;
	struct buffer	*bufp;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a positive fixnum");
		count = gfixnum(arg.vl_data);
	}
	bufp = windowbuffer(winp, BUFF_SOURCE);

	/* get next key stroke typed */
	while (input_event(TRUE, &code) <= 0)
		;
	for (i = 0; i < count; i++) {
		insert_char(bufp, code);
		winp->wi_point++;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, code);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: insert-string
 *  Call: (insert-string 'string [ 'buffer | 'window ])
 *  Retu: fixnum
 *  Desc: This function inserts the given text string into
 *	the given buffer before point.  If no buffer is
 *	given as the optional second argument, the current
 *	buffer is used.  The number of characters transfered
 *	(the length of the string) is returned.
 *  Side: The buffer becomes modified and characters are inserted
 *	before point, if the string is longer than zero characters.
 *  SeeA: insert-char
 */

DEFUN(doinsertstring, "insert-string", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct window	*winp = current_window;
	struct buffer	*bufp = NULL;
	struct string	*text;
	int		nchars;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string");
	text = gstring(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (stringp(arg)) {
			/* just insert into the buffer */
			bufp = buffer_get(gstring(arg.vl_data), TRUE);
			winp = NULL;
		} else if (fixnump(arg)) {
			/* insert into the window/buffer */
			winp = getwindow(gfixnum(arg.vl_data));
			bufp = NULL;
		} else {
			/* not a buffer or window */
			BADARGN(2, "a buffer or window");
		}
	}
	if (bufp == NULL) {
		bufp = windowbuffer(winp, BUFF_SOURCE);

		/* set the buffer's point and mark */
		setpoint(bufp, winp->wi_point);
		setmark(bufp, winp->wi_mark);
	}

	/* insert the characters now */
	nchars = insert_string(bufp, text);

	/* update the window's point and mark */
	if (winp != NULL) {
		winp->wi_point = getpoint(bufp);
		winp->wi_mark = getmark(bufp);
		winp->wi_flags |= WIN_REFRESH;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, nchars);
	return (ret);
}

static char	READONLY[] = "Can't insert into %Y; buffer is read-only.";
#define CHECKWRITE(b)	(((b)->bu_flags & BUFF_READONLY) != 0 ? \
			 error(READONLY, (b)->bu_name) : 0)

insert_string(bufp, text)
	struct buffer	*bufp;
	struct string	*text;
{
	struct tblock	*next, *last;
	struct string	left;
	register int	point, nchars, inserted = 0;
	unsigned long	*txp, *tend, *begin;
	unsigned char	*cp;
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't insert text into this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* quick check to make sure there is something to do */
	if (text->st_length <= 0 || text->st_buffer == NULL)
		return (0);

	/* can't do it if buffer is read-only */
	CHECKWRITE(bufp);

	/* find the block containing the insertion */
	point = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (point < next->tb_offset + next->tb_length)
			break;
		last = next;
	}

	PROTECT();
	if (next == NULL) {
		/* need to make a new block at end, we're appending */
		next = save_tblock();
		if (last == NULL) {
			srcp->sb_text = next;
			next->tb_offset = 0;
		} else {
			last->tb_next = next;
			next->tb_offset = last->tb_offset + last->tb_length;
		}
		next->tb_prev = last;
		next->tb_length = 0;
		next->tb_next = NULL;
	}

	/* now, we know that point is somewhere inside next */
	if (point < next->tb_offset + next->tb_length) {
		/* we have to split the current tblock to insert */
		last = next;
		next = save_tblock();
		next->tb_offset = point;
		next->tb_length = last->tb_offset + last->tb_length - point;
		bcopy(last->tb_text + (point - last->tb_offset),
		    next->tb_text, next->tb_length * sizeof (unsigned long));
		last->tb_length = point - last->tb_offset;
		next->tb_next = last->tb_next;
		last->tb_next = next;
		next->tb_prev = last;
		next = last;
	}

	/* now we know that we're appending to next */
	left = *text;
	while (left.st_length > 0) {
		/* transfer as many characters as possible */
		nchars = TBLOCKSIZE - next->tb_length;
		if (nchars > left.st_length)
			nchars = left.st_length;
		tend = next->tb_text + next->tb_length + nchars;
		begin = txp = next->tb_text + next->tb_length;
		for (cp = left.st_buffer; txp < tend; cp++, txp++)
			*txp = charID(*cp, 0, newcharID(*cp));
		next->tb_length += nchars;

		/* subtract written portion from copy of string */
		left.st_length -= nchars;
		left.st_buffer += nchars;
		inserted += nchars;
		if (left.st_length > 0) {
			/* create another tblock to insert into */
			last = next;
			next = save_tblock();
			next->tb_next = last->tb_next;
			last->tb_next = next;
			next->tb_prev = last;
			next->tb_offset = last->tb_offset + last->tb_length;
			next->tb_length = 0;
		}
	}

	if (inserted > 0) {
		/* send inserted portion to formatter */
		send_insert(bufp, point, inserted);
	
		/* increment buffer length and succeeding offsets */
		srcp->sb_length += inserted;
		if (srcp->sb_mark >= srcp->sb_point)
			srcp->sb_mark += inserted;
		srcp->sb_point += inserted;
		for (next = next->tb_next; next != NULL; next = next->tb_next)
			next->tb_offset += inserted;
		bufp->bu_flags |= BUFF_MODIFIED|BUFF_CHANGED;
	}
	PROTECT();

	return (text->st_length);
}

/*
 *  DOCUMENTATION
 *
 *  Name: insert-char
 *  Call: (insert-char 'code [ 'buffer ])
 *  Retu: code
 *  Desc: This function inserts the given \sc{ASCII} character
 *	code into the given buffer before point.  If no buffer
 *	is given as the optional second argument, the current
 *	buffer is used.
 *  Side: The buffer becomes modified and a character is inserted
 *	before point.
 *  SeeA: self-insert insert-string
 */

DEFUN(doinsertchar, "insert-char", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp;
	int		code;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a character code fixnum");
	code = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a buffer name string");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		/* just use the current buffer */
		bufp = current_buffer;
	}

	/* insert the character now */
	insert_char(bufp, code);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, code);
	return (ret);
}

insert_char(bufp, code)
	struct buffer	*bufp;
	int		code;
{
	struct tblock	*next, *last;
	register int	point;
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't insert characters into this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	if (code < 0 || code > 255) {
		error("Bad character code %d to insert into buffer %Y!",
		    code, bufp->bu_name);
	}

	/* can't do it if buffer is read-only */
	CHECKWRITE(bufp);

	/* find the block containing the insertion */
	point = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (point < next->tb_offset + next->tb_length)
			break;
		last = next;
	}

	PROTECT();
	if (next == NULL) {
		/* need to make a new block at end, we're appending */
		next = save_tblock();
		if (last == NULL) {
			srcp->sb_text = next;
			next->tb_offset = 0;
		} else {
			last->tb_next = next;
			next->tb_offset = last->tb_offset + last->tb_length;
		}
		next->tb_prev = last;
		next->tb_next = NULL;
		next->tb_length = 0;
	}

	/* now, we know that point is somewhere inside next */
	if (point < next->tb_offset + next->tb_length) {
		/* we have to split the current tblock to insert */
		last = next;
		next = save_tblock();
		next->tb_offset = point;
		next->tb_length = last->tb_offset + last->tb_length - point;
		bcopy(last->tb_text + (point - last->tb_offset),
		    next->tb_text, next->tb_length * sizeof (unsigned long));
		last->tb_length = point - last->tb_offset;
		next->tb_next = last->tb_next;
		last->tb_next = next;
		next->tb_prev = last;
		next = last;
	}

	/* now we know that we're appending to next */
	next->tb_text[next->tb_length] = charID(code, 0, newcharID(code));
	next->tb_length++;

	/* send insert to formatter */
	send_insert(bufp, point, 1);

	/* increment buffer length and succeeding offsets */
	srcp->sb_length++;
	if (srcp->sb_mark >= srcp->sb_point)
		srcp->sb_mark++;
	srcp->sb_point++;
	for (next = next->tb_next; next != NULL; next = next->tb_next)
		next->tb_offset++;
	bufp->bu_flags |= BUFF_MODIFIED|BUFF_CHANGED;
	UNPROTECT();

	return (code);
}

insert_buffer(bufp, from)
	struct buffer	*bufp, *from;
{
	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: insert-file
 *  Call: (insert-file 'file [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function inserts the contents of the \sc{UNIX} file
 *	specified by its argument into the given buffer, or into the
 *	current buffer if none is given, before point.  The number
 *	of characters transfered, the length of the file, is
 *	returned.
 *  Side: The buffer becomes modified if the file contained any
 *	characters.
 *  SeeA: insert-string insert-buffer
 */

DEFUN(doinsertfile, "insert-file", FLAG_NONE, "FInsert file: ")
{
	struct value	arg, ret;
	struct buffer	*bufp;
	struct string	*str;
	char		*file, fbuf[STRBUF];
	int		nchars;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");
	str = gstring(arg.vl_data);
	if (str->st_length < 1)
		BADARGN(1, "a non-zero length file name string");
	makecstring(str, fbuf, sizeof (fbuf));
	file = fixpath(fbuf);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		/* just use the current buffer */
		bufp = current_buffer;
	}

	/* insert the characters now */
	nchars = insert_file(bufp, file);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, nchars);
	return (ret);
}

insert_file(bufp, file)
	struct buffer	*bufp;
	char		*file;
{
	struct tblock	*next, *last;
	register int	point, nchars, infd, total;
	unsigned long	*txp, *tend, *begin;
	unsigned char	*cp, cbuf[TBLOCKSIZE];
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't insert a file into this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* can't do it if buffer is read-only */
	CHECKWRITE(bufp);

	/* try to open file now */
	if ((infd = open(file, 0, 0)) < 0)
		perror("Can't open file %s to insert", file);

	/* find the block containing the insertion */
	point = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (point < next->tb_offset + TBLOCKSIZE)
			break;
		last = next;
	}

	PROTECT();
	if (next == NULL) {
		/* need to make a new block at end, we're appending */
		next = save_tblock();
		if (last == NULL) {
			srcp->sb_text = next;
			next->tb_offset = 0;
		} else {
			last->tb_next = next;
			next->tb_offset = last->tb_offset + last->tb_length;
		}
		next->tb_length = 0;
		next->tb_prev = last;
		next->tb_next = NULL;
	}

	/* now, we know that point is somewhere inside next */
	if (point < next->tb_offset + next->tb_length) {
		/* we have to split the current tblock to insert */
		last = next;
		next = save_tblock();
		next->tb_offset = point;
		next->tb_length = last->tb_offset + last->tb_length - point;
		bcopy(last->tb_text + (point - last->tb_offset),
		    next->tb_text, next->tb_length * sizeof (unsigned long));
		last->tb_length = point - last->tb_offset;
		next->tb_next = last->tb_next;
		last->tb_next = next;
		next->tb_prev = last;
		next = last;
	}

	/* now we know that we're appending to next */
	total = 0;
	nchars = read(infd, cbuf, TBLOCKSIZE - next->tb_length);
	while (nchars > 0) {
		tend = next->tb_text + next->tb_length + nchars;
		begin = txp = next->tb_text + next->tb_length;
		for (cp = cbuf; txp < tend; cp++, txp++)
			*txp = charID(*cp, 0, newcharID(*cp));
		next->tb_length += nchars;
		total += nchars;

		/* read as many more characters as possible */
		nchars = read(infd, cbuf, TBLOCKSIZE);
		if (nchars > 0) {
			/* create another tblock to insert into */
			last = next;
			next = save_tblock();
			next->tb_next = last->tb_next;
			last->tb_next = next;
			next->tb_offset = last->tb_offset + last->tb_length;
			next->tb_length = 0;
			next->tb_prev = last;
			if (next->tb_next != NULL)
				next->tb_next->tb_prev = next;
		}
	}
	close(infd);

	if (total > 0) {
		/* send insert to formatter */
		send_insert(bufp, point, total);

		/* increment buffer length and succeeding offsets */
		srcp->sb_length += total;
		if (srcp->sb_mark >= srcp->sb_point)
			srcp->sb_mark += total;
		srcp->sb_point += total;
		for (next = next->tb_next; next != NULL; next = next->tb_next)
			next->tb_offset += total;
		bufp->bu_flags |= BUFF_MODIFIED|BUFF_CHANGED;
	}
	UNPROTECT();

	return (total);
}

/*
 *  DOCUMENTATION
 *
 *  Name: append-string
 *  Call: (append-string 'string [ 'buffer ])
 *  Retu: fixnum
 *  Desc: This function appends the given text string to the
 *	the given buffer .  If no buffer is given as the optional
 *	second argument, the current buffer is used.  The number
 *	of characters transfered (the length of the string) is returned.
 *  Side: The buffer becomes modified and characters are appended
 *	to the buffer, if the string is longer than zero characters.
 *	Neither the point or mark of the buffer is modified.
 *  SeeA: insert-string
 */

DEFUN(doappendstring, "append-string", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp;
	struct string	*text;
	int		nchars;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string");
	text = gstring(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a buffer name string");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		/* just use the current buffer */
		bufp = current_buffer;
	}

	/* insert the characters now */
	nchars = append_string(bufp, text);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, nchars);
	return (ret);
}

append_string(bufp, text)
	struct buffer	*bufp;
	struct string	*text;
{
	struct tblock	*next, *last;
	struct string	left;
	unsigned long	*txp, *tend;
	register int	nchars;
	unsigned char	*cp;
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't append text to this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* quick check to make sure there is something to do */
	if (text->st_length <= 0 || text->st_buffer == NULL)
		return (0);

	/* find the last block to append to */
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next)
		last = next;

	PROTECT();
	left = *text;
	while (left.st_length > 0) {
		/* need to make a new block at end, we're appending */
		next = save_tblock();
		if (last == NULL) {
			srcp->sb_text = next;
			next->tb_offset = 0;
		} else {
			last->tb_next = next;
			next->tb_offset = last->tb_offset + last->tb_length;
		}
		next->tb_prev = last;
		last = next;

		/* transfer as many characters as possible */
		if (left.st_length < TBLOCKSIZE)
			nchars = left.st_length;
		else
			nchars = TBLOCKSIZE;
		tend = next->tb_text + nchars;
		txp = next->tb_text;
		for (cp = left.st_buffer; txp < tend; cp++, txp++)
			*txp = charID(*cp, 0, newcharID(*cp));
		next->tb_length = nchars;

		/* advance string counter */
		left.st_buffer += nchars;
		left.st_length -= nchars;
	}

	/* increment buffer length and mark as modified */
	srcp->sb_length += text->st_length;
	bufp->bu_flags |= BUFF_MODIFIED|BUFF_CHANGED;
	UNPROTECT();

	return (text->st_length);
}

/* VARARGS */
binsert(va_alist)
	va_dcl
{
#include "fmtdecl.h"
	struct buffer	*bufp;
	struct string	sbuf;

	STARTVA();
	bufp = GETVARG(struct buffer *);
	ASSERT(bufp != NULL);

#include "fmtcode.h"

	sbuf.st_length = msglen;
	sbuf.st_buffer = (unsigned char *)msgbuf;
	return insert_string(bufp, &sbuf);
}

/* VARARGS */
bappend(va_alist)
	va_dcl
{
#include "fmtdecl.h"
	struct buffer	*bufp;
	struct string	sbuf;

	STARTVA();
	bufp = GETVARG(struct buffer *);
	ASSERT(bufp != NULL);

#include "fmtcode.h"

	sbuf.st_length = msglen;
	sbuf.st_buffer = (unsigned char *)msgbuf;
	return append_string(bufp, &sbuf);
}

/*
 *  Table of the next available character ID for each of the 128
 *  possible ASCII codes.  We can reset set them by zeroing the
 *  buffer, as done by reset_charIDs() below.
 */
long	next_charIDs[128];

restart_charIDs()
{
	bzero((char *)next_charIDs, sizeof (next_charIDs));

	return (0);
}


int		pending_inserts = FALSE;
static int	insert_count = 0;

send_insert(bufp, offset, length)
	struct buffer	*bufp;
{
	extern char	*alloca();
	extern int	format_socket;
	extern int	pending_deletes;
	struct tblock	*next;
	unsigned long	*data, *dp, *dend, *cp;
	struct source	*srcp;
	int		len;

	/* make sure we have a formatter */
	if (format_socket < 0)
		return (1);

	/* flush pending deletes so they don't get mingled with inserts */
	if (pending_deletes)
		flush_deletes();

	/* get the source buffer info. and make sure this is a TeX buffer */
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	if ((bufp->bu_flags & BUFF_TEXFILE) == 0 || srcp->sb_fileID < 0)
		return (1);

	/* find block containing the offset */
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (offset < next->tb_offset + next->tb_length)
			break;
	}
	ASSERT(next != NULL);

	PROTECT();
	/* allocate temporary space to store data */
	len = (length + 3) * sizeof (long);
	data = (unsigned long *)alloca(len);
	ASSERT(data != NULL);
	dp = data + 3;
	dend = dp + length;

	/* save the insert parameters */
	data[0] = htonl(IRS_BOF);
	data[1] = htonl(offset);
	data[2] = htonl(length);

	/* copy the inserted characters into a contiguous block */
	cp = next->tb_text + (offset - next->tb_offset);
	while (dp < dend) {
		if (cp >= next->tb_text + next->tb_length) {
			next = next->tb_next;
			ASSERT(next != NULL);
			cp = next->tb_text;
		}
		putchar(charof(*cp));
		*dp++ = htonl(*cp);
		cp++;
	}

	/* send this insertion now */
	sendformat(TSC_INSERT, srcp->sb_fileID, len, (char *)data);
	insert_count += length;
	UNPROTECT();

	return (0);
}

flush_inserts()
{
	if (!pending_inserts)
		return (0);

	debug(DITEX, "Sent %d newly inserted characters to formatter.",
	      insert_count);
	insert_count = 0;

	pending_inserts = FALSE;
	return (0);
}
