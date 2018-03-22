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
 *  RCS Info: $Header: doconcat.c,v 0.1 87/05/01 11:38:36 john Locked $
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
 *  doconcat.c - various concatenation functions
 */
static char _ID[] = "@(#)doconcat.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: concat
 *  Call: (concat 'string 'string [ 'string ... ])
 *  Retu: string
 *  Desc: This function evaluates all its arguments and concatenates
 *	time into a single string, which is returned.  All arguments
 *	must evaluate to strings (or symbols), and they are
 *	appended to the returned string in order.  No characters are
 *	inserted between the arguments in the output string.
 *  Xref: strcat
 *  SeeA: concata
 */

DEFUN(doconcat, "concat", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	char		cbuf[BIGBUF], *cend = cbuf + sizeof (cbuf) - 1;
	struct string	*str, junk;
	register int	count, argc;
	register char	*sp;

	CHECKAC(1, -1);
	argc = GETACOUNT();

	sp = cbuf;
	for (count = 1; count <= argc; count++) {
		arg = EVALARGN(count);
		switch (arg.vl_type) {
		case LISP_SYMBOL:
			str = gsymbol(arg.vl_data)->sy_pname;
			break;
		case LISP_STRING:
			str = gstring(arg.vl_data);
			break;
		case LISP_FIXNUM:
			junk.st_buffer = (unsigned char *)
			    pfixnum(gfixnum(arg.vl_data));
			junk.st_length = strlen(junk.st_buffer);
			str = &junk;
			break;
		default:
			BADARGN(count, "a string or symbol");
		}
		if (sp + str->st_length > cend)
			break;
		bcopy(str->st_buffer, sp, str->st_length);
		sp += str->st_length;
	}

	ret.vl_type = LISP_STRING;
	str = save_string(cbuf, sp - cbuf);
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: concata
 *  Call: (concata 'symbol 'symbol ...)
 *  Retu: symbol
 *  Desc: This function evaluates its argument and creates an
 *	symbol which is the concatenation of all these symbol
 *	arguments.  The arguments must evaluate to symbols (or
 *	strings) and will be appended in order in the output
 *	symbol without extra characters being inserted
 *	between the text from individual arguments.
 *
 *	For historical reasons, this function is also called
 *	\sym{mash}.  Both \sym{concata} and \sym{mash} work
 *	exactly the same and are, in fact, the same code.
 *  Xref: mash
 *  SeeA: concat
 */

DEFUN(doconcata, "concata", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	char		cbuf[BIGBUF], *cend = cbuf + sizeof (cbuf) - 1;
	struct string	*str;
	struct symbol	*sym;
	register int	count, argc;
	register char	*sp;

	CHECKAC(1, -1);
	argc = GETACOUNT();

	sp = cbuf;
	for (count = 1; count <= argc; count++) {
		arg = EVALARGN(count);
		switch (arg.vl_type) {
		case LISP_SYMBOL:
			str = gsymbol(arg.vl_data)->sy_pname;
			break;
		case LISP_STRING:
			str = gstring(arg.vl_data);
			break;
		default:
			BADARGN(count, "a symbol or string");
		}
		if (sp + str->st_length > cend)
			break;
		bcopy(str->st_buffer, sp, str->st_length);
		sp += str->st_length;
	}
	if (sp - cbuf < 1)
		error("It would be a bad thing to have a zero-length symbol.");

	ret.vl_type = LISP_SYMBOL;
	str = save_string(cbuf, sp - cbuf);
	sym = save_symbol(str);
	ssymbol(ret.vl_data, sym);
	return (ret);
}

DEFSAME(doconcata, "mash")
