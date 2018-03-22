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
 *  RCS Info: $Header: dosyntax.c,v 0.1 87/05/01 12:04:41 john Locked $
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
 *  dosyntax.c - functions to manipulate the vLisp read syntax
 */
static char _ID[] = "@(#)dosyntax.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  Storage for syntax specification characters.  These may
 *  be set with the syntax function, and make the is<what>(c)
 *  macros work.  These macros compare input characters against
 *  the characters in these variables.  Everything except space
 *  characters is changeable simply by changing the contents
 *  of these variables.
 */
short	p_blistc = '(',		/* beginning of list token */
	p_elistc = ')',		/* end if list token */
	p_quotec = '\'',	/* quote a symbol or list */
	p_bquotec = '`',	/* backquote character */
	p_dtprc = '.',		/* dotted pair separator */
	p_stringc = '"',	/* begin/end a string */
	p_barrayc = '[',	/* begin an array */
	p_earrayc = ']',	/* end an array */
	p_literalc = '?',	/* create a character constant */
	p_escapec = '\\',	/* escape a special char */
	p_commentc = ';',	/* comment rest of line */
	p_delimc = '|';		/* symbol name delimiter */

struct syntax {
	struct string	sn_what;	/* syntactic function name */
	char		*sn_cname;	/* string C name initially */
	short		*sn_addr;	/* where to place character */
};

static struct syntax	read_syntax[] = {
	{ { 0 }, "blist",		&p_blistc },
	{ { 0 }, "elist",		&p_elistc },
	{ { 0 }, "quote",		&p_quotec },
	{ { 0 }, "bquote",		&p_bquotec },
	{ { 0 }, "dtpr",		&p_dtprc },
	{ { 0 }, "string",		&p_stringc },
	{ { 0 }, "barray",		&p_barrayc },
	{ { 0 }, "earray",		&p_earrayc },
	{ { 0 }, "literal",		&p_literalc },
	{ { 0 }, "escape",		&p_escapec },
	{ { 0 }, "comment",		&p_commentc },
	{ { 0 }, "delim",		&p_delimc },
	{ NULL,	NULL,			NULL }
};

/*
 *  DOCUMENTATION
 *
 *  Name: syntax
 *  Call: (syntax 'function [ 'character ])
 *  Retu: number
 *  Desc: This function sets or returns a reader syntax character.
 *	The first argument must evaluate to a symbol, one of the
 *	syntax characters listed below.  The second argument, if
 *	present, must evaluate to a character specification.  This
 *	function returns the character code assigned to the given
 *	function as a fixnum.
 *
 *	The vlisp reader compares characters as read to the character
 *	codes stored for each syntax function to parse the expression.
 *	Every character that is not a syntax function (and not a space)
 *	is a normal text character that may be part of a symbol.  Any
 *	special character may be made a normal text character by
 *	escaping it with the \lit{escape} syntax character (a doubled
 *	\lit{escape} means the single text character, \lit{escape}).
 *
 *	\tab{The list of syntax functions is:
 *	blist	beginning-of-list delimiter, usually \lit{(}
 *	elist	end-of-list delimiter, usually \lit{)}
 *	quote	quote read macro, usually \lit{'}
 *	bquote	backquote read macro, usually \lit{`}
 *	dtpr	dotted pair designator, usually \lit{.}
 *	string	string constant delimiter, usually \lit{"}
 *	blist	beginning-of-array delimiter, usually \lit{[}
 *	elist	end-of-array delimiter, usually \lit{]}
 *	escape	cancel special meaning of next character, usually \lit{\\}
 *	comment	ignore rest of line, usually \lit{;}
 *	delim	show boundaries of a symbol, usually \lit{|}}
 *
 *	Note that the syntax functions \lit{barray}, \lit{earray} and
 *	\lit{delim} are not special to the reader, they are used only
 *	by \sym{print}. The \lit{barray} and \lit{earray} characters
 *	delimit arrays and \lit{delim} surrounds symbol names containing
 *	``funny'' characters.
 *
 *	If the character value is given as \lit{undef} (or \lit{-1}),
 *	that syntax function is disabled completely, otherwise the
 *	second argument, if present, should be a character code.
 *  Side: Future characters read by the lisp reader may be interpreted
 *	in very different ways than previous to this call because the
 *	syntax characters may have changed.  These characters are also
 *	used when printing s-expressions so that output mirrors input.
 *
 *	Note that if you aren't careful and set the syntax of some
 *	fo
 *  SeeA: read print
 */
extern struct syntax	*getsynname();

MKSTRING(UNDEFNAME, "undef");

DEFUN(dosyntax, "syntax", FLAG_NONE, NULL)
{
	struct syntax	*sp;
	struct value	arg1, arg2, ret;
	struct string	*str;
	int		code;

	CHECKAC(1, 2);
	/* we want to return character of this syntax */
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a syntax function symbol");
	str = gsymbol(arg1.vl_data)->sy_pname;
	sp = getsynname(str);

	if (GETACOUNT() > 1) {
		/* we want to set a syntax characters */
		arg2 = EVALARGN(2);
		switch (arg2.vl_type) {
		case LISP_SYMBOL:
			str = gsymbol(arg2.vl_data)->sy_pname;
			if (!sequal(str, UNDEFNAME))
				BADARGN(2, "a character or undef");
			code = -1;
			break;
		case LISP_FIXNUM:
			code = gfixnum(arg2.vl_data);
			if (code < 0)
				code = -1;
			else
				code &= 0377;
			break;
		default:
			BADARGN(2, "a character code");
		}
		*sp->sn_addr = code;
	}

	/* print out the value of the given character */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, *sp->sn_addr);
	return (ret);
}

static struct syntax *
getsynname(name)
	struct string	*name;
{
	struct syntax	*sp;

	for (sp = read_syntax; sp->sn_addr != NULL; sp++) {
		if (sp->sn_what.st_buffer == NULL) {
			sp->sn_what.st_buffer = (unsigned char *)sp->sn_cname;
			sp->sn_what.st_length = strlen(sp->sn_cname);
		}
		if (sequal(&sp->sn_what, name))
			break;
	}
	if (sp->sn_addr == NULL)
		error("Unknown syntax function %Y.", name);
}
