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
 *  RCS Info: $Header: doexplode.c,v 0.1 87/05/01 11:45:10 john Locked $
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
 *  doexplode.c - various functions to explode an atom
 */
static char _ID[] = "@(#)doexplode.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: explode
 *  Call: (explode 'symbol)
 *  Retu: list
 *  Desc: This function takes a symbol or string and returns its print
 *	name expanded into a list so that each character is a separate
 *	symbol in the list.  Since the print name is actually exploded
 *	(rather than it's text), strings will also have the string
 *	delimiter characters appear in the output, and atoms that are
 *	quoted will have the quote character appear, etc.
 *  SeeA: explodec exploden
 */

DEFUN(doexplode, "explode", FLAG_NONE, NULL)
{
	extern char	*psymbol(), *pstring();
	struct value	ret, arg;
	struct ccell	*elt, *last;
	register char	*image;
	struct symbol	*sym;
	struct string	*str;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_SYMBOL:
		str = gsymbol(arg.vl_data)->sy_pname;
		image = psymbol(str);
		break;
	case LISP_STRING:
		str = gstring(arg.vl_data);
		image = pstring(str);
		break;
	default:
		BADARGN(1, "a string or symbol");
	}
	if (*image == '\0')
		return (v_nil);

	ret.vl_type = LISP_CONS;
	last = NULL;
	while (*image != '\0') {
		elt = save_ccell();
		if (last == NULL)
			slist(ret.vl_data, elt);
		else
			slist(last->cc_cdr, elt);
		elt->cc_tcar = LISP_SYMBOL;
		sym = save_symbol(save_string(image, 1));
		ssymbol(elt->cc_car, sym);
		elt->cc_tcdr = LISP_CONS;
		last = elt;
		image++;
	}
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);

	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: explodec
 *  Call: (explodec 'symbol)
 *  Retu: list
 *  Desc: This function takes a symbol or string and returns a list
 *	which is made up of the characters in its direct image, each
 *	as a separate symbol.  This is like \sym{explode} except
 *	that string delimiters and quotes never show up, and symbols
 *	containing ``funny characters'' are not escaped.
 *  SeeA: explode exploden
 */

DEFUN(doexplodec, "explodec", FLAG_NONE, NULL)
{
	struct value	ret, arg;
	struct ccell	*elt, *last;
	unsigned char	*image, *iend;
	struct string	*str;
	struct symbol	*sym;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_SYMBOL:
		str = gsymbol(arg.vl_data)->sy_pname;
		break;
	case LISP_STRING:
		str = gstring(arg.vl_data);
		break;
	default:
		BADARGN(1, "a string or symbol");
	}
	if (str->st_length == 0)
		return (v_nil);
	image = str->st_buffer;
	iend = str->st_buffer + str->st_length;

	ret.vl_type = LISP_CONS;
	last = NULL;
	while (image < iend) {
		elt = save_ccell();
		if (last == NULL)
			slist(ret.vl_data, elt);
		else
			slist(last->cc_cdr, elt);
		elt->cc_tcar = LISP_SYMBOL;
		sym = save_symbol(save_string(image, 1));
		ssymbol(elt->cc_car, sym);
		elt->cc_tcdr = LISP_CONS;
		last = elt;
		image++;
	}
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);

	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: exploden
 *  Call: (exploden 'symbol)
 *  Retu: list
 *  Desc: This function takes a symbol or string and returns a list which
 *	contains fixnums representing the \sc{ASCII} character codes
 *	of the characters in its direct image.  This is much
 *	the same as \sym{explodec}, except that character codes,
 *	not symbols whose names are the characters, are returned.
 *  SeeA: explode explodec
 */

DEFUN(doexploden, "exploden", FLAG_NONE, NULL)
{
	struct value	ret, arg;
	struct ccell	*elt, *last;
	unsigned char	*image, *iend;
	struct string	*str;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_SYMBOL:
		str = gsymbol(arg.vl_data)->sy_pname;
		break;
	case LISP_STRING:
		str = gstring(arg.vl_data);
		break;
	default:
		BADARGN(1, "a string or symbol");
	}
	if (str->st_length == 0)
		return (v_nil);
	image = str->st_buffer;
	iend = str->st_buffer + str->st_length;

	ret.vl_type = LISP_CONS;
	last = NULL;
	while (image < iend) {
		elt = save_ccell();
		if (last == NULL)
			slist(ret.vl_data, elt);
		else
			slist(last->cc_cdr, elt);
		elt->cc_tcar = LISP_FIXNUM;
		sfixnum(elt->cc_car, *image);
		elt->cc_tcdr = LISP_CONS;
		last = elt;
		image++;
	}
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);

	return (ret);
}
