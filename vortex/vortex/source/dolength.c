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
 *  RCS Info: $Header: dolength.c,v 0.1 87/05/01 11:52:00 john Locked $
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
 *  dolength.c - functions to report length of lists and arrays
 */
static char _ID[] = "@(#)dolength.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: length
 *  Call: (length 'list)
 *  Retu: fixnum
 *  Desc: This function counts the number of elements in its single
 *	argument, which should evaluate to a list or array.  If the
 *	argument is nil, the count is zero, otherwise the count should
 *	be a positive number.  Note that the second element of a dotted
 *	pair counts as an element here, so
 *
 *	\lit{(length '(a b . c))}
 *
 *	will evaluate to \lit{3} (three elements, including the symbol
 *	\lit{c}, the second element of the dotted pair).
 */

DEFUN(dolength, "length", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct array	*arr;

	CHECKAC(1, 1);
	arg = EVALARGN(1);

	switch (arg.vl_type) {
	case LISP_CONS:
		/* count the elements ... */
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, length(arg));
		return (ret);
	case LISP_NIL:
		/* nil list has no elements... */
		return (v_zero);
	case LISP_ARRAY:
		arr = garray(arg.vl_data);
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, arr->ar_length);
		return (ret);
	default:
		BADARGN(1, "a list");
	}
	/* NOTREACHED */
}

length(sexpr)
	struct value	sexpr;
{
	register int	count;
	struct ccell	*elt;
	struct array	*arr;

	if (nullp(sexpr))
		return (0);

	if (arrayp(sexpr)) {
		arr = garray(sexpr.vl_data);
		return (arr->ar_length);
	}

	if (!dtprp(sexpr))
		ierror("length: Non-list argument to count length of!");

	elt = glist(sexpr.vl_data);
	for (count = 1; elt->cc_tcdr == LISP_CONS; count++)
		elt = glist(elt->cc_cdr);
	if (elt->cc_tcdr != LISP_NIL)
		count++;

	return (count);
}
