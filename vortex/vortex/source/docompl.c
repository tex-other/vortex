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
 *  RCS Info: $Header: docompl.c,v 0.1 87/05/01 11:38:12 john Locked $
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
 *  docompl.c - vLisp function to perform general completion
 */
static char _ID[] = "@(#)docompl.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: complete-from-list
 *  Call: (complete-from-list 'partial 'list)
 *  Retu: any
 *  Desc: This function compares the partial pattern given by the
 *	first argument against the strings in the list and returns
 *	the element of the list whose uniqe prefix it is, or nil
 *	if none.  The first argument and the elements of the list
 *	must all be strings or symbols.
 *
 *	This matching works exactly as does escape completion (when
 *	typing arguments to function in the minibuffer).  If the
 *	argument uniquely specifies one of the elements of the list,
 *	that element is returned, otherwise nil.
 *  SeeA: minibuf-input interactive
 */

DEFUN(docompletelist, "complete-from-list", FLAG_NONE, NULL)
{
	struct value	arg;
	struct value	elt, match;
	struct string	*pat, *str;
	register int	n, count;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_STRING:
		pat = gstring(arg.vl_data);
		break;
	case LISP_SYMBOL:
		pat = gsymbol(arg.vl_data)->sy_pname;
		break;
	default:
		BADARGN(1, "a partial match string");
	}
	arg = EVALARGN(2);
	switch (arg.vl_type) {
	case LISP_NIL:
		return (v_nil);
	case LISP_CONS:
		break;
	default:
		BADARGN(2, "a list of strings");
	}

	count = length(arg);
	match = v_nil;
	for (n = 0; n < count; n++) {
		elt = nth(n, arg);
		switch (elt.vl_type) {
		case LISP_STRING:
			str = gstring(elt.vl_data);
			break;
		case LISP_SYMBOL:
			str = gsymbol(elt.vl_data)->sy_pname;
			break;
		default:
			BADARGN(2, "a list of strings");
		}
		if (pat->st_length <= str->st_length && sprefix(pat, str)) {
			if (truep(match))
				return (v_nil);
			else
				match = elt;
		}
	}
	return (match);
}

static int
sprefix(s1, s2)
	struct string	*s1, *s2;
{
	register int	n, len;
	unsigned char	*c1, *c2;

	len = s1->st_length;
	if (len > s2->st_length)
		len = s2->st_length;
	c1 = s1->st_buffer;
	c2 = s2->st_buffer;
	for (n = 0; n < len; n++) {
		if (*c1 != *c2)
			return (FALSE);
		c1++; c2++;
	}
	return (TRUE);
}
