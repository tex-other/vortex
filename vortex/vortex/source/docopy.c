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
 *  RCS Info: $Header: docopy.c,v 0.1 87/05/01 11:41:38 john Locked $
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
 *  docopy.c - copy any vLisp type (equal but not eq)
 */
static char _ID[] = "@(#)docopy.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: copy
 *  Call: (copy 'list)
 *  Retu: list
 *  Desc: This function takes a list (or an array) and returns a
 *	new value that is \sym{equal}, but not \sym{eq} (stored
 *	in the same memory locations) as the argument value.
 *  Side: New cons cells must be allocated, by definition, which
 *	uses up memory, and if the list is large, will force
 *	a garbage collection that much sooner.
 *
 *	If the argument is a list, sublists are copied recursively.
 *	If the argument is an array, sublists of the array are
 *	not copied recursively.  This means the elements of the
 *	old and new arrays will be \sym{eq}.
 *  SeeA: cons
 */

DEFUN(docopy, "copy", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct array	*old, *new;
	struct value	*v1, *v2, *vend;

	CHECKAC(1, 1);
	arg = EVALARGN(1);

	if (dtprp(arg)) {
		/* return a copy of the list */
		return copy(arg);
	} else if (arrayp(arg)) {
		old = garray(arg.vl_data);
		new = save_array(old->ar_length);
		v2 = new->ar_array;
		vend = old->ar_array + old->ar_length;
		for (v1 = old->ar_array; v1 < vend; v1++)
			*v2++ = *v1;
		ret.vl_type = LISP_ARRAY;
		sarray(ret.vl_data, new);
		return (ret);
	} else {
		/* it's not a list or array, just return it */
		return (arg);
	}
}

struct value
copy(sexpr)
	struct value	sexpr;
{
	struct value	ret, tmp;
	struct ccell	*old, *new, *last;

	if (!dtprp(sexpr))
		ierror("copy: Non-list argument to copy!");

	ret.vl_type = LISP_CONS;
	last = NULL;
	old = glist(sexpr.vl_data);
	do {
		new = save_ccell();
		if (last == NULL)
			slist(ret.vl_data, new);
		else
			slist(last->cc_cdr, new);
		if (old->cc_tcar == LISP_CONS) {
			/* copy the list recursively */
			tmp.vl_type = LISP_CONS;
			tmp.vl_data = old->cc_car;
			tmp = copy(tmp);
			new->cc_tcar = tmp.vl_type;
			new->cc_car = tmp.vl_data;
		} else {
			/* just reference the element */
			new->cc_tcar = old->cc_tcar;
			new->cc_car = old->cc_car;
		}
		new->cc_tcdr = LISP_CONS;
		last = new;
	} while (old->cc_tcdr == LISP_CONS && (old = glist(old->cc_cdr)));
	ASSERT(old != NULL);
	last->cc_tcdr = old->cc_tcdr;
	last->cc_cdr = old->cc_cdr;

	return (ret);
}
