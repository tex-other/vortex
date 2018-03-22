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
 *  RCS Info: $Header: dolist.c,v 0.1 87/05/01 11:52:59 john Locked $
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
 *  dolist.c - more vLisp list creation functions
 */
static char _ID[] = "@(#)dolist.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: list
 *  Call: (list 'any ...)
 *  Retu: list
 *  Desc: This function creates a new list using its evaluated
 *	arguments as the list elements.
 *  SeeA: cons append
 */

DEFUN(dolist, "list", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct ccell	*elt, *last;
	register int	count, argc;

	CHECKAC(1, -1);
	argc = GETACOUNT();

	ret.vl_type = LISP_CONS;
	last = NULL;
	for (count = 1; count <= argc; count++) {
		arg = EVALARGN(count);
		elt = save_ccell();
		if (last == NULL)
			slist(ret.vl_data, elt);
		else
			slist(last->cc_cdr, elt);
		elt->cc_tcar = arg.vl_type;
		elt->cc_car = arg.vl_data;
		elt->cc_tcdr = LISP_CONS;
		last = elt;
	}
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);

	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: append
 *  Call: (append 'list 'any)
 *  Retu: list
 *  Desc: This function returns a the list given by its first
 *	argument, with the additional element, given by the second
 *	argument, appended.
 *
 *	Trying to append to a non-list or to a dottted pair will
 *	cause an error.  Appending to the nil list does create a
 *	new list, since modifying nil doesn't make sense.
 *  Side: This modifies the list passed, not a copy of this list,
 *	this unpredictable side effects are possible.
 *  SeeA: cons list
 */

DEFUN(doappend, "append", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!listp(arg1))
		BADARGN(1, "a list");
	arg2 = EVALARGN(2);

	return append(arg1, arg2);
}

struct value
append(list, elt)
	struct value	list, elt;
{
	struct value	ret;
	struct ccell	*new, *last;

	new = save_ccell();
	new->cc_tcar = elt.vl_type;
	new->cc_car = elt.vl_data;
	new->cc_tcdr = LISP_NIL;
	slist(new->cc_cdr, NULL);

	/* append this new cons cell to existing list */
	if (dtprp(list)) {
		last = glist(list.vl_data);
		while (last != NULL && last->cc_tcdr == LISP_CONS)
			last = glist(last->cc_cdr);
		if (last == NULL || last->cc_tcdr != LISP_NIL)
			error("Cdr of last element in list isn't nil!");
		last->cc_tcdr = LISP_CONS;
		slist(last->cc_cdr, new);
		ret = list;
	} else if (nullp(list)) {
		ret.vl_type = LISP_CONS;
		slist(ret.vl_data, new);
	} else {
		/* this shouldn't happen */
		ierror("Non-list to append to!");
	}

	return (ret);
}

/* VARARGS */
struct value
makelist(va_alist)
	va_dcl
{
	va_list		vargs;
	struct value	val, ret;
	struct ccell	*new, *all, *last;
	register int	argc;

	last = all = NULL;
	va_start(vargs);
	for (argc = va_arg(vargs, int); argc > 0; argc--) {
		/* get the next list value */
		val = va_arg(vargs, struct value);

		/* allocate new cons cell and set car */
		new = save_ccell();
		new->cc_tcar = val.vl_type;
		new->cc_car = val.vl_data;

		if (all == NULL) {
			/* this is the head of the list */
			all = new;
		} else {
			/* set cdr of previous cons cell */
			last->cc_tcdr = LISP_CONS;
			slist(last->cc_tcdr, new);
		}
		last = new;
	}
	va_end(vargs);
	if (last == NULL)
		return (v_nil);
	ASSERT(all != NULL);

	/* terminate the cdr of the last cons cell and return list */
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);
	ret.vl_type = LISP_CONS;
	slist(ret.vl_data, all);
	return (ret);
}
