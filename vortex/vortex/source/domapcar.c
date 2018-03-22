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
 *  RCS Info: $Header: domapcar.c,v 0.1 87/05/01 11:54:29 john Locked $
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
 *  domapcar.c - vLisp mapcar and mapc functions
 */
static char _ID[] = "@(#)domapcar.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: mapcar
 *  Call: (mapcar 'function 'list)
 *  Retu: list
 *  Desc: This function applies the given function to each element
 *	in the given list and returns a list containing the return
 *	value of the call for each element.  Both arguments are
 *	evaluated, the first must evaluate to a function, the second
 *	to a non-nil list.
 *
 *	This has the effect of calling the function given as the
 *	first argument for each element in the list with that
 *	element as the argument.  A list is created in which each
 *	element is the return value of the function called with the
 *	list element with the same index.  Thus, evaluating the
 *	expression \lit{(mapcar 'length '('(a) nil '(a b)))} would call
 *	the function \sym{eval} three times, once as \lit{(length '(a))},
 *	once as \lit{(length nil)}, and once as \lit{(length '(a b))}.
 *	After all this, \sym{mapcar} will return the list containing
 *	the return values \lit{(1 0 2)}.
 *
 *	Of course, each element of the list must be a proper argument
 *	list to the given function.  If an element is not a list, it
 *	will act like a single element list, which will work for a
 *	function which takes just a single argument.  A nil list means
 *	no arguments at all.
 *  Side: The given function will get called once for each element in
 *	the given list.  Of course, side effects of the function being
 *	called will occur once for each element in the list.
 *  SeeA: mapc
 */

DEFUN(domapcar, "mapcar", FLAG_NONE, NULL)
{
	struct value	arg1, fval, arg2, ret, list;
	register int	len, count;
	struct ccell	*last, *next;

	CHECKAC(2, 2);

	/* get the function, but also set up print name */
	arg1 = GETARGN(1);
	fval = evalsexpr(arg1);
	if (!funcp(fval))
		BADARGN(1, "a function");

	/* get second argument, must be a list */
	arg2 = EVALARGN(2);
	if (!dtprp(arg2))
		BADARGN(2, "a non-nil list");

	/* call the function repeatedly and build list */
	last = NULL;
	len = length(arg2);
	for (count = 0; count < len; count++) {
		/* evaluate this function call */
		ret = call_function(arg1, fval, nth(count, arg2));

		/* append this return value to list */
		next = save_ccell();
		if (last == NULL) {
			list.vl_type = LISP_CONS;
			slist(list.vl_data, next);
		} else {
			last->cc_tcdr = LISP_CONS;
			slist(last->cc_cdr, next);
		}
		next->cc_tcar = ret.vl_type;
		next->cc_car = ret.vl_data;
		last = next;
	}
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);
	return (list);
}

/*
 *  DOCUMENTATION
 *
 *  Name: mapc
 *  Call: (mapc 'function 'list)
 *  Retu: nil
 *  Desc: This function applies the given function to each element
 *	in the given list and throws the results away.
 *	Both arguments are evaluated, the first must evaluate to a
 *	function, the second to a non-nil list.  This function works
 *	just like \sym{mapcar}, except that the individual return
 *	values are not kept and nil is always returned.
 *
 *	This has the effect of calling the function given as the
 *	first argument for each element in the list with that
 *	element as the argument.  Thus, evaluating the expression
 *	\lit{(mapc 'print '('(a) nil '(a b)))} would call the
 *	function \sym{print} three times, once as \lit{(print '(a))},
 *	once as \lit{(print nil)}, and once as \lit{(print '(a b))}.
 *	After all this, \sym{mapc} will return nil.
 *
 *	Of course, each element of the list must be a proper argument
 *	list to the given function.  If an element is not a list, it
 *	will act like a single element list, which will work for a
 *	function which takes just a single argument.  A nil list means
 *	no arguments at all.
 *  Side: The given function will get called once for each element in
 *	the given list.  Of course, side effects of the function being
 *	called will occur once for each element in the list.
 *
 *	The only reason to use \sym{mapc} over \sym{mapcar} is that
 *	\sym{mapc} is very slightly faster since it doesn't have to
 *	build the list of return values.  It is expected that \sym{mapc}
 *	will be used on a long list when only the side effects of the
 *	function's evaluation are desired.
 *  SeeA: mapcar
 */

DEFUN(domapc, "mapc", FLAG_NONE, NULL)
{
	struct value	arg1, fval, arg2;
	register int	len, count;

	CHECKAC(2, 2);

	/* get the function, but also set up print name */
	arg1 = GETARGN(1);
	fval = evalsexpr(arg1);
	if (!funcp(fval))
		BADARGN(1, "a function");

	/* get second argument, must be a list */
	arg2 = EVALARGN(2);
	if (!dtprp(arg2))
		BADARGN(2, "a non-nil list");

	/* call the function repeatedly on elements */
	len = length(arg2);
	for (count = 0; count < len; count++) {
		/* evaluate this function call */
		call_function(arg1, fval, nth(count, arg2));
	}
	return (v_nil);
}
