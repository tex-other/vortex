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
 *  RCS Info: $Header: doequal.c,v 0.1 87/05/01 11:43:30 john Locked $
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
 *  doequal.c - vLisp equal and eq functions
 */
static char _ID[] = "@(#)doequal.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: equal
 *  Call: (equal 'any 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its two arguments
 *	evaluate to lisp values that ``look the same'' and nil
 *	otherwise.  Two values are \sym{equal} if they would appear
 *	the same when \sym{print}ed.
 *  SeeA: eq
 *
 *  END
 *
 *  Note that we optimize the case which the values are actually
 *  equal by checking first if they're eq.  If not, we go for the
 *  full equal comparison.
 */

DEFUN(doequal, "equal", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	arg2 = EVALARGN(2);

	if (eq(arg1, arg2) || equal(arg1, arg2)) {
		/* they are equal */
		return (v_t);
	} else {
		/* they aren't equal */
		return (v_nil);
	}
}

/*
 *  Do the exhasutive equal comparison.  This could be called other
 *  places to compare two lisp values.  Note that eq() (for C) is
 *  actually a macro, so it's fast as can be.  We check to see if
 *  two values are eq before going the full course.
 *
 *  The individual equality testers for different types are in the
 *  appripriate files for that storage type (for example, sequal()
 *  is in "strings.c").
 */

equal(one, two)
	struct value	one, two;
{
	/* if they are eq, we already know they're equal */
	if (eq(one, two))
		return (TRUE);

	/* if they're not the same type, they can't be equal */
	if (one.vl_type != two.vl_type)
		return (FALSE);
	switch (one.vl_type) {
	case LISP_NIL:
		return (TRUE);
	case LISP_CONS:
		return equal(car(one), car(two)) && equal(cdr(one), cdr(two));
	case LISP_STRING:
		return sequal(gstring(one.vl_data), gstring(two.vl_data));
	case LISP_FUNC:
		return fequal(gfunct(one.vl_data), gfunct(two.vl_data));
	case LISP_ARRAY:
		return aequal(garray(one.vl_data), garray(two.vl_data));
	default:
		return (one.vl_data == two.vl_data);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: eq
 *  Call: (eq 'any 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its two arguments
 *	evaluate to lisp values that are exactly the same.  Two
 *	values are \sym{eq} if they are stored in the same place
 *	internally.
 *
 *	This function is somewhat faster than \sym{equal}, but doesn't
 *	always give the same results.  Numbers and strings are always
 *	\sym{eq} if they are \sym{equal}, but symbols, lists, arrays
 *	and functions are only \sym{eq} if they were created from the
 *	same value (actually, \sym{equal} symbols will always be \sym{eq}
 *	under normal circumstances).
 *  SeeA: equal
 */

DEFUN(doeq, "eq", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	arg2 = EVALARGN(2);

	/*
	 *  Compare the two actual values.  If they are exactly the
	 *  same, then they are equal, otherwise they aren't.  This
	 *  is nice and fast, but depends on the storage method.
	 */
	if (eq(arg1, arg2)) {
		/* types and data match exactly */
		return (v_t);
	} else {
		/* not eq, but maybe equal */
		return (v_nil);
	}
}
