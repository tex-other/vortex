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
 *  RCS Info: $Header: doand.c,v 0.1 87/05/01 11:33:34 john Locked $
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
 *  doand.c - various logical operator functions
 */
static char _ID[] = "@(#)doand.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: and
 *  Call: (and 'any 'any [ 'any ... ])
 *  Retu: t or nil
 *  Desc: This predicate function returns t if all its arguments
 *	evaluate non-nil and nil otherwise.  When an argument
 *	evaluates to nil, other arguments are not evaluated and
 *	the function returns nil immediately, which means that
 *	only arguments up to the first ``false'' one are evaluated.
 *  SeeA: or not
 */

DEFUN(doand, "and", FLAG_NONE, NULL)
{
	struct value	arg;
	register int	count, argc;

	CHECKAC(2, -1);
	argc = GETACOUNT();
	for (count = 1; count <= argc; count++) {
		arg = EVALARGN(count);
		if (nullp(arg))
			return (v_nil);
	}
	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: or
 *  Call: (or 'any 'any [ 'any ... ])
 *  Retu: any
 *  Desc: This predicate function returns nil if all its arguments
 *	evaluate to nil and some other value otherwise.  When an
 *	argument, processed in order, evaluates non-nil, other
 *	arguments are not evaluated and the function returns the
 *	non-nil value immediately, which means that only arguments
 *	up to the first ``true'' one are evaluated.
 *  SeeA: and not
 */

DEFUN(door, "or", FLAG_NONE, NULL)
{
	struct value	arg;
	register int	count, argc;

	CHECKAC(2, -1);
	argc = GETACOUNT();
	for (count = 1; count <= argc; count++) {
		arg = EVALARGN(count);
		if (truep(arg))
			return (v_t);
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: not
 *  Call: (not 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to nil and returns nil otherwise.
 *  SeeA: and or
 */

DEFUN(donot, "not", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	/* t if nil, else nil */
	if (nullp(arg)) {
		/* it's nil, so return t */
		return (v_t);
	} else {
		/* it's non-nil so return nil */
		return (v_nil);
	}
}
