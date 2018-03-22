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
 *  RCS Info: $Header: doloop.c,v 0.1 87/05/01 11:53:45 john Locked $
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
 *  doloop.c - various vLisp looping functions
 */
static char _ID[] = "@(#)doloop.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: loop
 *  Call: (loop symbol list body ...)
 *  Retu: any
 *  Desc: This function repeatedly executes the code given by the
 *	body expressions setting the named symbol to successive values
 *	in the initialization list each time.  The function returns
 *	the value of the last expression the last time the body is
 *	executed.
 *
 *	This is much like the \em{for} of the Bourne shell \em{sh(1)}
 *	or the \em{foreach} of the C-shell \em{csh(1)}.  The named
 *	symbol is bound to each of the values in the initial list,
 *	which is evaluated at that time.  Then, all the body statements
 *	are evaluated.
 *  Side: The body expressions will be evaluated once for each element
 *	in the value list.
 *  SeeA: while prog
 */

DEFUN(doloop, "loop", FLAG_NONE, NULL)
{
	struct value	sym, vlist, result;
	struct string	*name;
	register int	n, len, arg, argc;

	CHECKAC(3, -1);
	sym = GETARGN(1);
	if (!symbolp(sym))
		BADARGN(1, "a symbol name");
	name = gsymbol(sym.vl_data)->sy_pname;
	vlist = GETARGN(2);
	if (!dtprp(vlist))
		BADARGN(2, "a non-nil list");
	len = length(vlist);
	argc = GETACOUNT();

	result = v_nil;
	for (n = 0; n < len; n++) {
		pushglobal(name, evalsexpr(nth(n, vlist)), FLAG_NONE);
		for (arg = 3; arg <= argc; arg++)
			result = EVALARGN(arg);
		popglobal(name);
	}
	return (result);
}

/*
 *  DOCUMENTATION
 *
 *  Name: while
 *  Call: (while test body ...)
 *  Retu: any
 *  Desc: This function executes the body expressions repeatedly
 *	until the test expression evaluates to nil.  The result
 *	of evaluating the last expression in the body on the last
 *	loop through the body expressions is returned, or nil if
 *	no expressions were evaluated.
 *
 *	If the test expression (the first argument) evaluates
 *	initially to nil, the body (all other arguments) is never
 *	executed at all.  Otherwise, the test is evaluated, and
 *	\sym{while} evaluates non-nil, the body is evaluated.
 *	When the test expression evaluates to nil, the function
 *	returns the result of the last body evaluation.
 *  SeeA: loop prog cond
 */

DEFUN(dowhile, "while", FLAG_NONE, NULL)
{
	struct value	test, arg1, result;
	register int	count, argc;

	CHECKAC(2, -1);
	arg1 = GETARGN(1);
	argc = GETACOUNT();

	result = v_nil;
	test = evalsexpr(arg1);
	while (!eq(test, v_nil)) {
		for (count = 1; count <= argc; count++)
			result = EVALARGN(count);
		test = evalsexpr(arg1);
	}
	return (result);
}
