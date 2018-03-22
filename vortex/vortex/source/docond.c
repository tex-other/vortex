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
 *  RCS Info: $Header: docond.c,v 0.1 87/05/01 11:39:18 john Locked $
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
 *  docond.c - various conditional functions
 */
static char _ID[] = "@(#)docond.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: cond
 *  Call: (cond clause-list)
 *  Retu: any
 *  Desc: This function takes a list of clauses as its unevaluated
 *	argument and test each clause until one of them evaluates
 *	non-nil.  The statement associated with this test is then
 *	evaluated and the result returned as the value of the function.
 *	If none of the clauses are ``true'', nil is returned.
 *
 *	Each clause is made up of a list containing two or more elements.
 *	The first element is usually a predicate expression, which is
 *	the test to be performed.  Succeeding elements make up the
 *	statement of the cluase. If the test expression evaluates
 *	non-nil, then each of the expressions in the statement is
 *	evaluated and the result of the last one in the list returned.
 *	When one of the test evaluates non-nil, the remaining clauses
 *	are not even tested.
 *
 *	This function is most often used to select one among a number
 *	of actions, providing the if/else/else construct of other
 *	programming languages in a general way.  This is also like
 *	the Pascal \lit{case} or C \lit{switch} statement, except
 *	that it is much more general.  To emulate the \lit{default}
 *	case in one of these structures, the test may be simply t,
 *	which is always non-nil.
 */

DEFUN(docond, "cond", FLAG_NONE, NULL)
{
	struct value	clause, test, body, result;
	register int	argn, argc, cnt, len;

	CHECKAC(1, -1);
	argc = GETACOUNT();
	for (argn = 1; argn <= argc; argn++) {
		clause = GETARGN(argn);
		if (!dtprp(clause))
			BADARGN(argn, "a list clause");
		if (length(clause) < 2)
			error("Cond clause needs at least two elements.");
		test = evalsexpr(car(clause));
		if (!eq(test, v_nil)) {
			body = cdr(clause);
			if (listp(body)) {
				/* evaluate each element of body list */
				len = length(body);
				for (cnt = 0; cnt < len; cnt++)
					result = evalsexpr(nth(cnt, body));
			} else {
				/* no list to take elements of */
				result = evalsexpr(body);
			}
			return (result);
		}
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: if
 *  Call: (if 'condition 'statement [ 'statement ... ])
 *  Retu: any
 *  Desc: This function immitates the conditional statement of
 *	iterative languages.  If the first argument evaluates
 *	non-nil, the second argument is evaluated and returned,
 *	otherwise if additional arguments exist, they are evaluated
 *	and the result of evaluating the last one is returned.  If
 *	there are no ``else'' statements, nil is returned.
 *  SeeA: cond while
 */

DEFUN(doif, "if", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	register int	n, count;

	CHECKAC(2, -1);
	arg = EVALARGN(1);
	if (truep(arg)) {
		/* evaluate single statement ``then'' clause */
		ret = EVALARGN(2);
	} else if ((count = GETACOUNT()) <= 2) {
		/* no ``else'' clause, return nil */
		ret = v_nil;
	} else {
		/* evaluate multi-statement ``else'' clause */
		for (n = 3; n <= count; n++)
			ret = EVALARGN(n);
	}
	return (ret);
}
