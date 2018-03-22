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
 *  RCS Info: $Header: dolet.c,v 0.1 87/05/01 11:52:13 john Locked $
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
 *  dolet.c - vLisp let function
 */
static char _ID[] = "@(#)dolet.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: let
 *  Call: (let list 'body ...)
 *  Retu: any
 *  Desc: This function makes local instances of a list symbol names
 *	and then evaluates the expressions in the body list, returning
 *	the result of the last one.  There must be at least two arguments
 *	given, the list of local symbol names and initial values and
 *	one or more expressions which form the body.
 *
 *	The first argument, which is not evaluated, should be a list
 *	of local symbol declarations.  Each declaration is a list of
 *	two elements; the first must be an symbol, and is the name of
 *	the local variable, the second is evaluated, and the result bound
 *	to the symbol name locally.  When this instance of \sym{let}
 *	returns, these local symbols lose this binding.
 *
 *	All arguments after the first are evaluated as through they
 *	were statements in a function.  Of course, the local symbols
 *	declared in the first argument are bound as described when
 *	the body expressions are being evaluated.  This function
 *	returns the result of evaluating the last expression in the
 *	list.
 *  Side: Outer level variables with the same names as any of these
 *	defined local symbols are not lost, but they are not available
 *	until this invocation of \sym{let} returns.
 *
 *	Each pair in the declaration argument is bound as soon as seen,
 *	so these symbols have their values available for the later
 *	declarations in the same \sym{let}.  In some lisps this is not
 *	the case, and a function \lit{let*} has this action.  However,
 *	the vlisp \sym{let} is equvalent to the \lit{let*} of these
 *	other lisps.
 *
 *	There is an upper limit on the number of local bindings one
 *	can create with each \sym{let}, but this should not be a
 *	problem (by default the limit is 100).
 *  SeeA: set
 */
#define MAXLOCALS	100

DEFUN(dolet, "let", FLAG_NONE, NULL)
{
	extern char	*nthname();
	struct value	decl, exp, sym, ret;
	register int	count, argc, ndecl;
	struct string	*lnames[MAXLOCALS];

	/* check that we have two or more arguments */
	CHECKAC(2, -1);

	/* set local symbols from list, first argument */
	decl = GETARGN(1);
	if (!dtprp(decl))
		BADARGN(1, "a non-nil list");
	ndecl = length(decl);
	if (ndecl > MAXLOCALS)
		error("Too many let declarations, maximum is %d!", MAXLOCALS);
	for (count = 0; count < ndecl; count++) {
		exp = nth(count, decl);
		/* extract the two parts of this declaration */
		if (!dtprp(exp) || length(exp) != 2)
			error("The %s symbol declaration is malformed.",
			    nthname(count + 1));
		sym = car(exp);
		if (!symbolp(sym))
			error("The %s variable name isn't an symbol!",
			    nthname(count + 1));
		lnames[count] = gsymbol(sym.vl_data)->sy_pname;
		ret = evalsexpr(car(cdr(exp)));

		/* set the local binding */
		pushglobal(lnames[count], ret, FLAG_NONE);
	}

	/* now execute the body statements one by one */
	argc = GETACOUNT();
	for (count = 2; count <= argc; count++)
		ret = evalsexpr(GETARGN(count));

	/* pop the local bindings we made above */
	for (count = 0; count < ndecl; count++)
		popglobal(lnames[count]);

	return (ret);
}
