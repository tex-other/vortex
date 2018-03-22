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
 *  RCS Info: $Header: dolambda.c,v 0.1 87/05/01 11:51:41 john Locked $
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
 *  dolambda.c - function and routines to handle lambda functions
 */
static char _ID[] = "@(#)dolambda.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"

/*
 *  DOCUMENTATION
 *
 *  Name: lambda
 *  Call: (lambda args body ...)
 *  Retu: function
 *  Desc: This function builds a function value out of a description
 *	of a ``lambda form'' function.  The first argument to \sym{lambda}
 *	must be a (possibly nil) list, containing symbol names which
 *	will correspond the the names of the arguments when this
 *	function is called.  The remaining arguments form the body
 *	of the function, they will be evaluated when this newly
 *	created function is called.
 *
 *	The more usual way to create functions is with \sym{defun},
 *	which hides some of the details of function implementation.
 *	However, \sym{defun} will just call \sym{lambda} if the
 *	function being defined would be a lambda form.
 *
 *	The list of argument reflects the special nature of the
 *	lambda form.  Lambda form functions take a fixed number of
 *	arguments, all of which are evaluated.  When in the body
 *	of the lambda form function, the names given in the first
 *	argument to \sym{lambda} wil be bound to the values passed
 *	in as arguments when the function is called.
 *
 *	This newly created function is not bound to any symbol, so
 *	to call it the lambda expression must be the first element
 *	in an unquoted list which is gbeing evaluated.  The more
 *	usualy proceedure is to bind this function to a symbol so
 *	that the function can be called by some appropriate name
 *	rather than the full lambda form.  An example, which makes
 *	a new function called \sym{plus4} is given below:
 *
 *	\lit{(setq plus4 (lambda (x) (add 4 x)))}
 *
 *	After this, one may call the function \sym{plus4} just as
 *	if it was a standard vlisp function.  However, one must
 *	always give exactly one argument, which will be evaluated.
 *
 *	Note in the above example that we didn't quote the
 *	lambda form.  It is important that it is evaluated as a
 *	call to the function \sym{lambda} which will evaluate
 *	to a function value.  This function value can then be
 *	interpreted, when it appears as the \sym{car} of an
 *	evaluated list.
 *  SeeA: defun nlambda lexpr macro
 */

DEFUN(dolambda, "lambda", FLAG_NONE, NULL)
{
	struct value	body, args;
	struct function	*new;
	struct value	result;

	CHECKAC(2, -1);

	/* extract argument list and function body */
	body = GETALIST();
	args = car(body);
	body = cdr(body);

	new = make_lambda(args, body);
	ASSERT(new != NULL);

	result.vl_type = LISP_FUNC;
	sfunct(result.vl_data, new);
	return (result);
}

struct function *
make_lambda(args, body)
	struct value	args, body;
{
	struct function	fbuf, *new = &fbuf;
	char		buf[SMALLBUF];
	register int	argc;
	struct value	arg;

	/* allocate and set up the new function struct */
	new->fn_flags = FLAG_NONE;
	new->fn_disc = DISC_LAMBDA;
	new->fn_funct = NULL;
	new->fn_body = body;
	sprintf(buf, "#%x-lambda", (unsigned long)new);
	new->fn_pname = save_string(buf, strlen(buf));

	/* make up lambda form from argument list */
	if (nullp(args)) {
		/* no arguments at all */
		new->fn_argc = 0;
		new->fn_alist[0] = NULL;
	} else if (dtprp(args)) {
		new->fn_argc = length(args);
		if (new->fn_argc > MAXARGC)
			error("Too many lambda arguments, maximum %d!",
			    MAXARGC);
		for (argc = 0; argc < new->fn_argc; argc++) {
			arg = nth(argc, args);
			if (!symbolp(arg))
				error("All argument names must be symbols.");
			new->fn_alist[argc] = gsymbol(arg.vl_data)->sy_pname;
		}
	} else {
		/* not a list, don't know what to do with it */
		error("Lambda argument specification must be a list");
	}
	return save_funct(new);
}

struct value
call_lambda()
{
	struct value	arg, result;
	struct function	*func;
	register int	i, len;

	/* make sure we got the right number of arguments */
	arg = GETFUNCT();
	func = gfunct(arg.vl_data);
	CHECKAC(func->fn_argc, func->fn_argc);

	/* push local arguments onto variable stack */
	for (i = 0; i < func->fn_argc; i++) {
		arg = EVALARGN(i + 1);
		pushglobal(func->fn_alist[i], arg, FLAG_NONE);
	}

	/* evaluate function body; throw away all but last result */
	result = v_nil;
	ASSERT(dtprp(func->fn_body));
	len = length(func->fn_body);
	for (i = 0; i < len; i++)
		result = evalsexpr(nth(i, func->fn_body));

	/* return value of statements */
	return (result);
}
