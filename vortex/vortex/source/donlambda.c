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
 *  RCS Info: $Header: donlambda.c,v 0.1 87/05/01 11:55:53 john Locked $
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
 *  donlambda.c - function and routines to handle nlambda functions
 */
static char _ID[] = "@(#)donlambda.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"

/*
 *  DOCUMENTATION
 *
 *  Name: nlambda
 *  Call: (nlambda aname body ...)
 *  Retu: function
 *  Desc: This function builds a function value out of a description
 *	of a ``nlambda form'' function.  The first argument to
 *	\sym{nlambda} must be a symbol name which will correspond
 *	the the name of the single argument when this function is
 *	called.  The remaining arguments form the body of the
 *	function, they will be evaluated when this newly created
 *	function is invoked.
 *
 *	The more usual way to create functions is with \sym{defun},
 *	which hides some of the details of function implementation.
 *	However, \sym{defun} will just call \sym{nlambda} if the
 *	function being defined would be a nlambda form.
 *
 *	This single argument reflects the special nature of the
 *	nlambda form.  The user may give any number of arguments to
 *	a nlambda function, none of which are evaluated.  The
 *	arguments thus given are placed into a list, which is bound
 *	to the given argument name when the nlambda function is
 *	executing.
 *
 *	This newly created function is not bound to any symbol, so
 *	to call it the function value must be the first element
 *	in an unquoted list which is being evaluated.  The more
 *	usual proceedure is to bind this function to a symbol so
 *	that the function can be called by some appropriate name
 *	rather than the full nlambda form.  An example, which makes
 *	a new function called \sym{count} is given below:
 *
 *	\lit{(setq count (nlambda x (length x)))}
 *
 *	After this, one may call the function \sym{count} just as
 *	if it was a standard vlisp function.  One may give \sym{count}
 *	any number of arguments, none of which are evaluated, and it
 *	will return a count of the number of arguments it was given.
 *
 *	Note in the above example that we didn't quote the
 *	function form.  It is important that it is evaluated as a
 *	call to the function \sym{nlambda} which will evaluate
 *	to a function value.  This function value can then be
 *	interpreted, when it appears as the \sym{car} of an
 *	evaluated list.
 *  SeeA: defun lambda lexpr macro
 */

DEFUN(donlambda, "nlambda", FLAG_NONE, NULL)
{
	struct value	arg, body;
	struct function	*new;
	struct value	result;

	CHECKAC(2, -1);
	/* extract argument name; 1st argument */
	arg = GETARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "an argument name symbol");

	/* extract function body; 2nd - nth arguments */
	body = cdr(GETALIST());

	new = make_nlambda(arg, body);
	ASSERT(new != NULL);

	result.vl_type = LISP_FUNC;
	sfunct(result.vl_data, save_funct(new));
	return (result);
}

struct function *
make_nlambda(aname, body)
	struct value	aname, body;
{
	struct function	fbuf, *new = &fbuf;
	char		buf[SMALLBUF];

	/* check validity of argument name */
	if (dtprp(aname) && length(aname) == 1)
		aname = car(aname);

	/* allocate and set up the new function struct */
	new->fn_pname = NULL;
	new->fn_disc = DISC_NLAMBDA;
	new->fn_flags = FLAG_NONE;
	new->fn_funct = NULL;
	new->fn_body = body;
	sprintf(buf, "#%x-nlambda", (unsigned long)new);
	new->fn_pname = save_string(buf, strlen(buf));

	/* make up nlambda form argument list */
	if (!symbolp(aname))
		error("Argument name in an nlambda form must be an symbol.");
	new->fn_argc = 1;
	new->fn_alist[0] = gsymbol(aname.vl_data)->sy_pname;
	new->fn_alist[1] = NULL;

	return save_funct(new);
}

struct value
call_nlambda()
{
	struct value	result, arg, stmt;
	int		len, count, argc;
	struct function	*func;

	/* get function struct from value */
	arg = GETFUNCT();
	func = gfunct(arg.vl_data);

	/* get arguments, push argument name with list value */
	argc = GETACOUNT();
	if (argc == 0)
		arg = v_nil;
	else
		arg = GETALIST();
	pushglobal(func->fn_alist[0], arg, FLAG_NONE);

	/* evaluate each statement in the function body */
	result = v_nil;
	ASSERT(dtprp(func->fn_body));
	len = length(func->fn_body);
	for (count = 0; count < len; count++) {
		stmt = nth(count, func->fn_body);
		result = evalsexpr(stmt);
	}

	/* pop stack variable after use */
	popglobal(func->fn_alist[0]);

	return (result);
}
