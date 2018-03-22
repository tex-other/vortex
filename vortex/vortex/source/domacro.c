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
 *  RCS Info: $Header: domacro.c,v 0.1 87/05/01 11:54:13 john Locked $
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
 *  domacro.c - function and routines to handle macro form
 */
static char _ID[] = "@(#)domacro.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"

/*
 *  DOCUMENTATION
 *
 *  Name: macro
 *  Call: (macro arg body ...)
 *  Retu: function
 *  Desc: This function builds a function value out of a description
 *	of a ``macro form'' function.  The first argument to \sym{macro}
 *	must be a list containing exactly one symbol name which
 *	will correspond the the name of the single argument when this
 *	macro is called.  The remaining arguments form the body
 *	of the macro, they will be evaluated when this newly
 *	created macro is invoked.
 *
 *	The more usual way to create macros is to use \sym{defmacro}
 *	rather than the lower-level \sym{macro} directly because
 *	\sym{defmacro} uses syntax more like \sym{defun}, for
 *	functions.
 *
 *	This single argument reflects the special nature of the
 *	macro form.  The user may give any number of arguments to
 *	a macro, none of which are evaluated.  The macro name and
 *	arguments thus given are placed into a list, which is bound
 *	to the given argument name when the macro is executing
 *	A macro is expected to return (evaluate to) an s-expression
 *	which is evaluated as soon as the macro returns.  Thus, the
 *	macro produces more code which is then evaluated for the
 *	final result of evaluating the macro.
 *
 *	This newly created macro is not bound to any symbol, so
 *	to call it the function value must be the first element
 *	in an unquoted list which is being evaluated.  The more
 *	usual proceedure is to bind this macro to a symbol so
 *	that it can be called by some more mnemonic name
 *	rather than the full macro form.  An example, which makes
 *	a macro called \sym{ncons} is given below:
 *
 *	\lit{(setq ncons (macro (x) (list 'cons (cadr x) nil)))}
 *
 *	After this, one may call the macro \sym{ncons} just as
 *	if it was a standard vlisp function.  When \sym{ncons}
 *	is invoked, the macro body itself will evaluate to a
 *	list containg \lit{cons}, the macro argument, and \lit{nil} 
 *	Then the list is evaluated, note that it has conveniently
 *	become a call to \sym{cons} with nil as the second argument.
 *	Thus, in effect, calling
 *
 *	\lit{(ncons 'x)}
 *
 *	is equivalent to calling
 *
 *	\lit{(cons 'x nil)}.
 *
 *	Note in the above macro definition that we didn't quote the
 *	macro form.  It is important that it is evaluated as a
 *	call to the function \sym{macro} which will evaluate
 *	to a function value.  This function value can then be
 *	interpreted, when it appears as the \sym{car} of an
 *	evaluated list, or in this case, the symbol \sym{ncons}
 *	will evaluate to a function form which can be executed.
 *  SeeA: defmacro lambda nlambda lexpr
 */

DEFUN(domacro, "macro", FLAG_NONE, NULL)
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

	new = make_macro(arg, body);
	ASSERT(new != NULL);

	result.vl_type = LISP_FUNC;
	sfunct(result.vl_data, save_funct(new));
	return (result);
}

struct function *
make_macro(aname, body)
	struct value	aname, body;
{
	error("Function discipline macro not yet implemented, sorry.");
}

struct value
call_macro()
{
}
