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
 *  RCS Info: $Header: dodefun.c,v 0.1 87/05/01 11:42:19 john Locked $
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
 *  dodefun.c - vLisp defun function
 */
static char _ID[] = "@(#)dodefun.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"

/*
 *  DOCUMENTATION
 *
 *  Name: defun
 *  Call: (defun name [ discipline ] argspec body ...)
 *  Retu: function
 *  Desc: This function is the easiest way to create new functions
 *	(\sym{lambda}, \sym{nlambda}, \sym{lexpr} or \sym{macro} forms).
 *	The specified symbol is bound (as with \sym{set}) to a
 *	function value of the displine determined from the optional
 *	disipline specification.  If discipline is not given, \sym{lambda}
 *	is assumed.  The first argument must be a symbol name, the
 *	discipline argument, if present, must be a symbol also, one of
 *	the four discipline types, the function argument specification
 *	must be a list, unless the discipline is \sym{lexpr}, in which
 *	case it must be a symbol.  All remaining arguments form the body
 *	of the function, these are expressions that will be evaluated
 *	when the newly created function is called.
 *
 *	The argument specification for \sym{nlambda} and \sym{macro}
 *	must be a list contining a single symbol.  For \sym{lambda} forms
 *	the list may be any length, with the length corresponding to the
 *	number of arguments expected.  The special arguments \lit{&optional}
 *	and \lit{&rest} may also appear in this list (they are described
 *	under \sym{nlambda}).  The argument processing done for all these
 *	function disciplines is described under the discpline name.
 *
 *	If the first expression in the body of any type of function
 *	(or macro) is a call to the function \sym{interactive}, that
 *	call is evaluated at the time the function is created which
 *	specifies the interactive calling sequence (and the fact that
 *	the function can be called interactively at all).  See the
 *	documentation on \sym{interactive} for a description of the
 *	usage of that function.
 *  Side: \sym{Defun} binds the symbol which is the function name to
 *	the function value appropriate.  This binding is subject to the
 *	scoping rules of vlisp.  For example, if there is a local binding
 *	of the function name symbol, that local binding will be changed,
 *	and will go away, when the scope of that local binding is exited.
 *  SeeA: lambda nlmabda lexpr macro interactive defmacro
 */

DEFUN(dodefun, "defun", FLAG_NONE, NULL)
{
	struct value	name, disc, args, body;
	struct value	iexp, val;
	struct function	*func;
	int		argc, bstart, dtype;

	name = GETARGN(1);
	if (!symbolp(name))
		BADARGN(1, "a function name symbol");
	disc = GETARGN(2);
	if ((dtype = discname(disc)) > 0) {
		args = GETARGN(3);
		bstart = 4;
	} else {
		args = GETARGN(2);
		bstart = 3;
		if (symbolp(args)) {
			/* this discipline is lexpr */
			dtype = DISC_LEXPR;
		} else if (listp(args)) {
			/* assume discipline is lambda */
			dtype = DISC_LAMBDA;
		} else {
			/* user is confused */
			BADARGN(2, "a form specifier");
		}
	}

	/* check for an interactive statement */
	argc = GETACOUNT();
	if (argc < bstart)
		error("Missing body expressions for a new function!");
	iexp = GETARGN(bstart);
	if (!dtprp(iexp) || !isinteract(iexp))
		iexp = NOVALUE;

	/* rest of arguments for the body */
	body = nthcdr(bstart - 1, GETALIST());

	/* make a function of the appropriate type */
	switch (dtype) {
	case DISC_LAMBDA:
		func = make_lambda(args, body);
		break;
	case DISC_NLAMBDA:
		func = make_nlambda(args, body);
		break;
	case DISC_LEXPR:
		func = make_lexpr(args, body);
		break;
	case DISC_MACRO:
		func = make_macro(args, body);
		break;
	default:
		func = NULL;
		break;
	}
	ASSERT(func != NULL);
	if (dtprp(iexp))
		parse_istmt(iexp, func);
	
	/* set up the function in symbol table */
	val.vl_type = LISP_FUNC;
	sfunct(val.vl_data, func);
	setglobal(gsymbol(name.vl_data)->sy_pname, val, FLAG_NONE);
	return (name);
}

MKSTRING(LAMBDASTR, "lambda");
MKSTRING(NLAMBDASTR, "nlambda");
MKSTRING(LEXPRSTR, "lexpr");
MKSTRING(MACROSTR, "macro");

static int
discname(sym)
	struct value	sym;
{
	struct string	*disc;

	if (!symbolp(sym))
		return (-1);

	disc = gsymbol(sym.vl_data)->sy_pname;

	if (sequal(disc, LAMBDASTR))
		return (DISC_LAMBDA);
	if (sequal(disc, NLAMBDASTR))
		return (DISC_NLAMBDA);
	if (sequal(disc, LEXPRSTR))
		return (DISC_LEXPR);
	if (sequal(disc, MACROSTR))
		return (DISC_MACRO);

	return (0);
}

MKSTRING(INTERACTSTR, "interactive");

static int
isinteract(exp)
	struct value	exp;
{
	struct value	cmd;
	struct string	*str;

	if (!dtprp(exp))
		return (FALSE);

	cmd = car(exp);
	if (!symbolp(cmd))
		return (FALSE);
	str = gsymbol(cmd.vl_data)->sy_pname;
	if (sequal(str, INTERACTSTR))
		return (TRUE);
	else
		return (FALSE);
}
