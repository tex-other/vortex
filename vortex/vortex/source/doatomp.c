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
 *  RCS Info: $Header: doatomp.c,v 0.1 87/05/01 11:34:10 john Locked $
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
 *  doatomp.c - various type testing predicates
 */
static char _ID[] = "@(#)doatomp.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: atomp
 *  Call: (atomp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument evaluates
 *	to an atom, which is defined as anything but a list (cons cell)
 *	or an array, and nil otherwise.  Note that a nil list is an atom
 *	as far as \sym{atomp} is concerned.
 *  Xref: atom
 *  SeeA: listp
 */

DEFUN(doatomp, "atomp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (atomp(arg)) {
		/* anything except a non-nil list */
		return (v_t);
	} else {
		/* it is a list */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: dtprp
 *  Call: (dtprp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if it's argument
 *	is a non-nil list (in actuality the value is stoed with
 *	a cons cell) and nil otherwise.  This is equivalent to
 *	\sym{listp} except that nil is not considered a dotted-pair
 *	by \sym{dtprp}.
 *  Xref: dtpr
 *  SeeA: atomp listp
 */

DEFUN(dodtprp, "dtprp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (dtprp(arg)) {
		/* only a non-nil list */
		return (v_t);
	} else {
		/* anything else */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: listp
 *  Call: (listp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument evaluates
 *	to a list (or to nil) and nil otherwise.  Note that nil is
 *	both an atom and a list according to \sym{atomp} and \sym{listp}.
 *  SeeA: atomp
 */

DEFUN(dolistp, "listp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (listp(arg)) {
		/* a proper list or nil */
		return (v_t);
	} else {
		/* anything else */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: symbolp
 *  Call: (symbolp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument evaluates
 *	to a symbol, which is a name that can be bound to a vlisp value
 *	(such as with \sym{setq}).
 *  SeeA: atomp
 */

DEFUN(dosymbolp, "symbolp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (symbolp(arg)) {
		/* it is a atom */
		return (v_t);
	} else {
		/* something else */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: stringp
 *  Call: (stringp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to a string and nil otherwise.  A string is
 *	always specified initially as text between double quotes
 *	(``symbols'' are not strings).
 *  SeeA: symbolp atomp
 */

DEFUN(dostringp, "stringp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (stringp(arg)) {
		/* a string value */
		return (v_t);
	} else {
		/* anything else */
		return (v_nil);
	}
}


/*
 *  DOCUMENTATION
 *
 *  Name: arrayp
 *  Call: (arrayp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to an array and nil otherwise.  An array is
 *	usually created by \sym{array}.
 *  SeeA: array
 */

DEFUN(doarrayp, "arrayp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (arrayp(arg)) {
		/* a string value */
		return (v_t);
	} else {
		/* anything else */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: functionp
 *  Call: (functionp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to a function value and nil otherwise.
 *
 *	In vlisp, unlike some other lisps, functions are a value
 *	type just like strings and lists, not an alternate form
 *	of a symbol (I.e., \em{putd}, etc.).  This means that a
 *	variable (symbol) bound to a function will evaluate to
 *	that function.
 *  SeeA: defun defmacro
 */

DEFUN(dofunctionp, "functionp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (arrayp(arg)) {
		/* a string value */
		return (v_t);
	} else {
		/* anything else */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: nullp
 *  Call: (nullp any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to a nil (an ``empty list'') and returns
 *	nil otherwise.
 *  SeeA: not
 */

DEFUN(donullp, "nullp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (nullp(arg)) {
		/* nil itself */
		return (v_t);
	} else {
		/* anything else */
		return (v_nil);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: boundp
 *  Call: (boundp 'symbol)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to the name of a currently bound symbol.
 *	and nil otherwise.  This is the safe way to test if
 *	a symbol has been \sym{set}, because simply evaluating
 *	an atom that is not bound causes an error.
 *
 *	The symbol is checked both locally to the current
 *	buffer and globally in the interpreter.
 *  SeeA: set
 */

DEFUN(doboundp, "boundp", FLAG_NONE, NULL)
{
	struct value	arg, val;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	val = get_variable(gsymbol(arg.vl_data)->sy_pname, current_buffer);
	if (eq(val, NOVALUE)) {
		/* it isn't bound */
		return (v_nil);
	} else {
		/* it must be */
		return (v_t);
	}
}
