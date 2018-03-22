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
 *  RCS Info: $Header: doeval.c,v 0.1 87/05/01 11:44:09 john Locked $
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
 *  doeval.c - vLisp eval function
 */
static char _ID[] = "@(#)doeval.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: eval
 *  Call: (eval 'any)
 *  Retu: any
 *  Desc: This function is the basic lisp evaluator.  It takes a lisp
 *	value of any type and evaluates it, returning the result of
 *	that evaluation.  Numbers and strings \sym{eval} to themselves,
 *	symbols evaluate as variables (names) and lists evaluate as
 *	function calls.  Nil, the empty list, always evaluates to itself.
 *
 *	Every ``execution'' is really just an evaluation.  Usually, one
 *	evaluates symbols and lists.  Other objects just evaluate to
 *	themselves.  Symbols are evaluated by looking up their bindings
 *	as created and modified by \sym{set}.  The result of evaluating
 *	a symbol is the value of that symbol; if the symbol is unbound
 *	(has not been \sym{set} to another value), an error occurs.
 *	Note that the function \sym{symeval} is defined, but identical
 *	to \sym{eval}.
 *
 *	Lists are evaluated as calls to a function.  The function is
 *	specified by the first element of the list, and any further
 *	elements in the list are arguments to that function.  The first
 *	element of the list is (recursively) evaluated.  It must, at this
 *	point, evaluate to a function value.  The rest of the arguments
 *	are then processed in a manner dependent on the pertinent function
 *	discipline.  Functions are created and bound to names by the
 *	function \sym{defun}, or one may \sym{set} a symbol to a function
 *	value created by one of the discipline forms or to an already
 *	existing function.
 *  Side: The expression you pass to \sym{eval} is actually evaluated
 *	twice.  The argument is evaluated to an expression which is
 *	again evaluated and the result returned.
 *  Xref: symeval
 *  SeeA: set defun defmacro
 */

DEFUN(doeval, "eval", FLAG_NONE, NULL)
{
	struct value	arg, result;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	result = evalsexpr(arg);
	return (result);
}
DEFSAME(doeval, "symeval")
