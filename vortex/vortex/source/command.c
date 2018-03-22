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
 *  RCS Info: $Header: command.c,v 0.1 87/05/01 11:27:11 john Locked $
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
 *  command.c - editor functions extended-command and eval-expression
 */
static char _ID[] = "@(#)command.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: extended-command
 *  Call: (extended-command 'symbol)
 *  Retu: any
 *  Desc: This function calls any interactive editor function explicitly.
 *	It is generally used to invoke functions which are not bound to
 *	keys.  The argument must be a symbol name and must evaluate to a
 *	function.  This function must be interactive.
 *
 *	The function is invoked.  If it requires any arguments, they are
 *	prompted for using the interactive input specification for that
 *	command, as though it has been called by typing a key to which
 *	it was bound.
 * Side: This can also be used to call an interactive function interactively
 *	from inside vlisp code, in case this should be desired.
 *  SeeA: eval-expression
 */
extern struct string	*typed_keys;

DEFUN(doextcommand, "extended-command", FLAG_NONE, "I: ")
{
	extern struct value	call_interactive();
	struct value		arg, ret;

	CHECKAC(1, 1);
	arg = GETARGN(1);
	if (nullp(arg))
		return (v_nil);

	/* call the entered value as a function */
	ret = call_interactive(arg, typed_keys);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: eval-expression
 *  Call: (eval-expression string)
 *  Retu: any
 *  Desc: This function reads a single lisp s-expression from its string
 *	argument and evaluates it.  This is the usual way to type lisp
 *	code to the editor while editing interactively.
 *  Side: When called from lisp, this is not identical to \sym{eval}
 *	since \sym{eval-expression} does not evaluate its argument.
 *	Thus, the argument is only evaluated once, as the action of
 *	the function.
 *  SeeA: extended-command
 */

DEFUN(doevalexpr, "eval-expression", FLAG_NONE, "lEval: ")
{
	struct value	arg, ret;

	CHECKAC(1, 1);
	arg = GETARGN(1);
	ret = evalsexpr(arg);

	message("%v", ret);
	return (ret);
}
