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
 *  RCS Info: $Header: doprog.c,v 0.1 87/05/01 11:58:54 john Locked $
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
 *  doprog.c - various prog functions
 */
static char _ID[] = "@(#)doprog.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "catch.h"

MKSTRING(PROGRETSTR, "*prog-return*");
MKSTRING(PROGGOSTR, "*prog-go*");

static int	prog_level = 0;

/*
 *  DOCUMENTATION
 *
 *  Name: prog
 *  Call: (prog 'list | symbol ...)
 *  Retu: any
 *  Desc: This function allows one to execute expressions in an
 *	iterative manner.  The arguments to \sym{prog} are either
 *	symbols (which are not evaluate and act as labels, see
 *	\sym{go}) or anything else, which is evaluated.  Two
 *	special functions are valid during the execution of a
 *	\sym{prog} body, \sym{go} and \sym{return}.  The \sym{go}
 *	function takes a symbol (which isn't evaluated) and
 *	execution branches to just after that symbol in the
 *	current prog body.
 *
 *	Any time during the exection of this \sym{prog}, if the
 *	function \sym{go} is called, it will cause execution to
 *	branch to the label it specifies.  These labels may occur
 *	before or after the call to 
 *  Side: Because this function uses \sym{catch} internally to
 *	set up the labels, it is unwise to use labels that will
 *	conflict with other catches, unless this is specifically
 *	desired.
 *  SeeA: return go catch
 */

DEFUN(doprog, "prog", FLAG_NONE, NULL)
{
	struct value	arg, sym, rval, alist;
	jmp_buf		retbuf, gobuf;
	int		count, argc;

	CHECKAC(2, -1);
	argc = GETACOUNT();
	alist = GETALIST();
	if (nullp(alist))
		return (v_nil);
	if (!dtprp(alist))
		error("Prog body must form a list!");

	/* bump prog nexsting level, go and return check this */
	prog_level++;

	/* make the return catch */
	switch (setjmp(retbuf)) {
	case -1:	/* setjmp failed */
		panic("Setjmp failed for prog return catch!");
	case 0:		/* setjmp successful */
		int_catch(PROGRETSTR, retbuf, FLAG_NONE);
		rval = v_nil;
		break;
	default:	/* this is a return throw */
		debug(DTHROW, "Returned from prog return throw.");
		rval = throw_value(PROGRETSTR);
		if (eq(rval, NOVALUE))
			rval = v_nil;
		goto done;
	}

	/* make the go catch */
	switch (setjmp(gobuf)) {
	case -1:	/* setjmp failed */
		panic("Setjmp failed for prog go catch!");
	case 0:		/* setjmp successful */
		int_catch(PROGGOSTR, gobuf, FLAG_NONE);
		count = 0;
		break;
	default:	/* this is a go throw */
		debug(DTHROW, "Returned from prog go throw.");
		sym = throw_value(PROGGOSTR);
		ASSERT(symbolp(sym));
		/* try to find this symbol */
		for (count = 0; count < argc; count++) {
			arg = nth(count, alist);
			if (symbolp(arg) && equal(arg, sym))
				break;
		}
		if (count >= argc) {
			error("Go label %Y not found in innermost prog!", sym);
			/* NOTREACHED */
		}
		count++;
		break;
	}

	/* evaluate expressions until end of prog body */
	while (count < argc) {
		arg = nth(count, alist);
		if (dtprp(arg))
			evalsexpr(arg);
		count++;
	}

done:	/* returing from the prog */
	prog_level--;
	int_catch(PROGRETSTR, NULL, CATCH_DELETE);
	int_catch(PROGGOSTR, NULL, CATCH_DELETE);
	return (rval);
}

/*
 *  DOCUMENTATION
 *
 *  Name: go
 *  Call: (go symbol)
 *  Desc: This pseudo-function is only available inside a \sym{prog}
 *	body, and moves the point of execution in that probg body to
 *	just after the symbol its argument matches.
 *  Side: This function is only useful for its side effect, it never
 *	returns, obviously, since it acts like a \sym{throw} to the
 *	label declared in a \sym{prog}.
 *  Xref: goto
 *  SeeA: prog
 */

DEFUN(dogo, "go", FLAG_NONE, NULL)
{
	struct value	arg;

	if (prog_level <= 0)
		error("Can't ``go'' anywhere; not inside a prog body!");

	CHECKAC(1, 1);
	arg = GETARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a prog label symbol");
	int_throw(PROGGOSTR, arg);
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: return
 *  Call: (return [ 'any ])
 *  Desc: This function causes the last call to the \sym{prog} function
 *	to return with value which the argument to \sym{return}
 *	evaluates to.  If no return occurs inside a prog, nil is
 *	the return value.
 *  Side: This function never returns, it causes the most recent
 *	call to \sym{prog} to return instead.
 *
 *	This action is actually implemented as a \sym{throw} whose
 *	corresponding \sym{catch} is inside the original \sym{prog}
 *	function.  This means that the return may occur anywhere
 *	during the execution of the \sym{prog}.
 *  SeeA: prog throw
 */

DEFUN(doreturn, "return", FLAG_NONE, NULL)
{
	struct value	arg;

	if (prog_level <= 0)
		error("Can't ``return''; not inside a prog body!");

	CHECKAC(0, 1);
	if (GETACOUNT() > 0)
		arg = EVALARGN(1);
	else
		arg = v_nil;
	int_throw(PROGRETSTR, arg);
	/* NOTREACHED */
}

clean_progs()
{
	prog_level = 0;
	
	int_catch(PROGRETSTR, NULL, CATCH_DESTROY);
	int_catch(PROGGOSTR, NULL, CATCH_DESTROY);

	return (0);
}
