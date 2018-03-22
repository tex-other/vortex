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
 *  RCS Info: $Header: doadd.c,v 0.1 87/05/01 11:33:10 john Locked $
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
 *  doadd.c - various arithmetic functions
 */
static char _ID[] = "@(#)doadd.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

#define OP_ADD	1	/* addition operation */
#define OP_SUB	2	/* subtraction operation */
#define OP_MUL	3	/* multiplication operation */
#define OP_DIV	4	/* division operation */

/*
 *  DOCUMENTATION
 *
 *  Name: add
 *  Call: (add 'number 'number [ 'number ... ])
 *  Retu: number
 *  Desc: The function \sym{add} takes a list of numbers and returns their
 *	arithmetic sum.  All arguments are evaluated, and must be
 *	numbers.
 *  Xref: +
 *  SeeA: minus times quotient
 */

DEFUN(doadd, "add", FLAG_NONE, NULL)
{
	struct value	arithfun();
	register char	*fname;
	int		what;

	fname = GETFNAME();
	if (!strcmp(fname, "add") || !strcmp(fname, "+"))
		what = OP_ADD;
	else if (!strcmp(fname, "minus") || !strcmp(fname, "-"))
		what = OP_SUB;
	else if (!strcmp(fname, "times") || !strcmp(fname, "*"))
		what = OP_MUL;
	else if (!strcmp(fname, "quotient") || !strcmp(fname, "/"))
		what = OP_DIV;
	else {
		error("Don't know what arithmetic function %Y is!", fname);
	}

	return arithfun(what);
}
DEFSAME(doadd, "+")

/*
 *  DOCUMENTATION
 *
 *  Name: minus
 *  Call: (minus 'number 'number [ 'number ... ])
 *  Retu: number
 *  Desc: The function \sym{minus} takes a list of numbers and returns
 *	the sucessive arithmetic difference.  All arguments are
 *	evaluated, and must be numbers.
 *  Xref: -
 *  SeeA: add times quotient
 */

DEFSAME(doadd, "minus")
DEFSAME(doadd, "-")

/*
 *  DOCUMENTATION
 *
 *  Name: times
 *  Call: (times 'number 'number [ 'number ... ])
 *  Retu: number
 *  Desc: The function \sym{times} takes a list of numbers and returns
 *	their arithmetic product.  All arguments are evaluated, and must
 *	be numbers.
 *  Xref: *
 *  SeeA: add minus quotient
 */

DEFSAME(doadd, "times")
DEFSAME(doadd, "*")

/*
 *  DOCUMENTATION
 *
 *  Name: quotient
 *  Call: (quotient 'number 'number [ 'number ... ])
 *  Retu: number
 *  Desc: The function \sym{quotient} takes a list of numbers and
 *	returns their arithmetic quotient.  All arguments are evaluated,
 *	and must be numbers.
 *  Xref: /
 *  SeeA: add minus times
 */

DEFSAME(doadd, "quotent")
DEFSAME(doadd, "/")

static struct value
arithfun(op)
{
	struct value	arg, ret;
	register int	count, argc, number, result;

	CHECKAC(2, -1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum");
	result = gfixnum(arg.vl_data);
	argc = GETACOUNT();
	for (count = 2; count <= argc; count++) {
		arg = EVALARGN(count);
		if (!fixnump(arg))
			BADARGN(count, "a fixnum");
		number = gfixnum(arg.vl_data);
		switch (op) {
		case OP_ADD:
			result += number;
			break;
		case OP_SUB:
			result -= number;
			break;
		case OP_MUL:
			result *= number;
			break;
		case OP_DIV:
			if (number == 0) {
				/* avoid n / 0 at all costs */
				error(
			    "Trust me; we don't want to divide by zero.");
			}
			result /= number;
			break;
		}
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, result);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: mod
 *  Call: (mod 'fixnum 'fixnum)
 *  Retu: integer
 *  Desc: This function returns the integer remainder when the first
 *	argument is divided by the second.  This remainder is always
 *	positive, despite the signs of the arguments.
 *  Xref: %
 *  SeeA: quotient
 */

DEFUN(domod, "mod", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, ret;
	int		result, number, modulus;

	arg1 = EVALARGN(1);
	if (!fixnump(arg1))
		BADARGN(1, "a fixnum");
	arg2 = EVALARGN(2);
	if (!fixnump(arg2))
		BADARGN(2, "a fixnum");

	/* return the positive modulus */
	number = gfixnum(arg1.vl_data);
	modulus = gfixnum(arg2.vl_data);
	if (modulus == 0)
		error("Mod'ing by zero would involve division by zero.");
	result = number % modulus;
	if (result < 0)
		result *= -1;
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, result);
	return (ret);
}
DEFSAME(domod, "%")
