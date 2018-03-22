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
 *  RCS Info: $Header: donumberp.c,v 0.1 87/05/01 11:57:02 john Locked $
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
 *  donumberp.c - various numeric predicates
 */
static char _ID[] = "@(#)donumberp.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: numberp
 *  Call: (numberp 'any)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to a number (currently only fixnums, integers,
 *	are implemented) and nil otherwise.
 *  Xref: numbp
 */

DEFUN(donumberp, "numberp", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (fixnump(arg))
		return (v_t);
	else
		return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: oddp
 *  Call: (oddp 'number)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to a number that is \em{not} evenly divisible
 *	by two (an ``odd number'') and returns nil otherwise.
 *  SeeA: evenp numberp
 */

DEFUN(dooddp, "oddp", FLAG_NONE, NULL)
{
	struct value	arg;
	int		num;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a number");
	num = gfixnum(arg.vl_data);
	if (num % 2 == 0)
		return (v_nil);
	else
		return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: evenp
 *  Call: (evenp 'number)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument
 *	evaluates to a number that is evenly divisible by two
 *	(an ``even number'') and returns nil otherwise.
 *  SeeA: oddp numberp
 */

DEFUN(doevenp, "evenp", FLAG_NONE, NULL)
{
	struct value	arg;
	int		num;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a number");
	num = gfixnum(arg.vl_data);
	if (num % 2 == 0)
		return (v_t);
	else
		return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: lessp
 *  Call: (lessp 'number 'number)
 *  Retu: t or nil
 *  Desc: This predicate function compares its two numeric arguments
 *	and determines whether the first is less than the second or
 *	not.  Both arguments must evaluate to numbers and t is returned
 *	if the first arugment is strictly less than the second, otherwise
 *	nl is returned.  This function is often invoked by its shorter
 *	name, \lit{<}.
 *  Xref: <
 *  SeeA: greaterp equal numberp
 */

DEFUN(dolessp, "lessp", FLAG_NONE, NULL)
{
	struct value	one, two;

	CHECKAC(2, 2);
	one = EVALARGN(1);
	if (!fixnump(one))
		BADARGN(1, "a number");
	two = EVALARGN(2);
	if (!fixnump(two))
		BADARGN(2, "a number");
	if (gfixnum(one.vl_data) < gfixnum(two.vl_data))
		return (v_t);
	else
		return (v_nil);
}
DEFSAME(dolessp, "<")

/*
 *  DOCUMENTATION
 *
 *  Name: lesseqp
 *  Call: (lesseqp 'number 'number)
 *  Retu: t or nil
 *  Desc: This predicate function compares its two numeric arguments
 *	and determines whether the first is less than or equal to the
 *	second or not.  Both arguments must evaluate to numbers and
 *	t is returned if the first arugment is less than or equal to
 *	the second, otherwise nil is returned.  This function is
 *	often invoked by its shorter name, \lit{<}.
 *  Xref: <=
 *  SeeA: less equal numberp
 */

DEFUN(dolesseqp, "lesseqp", FLAG_NONE, NULL)
{
	struct value	one, two;

	CHECKAC(2, 2);
	one = EVALARGN(1);
	if (!fixnump(one))
		BADARGN(1, "a number");
	two = EVALARGN(2);
	if (!fixnump(two))
		BADARGN(2, "a number");
	if (gfixnum(one.vl_data) <= gfixnum(two.vl_data))
		return (v_t);
	else
		return (v_nil);
}
DEFSAME(dolesseqp, "<=")

/*
 *  DOCUMENTATION
 *
 *  Name: greaterp
 *  Call: (greaterp 'number 'number)
 *  Retu: t or nil
 *  Desc: This predicate function compares its two numeric arguments
 *	and determines whether the first is greater than the second or
 *	not.  Both arguments must evaluate to numbers and t is returned
 *	if the first arugment is strictly greater than the second, otherwise
 *	nl is returned.  This function is often invoked by its shorter
 *	name, \lit{>}.
 *  Xref: >
 *  SeeA: greaterp equal numberp
 */

DEFUN(dogreaterp, "greaterp", FLAG_NONE, NULL)
{
	struct value	one, two;

	CHECKAC(2, 2);
	one = EVALARGN(1);
	if (!fixnump(one))
		BADARGN(1, "a number");
	two = EVALARGN(2);
	if (!fixnump(two))
		BADARGN(2, "a number");
	if (gfixnum(one.vl_data) > gfixnum(two.vl_data))
		return (v_t);
	else
		return (v_nil);
}
DEFSAME(dogreaterp, ">")

/*
 *  DOCUMENTATION
 *
 *  Name: greatereqp
 *  Call: (greatereqp 'number 'number)
 *  Retu: t or nil
 *  Desc: This predicate function compares its two numeric arguments
 *	and determines whether the first is at least as large as the
 *	second or not.  Both arguments must evaluate to numbers and t
 *	is returned if the first arugment is greater than or equal to
 *	the second.  This function is commonly called by its shorter
 *	name, \lit{>=}.
 *  Xref: >=
 *  SeeA: greaterp equal numberp
 */

DEFUN(dogreatereqp, "greatereqp", FLAG_NONE, NULL)
{
	struct value	one, two;

	CHECKAC(2, 2);
	one = EVALARGN(1);
	if (!fixnump(one))
		BADARGN(1, "a number");
	two = EVALARGN(2);
	if (!fixnump(two))
		BADARGN(2, "a number");
	if (gfixnum(one.vl_data) >= gfixnum(two.vl_data))
		return (v_t);
	else
		return (v_nil);
}
DEFSAME(dogreatereqp, ">=")

/*
 *  DOCUMENTATION
 *
 *  Name: zerop
 *  Call: (zerop 'number)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if its argument evaluates
 *	to the number zero and nil otherwise.
 *  SeeA: equal numberp
 */

DEFUN(dozerop, "zerop", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a number");
	if (gfixnum(arg.vl_data) == 0)
		return (v_t);
	else
		return (v_nil);
}
