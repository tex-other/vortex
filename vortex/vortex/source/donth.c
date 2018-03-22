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
 *  RCS Info: $Header: donth.c,v 0.1 87/05/01 11:56:36 john Locked $
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
 *  donth.c - various list and array extraction functions
 */
static char _ID[] = "@(#)donth.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: nth
 *  Call: (nth 'number 'list)
 *  Retu: any
 *  Desc: This function returns the number'th element in the given
 *	list.  The first argument must evaluate to a positive fixnum
 *	and the second to a list.  If the list has fewer elements than
 *	the given index, the function returns nil, otherwise it returns
 *	the \sym{car} after number \sym{cdr}s (equivalent to using number
 *	as a zero based index into the list).  Thus, \lit{(nth 0 '(a b))}
 *	is equivalent to \lit{(car '(a b))} and evaluates to \lit{a}.
 *  SeeA: car cdr nthcdr
 */

DEFUN(donth, "nth", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	register int	elem;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!fixnump(arg1) || (elem = gfixnum(arg1.vl_data)) < 0)
		BADARGN(1, "a positive fixnum");
	arg2 = EVALARGN(2);
	if (nullp(arg2))
		return (v_nil);
	else if (!dtprp(arg2))
		BADARGN(2, "a list");

	return (nth(elem, arg2));
}

struct value
nth(elem, list)
	struct value	list;
{
	register int	count;
	struct value	ret;

	ret = list;
	for (count = 0; count < elem && !eq(ret, v_nil); count++)
		ret = cdr(ret);
	if (dtprp(ret))
		return car(ret);
	else
		return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: nthcdr
 *  Call: (nthcdr 'number 'list)
 *  Retu: any
 *  Desc: This function returns the result of performing number
 *	\sym{cdr}s on the given list.  The first argument must
 *	evaluate to a positive fixnum and the second to a list.
 *	This basically returns a sub-list containg all the elements
 *	of the original list of index greater than number.  Thus,
 *	\lit{(nthcdr 2 '(a b c))} evaluates to \lit{(c)}.
 *  SeeA: nth cdr
 */

DEFUN(donthcdr, "nthcdr", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	register int	elem;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!fixnump(arg1) || (elem = gfixnum(arg1.vl_data)) < 0)
		BADARGN(1, "a positive fixnum");
	arg2 = EVALARGN(2);
	if (nullp(arg2))
		return (v_nil);
	else if (!dtprp(arg2))
		BADARGN(2, "a list");

	return (nthcdr(elem, arg2));
}

struct value
nthcdr(elem, list)
	struct value	list;
{
	register int	count;
	struct value	ret;

	ret = list;
	for (count = 0; count < elem && !eq(ret, v_nil); count++)
		ret = cdr(ret);
	return (ret);
}
