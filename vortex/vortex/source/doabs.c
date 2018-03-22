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
 *  RCS Info: $Header: doabs.c,v 0.1 87/05/01 11:32:38 john Locked $
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
 *  doabs.c - various numeric comparison functions
 */
static char _ID[] = "@(#)doabs.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: abs
 *  Call: (abs 'fixnum)
 *  Retu: fixnum
 *  Desc: This function returns the absolute value of its evaluated
 *	argument.  That is, if the number is negative, its positive
 *	counterpart is returned, otherwise the number itself is
 *	returned.
 */

DEFUN(doabs, "abs", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	int		n;

	CHECKAC(1, 1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum");
	n = gfixnum(arg.vl_data);
	if (n >= 0) {
		/* just return the number */
		return (arg);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, -n);
		return (ret);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: max
 *  Call: (max 'fixnum 'fixnum [ 'fixnum ... ])
 *  Retu: fixnum
 *  Desc: This function returns the maximum of its list of
 *	evaluated numeric arguments.  The largest number in
 *	the list is returned, just as it evaluated as an
 *	argument, thus \lit{(max -5 3 6)} evaluates to \lit{6}.
 *  SeeA: min
 */

DEFUN(domax, "max", FLAG_NONE, NULL)
{
	struct value	arg, max;
	register int	i, cmax = MIN_FIXNUM;

	CHECKAC(2, -1);
	for (i = GETACOUNT(); i > 0; i--) {
		arg = EVALARGN(i);
		if (!fixnump(arg))
			BADARGN(i, "a fixnum");
		if (gfixnum(arg.vl_data) > cmax)
			max = arg;
	}
	return (max);
}

/*
 *  DOCUMENTATION
 *
 *  Name: min
 *  Call: (min 'fixnum 'fixnum [ 'fixnum ... ])
 *  Retu: fixnum
 *  Desc: This function returns the minimum of its list of
 *	evaluated numeric arguments.  The smallest number in
 *	the list is returned, just as it evaluated as an
 *	argument, thus \lit{(min 7 5)} evaluates to \lit{5}.
 *  SeeA: max
 */

DEFUN(domin, "min", FLAG_NONE, NULL)
{
	struct value	arg, min;
	register int	i, cmin = MAX_FIXNUM;

	CHECKAC(2, -1);
	for (i = GETACOUNT(); i > 0; i--) {
		arg = EVALARGN(i);
		if (!fixnump(arg))
			BADARGN(i, "a fixnum");
		if (gfixnum(arg.vl_data) < cmin)
			min = arg;
	}
	return (min);
}
