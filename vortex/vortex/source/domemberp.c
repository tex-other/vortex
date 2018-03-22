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
 *  RCS Info: $Header: domemberp.c,v 0.1 87/05/01 11:54:47 john Locked $
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
 *  domemberp.c - various member predicates
 */
static char _ID[] = "@(#)domemberp.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: memberp
 *  Call: (memberp 'any 'list)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if the first argument is
 *	\sym{equal} to any member of the second argument (which must
 *	evaluate to a list).  Both arguments are evaluated.  Note that
 *	only the original elements of the list are checked for equality
 *	with the given atom.  Thus,
 *
 *	\lit{(memberp 'x '(a (b x) c))}
 *
 *	will evaluate to nil, since neither \lit{a}, \lit{(b x)} nor
 *	\lit{c} is \sym{equal} to \lit{x}.
 *  Xref: member
 *  SeeA: memeqp equal
 */

DEFUN(domemberp, "memberp", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, elt;
	register int	count, len;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	arg2 = EVALARGN(2);
	if (!dtprp(arg2))
		BADARGN(2, "a non-nil list");

	/* search through list for the first argument */
	len = length(arg2);
	for (count = 0; count < len; count++) {
		elt = nth(count, arg2);
		if (equal(elt, arg1))
			return (v_t);
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: memeqp
 *  Call: (memeqp 'any 'list)
 *  Retu: t or nil
 *  Desc: This predicate function returns t if the first argument is
 *	\sym{eq} to any member of the second argument (which must
 *	evaluate to a list).  Both arguments are evaluated.  Note that
 *	only the original elements of the list are checked for equality
 *	with the given atom.  Thus,
 *
 *	\lit{(memeqp 'x '(a (b x) c))}
 *
 *	will evaluate to nil, since neither \lit{a}, \lit{(b x)} nor
 *	\lit{c} is \sym{equal} to \lit{x}.
 *
 *	Because \sym{memeqp} uses \sym{eq} instead of \sym{equal}
 *	(like \sym{memberp} does), it is really just useful for
 *	finding atoms in the list.  Most often, two lists won't be
 *	\sym{eq} unless they are from exactly the same value.  However,
 *	\sym{memeqp} is significantly faster when the list is long
 *	or contains sub-lists.
 *  Xref: memeq
 *  SeeA: memberp eq
 */

DEFUN(domemeqp, "memeqp", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, elt;
	register int	count, len;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	arg2 = EVALARGN(2);
	if (!dtprp(arg2))
		BADARGN(2, "a non-nil list");

	/* search through list for the first argument */
	len = length(arg2);
	for (count = 0; count < len; count++) {
		elt = nth(count, arg2);
		if (eq(elt, arg1))
			return (v_t);
	}
	return (v_nil);
}
