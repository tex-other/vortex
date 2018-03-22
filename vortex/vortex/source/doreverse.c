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
 *  RCS Info: $Header: doreverse.c,v 0.1 87/05/01 12:00:27 john Locked $
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
 *  doreverse.c - reverse a list or array
 */
static char _ID[] = "@(#)doreverse.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: reverse
 *  Call: (reverse 'list)
 *  Retu: list
 *  Desc: This function takes the given list and returns another
 *	list whose elements are all the same as those in the
 *	argument list, but with the order reversed.  The new list
 *	(whose elements are \sym{eq} as well as \sym{equal} to
 *	those in the argument list) is returned.
 *
 *	An example in case the usage of this function isn't
 *	already clear: \lit{(reverse '(a b c))} will evaluate
 *	to \lit{(c b a)}.  However, only elements at the top
 *	level will be reversed; \lit{(reverse '((a b) c d))} will
 *	evaluate to \lit{(d c (a b))}.
 *  Side: The original list is not modified, in other words the
 *	returned list is not \sym{eq} to the argument list.
 */

DEFUN(doreverse, "reverse", FLAG_NONE, NULL)
{
	struct value	reverse();
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!dtprp(arg))
		BADARGN(1, "a non-nil list");

	return reverse(arg);
}

struct value
reverse(list)
	struct value	*list;
{
	register int	index, count;
	struct value	ret, elt;

	ret = v_nil;
	count = length(list);
	for (index = 0; index < count; index++) {
		elt = nth(index, list);
		ret = cons(elt, ret);
	}
	return (ret);
}
