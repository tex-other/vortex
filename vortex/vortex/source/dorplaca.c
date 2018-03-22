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
 *  RCS Info: $Header: dorplaca.c,v 0.1 87/05/01 12:00:44 john Locked $
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
 *  dorplaca.c - vLisp rplaca and rplacd functions
 */
static char _ID[] = "@(#)dorplaca.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: rplaca
 *  Call: (rplaca 'list 'any)
 *  Retu: list
 *  Desc: This function changes what is in the \sym{car} of the
 *	cons cell (head of the list of the first argument) to
 *	be the second value.  The first argument must evaluate
 *	to a non-nil list, the second may evaluate to anything.
 *	The changed list is returned.
 *
 *	This is not the normal way to make a new list; \sym{rplaca}
 *	does not make a new list at all, but modifies the value
 *	passed to it.  This action is the reason this is used at
 *	all, no new list neads to be created to make a change.
 *  Side: This function modifies an existing vlisp list, rather
 *	than just copying it!  Beware, other references to this
 *	changed value will be changed also.  This side effect makes
 *	it extremely dangerous.
 *
 *	Thus, if the \lit{x} is bound to \lit{(a b)}, and the
 *	expression \lit{(rplaca x 'A)} is evaluated, the list
 *	\lit{(A b)} will be returned and the vlisp value \lit{x}
 *	is bound to will also have changed.
 *  SeeA: rplacd car cons
 */

DEFUN(dorplaca, "rplaca", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct ccell	*head;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!dtprp(arg1))
		BADARGN(1, "a cons cell");
	arg2 = EVALARGN(2);

	head = glist(arg1.vl_data);
	head->cc_tcar = arg2.vl_type;
	head->cc_car = arg2.vl_data;
	return (arg1);
}

rplaca(cell, val)
	struct value	cell, val;
{
	struct ccell	*head;

	if (!dtprp(cell))
		ierror("rplaca: Non-list to replace car of!");

	head = glist(cell.vl_data);
	head->cc_tcar = val.vl_type;
	head->cc_car = val.vl_data;

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: rplacd
 *  Call: (rplacd 'list 'any)
 *  Retu: list
 *  Desc: This function changes what is in the \sym{cdr} of the
 *	cons cell (head of the list of the first argument) to
 *	be the second value.  The first argument must evaluate
 *	to a non-nil list, the second may evaluate to anything.
 *	The changed list is returned.
 *
 *	This is not the normal way to make a new list; \sym{rplaca}
 *	does not make a new list at all, but modifies the value
 *	passed to it.  This action is the reason this is used at
 *	all, no new list neads to be created to make a change.
 *  Side: This function modifies an existing vlisp list, rather
 *	than just copying it!  Beware, other references to this
 *	changed value will be changed also.  This side effect makes
 *	it extremely dangerous.
 *
 *	Thus, if the \lit{x} is bound to \lit{(a b c)}, and the
 *	expression \lit{(rplacd x 'xxx)} is evaluated, the dotted
 *	pair \lit{(a . xxx)} will be returned and the vlisp value
 *	\lit{x} is bound to will also have changed.
 *  SeeA: rplaca cdr cons
 */

DEFUN(dorplacd, "rplacd", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct ccell	*head;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!dtprp(arg1))
		BADARGN(1, "a cons cell");
	arg2 = EVALARGN(2);

	head = glist(arg1.vl_data);
	head->cc_tcdr = arg2.vl_type;
	head->cc_cdr = arg2.vl_data;
	return (arg1);
}

rplacd(cell, val)
	struct value	cell, val;
{
	struct ccell	*head;

	if (!dtprp(cell))
		ierror("rplacd: Non-list to replace cdr of!");

	head = glist(cell.vl_data);
	head->cc_tcdr = val.vl_type;
	head->cc_cdr = val.vl_data;

	return (0);
}
