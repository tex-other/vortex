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
 *  RCS Info: $Header: docons.c,v 0.1 87/05/01 11:40:17 john Locked $
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
 *  docons.c - basic list construction functions
 */
static char _ID[] = "@(#)docons.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

struct value	cons_lastcell;

/*
 *  DOCUMENTATION
 *
 *  Name: cons
 *  Call: (cons 'any 'list)
 *  Retu: list
 *  Desc: This function is the fundamental way that new lists are
 *	created in vlisp.  It takes an element and a list and
 *	returns a new list whose \sym{car} is that new element
 *	and whose \sym{cdr} is the given list.  The first argument
 *	may evaluate to anything and the second must evaluate
 *	to a list (although nil is okay).
 *  SeeA: car cdr
 */

DEFUN(docons, "cons", FLAG_NONE, NULL)
{
	struct value	ret, arg1, arg2;
	struct ccell	*new;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	arg2 = EVALARGN(2);

	/* make the new cons cell, assign car */
	new = save_ccell();
	new->cc_tcar = arg1.vl_type;
	new->cc_car = arg1.vl_data;
	/* assign cdr of new cons cell */
	new->cc_tcdr = arg2.vl_type;
	new->cc_cdr = arg2.vl_data;

	/* return the new cons cell */
	ret.vl_type = LISP_CONS;
	slist(ret.vl_data, new);
	return (ret);
}

struct value
cons(vcar, vcdr)
	struct value	vcar, vcdr;
{
	struct value	ret;
	struct ccell	*cell;

	/* set up cons cell */
	cell = save_ccell();
	cell->cc_tcar = vcar.vl_type;
	cell->cc_car = vcar.vl_data;
	cell->cc_tcdr = vcdr.vl_type;
	cell->cc_cdr = vcdr.vl_data;

	/* make up list value */
	ret.vl_type = LISP_CONS;
	slist(ret.vl_data, cell);
	cons_lastcell = ret;
	return (ret);
}
