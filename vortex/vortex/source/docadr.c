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
 *  RCS Info: $Header: docadr.c,v 0.1 87/05/01 11:35:37 john Locked $
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
 *  docadr.c - basic vLisp list access functions
 */
static char _ID[] = "@(#)docadr.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: car
 *  Call: (car 'list)
 *  Retu: any
 *  Desc: The function \sym{car} takes as its argument an
 *	evaluated list and returns the fist element.  If the
 *	argument is nil, nil is returned and it is an error for
 *	the argument to be a non-list.
 *  SeeA: cdr
 */

DEFUN(docar, "car", FLAG_NONE, NULL)
{
	struct value	arg;

	/* make sure we have a single list argument */
	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (nullp(arg))
		return (v_nil);
	else if (!dtprp(arg))
		BADARGN(1, "a list");
	return car(arg);
}

struct value
car(sexpr)
	struct value	sexpr;
{
	struct ccell	*ccell;
	struct value	ret;

	if (!dtprp(sexpr))
		ierror("car: Non-list argument to take car of!");

	ccell = glist(sexpr.vl_data);
	ret.vl_type = ccell->cc_tcar;
	ret.vl_data = ccell->cc_car;
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: cdr
 *  Call: (cdr 'list)
 *  Retu: any
 *  Desc: The function \sym{car} takes as its argument an
 *	evaluated list and returns all but the fist element.
 *	If the argument is nil, nil is returned and it is an
 *	error for the argument to be a non-list.
 *  SeeA: car
 */

DEFUN(docdr, "cdr", FLAG_NONE, NULL)
{
	struct value	arg;

	/* make sure we have a single list argument */
	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (nullp(arg))
		return (v_nil);
	else if (!dtprp(arg))
		BADARGN(1, "a list");
	return cdr(arg);
}

struct value
cdr(sexpr)
	struct value	sexpr;
{
	struct ccell	*ccell;
	struct value	ret;

	if (!dtprp(sexpr))
		ierror("cdr: Non-list argument to take cdr of!");

	ccell = glist(sexpr.vl_data);
	ret.vl_type = ccell->cc_tcdr;
	ret.vl_data = ccell->cc_cdr;
	return (ret);
}
