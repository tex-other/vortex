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
 *  RCS Info: $Header: douwprotect.c,v 0.1 87/05/01 12:05:32 john Locked $
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
 *  douwprotect.c - vLisp unwind-protect interface functions
 */
static char _ID[] = "@(#)douwprotect.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: unwind-protect
 *  Call: (unwind-protect body cleanup ...)
 *  Retu: any
 *  Desc: This function typically evaluates its first argument,
 *	saves the result of this evaluation, and evaluates the
 *	remaining arguments, if any.  Then, the result of evaluating
 *	the first argument is returned.  However, if something
 *	occurs that would normally cause control to return back
 *	``through'' the call to this function, the cleanup arguments
 *	are evaluated before continuing to unwind the call stack.
 *
 *	This is useful for performing some cleanup no matter what
 *	happens during sensitive processing.  Basically, the
 *	cleanup expressions will always get evaluated once on
 *	enters the call to \sym{uwprotect}.  Here is an example
 *	where this is useful:
 *
 *	\tab{\lit{(setq chan (fopen filename))}
 *	\lit{(uwprotect (let ((m1 (readc chan))}
 *	\lit{                 (m2 (readc chan)))}
 *	\lit{            (cond}
 *	\lit{             ((and (eq m1 ?%) (eq m2 ?!)) t)}
 *	\lit{             (t nil)))}
 *	\lit{ (close chan))}}
 *
 *	This code opens a file and reads two charcters from
 *	it.  The characters are then compared to see if they are
 *	equal to percent-sign and exclamation-point respectively.
 *	If both characters are, the result is t, else it is nil.
 *	In any case, the \lit{(close chan)} is evaluated.
 *
 *	Note that \sym{uwprotect}, unlike \sym{catch} does not
 *	prevent an error condition, it just guarantees that the
 *	cleanup statements will be executed no matter what.
 *  SeeA: catch error cerror
 *
 *  END
 *
 *  Note that we can't call unwind-protect too many times
 *  recursively.  The number of nested calls to unwind-protect
 *  is limited to MAXSTACK.  This shouldn't be too severe a
 *  limitation.
 */

#define MAXSTACK	1000

static struct value	cleanup_stack[MAXSTACK];
static struct value	*cleanup_ptr = cleanup_stack - 1,
			*cleanup_top = cleanup_stack + NITEMS(cleanup_stack);

static struct value
user_cleanup()
{
	struct value	result;
	struct value	stmt;
	register int	count, index;

	if (cleanup_ptr < cleanup_stack) {
		message("User unwind protect stack underflow!");
		return (v_nil);
	}

	result = v_nil;
	if (dtprp(*cleanup_ptr)) {
		count = length(*cleanup_ptr);
		for (index = 0; index < count; index++) {
			stmt = nth(index, *cleanup_ptr);
			result = evalsexpr(stmt);
		}
	}
	cleanup_ptr--;
	return (result);
}

DEFUN(douwprotect, "unwind-protect", FLAG_NONE, NULL)
{
	struct value	alist, body, clean;

	CHECKAC(2, -1);
	alist = GETALIST();
	body = car(alist);
	clean = cdr(alist);
	/* set up the 2nd through nth arguments as cleanup */
	if (cleanup_ptr + 1 >= cleanup_top)
		error("Too many nested calls to unwind-protect!");
	*++cleanup_ptr = clean;
	uwprotect(user_cleanup);

	/* evaluate first argument and return it */
	return evalsexpr(body);
}

uwprotect(cfunc)
	struct value	(*cfunc)();
{
	if (stack_ptr != NULL) {
		/* set new unwind protect function */
		stack_ptr->st_flags |= STACK_UNWIND;
		stack_ptr->st_unwind = cfunc;
	}

	return (0);
}
