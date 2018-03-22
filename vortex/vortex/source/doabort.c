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
 *  RCS Info: $Header: doabort.c,v 0.1 87/05/01 11:32:01 john Locked $
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
 *  doabort.c - function to handle abort action
 */
static char _ID[] = "@(#)doabort.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"


/*
 *  DOCUMENTATION
 *
 *  Name: break-on-abort
 *  Desc: This variable controls the action taken when the
 *	abort function (bound by default to C-g) is invoked.
 *	Usually all processing aborts and control returns to
 *	the top level of the interpreter, but if this
 *	variable is set to a non-nil value, a continuable
 *	break loop is entered.  Continuing the break loop
 *	causes the normal abort processing to resume.
 *  SeeA: break-on-error abort-function
 */
MKSTRING(BREAKABORT, "break-on-abort");

/*
 *  DOCUMENTATION
 *
 *  Name: abort-function
 *  Call: (abort-function)
 *  Desc: This function causes a return to the top-level of the
 *	interpreter, typically used when the user wants to abandon
 *	current processing and get back to the ``main loop'' of
 *	the editor.
 *  Side: This is implemented internally as a \sym{throw} to the
 *	\lit{abort} catch at the top level.  At the top level, the
 *	throw is handled by printing a message and resuming as
 *	though nothing had happened.
 *
 *	If the variable \sym{break-on-abort} is set to a non-nil
 *	value, we actually do a break loop before the throw to
 *	top-level.  This means that the user can grap control,
 *	for debugging, of abort actions.
 *  Xref: abort
 *  SeeA: throw break-on-abort
 */
extern struct string	*ABORTCATCH;

DEFUN(doabort, "abort-function", FUNC_VISUAL, "")
{
	extern struct value	break_loop();

	CHECKAC(0, 0);
	int_abort();
	panic("Internal abort function returned!");
}

int_abort()
{
	if (truevar(BREAKABORT))
		break_loop(TRUE, "Abort function invoked.");
	int_throw(ABORTCATCH, v_t);
	/* NOTREACHED */
}
