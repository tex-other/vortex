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
 *  RCS Info: $Header: doexit.c,v 0.1 87/05/01 11:44:48 john Locked $
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
 *  doexit.c - functions to exit the editor
 */
static char _ID[] = "@(#)doexit.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: exit-vortex
 *  Call: (exit-vortex [ 'force ])
 *  Desc: This function exits the editor, cleaning up any left
 *	over modified file-visiting buffers and processes.  If
 *	the option argument is given as non-nil, modified file
 *	visiting buffers are written and active processes are
 *	killed automatically.
 *
 *	If no argument is given (or the argument evaluates to nil),
 *	the user is prompted about each modified file-visiting
 *	buffer and once for about the running processes.
 *  Side: Causes the editor to exit and control the return to
 *	parent process, usually the user's shell.
 *  SeeA: exit
 */

DEFUN(doexitvortex, "exit-vortex", FLAG_NONE, "p")
{
	struct value	arg;
	int		force = FALSE;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (truep(arg))
			force = TRUE;
	}

	/* query writes for modified file-visiting buffers */

	/* query kills of running processes */

	int_exit(force);
	/* NOTREACHED */
}

int_exit(force)
{
	extern int	proof_socket, format_socket;

	quitting = TRUE;

	/* close down remote connections */
	if (proof_socket > 0)
		quitproof(0);
	if (format_socket > 0)
		quitformat(0);

	/* checkpoint modified file-visiting buffers */

	exit(0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: exit
 *  Call: (exit [ 'fixnum ])
 *  Desc: This function exits the lisp interpreter, returning
 *	control to the parent process.  No clean up of any kind
 *	is done.  The proper function to call to end an editing
 *	session is \sym{exit-vortex}.
 *  Side: Never returns; the process terminates.
 *  SeeA: exit-vortex
 */

DEFUN(doexit, "exit", FLAG_NONE, NULL)
{
	int		status = 0;
	struct value	estat;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		estat = EVALARGN(1);
		if (fixnump(estat)) {
			/* given exit status */
			status = gfixnum(estat.vl_data);
		} else {
			/* something funky is going on */
			status = 1;
		}
	}

	exit(status);
}
