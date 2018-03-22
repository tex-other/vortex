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
 *  RCS Info: $Header: donotify.c,v 0.1 87/05/01 11:56:15 john Locked $
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
 *  donotify.c - process change notifier handling function
 */
static char _ID[] = "@(#)donotify.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "process.h"

/*
 *  DOCUMENTATION
 *
 *  Name: notify
 *  Call: (notify 'process 'function)
 *  Retu: symbol
 *  Desc: This function registers a user function which is to be
 *	notified whenever the process specified by the first argument
 *	changes state.  Both arguments are evaluated, the first
 *	must evaluate to a process-id (a fixnum) and the second to
 *	a function name (a symbol).  The function returns the symbol
 *	name it will call when the process changes state.  If the
 *	function is given as nil, no notifier function is associated
 *	with this process (any existing one is removed) and
 *	\sym{notify} returns nil.
 *
 *	The given function will be called asynchronously when the
 *	specified process changes state.  It will be called with
 *	a single argument, the process-id of the process affected
 *	(so that a single notifier function may be used to handle
 *	multiple children).  Inside this notifier function, the
 *	user will most likely want to call \sym{status} to determine
 *	what do do with the child.
 *
 *	The process-id argument which specifies a process is the
 *	value returned by \sym{exec} when a process is started.
 *	The function \sym{status} takes as its argument this id
 *	and returns information about the current state of the
 *	process.
 *  Side: The function registered as a notifier may be called at
 *	unexpected times, since \sc{UNIX} processes won't usually
 *	know anything at all about the editor.  However, two notifier
 *	functions will never be executing at the same time, nor
 *	should there be race condition problems.
 *  SeeA: exec status process
 */

DEFUN(donotify, "notify", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct symbol	*sym;
	struct process	*proc;
	register int	pid;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!fixnump(arg1))
		BADARGN(1, "a process-id");
	pid = gfixnum(arg1.vl_data);
	arg2 = EVALARGN(2);
	if (symbolp(arg2))
		sym = gsymbol(arg2.vl_data);
	else if (nullp(arg2))
		sym = NULL;
	else
		BADARGN(2, "a function name symbol");

	/* search for the asked-for process */
	if ((proc = find_process(pid)) == NULL)
		error("No current process with an id of %d!", pid);

	/* set the notifier function to this symbol */
	proc->pr_notify = sym;

	return (arg2);
}
