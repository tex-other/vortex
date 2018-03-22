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
 *  RCS Info: $Header: dowait.c,v 0.1 87/05/01 12:06:33 john Locked $
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
 *  dowait.c - vLisp wait function (wait on child processes)
 */
static char _ID[] = "@(#)dowait.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "process.h"

/*
 *  DOCUMENTATION
 *
 *  Name: wait
 *  Call: (wait [ 'process ])
 *  Retu: process
 *  Desc: This function waits until the specified process has
 *	changed state, or, if no process was specified, until
 *	the first child process changes state.  The optional
 *	arguent must evaluate to the process-id (a fixnum) of
 *	a current process.  If that process has already terminated
 *	the function returns immediately.
 *
 *	This function is meant to be used when \sc{UNIX} processes
 *	are to be run synchronously with vlisp code.  That is, nothing
 *	else will happen until the process changes state (usually
 *	when it terminates).  At that point, the process-id is
 *	returned, which can be used to obtain the \sym{status} of
 *	the process, etc.
 *  Side: The notifier function for the process which was waited
 *	upon is not called, because it is assumed that whomever
 *	performs the \sym{wait} will handle it.  The user can
 *	always call the notifier function by hand if so desired.
 *
 *	Note that if the last child process has already terminated,
 *	this function will return nil, meaning there was nothing
 *	to wait on.  The condition in which there are no current
 *	processes at all, is handled by an error.
 *  SeeA: exec status notify process
 */

DEFUN(dowait, "wait", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	int		pid;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a process id");
		pid = gfixnum(arg.vl_data);
		/* make sure that this is a real process */
		if (find_process(pid) == NULL)
			error("No current process with an id of %d!", pid);
	} else {
		/* wait on any process */
		pid = -1;
		/* make sure we have some process running */
		if (process_list == NULL)
			error("There are no current processes!");
	}

	if ((pid = wait_process(pid)) > 0) {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, pid);
		return (ret);
	} else {
		/* no process to wait on */
		return (v_nil);
	}
}

static int
wait_process(pid)
{
	union wait	w;
	struct rusage	ru;
	int		ret;

	SIGBLOCK(SIGCHLD);
again:	/* wait for our special process */
	ret = wait3(&w, WUNTRACED, &ru);
	if (pid > 0 && ret != pid) {
		/* some other process */
		update_process(ret, &w, &ru);
		goto again;
	}
	pid = ret;

	/* take care of the waited for process */
	if (pid > 0)
		update_process(pid, &w, &ru);
	SIGRELSE(SIGCHLD);

	return (pid);
}
