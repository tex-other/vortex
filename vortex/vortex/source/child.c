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
 *  RCS Info: $Header: child.c,v 0.1 87/05/01 11:26:49 john Locked $
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
 *  child.c - vLisp child process management routines
 */
static char _ID[] = "@(#)child.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <errno.h>
#include "vse.h"
#include "process.h"

/* ARGSUSED */
children(signo)
{
	union wait  	w;
	int 		pid;
	struct rusage	ru;

	PROTECT();
	for (;;) {
		/* wait for a process */
		pid = wait3(&w, WNOHANG | WUNTRACED, &ru);
		if (pid <= 0)
			break;

		/* this process has changed */
		update_process(pid, &w, &ru);
	}
	UNPROTECT();
}

/* ARGSUSED */
update_process(pid, w, ru)
	union wait	*w;
	struct rusage	*ru;
{
	struct process	*proc;

	if ((proc = find_process(pid)) == NULL)
		ierror("update_process: Called to handle unknown child!");

	if (WIFSTOPPED(*w)) {
		/* process stopped */
		proc->pr_state = STATE_STOPPED;
		proc->pr_signal = w->w_stopsig;
		proc->pr_exit = 0;
	} else if (WIFSIGNALED(*w)) {
		/* process died of signal */
		proc->pr_state = STATE_SIGNAL;
		proc->pr_signal = w->w_termsig;
		if (w->w_coredump)
			proc->pr_flags |= PROC_CORE;
	} else {
		/* died more-or-less normally */
		proc->pr_state = STATE_EXITED;
		proc->pr_signal = 0;
		proc->pr_exit = w->w_retcode;
	}

	/* user needs to be notified */
	proc->pr_flags |= PROC_REPORT;

	return (0);
}
