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
 *  RCS Info: $Header: dostatus.c,v 0.1 87/05/01 12:03:57 john Locked $
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
 *  dostatus.c - vLisp child process status function
 */
static char _ID[] = "@(#)dostatus.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "process.h"

/*
 *  DOCUMENTATION
 *
 *  Name: status
 *  Call: (status 'process)
 *  Retu: dotted pair
 *  Desc: This function returns the status of a child process
 *	as a state and value.  It takes a single argument, which
 *	must evaluate to the process-id of a current \sc{UNIX}
 *	process created with \sym{exec}.
 *
 *	The first part of the dotted pair is the current state
 *	of the process, as a symbol.  It will be one of:
 *
 *	\tab{run	process is running merrily
 *	stop	process stopped by a signal
 *	signal	process killed by a signal
 *	exit	process exited (somewhat) normally}
 *
 *	When the state is \lit{run}, the value (second element
 *	of the dotted pair) is not used.  Otherwise, it will
 *	contain subsidiary information on the process.  If the
 *	state is \lit{stop} or \lit{signal}, this value will
 *	be the number of the signal which caused this change
 *	of state.  Otherwise, when state is \lit{exit}, the value
 *	will be the exit status, the argument passed to \em{exit(2)}
 *	in a C program.
 *  Side: When one queries the status of a process that has
 *	gone away (state is \lit{signal} or \lit{exit}), that
 *	process is removed from the list of current processes.
 *	I.e., trying to get the status of a defunct process more
 *	than once will cause the second call to \sym{status} to
 *	raise an error.  This is the only way internal state for
 *	defunct processes can be disposed of.
 *  SeeA: exec notify process
 */

static char	*state_cnames[] = {
	"illegal", "run", "stop", "exit", "signal"
};
static struct symbol	*state_symbols[NITEMS(state_cnames)];

DEFUN(dostatus, "status", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct ccell	*cell;
	struct process	*proc;
	struct string	*str;
	register int	pid, state, value;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a process-id");
	pid = gfixnum(arg.vl_data);

	/* get for the asked-for process */
	if ((proc = find_process(pid)) == NULL)
		error("No current process with an id of %d!", pid);

	/* get state, it shouldn't ever be wrong, though */
	state = proc->pr_state;
	if (state == STATE_RUNNING)
		value = 0;
	else if (state == STATE_EXITED)
		value = proc->pr_exit;
	else
		value = proc->pr_signal;
	if (state < STATE_RUNNING || state > STATE_SIGNAL) {
		/* this shouldn't happen */
		state = STATE_EXITED;
		value = -1;
	}

	/* make up state for return value */
	cell = save_ccell();
	ret.vl_type = LISP_CONS;
	slist(ret.vl_data, cell);
	cell->cc_tcar = LISP_SYMBOL;
	if (state_symbols[state] == NULL) {
		str = save_string(state_cnames[state],
				  strlen(state_cnames[state]));
		state_symbols[state] = save_symbol(str);
	}
	ssymbol(cell->cc_car, state_symbols[state]);
	cell->cc_tcdr = LISP_FIXNUM;
	sfixnum(cell->cc_cdr, value);

	/* check if this process has gone away */
	if (proc->pr_state != STATE_RUNNING &&
	    proc->pr_state != STATE_STOPPED) {
		/* delete this process, it's now gone */
		delete_process(proc);
	}

	return (ret);
}
