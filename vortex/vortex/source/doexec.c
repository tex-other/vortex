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
 *  RCS Info: $Header: doexec.c,v 0.1 87/05/01 11:44:30 john Locked $
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
 *  doexec.c - vLisp process initiation functions
 */
static char _ID[] = "@(#)doexec.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "process.h"

/*
 *  DOCUMENTATION
 *
 *  Name: process
 *  Desc: The \em{process} paradigm was designed to allow the
 *	lisp programmer to call \sc{UNIX} commands from within
 *	lisp programs and interact with it in various ways.  There
 *	are functions to create a process, to send it signals,
 *	to enquire its status and to specify a handler to reap
 *	children as they change state or die.  Also, input and
 *	output of the process can be handled through the different
 *	types of I/O channels available.
 *
 *	The function \sym{exec} creates a new process, where the
 *	user may specify input and output handlers and the argument
 *	list of the child.  The child process can also recieve information
 *	through it \sc{UNIX} \em{environment}, which is inherited
 *	from the parent process.
 *
 *	Once a process is started, the controlling lisp process can
 *	watch it in several ways.  It can \sym{wait} until the process
 *	has completed, it can ask explicitly for the \sym{status} of
 *	the process at any time, and it can specify a function which
 *	will be called to \sym{notify} the user's system when the process
 *	changes state.  Signals can be sent to the child process using
 *	\sym{kill}, which is most commonly used to terminate a process
 *	abnormally.
 *  SeeA: exec wait status notify kill channel
 */

/*
 *  DOCUMENTATION
 *
 *  Name: exec
 *  Call: (exec 'command 'input 'output 'argument ...)
 *  Retu: process
 *  Desc: This function starts up a \sc{UNIX} process, the command
 *	specified by the first argument, with arguments as specified
 *	by the fourth and following argument.  The command and all
 *	the args must evaluate to strings.  The command is the full
 *	pathname of the binary file to execute.  The args are passed
 *	to the process through the \sc{UNIX} paradigm as \lit{argv[]}.
 *	The second and third arguments must evaluate to channels or
 *	nil; these specify the handling of input and output of the
 *	process.  Valid channel numbers provide ``pipes'' into and
 *	out of the process, while a nil argument specifies that the
 *	channel is to be closed.
 *
 *	The channel arguments, if non-nil, should have been created
 *	by one of the I/O channel functions.  For example, if a
 *	channel was opened for writing into buffer ``foo'' with:
 *
 *	\tab{\lit{(setq cout (bopen "foo" "w"))}}
 *
 *	and then the \sc{UNIX} command ``date'' was invoked with
 *	\sym{exec} as:
 *
 *	\tab{\lit{(setq pid (exec "/bin/date" nil cout "date"))}
 *	\lit{(wait pid)}
 *	\lit{(close cout)}}
 *
 *	``date'' will run with its output going into the buffer
 *	``foo'', which should cause a string such as
 *	\lit{"Fri Jul 18 11:30:13 PDT 1986^J"} to be inserted into
 *	buffer ``foo'' before point.  Note that after the ``date''
 *	process exited, we had to close the channel explicitly, since
 *	the channel may still be wanted for writing into that buffer.
 *  Side: Information can be passed to child processes created by
 *	\sym{exec} through the environment.  When VorTeX starts up,
 *	it imports the environment from its parent and passes this
 *	on to its children.  The variables recieved by child processes
 *	may be queried with \sym{getenv} and changed with \sym{setenv}.
 *  Xref: execl
 *  SeeA: channel process getenv setenv
 */

DEFUN(doexec, "exec", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct string	*str;
	struct channel	*cin, *cout;
	struct process	*proc;
	char		**cargv, abuf[STRBUF];
	char		pbuf[STRBUF], *path;
	register int	count, largc, cargc;

	CHECKAC(4, -1);

	/* get the binary program name */
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");
	str = gstring(arg.vl_data);
	makecstring(str, pbuf, sizeof (pbuf));
	path = fixpath(pbuf);
	if (*pbuf == '\0')
		error("Binary pathname must not be empty!");
	if (access(path, X_OK) != 0)
		perror("Can't execute file %s", path);

	/* get input/output channels */
	arg = EVALARGN(2);
	if (nullp(arg))
		cin = NULL;
	else if (fixnump(arg))
		cin = get_channel(gfixnum(arg.vl_data));
	else
		BADARGN(2, "a channel number");
	arg = EVALARGN(3);
	if (nullp(arg))
		cout = NULL;
	else if (fixnump(arg))
		cout = get_channel(gfixnum(arg.vl_data));
	else
		BADARGN(3, "a channel number");

	/* can't connect multiple process to a single channel */
	if ((cin != NULL && (cin->ch_flags & CHAN_PROCESS) != 0) ||
	    (cout != NULL && (cout->ch_flags & CHAN_PROCESS) != 0))
		error("Can't connect more than one process to a channel.");

	/* make up argument list from rest of arguments */
	largc = GETACOUNT();
	cargv = (char **)valloc((largc - 2) * sizeof (char *));
	cargc = 0;
	for (count = 4; count <= largc; count++) {
		arg = EVALARGN(count);
		if (!stringp(arg)) {
			/* free the space we allocated */
			for (count = 0; count < cargc; count++)
				vfree(cargv[count]);
			vfree(cargv);
			BADARGN(count, "a string argument");
		}
		str = gstring(arg.vl_data);
		makecstring(str, abuf, sizeof (abuf));
		cargv[cargc++] = strsave(abuf);
	}
	cargv[cargc] = NULL;

	/* make up the new process structure */
	proc = (struct process *)valloc(sizeof (struct process));
	bzero(proc, sizeof (struct process));
	strncpy(proc->pr_comm, path, sizeof (proc->pr_comm) - 1);
	proc->pr_comm[sizeof (proc->pr_comm) - 1] = '\0';
	proc->pr_ichan = cin;
	if (cin != NULL) {
		cin->ch_flags |= CHAN_PROCESS;
		cin->ch_process = proc;
	}
	proc->pr_ochan = cout;
	if (cout != NULL) {
		cout->ch_flags |= CHAN_PROCESS;
		cout->ch_process = proc;
	}
	proc->pr_argc = cargc;
	proc->pr_argv = cargv;

	/* insert this process into process list and run it */
	start_process(proc, TRUE, -1, -1);

	/* return the process-id as handle */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, proc->pr_pid);
	return (ret);
}
