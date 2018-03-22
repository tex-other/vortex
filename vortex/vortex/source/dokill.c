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
 *  RCS Info: $Header: dokill.c,v 0.1 87/05/01 11:51:07 john Locked $
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
 *  dokill.c - functions to signal child processes
 */
static char _ID[] = "@(#)dokill.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include "vse.h"
#include "vlisp.h"
#include "process.h"

/*
 *  DOCUMENTATION
 *
 *  Name: kill
 *  Call: (kill 'signal 'process)
 *  Retu: t
 *  Desc: This function sends a signal to the given process, which
 *	must either have been started from this invocation of
 *	VorTeX or be owned by the current user.  The first
 *	argument is the signal to send (see \em{kill(2)}),
 *	which can be given either as a number or a symbol.
 *	For example, to specify the hangup signal be sent to
 *	process id 4567, one would evaluate:
 *
 *	\lit{(kill 'HUP 4567)}
 *
 *	Other ways to specify this action are:
 *
 *	\tab{\lit{(kill 1 4567)}
 *	\lit{(kill 'SIGHUP 4567)}}
 *
 *	The different signals are listed in the \sc{UNIX} manual
 *	page for the \em{kill(2)} system call.  As you can see
 *	from above, either the symbol names or the signal numbers
 *	may be used.
 *  Side: Since this signal may change the state of the process
 *	affected (it seems quite likely to have this effect), a
 *	call the user's notify function (if one has been specified
 *	with \sym{notify}) may be generated, or the process may
 *	change state and need to be reaped.
 *  Xref: signal
 *  SeeA: wait notify status process
 */

DEFUN(dokill, "kill", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct string	*str;
	int		signo, pid;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (fixnump(arg1)) {
		signo = gfixnum(arg1.vl_data);
		if (signo < 1 || signo > NSIG)
			error("Bad signal number %d!", signo);
	} else if (symbolp(arg1)) {
		str = gsymbol(arg1.vl_data)->sy_pname;
		signo = get_signal(str);
	} else {
		/* wrong type argument */
		BADARGN(1, "a signal number or name");
	}

	arg2 = EVALARGN(2);
	if (!fixnump(arg2))
		BADARGN(2, "a process-id");
	pid = gfixnum(arg2.vl_data);

	/* actually kill the process now */
	if (kill(pid, signo) < 0)
		perror("Can't kill process %d", pid);

	return (v_t);
}

static char	*signal_names[] = {
	NULL,
	"HUP", "INT", "QUIT", "ILL", "TRAP", "IOT", "EMT", "FPE", "KILL",
	"BUS", "SEGV", "SYS", "PIPE", "ALRM", "TERM", "URG", "STOP", "TSTP",
	"CONT", "CHLD", "TTIN", "TTOU", "IO", "XCPU", "XFSZ", "VTALRM",
	"PROF", "WINCH", "USR1", "USR2", "32"
};

static int
get_signal(str)
	struct string	*str;
{
	char		cbuf[SMALLBUF];
	register char	*cp;
	register int	sig;

	makecstring(str, cbuf, sizeof (cbuf));
	for (cp = cbuf; *cp != '\0'; cp++)
		if (islower(*cp))
			*cp = toupper(*cp);
	if (!strncmp(cbuf, "SIG", 3))
		cp = cbuf + 3;
	else
		cp = cbuf;

	for (sig = 1; sig <= NSIG; sig++)
		if (!strcmp(cp, signal_names[sig]))
			return (sig);
	error("Unknown signal name %Y!", str);
	/* NOTREACHED */
}
