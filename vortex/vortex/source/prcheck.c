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
 *  RCS Info: $Header: prcheck.c,v 0.1 87/05/01 12:23:32 john Locked $
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
 *  prcheck.c - internal child process handler routines
 */
static char _ID[] = "@(#)prcheck.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <errno.h>
#include "vse.h"
#include "value.h"
#include "channel.h"
#include "process.h"

process_check()
{
	struct process	*proc;
	register int	count, nfds;
	unsigned long	rmask[FDM_SIZE];
	struct timeval	tmout;

	/* check for processes that have produced output */
	tmout.tv_sec = tmout.tv_usec = 0;
	bcopy(process_mask, rmask, sizeof (rmask));
	nfds = select(NOFILE, rmask, NULL, NULL, &tmout);
	if (nfds < 0)
		perror("process_check: Select failed");
	/* check all processes for bit being set in read mask */
	for (proc = process_list; proc != NULL; proc = proc->pr_next) {
		/* don't check this process any more if there is an error */
		if (isset(rmask, proc->pr_iochan) && collect_output(proc) < 0)
			clrbit(process_mask, proc->pr_iochan);
	}

	/* check for process that need notification */
	count = 0;
	for (proc = process_list; proc != NULL; proc = proc->pr_next) {
		if ((proc->pr_flags & PROC_REPORT) != 0 &&
		    proc->pr_notify != NULL) {
			/* call user's notifier function */
			call_notifier(proc);
			proc->pr_flags &= ~PROC_REPORT;
		}
		count++;
	}
	return (count);
}

static
call_notifier(proc)
	struct process	*proc;
{
	struct symbol	*notify;
	struct value	arg, cmd, fval, ret;

	if ((notify = proc->pr_notify) == NULL)
		return (-1);
	debug(DPROC, "Calling notifier %Y on process %d, state is %d.",
	      proc->pr_notify->sy_pname, proc->pr_pid, proc->pr_state);

	cmd.vl_type = LISP_SYMBOL;
	ssymbol(cmd.vl_data, notify);
	fval = evalsexpr(cmd);
	if (!funcp(fval))
		error("Notifier function %v isn't a function!", cmd);
	arg.vl_type = LISP_FIXNUM;
	sfixnum(arg.vl_type, proc->pr_pid);
	ret = call_function(cmd, fval, arg);

	return (0);
}

static int
collect_output(proc)
	struct process	*proc;
{
	extern int	errno;
	struct channel	*ochan;
	unsigned char	*outend;
	unsigned char	buf[CHANBUF - 1];
	int		nbytes;

	if ((ochan = proc->pr_ochan) == NULL)
		return (-1);

	/* read the input and ``write'' it to channel */
	nbytes = read(proc->pr_iochan, buf, sizeof (buf));
	if (nbytes <= 0)
		return (-1);

	PROTECT();
	outend = ochan->ch_output + sizeof (ochan->ch_output);
	if (ochan->ch_outptr + nbytes >= outend)
		cflush(ochan);
	bcopy(buf, ochan->ch_outptr, nbytes);
	ochan->ch_outptr += nbytes;
	UNPROTECT();

	return (nbytes);
}
