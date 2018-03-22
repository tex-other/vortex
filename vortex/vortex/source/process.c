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
 *  RCS Info: $Header: process.c,v 0.1 87/05/01 12:24:37 john Locked $
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
 *  process.c - internal process initiation routines
 */
static char _ID[] = "@(#)process.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <errno.h>
#include <signal.h>
#include "vse.h"
#include "process.h"

#define ERRDELAY	2	/* pause two seconds if child is to exit */

#define MAJOR		8	/* major part of name [pqrs] */
#define MINOR		9	/* minor part of name [0..f] */
#define TYPE		5	/* type index, either 'p' or 't' */

struct process	*process_list = NULL;
unsigned long	process_mask[FDM_SIZE];

extern struct sgttyb	save_sgttyb;
extern struct tchars	save_tchars;
extern struct ltchars	save_ltchars;

initprocess()
{
	extern int	children();

	bzero(process_mask, sizeof (process_mask));
	process_list = NULL;

	signal(SIGCHLD, children);

	return (0);
}

start_process(proc, ontty, in, out)
	struct process	*proc;
{
	extern int	errno;
	register int	i;
	char		slave[36];
	int		newd = NTTYDISC, omask;
	int		newproc;

	proc->pr_iochan = -1;
	if (ontty && (proc->pr_iochan = findpty(proc->pr_pty)) < 0)
		error("No more free pseudo ports, no more processes.");

	/* insert this process into the list */
	proc->pr_next = process_list;
	process_list = proc;

	/* v?fork to make the new process */
	if (ontty || in > 0 || out > 0) {
		proc->pr_pid = fork();
		newproc = TRUE;
	} else {
		proc->pr_pid = vfork();
		newproc = FALSE;
	}

	/* the parent/child process switch */
	switch (proc->pr_pid) {
	case -1:	/* error */
		if (errno == EAGAIN)
			error("No more processes (too many already).");
		else if (errno == ENOMEM)
			error("No more swap space for a new process!");
		else
			perror("Can't fork another process");

	default:	/* parent */
		/* don't want to get this just yet */
		omask = sigblock(SIGMASK(SIGCHLD)|SIGMASK(SIGINT));

		/* fill out rest of process struct */
		proc->pr_flags = FLAG_NONE;
		proc->pr_state = STATE_RUNNING;

		if (ontty) {
			/* set pty and child to same process group */
			if (setpgrp(proc->pr_pid, proc->pr_pid) < 0 ||
			    (proc->pr_pgrp = getpgrp(proc->pr_pid)) < 0) {
				sigsetmask(omask);
				close(proc->pr_iochan);
				perror("Can't set/get child process group");
			}
			if (ioctl(proc->pr_iochan, TIOCSPGRP,
				  &proc->pr_pgrp) < 0) {
				sigsetmask(omask);
				close(proc->pr_iochan);
				perror("Can't set pty process group");
			}
			proc->pr_flags |= PROC_ONTTY;
		} else {
			/* no pty so process group is that of parent */
			proc->pr_pgrp = getpgrp(proc->pr_pid);
			proc->pr_iochan = -1;
		}

		/* mark this bit for output checking */
		if (ontty && proc->pr_ochan != NULL)
			FDM_SET(process_mask, proc->pr_iochan);

		/* now it's all in shape */
		sigsetmask(omask);
		return (0);

	case 0:		/* child */
		if (newproc) {
			/* we want to avoid strange situations here */
			subprocess = TRUE;

			/* set signals back to what they should be */
			for (i = 1; i < NSIG; i++)
				signal(i, SIG_DFL);
			/* several signals are too dangerous for a child */
			signal(SIGTTOU, SIG_IGN);
			signal(SIGTSTP, SIG_IGN);
			/* unblock all signals for the child */
			sigsetmask(0);
		}

		if (ontty) {
			/* hook up to slave side of pty */
			strcpy(slave, proc->pr_pty);
			slave[TYPE] = 't';

			/* this better work, no more stdio! */
			ioctl(STDIN, TIOCNOTTY, NULL);
			for (i = getdtablesize() - 1; i >= STDIN; i--)
				close(i);
			if (open(slave, O_RDWR) != STDIN ||
			    dup(0) != STDOUT || dup(1) != STDERR) {
				/* this can't happen! */
				sleep(ERRDELAY);
				exit(1);
			}

			/* set some reasonable default tty modes */
			ioctl(STDIN, TIOCSETP, &save_sgttyb);
			ioctl(STDIN, TIOCSETC, &save_tchars);
			ioctl(STDIN, TIOCSLTC, &save_ltchars);
			/* set new line discipline */
			ioctl(STDIN, TIOCSETD, &newd);
		} else if (newproc) {
			/* re-open standard input as appropriate */
			if (in > 0) {
				dup2(in, STDIN);
			} else {
				close(STDIN);
				open("/dev/null", O_RDWR, 0);
			}

			/* re-open stdout and stderr if appropriate */
			if (out > 0) {
				dup2(out, STDOUT);
				dup2(out, STDERR);
			}
	
			/* close all but standard files */
			for (i = getdtablesize() - 1; i > STDERR; i--)
				close(i);
		}

		/* exec program now, we have full path of program */
		execv(proc->pr_comm, proc->pr_argv);
		perror("Can't execute %s", proc->pr_comm);
		_exit(1);
	}
	/* NOTREACHED */
}

static int
findpty(ptybuf)
	char	ptybuf[];
{
	register char	*maj, *min;
	int		master, status;

	/* find the next free pty */
	strcpy(ptybuf, "/dev/ptyXX");
	for (maj = "pqrs"; *maj != '\0'; maj++) {
		ptybuf[MAJOR] = *maj;
		for (min = "0123456789abcdef"; *min != '\0'; min++) {
			ptybuf[MINOR] = *min;

			/* try to open the master side */
			master = open(ptybuf, O_RDWR, 0);
			if (master >= 0) {
				ptybuf[TYPE] = 't';
				status = access(ptybuf, R_OK|W_OK);
				ptybuf[TYPE] = 'p';
				if (status < 0) {
					/* strange, must be a race */
					close(master);
					continue;
				}
				/* we found one */
				return (master);
			}
		}
	}

	/* none to be had */
	*ptybuf = '\0';
	return (-1);
}

add_process(proc)
	struct process	*proc;
{
	proc->pr_next = process_list;
	process_list = proc;
}

struct process *
find_process(pid)
{
	struct process	*proc;

	for (proc = process_list; proc != NULL; proc = proc->pr_next)
		if (proc->pr_pid == pid)
			return (proc);
	return (NULL);
}

delete_process(proc)
	struct process	*proc;
{
	struct process	*last, *next;
	struct channel	*cin, *cout;
	register int	n;

	last = NULL;
	for (next = process_list; next != NULL; next = next->pr_next) {
		if (next == proc)
			break;
		last = next;
	}
	if (next == NULL)
		return (-1);

	/* unlink this process from the list */
	if (last == NULL)
		process_list = next->pr_next;
	else
		last->pr_next = next->pr_next;

	/* close I/O fd, freeing pty */
	close(next->pr_iochan);
	FDM_CLR(process_mask, proc->pr_iochan);

	/* disconnect any channels */
	if ((cin = next->pr_ichan) != NULL) {
		cin->ch_flags &= ~CHAN_PROCESS;
		cin->ch_process = NULL;
	}
	if ((cout = next->pr_ochan) != NULL) {
		cout->ch_flags &= ~CHAN_PROCESS;
		cout->ch_process = NULL;
	}
	/* free storage of UNIX arguments */
	for (n = 0; n < next->pr_argc; n++)
		vfree(next->pr_argv[n]);
	vfree(next->pr_argv);
	vfree(next);

	return (0);
}
