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
 *  RCS Info: $Header: process.h,v 0.1 87/04/30 20:54:44 john Locked $
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
 *  process.h - vLisp process managment data structures
 */
 
#ifndef _PROCESS_
#define _PROCESS_

#include <sys/param.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include "channel.h"

struct process {
	short		pr_pid;		/* process id from kernel */
	short		pr_flags;	/* flags of process */
	short		pr_pgrp;	/* process group (job control) */
	short		pr_state;	/* run/stop/exit status */
	short		pr_exit;	/* exit state (e.g. ``exit(n)'') */
	short		pr_signal;	/* signal that hurt(?) us */
	char		pr_pty[20];	/* pathname of pseudo tty */
	int		pr_iochan;	/* pty I/O descriptor */
	struct channel	*pr_ichan;	/* input channel to process */
	struct channel	*pr_ochan;	/* output channel from process */
	struct symbol	*pr_notify;	/* lisp function name be called */
	char		pr_comm[1024];	/* full pathname of program */
	int		pr_argc;	/* argument count for process */
	char		**pr_argv;	/* argument list for process */
	struct process	*pr_next;	/* next process in list */
};

#define PROC_CORE	(1 << 0)	/* process dumped core */
#define PROC_REPORT	(1 << 1)	/* process has been reported */
#define PROC_ONTTY	(1 << 2)	/* running on a pseudo-tty */

#define STATE_RUNNING	1		/* process is off and running */
#define STATE_STOPPED	2		/* process is stopped somehow */
#define STATE_EXITED	3		/* process exited */
#define STATE_SIGNAL	4		/* process died from a signal */

#ifndef STDIN
#define STDIN	0
#define STDOUT	1
#define STDERR	2
#endif !STDIN

extern struct process	*process_list;
extern unsigned long	process_mask[];
extern struct process	*find_process();

#endif !_PROCESS_
