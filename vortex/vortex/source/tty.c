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
 *  RCS Info: $Header: tty.c,v 0.1 87/05/01 12:31:17 john Locked $
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
 *  tty.c - internal UNIX tty (editing characters) routines
 */
static char _ID[] = "@(#)tty.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include "vse.h"

struct sgttyb	cur_sgttyb, save_sgttyb = {
	B9600, B9600, CONTROL(h), CONTROL(u), ECHO
};
struct tchars	cur_tchars, save_tchars = {
	CONTROL(c), CONTROL(\\), CONTROL(q), CONTROL(s), CONTROL(d), -1
};
struct ltchars	cur_ltchars, save_ltchars = {
	CONTROL(z), CONTROL(y), CONTROL(r), CONTROL(o), CONTROL(w), CONTROL(v)
};

getmodes()
{
	int	ttyfd;

	ttyfd = fileno(stdin);
	if (!isatty(ttyfd))
		return (1);

	/* get current terminal characters and so forth */
	ioctl(ttyfd, TIOCGETP, &save_sgttyb);
	ioctl(ttyfd, TIOCGETC, &save_tchars);
	ioctl(ttyfd, TIOCGLTC, &save_ltchars);

	return (0);
}
