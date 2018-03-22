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
 *  RCS Info: $Header$
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
 *  syscall.c - various UNIX system calls for vLisp
 */
static char _ID[] = "@(#)syscall.c for VorTeX, Copyright (c) 1987 John Coker";

#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: gethostname
 *  Call: (gethostname)
 *  Retu: string
 *  Desc: This function returns the official host name of the machine
 *	on which VorTeX is running as known the the internet.  If there
 *	is some error in getting the name from the system, the name
 *	\lit{localhost} is returned.
 *  SeeA: gethostnameonly
 */
char	LOCALHOST[] = "localhost";

DEFUN(dogethostname, "gethostname", FLAG_NONE, NULL)
{
	struct value	ret;
	char		*host, hbuf[128];

	/* get the host name from the system */
	*hbuf = '\0';
	if (gethostname(hbuf, sizeof (hbuf)) < 0 || *hbuf == '\0')
		host = LOCALHOST;
	else
		host = hbuf;

	/* return this as a lisp string */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, save_string(host, strlen(host)));
	return (ret);
}
 
/*
 *  DOCUMENTATION
 *
 *  Name: gethostnameonly
 *  Call: (gethostnameonly)
 *  Retu: string
 *  Desc: This function returns the name of the machine on which VorTeX
 *	is running as known locally (i.e., without the domain portion of
 *	the internet name).  If there is some error in getting the name
 *	from the system, the name \lit{localhost} is returned.
 *  SeeA: gethostname
 */

DEFUN(dogethostnameonly, "gethostnameonly", FLAG_NONE, NULL)
{
	struct value	ret;
	char		*host, hbuf[128];
	register char	*hp;

	/* get the host name from the system */
	*hbuf = '\0';
	if (gethostname(hbuf, sizeof (hbuf)) < 0 || *hbuf == '\0')
		host = LOCALHOST;
	else
		host = hbuf;

	/* strip off the domain name portion */
	for (hp = host; *hp != '\0' && *hp != '.'; hp++)
		;
	if (hp > host && *hp == '.')
		*hp = '\0';

	/* return this as a lisp string */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, save_string(host, strlen(host)));
	return (ret);
}
