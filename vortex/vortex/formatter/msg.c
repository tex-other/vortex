/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifdef VORTEX

/*
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents.
 *
 *  This file is part of the VorTeX proof editor 
 *  written by Jeffrey W. McCarrell for the VorTeX project
 *  under the direction of Prof. Michael A. Harrison
 *  of the University of California at Berkeley.
 *
 *  Copyright (c) 1987 by Jeffrey W. McCarrell
 *  and The Regents of the University of California.
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  jwm@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  Modified by Pehong Chen (phc@berkeley.edu) for the formatter.
 */


/*
 * the message facility.
 */
#include	<stdio.h>
#include	<sys/errno.h>
#include	<sys/types.h>
#include	"msg.h"
#include	"failcodes.h"

/* from the C library */
extern int	errno,
		sys_nerr;
extern char	*sys_errlist[];

extern char	program[];
extern int	show_debug;

extern  char	colon[];

msg(flags, format, args)
	char	*format;
{
	FILE		junk;
	static u_char	mbuf[512],
			fmtbuf[512];
	extern char	log_fname[];
	extern FILE	*log_fp;

	if (format == 0 || *format == '\0')
		return(0);
	
	if ((! (show_debug)) && ((flags & MDBUG) || 
				 (flags & SDBUG) || 
				 (flags & PDBUG))) {
		return(0);
	}

	if (flags & LITERAL) {
		strcpy(fmtbuf, format);
	} else {
		/* set up the stuff for _doprint */
		junk._flag = _IOWRT | _IOSTRG;
		junk._ptr = fmtbuf;
		junk._cnt = sizeof (fmtbuf) - 1;
		junk._file = -1;
		/* print the message onto the string */
		_doprnt(format, &args, &junk);
		putc('\0', &junk);
	}

	if (flags & MDBUG) {
		strcpy(mbuf, "VIFDB: ");
	} else if (flags & SDBUG) {
		strcpy(mbuf, "TSDBG: ");
	} else if (flags & PDBUG) {
		strcpy(mbuf, "TPDBG: ");
	} else if (flags & FATAL || flags & STDERR) {
		strcpy(mbuf, program);
		strcat(mbuf, colon);
	} else {
		mbuf[0] = '\0';
	}

	strcat(mbuf, fmtbuf);

	if (flags & PERROR) {
		if (errno > 0 && errno <= sys_nerr) {
			strcat(mbuf, colon);
			strcat(mbuf, sys_errlist[errno]);
		}
	}

	if (log_fname[0] != '\0') {
		/*
		 * if the filename is non-null, we assume it has been
		 * opened and is ready for writing.
		 */
		fputs(mbuf, log_fp);
		fputc('\n', log_fp);
		fflush(log_fp);
	}

	if (flags & STDERR || flags & PERROR ||
	    flags & MDBUG || flags & SDBUG || flags & PDBUG) {
		fputs(mbuf, stderr);
		fputc('\n', stderr);
		fflush(stderr);
	} else {
		fputs(mbuf, stdout);
		fputc('\n', stdout);
		fflush(stdout);
	}

	if (flags & FATAL) {
		expire(ES_FATALMSG);
	}
	return(0);
}


#endif VORTEX
