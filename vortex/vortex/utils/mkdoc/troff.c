/* 
 * Copyright (c) 1986 The Regents of the University of California.
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
 *  VorTeX -- Visually ORiented TeX
 *
 *  Peehong Chen, John Coker, Jeff McCarrell, Steve Procter
 *  for Prof. Michael Harrison of the Computer Science Division
 *  University of California, Berkeley
 *
 *  mkdoc: prepare documentation from lisp and C source files
 *
 *  troff.c - produce a troff source file
 */

#include "mkdoc.h"

/*
 *  Produce troff source for typesetting this (struct docstr)
 *  as part of a typeset manual.
 */

puttroff(docp, filep)
	struct docstr	*docp;
	FILE		*filep;
{
	register int	i;

	/* spit out name, this must be present */
	putc('@', filep);
	troffdoc(docp->ds_name, COPYWORD, filep);
	putc('\n', filep);

	/* spit out calling sequence */
	if (docp->ds_call != NULL)
		troffdoc(docp->ds_call, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out return value stuff */
	if (docp->ds_retu != NULL)
		troffdoc(docp->ds_retu, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out description, this must be present */
	troffdoc(docp->ds_desc, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out side efects for ASCII terminal */
	if (docp->ds_side != NULL) {
		troffdoc(docp->ds_side, COPYTEXT, filep);
		putc('\n', filep);
	}

	/* spit out possible errors for ASCII terminal */
	if (docp->ds_errs != NULL) {
		troffdoc(docp->ds_errs, COPYTEXT, filep);
		putc('\n', filep);
	}

	/* spit out elements of see also */
	for (i = 0; i < MAXSEEA && docp->ds_seea[i] != NULL; i++) {
		if (i > 0)
			putc('\005', filep);
		troffdoc(docp->ds_seea[i], COPYWORD, filep);
	}
	putc('\n', filep);

	return (0);
}

struct fmtcmd	troff_cmds[] = {
	{ "sym",	"\\fB",		"\\fP",		COPYWORD },
	{ "lit",	"",		"",		COPYTEXT },
	{ "em",		"\\fI",		"\\fP",		COPYWORD },
	{ "tab",	"\n.nf\n",	"\n.fi\n",	COPYTEXT },
	{ "fn",		"\n.fs\n",	"\n.fe\n",	COPYTEXT },
	{ "sc",		"\\s-2",	"\\s+2",	COPYWORD },
	{ NULL,		"",		"",		COPYTEXT },
};

struct xlate	troff_xlate[] = {
	{ '\0',		0,		NULL },
};

troffdoc(docstr, init, filep)
	char	*docstr;
	FILE	*filep;
{
	return markup_format(docstr, troff_cmds, troff_xlate, init, filep);
}
