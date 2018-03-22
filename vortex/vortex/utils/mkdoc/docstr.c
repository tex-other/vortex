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
 *  docstr.c - produce a documentation string file
 */

#include "mkdoc.h"

/*
 *  Put out the fields of a (struct docstr) as a database entry
 *  in a DOCSTR file, later to be read in by the info system
 *  of VorTeX.  
 */

putdocstr(docp, filep)
	struct docstr	*docp;
	FILE		*filep;
{
	extern int	fc_curcol, fc_maxlength;
	register int	i;

	fc_maxlength = 70;

	/* spit out name, this must be present */
	fc_curcol = -1;
	putc('@', filep);
	docstrdoc(docp->ds_name, COPYWORD, filep);
	putc('\n', filep);

	/* spit out calling sequence */
	if (docp->ds_call != NULL)
		docstrdoc(docp->ds_call, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out return value stuff */
	if (docp->ds_retu != NULL)
		docstrdoc(docp->ds_retu, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out description, this must be present */
	fc_curcol = 0;
	docstrdoc(docp->ds_desc, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out side efects */
	fc_curcol = 0;
	if (docp->ds_side != NULL)
		docstrdoc(docp->ds_side, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out possible errors */
	fc_curcol = 0;
	if (docp->ds_errs != NULL)
		docstrdoc(docp->ds_errs, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out elements of cross reference */
	fc_curcol = -1;
	for (i = 0; i < MAXXREF && docp->ds_xref[i] != NULL; i++) {
		if (i > 0)
			putc('\005', filep);
		docstrdoc(docp->ds_xref[i], COPYWORD, filep);
	}
	putc('\n', filep);

	/* spit out elements of see also */
	for (i = 0; i < MAXSEEA && docp->ds_seea[i] != NULL; i++) {
		if (i > 0)
			putc('\005', filep);
		docstrdoc(docp->ds_seea[i], COPYWORD, filep);
	}
	putc('\n', filep);

	return (0);
}

/*
 *  Here are the formatting command descriptions for use
 *  when producing documentation.  Note that when we go into
 *  an environment, we delimit it with a control sequence
 *  specific to that particular environment and end with
 *  a ^E (coltrol-E, ASCII 5).  The environment specific
 *  introductory delimiters are: ^B for Boldface and ^U for
 *  Underscored.
 *
 *  Note that we also have to translate newlines into backslash-
 *  newline so that it won't end the string prematurely.  And
 *  in tabular mode, we prepend a tab to every line.  The actual
 *  device independant formatting is done by markup_format with
 *  this list of DOCSTR format commands and the newline
 *  translation passed in.
 */
struct fmtcmd	docstr_cmds[] = {
	{ "sym",	"\002",		"\005",		COPYWORD },
	{ "lit",	"",		"",		COPYTEXT },
	{ "em",		"\025",		"\005",		COPYWORD },
	{ "tab",	"\t",		"",		TABULAR },
	{ "fn",		"",		"",		DELETE },
	{ "sc",		"",		"",		COPYWORD },
	{ "tex",	"TeX",		"",		COPYTEXT },
	{ "vortex",	"VorTeX",	"",		COPYTEXT },
	{ "unix",	"UNIX",		"",		COPYTEXT },
	{ "PS",		"PostScript",	"",		COPYTEXT },
	{ NULL,		"",		"",		COPYTEXT },
};

struct xlate	docstr_xlate[] = {
	{ '\n',		TABULAR,	"\\\n\t" },
	{ '\n',		ALLACTS,	"\\\n" },
	{ '\0',		0,		NULL },
};

docstrdoc(docstr, init, filep)
	char	*docstr;
	FILE	*filep;
{
	return markup_format(docstr, docstr_cmds, docstr_xlate, init, filep);
}
