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
 *  ascii.c - produce an ASCII file for printing
 */

#include "mkdoc.h"

/*
 *  Produce this entry as an ASCII file that can be sent to a
 *  line printer.  We embolden things by overstriking and
 *  emphasize by underlining.
 */

putascii(docp, filep)
	struct docstr	*docp;
	FILE		*filep;
{
	extern int	fc_curcol;
	register int	i;

	/* spit out name, this must be present */
	fc_curcol = -1;
	fputs("\n\nName: ", filep);
	asciidoc(docp->ds_name, SPECIAL1, filep);
	putc('\n', filep);

	/* spit out calling sequence */
	if (docp->ds_call != NULL) {
		fputs("Usage: ", filep);
		asciidoc(docp->ds_call, COPYWORD, filep);
		putc('\n', filep);
	}

	/* spit out return value stuff */
	if (docp->ds_retu != NULL) {
		fputs("Returns: ", filep);
		asciidoc(docp->ds_retu, COPYWORD, filep);
		putc('\n', filep);
	}

	/* spit out description, this must be present */
	fputs("Description:\n\t", filep);
	fc_curcol = 8;
	asciidoc(docp->ds_desc, COPYTEXT, filep);
	putc('\n', filep);

	/* spit out side efects for ASCII terminal */
	if (docp->ds_side != NULL) {
		fputs("Side effects:\n\t", filep);
		fc_curcol = 8;
		asciidoc(docp->ds_side, COPYTEXT, filep);
		putc('\n', filep);
	}

	/* spit out possible errors for ASCII terminal */
	if (docp->ds_errs != NULL) {
		fputs("Errors: ", filep);
		fc_curcol = 8;
		asciidoc(docp->ds_errs, COPYTEXT, filep);
		putc('\n', filep);
	}

	/* spit out elements of see also */
	if (docp->ds_seea[0] != NULL) {
		fputs("See Also: ", filep);
		for (i = 0; i < MAXSEEA && docp->ds_seea[i] != NULL; i++) {
			if (i > 0)
				fputs(", ", filep);
			asciidoc(docp->ds_seea[i], COPYWORD, filep);
		}
		putc('\n', filep);
	}
	putc('\n', filep);

	return (0);
}

/*
 *  We format for output to a dumb printer directly.  This
 *  means we can't do most of the fun things we might like to.
 *  However, we can underline and boldface (overstrike) with
 *  the output.  This is about all we can do, though.  We use
 *  two special modes, one for underlining and one for
 *  underscoring.  The translation list gets built up automatically
 *  the first time we enter, and stays around thereafter.  This
 *  is for the sake of efficiency, because there is a significant
 *  amount of overhead in building this table.
 */
struct fmtcmd	ascii_cmds[] = {
	{ "sym",	"",		"",		SPECIAL1 },
	{ "lit",	"",		"",		COPYTEXT },
	{ "em",		"",		"",		SPECIAL2 },
	{ "tab",	"",		"",		TABULAR },
	{ "fn",		"",		"",		DELETE },
	{ "sc",		"",		"",		COPYWORD },
	{ "tex",	"TeX",		"",		COPYWORD },
	{ "vortex",	"VorTeX",	"",		COPYWORD },
	{ "unix",	"UNIX",		"",		COPYWORD },
	{ "PS",		"PostScript",	"",		COPYWORD },
	{ NULL,		"",		"",		COPYTEXT },
};

struct xlate	ascii_xlates[2 * ('~' - ' ' + 1) + 2];
static char	ascii_strbuf[2 * ('~' - ' ' + 1) * 4];
static int	ascii_init = 0;

asciidoc(docstr, init, filep)
	char	*docstr;
	FILE	*filep;
{
	register char	*ptr;
	struct xlate	*xlp;
	int		code;

	if (!ascii_init) {
		/* set pointers to beginning of buffers */
		xlp = ascii_xlates;
		ptr = ascii_strbuf;

		/* set up translations for boldface text */
		for (code = ' '; code <= '~'; code++) {
			/* write into this buffer */
			xlp->xl_char = code;
			xlp->xl_acts = SPECIAL1;
			xlp->xl_repl = ptr;
			ptr[0] = code;
			ptr[1] = '\b';
			ptr[2] = code;
			ptr[3] = '\0';

			/* advance pointers to next buffers */
			xlp++;
			ptr += 4;
		}

		/* set up translations for italicized text */
		for (code = ' '; code <= '~'; code++) {
			/* write into this buffer */
			xlp->xl_char = code;
			xlp->xl_acts = SPECIAL2;
			xlp->xl_repl = ptr;
			ptr[0] = '_';
			ptr[1] = '\b';
			ptr[2] = code;
			ptr[3] = '\0';

			/* advance pointers to next buffers */
			xlp++;
			ptr += 4;
		}

		/* add last translation for indenting text bodies */
		xlp->xl_char = '\n';
		xlp->xl_acts = COPYTEXT|TABULAR|COPYWORD;
		xlp->xl_repl = "\n\t";

		/* terminate translation list */
		xlp++;
		xlp->xl_char = '\0';
	}

	return markup_format(docstr, ascii_cmds, ascii_xlates, init, filep);
}
