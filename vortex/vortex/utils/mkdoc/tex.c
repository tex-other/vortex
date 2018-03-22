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
 *  tex.c - produce a TeX output file
 */

#include "mkdoc.h"

/*
 *  Spit out TeX source to format the documentation for this
 *  entry.  We basically just turn all the fields of the
 *  (struct docstr) into macros, except that we ignore the
 *  cross-references.  A special set of TeX macros will be
 *  used to format these fields appropriately.  The function
 *  texdoc() takes a string (like the ds_desc or ds_side)
 *  and handles TeX special characters and the generic markup
 *  commands.  We also call texdoc() when we spit out the contents
 *  of the other fields so that it can deal with funny characters
 *  that may appear.
 */
#define ismagic(c)	((c) == '(' || (c) == ')' || (c) == '[' || (c) == ']')

puttex(docp, filep)
	struct docstr	*docp;
	register FILE	*filep;
{
	char		token[1024];
	register int	i;
	register char	*cp, *tp;
	int		found = 0, isname = 0;

	/* spit out name, this must be present */
	fputs("\n\\docstr{", filep);
	texdoc(docp->ds_name, COPYWORD, filep);
	fputs("}\n", filep);

	/* spit out calling sequence, if given */
	if (docp->ds_call != NULL) {
		for (cp = docp->ds_call; isspace(*cp) || *cp == '('; cp++)
			;
		fputs("\\call{", filep);
		while (*cp != '\0') {
			if (isspace(*cp)) {
				putc(' ', filep);
				while (isspace(*cp))
					cp++;
			}
			if (ismagic(*cp)) {
				putc(*cp, filep);
				continue;
			}

			for (tp = token; *cp != '\0' && !isspace(*cp); tp++)
				*tp = *cp++;
			*tp = '\0';
			if (!found && !strcmp(token, docp->ds_name)) {
				isname = 1;
				found = 1;
				fputs("{\\bf ", filep);
			}
			/* output the collected token */
			for (tp = token; *tp != '\0'; tp++) {
				if (*tp == '-')
					putc(*tp, filep);
				if (*tp == '\'')
					fputs("\\quote ", filep);
				putc(*tp, filep);
			}
			if (isname) {
				putc('}', filep);
				isname = 0;
			}
		}
		fputs("}\n", filep);
	}

	/* spit out return value, if given */
	if (docp->ds_retu != NULL) {
		fputs("\\retu{", filep);
		texdoc(docp->ds_retu, COPYWORD, filep);
		fputs("}\n", filep);
	}

	/* spit out description, this must be rpesent */
	fputs("\\desc{", filep);
	texdoc(docp->ds_desc, COPYTEXT, filep);
	fputs("}\n", filep);

	/* spit out side effects for, if given */
	if (docp->ds_side != NULL) {
		fputs("\\side{", filep);
		texdoc(docp->ds_side, COPYTEXT, filep);
		fputs("}\n", filep);
	}

	/* spit out possible errors for, if given */
	if (docp->ds_errs != NULL) {
		fputs("\\errs{", filep);
		texdoc(docp->ds_errs, COPYTEXT, filep);
		fputs("}\n", filep);
	}

	/* spit out ``See also'' list, if appropriate */
	if (docp->ds_seea[0] != NULL) {
		fputs("\\seea{", filep);
		for (i = 0; i < MAXSEEA && docp->ds_seea[i] != NULL; i++) {
			fputs("{\\it ", filep);
			texdoc(docp->ds_seea[i], COPYWORD, filep);
			putc('}', filep);
			if (i < MAXSEEA - 1 && docp->ds_seea[i+1] != NULL)
				fputs(", ", filep);
		}
		fputs("}\n", filep);
	}

	return (0);
}

/*
 *  Here is the function which generates output for TeX
 *  to typeset.  We call the same base function to
 *  do the device independant formatting, markup_format,
 *  passing it our list of TeX markup commands and list of
 *  translations which do most of the formatting work here.
 *  All the characters which are special to TeX need to be
 *  escaped with a backslash (the document macros will make
 *  sure all those control sequences are defined).  We
 *  need to translate all the TeX special characters into
 *  escaped forms.  And, when outputting a control sequence,
 *  we need to translate single quotes into \quote macro calls.
 *  This is done by using the SPECIAL1 environment.
 */
struct fmtcmd	tex_cmds[] = {
	{ "sym",	"\\sym{",		"}",		COPYWORD },
	{ "lit",	"\\lit{",		"}",		VERBATIM },
	{ "em",		"\\em{",		"}",		COPYWORD },
	{ "tab",	"\\tab{\\tbnl ",	"\\cr}",	TABULAR },
	{ "fn",		"\\fn{",		"}",		COPYTEXT },
	{ "sc",		"\\sc{",		"}",		COPYWORD },
	{ "tex",	"{\\TeX}",		"",		COPYTEXT },
	{ "vortex",	"{\\VorTeX}",		"",		COPYTEXT },
	{ "PS",		"{\\PS}",		"",		COPYTEXT },
	{ "unix",	"{\\unix}",		"",		COPYTEXT },
	{ NULL,		"",			"",		COPYTEXT },
};

struct xlate	tex_xlate[] = {
	{ '&',		ALLACTS,		"\\&" },
	{ '#',		ALLACTS,		"\\#" },
	{ '$',		ALLACTS,		"\\$" },
	{ '^',		ALLACTS,		"\\^" },
	{ '~',		ALLACTS,		"\\~" },
	{ '_',		ALLACTS,		"\\_" },
	{ '%',		ALLACTS,		"\\%" },
	{ '\\',		ALLACTS,		"\\\\" },
	{ '{',		ALLACTS,		"\\{" },
	{ '}',		ALLACTS,		"\\}" },
	{ '\t',		TABULAR,		"\\tbcol " },
	{ '\n',		TABULAR,		"\\cr\n\\tbnl " },
	{ '-',		COPYWORD,		"--" },
	{ '<',		ALLACTS,		"$<$" },
	{ '>',		ALLACTS,		"$>$" },
	{ '\'',		SPECIAL1,		"\\quote " },
	{ ' ',		VERBATIM,		"\\space " },
	{ '\0',		0,			NULL },
};

texdoc(docstr, init, filep)
	char	*docstr;
	FILE	*filep;
{
	return markup_format(docstr, tex_cmds, tex_xlate, init, filep);
}
