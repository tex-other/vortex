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
 *  main.c - main routine and top-level file scanner
 */

#include "mkdoc.h"

char	*program;
char	USAGE[] = "usage: %s -f format [ -v ] [ -c | -l | -p ] [ -o file ] [ file ... ]\n";

#define SRC_UNDEF	0
#define SRC_CCODE	1
#define SRC_LISP	2
#define SRC_POSTSCRIPT	3

int	deftype = SRC_UNDEF;
int	srctype = SRC_UNDEF;
int	asciiout = 1;
char	*header = NULL;
char	*trailer = NULL;
int	verbose = 0;

char	_unputb[512], *_unputp = _unputb;
int	_ch;

/*
 *  One can add a new output type simply be defining an interface
 *  to the template routines and the generic markup formatter.  One
 *  simply needs to add an entry to the table below fo that type
 *  and then define the proceedure to format one (struct docstr)
 *  at a time.
 *
 *  This structure below specifies the name expected when the
 *  particular output format is wanted (with the command line
 *  option -f), the long name of that formatter, a string which
 *  preceeds all entries in the output, a string which is output
 *  after all entries, and a function to format a single entry.
 */
extern int	putdocstr(), puttex(), putascii(), puttroff();

struct outfmt {
	char		*of_name;	/* name to be referenced */
	char		*of_desc;	/* long description string */
	int		(*of_func)();	/* formatter for a single entry */
	char		*of_header;	/* output before any entries */
	char		*of_trailer;	/* output after all entries */
} outfmt_list[] = {
	{ "docstr",	"VorTeX documentation string file",	putdocstr,
		"DOCSTR %s@@\n",
		"@@\n" },
	{ "tex",	"TeX source for typeset manual",	puttex,
		"%% DOCSTR %s\n\\input docmac.tex\n\n\\startdoc\n",
		"\n\\enddoc\n" },
	{ "ascii",	"Ascii text for a printed manual",	putascii,
		NULL,
		NULL },
	{ "troff",	"Troff source for typeset manual",	puttroff,
		NULL,
		NULL },
	{ NULL,	NULL, NULL }
};

main(argc, argv)
	char	*argv[];
{
	extern char	*rindex();
	register char	*ap;
	FILE		*filep, *outfp = stdout;
	int		status;
	long		clock;
	struct docstr	*docp, *docstrs;
	char		*fmtname = NULL;
	struct outfmt	*outfmt = NULL;
	int		islisp, isccode;

	program = rindex(*argv, '/');
	if (program == NULL)
		program = *argv;
	else
		program++;

	while (--argc > 0 && **++argv == '-') {
		for (ap = ++*argv; *ap; ap++)
			switch (*ap) {
			case 'c':	/* force C code */
				deftype = SRC_CCODE;
				break;
			case 'l':	/* force lisp code */
				deftype = SRC_LISP;
				break;
			case 'p':	/* force PostScript code */
				deftype = SRC_POSTSCRIPT;
				break;
			case 'f':	/* specify output formatter */
				if (--argc < 1 || *++argv == NULL) {
					fprintf(stderr, USAGE, program);
					exit(1);
				}
				fmtname = *argv;
				break;
			case 'o':	/* output file */
				if (--argc < 1 || *++argv == NULL) {
					fprintf(stderr, USAGE, program);
					exit(1);
				}
				if ((outfp = fopen(*argv, "w")) == NULL) {
					fprintf(stderr, "%s: Can't create ",
					    program);
					perror(*argv);
					exit(1);
				}
				break;
			case 'v':	/* verbose option */
				verbose++;
				break;
			default:	/* unknown option */
				fprintf(stderr, USAGE, program);
				exit(1);
			}
	}

	/* get the requested output format */
	if (fmtname == NULL || *fmtname == '\0') {
		fprintf(stderr,
		    "%s: Must specify an output format type, try ``-f ?''.\n",
		    program);
		exit(1);
	}
	if (!strcmp(fmtname, "?") || !strcmp(fmtname, "help")) {
		printf("Possible output formats (specified with -f) are:\n");
		for (outfmt = outfmt_list; outfmt->of_name != NULL; outfmt++)
			printf("%8s - %s (use ``-f %s'').\n",
			    outfmt->of_name, outfmt->of_desc, outfmt->of_name);
		exit(0);
	}
	for (outfmt = outfmt_list; outfmt->of_name != NULL; outfmt++)
		if (!strcmp(outfmt->of_name, fmtname))
			break;
	if (outfmt->of_name == NULL) {
		fprintf(stderr, "%s: Unknown output format type \"%s\".\n",
		    program, fmtname);
		exit(1);
	}

	/* spit out header information */
	if (time(&clock) == -1) {
		perror("Can't get the current time");
		exit(1);
	}
	/* headers may depend on ctime's final '\n' */
	if (outfmt->of_header != NULL) {
		fprintf(outfp, outfmt->of_header, ctime(&clock));
		fflush(outfp);
	}

	docstrs = NULL;
	if (argc <= 0) {
		if ((srctype = deftype) == SRC_UNDEF) {
			fprintf(stderr,
		    "%s: Don't know type of code on standard input!\n",
				program);
			exit(1);
		}
		errfile = "(stdin)";
		errline = 1;
		status = scanfile(stdin, &docstrs) != 0;
	} else {
		status = 0;
		while (argc-- > 0) {
			if ((filep = fopen(*argv, "r")) == NULL) {
				fprintf(stderr,
				    "%s: Can't open file ", program);
				perror(*argv);
				status++;
			} else {
				if (deftype != SRC_UNDEF)
					srctype = deftype;
				else if ((*argv)[strlen(*argv) - 1] == 'l')
					srctype = SRC_LISP;
				else if ((*argv)[strlen(*argv) - 1] == 's')
					srctype = SRC_POSTSCRIPT;
				else
					srctype = SRC_CCODE;
				errfile = *argv;
				errline = 1;
				if (verbose) {
					fprintf(stderr,
				    "%s: Crunching %s file \"%s\" ...\n",
						program,
						srctype == SRC_LISP ? "Lisp" :
						    srctype == SRC_POSTSCRIPT ?
							"PostScript" :
						    srctype == SRC_CCODE ? "C" :
						    "unknown",  *argv);
					fflush(stderr);
				}
				status += scanfile(filep, &docstrs) != 0;
				fclose(filep);
			}
			/* advance down list */
			argv++;
		}
	}
	errfile = NULL;
	errline = -1;

	/* spit out the individual doc strings */
	if (verbose) {
		fprintf(stderr, "%s: Formatting documentation ...\n", program);
		fflush(stderr);
	}
	for (docp = docstrs; docp != NULL; docp = docp->ds_next)
		(*outfmt->of_func)(docp, outfp);

	/* spit out trailer information */
	if (outfmt->of_trailer != NULL)
		fputs(outfmt->of_trailer, outfp);

	if (outfp != stdout)
		fclose(outfp);
	exit(status);
}

/*
 *  Scan a C or lisp source file looking for documentation
 *  entries and inserting them into the list in alphabetical
 *  order as they are found.  We expect the global flag csource
 *  to be set TRUE if we should look for C comments and FALSE
 *  if we're searching for lisp comments.  A pointer to the
 *  first of the current list of (struct docstr)s is passed
 *  in, which we modify as appropriate.
 */

scanfile(filep, dlist)
	register FILE	*filep;
	struct docstr	**dlist;
{
	extern char	*alloc();
	register int	this, look;
	struct docstr	*docp, *next, *last;
	register int	found;
	register char	*name;

	start();

	/* allocate the first one to start with */
	docp = (struct docstr *)alloc(sizeof (struct docstr));

	this = input();
	look = input();
	while (this != EOF && look != EOF) {
		found = 0;
		if (srctype == SRC_CCODE && this == '/' && look == '*') {
			/* found a C comment */
			unput(look);
			unput(this);
			if (c_comment(filep, docp) == 0)
				found = 1;
		} else if (srctype == SRC_LISP &&
			   this == '\n' && look == ';') {
			/* found a lisp comment */
			unput(look);
			if (lisp_comment(filep, docp) == 0)
				found = 1;
		} else if (srctype == SRC_POSTSCRIPT &&
			   this == '\n' && look == '%') {
			/* found a lisp comment */
			unput(look);
			if (postscript_comment(filep, docp) == 0)
				found = 1;
		}
		if (found) {
			/*
			 *  We have a linearized, documentation comment.
			 *  Insert it into the list in order.  This could
			 *  be done better, but it's not worth the effort.
			 */
			name = docp->ds_name;
			last = NULL;
			for (next = *dlist; next != NULL; next =next->ds_next){
				if (strcmp(next->ds_name, name) >= 0)
					break;
				last = next;
			}
			if (next != NULL && !strcmp(next->ds_name, name)) {
				error(
		    "Duplicate documentation for \"%s\"; second entry used.",
				      name);
				next = next->ds_next;
			}
			if (last == NULL)
				*dlist = docp;
			else
				last->ds_next = docp;
			docp->ds_next = next;

			/* allocate space for next docstr */
			docp = (struct docstr *)alloc(sizeof (struct docstr));
		}
		this = look;
		look = input();
	}

	return (0);
}
