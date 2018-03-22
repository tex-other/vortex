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
 *  RCS Info: $Header: mksyms.c,v 0.1 87/02/22 01:18:28 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the utility program mksyms which
 *  builds the standard symbols for the source editor of VorTeX,
 *  written by John Coker under Prof. Michael A. Harrison of
 *  the University of California at Berkeley.
 *
 *  (c) 1987  John Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *
 *  All rights reserved by the author.  See the copyright
 *  notice distributed with this software for the complete
 *  description of the conditions under which this software
 *  is made available.
 *
 *  mksyms.c - generate C file from symbol data base
 */
 
#include "syms.h"

char	*program;
char	USAGE[] = "usage: %s [ -v ] [ -d database ] [ -o output ]\n";

int	verbose = 0;		/* print debugging information */
char	*dbfile = "SYMS";	/* name of database (no ".dir" or ".pag") */
char	*srcfile = NULL;	/* name of source file (NULL -> stdout) */

main(argc, argv)
	char	*argv[];
{
	extern char	*rindex();
	register char	*ap;
	int		status;
	DBM		*dbmp;
	FILE		*outfp;
	struct entry	*elist;

	/* extract command name from argv[0] */
	program = rindex(*argv, '/');
	if (program == NULL)
		program = *argv;
	else
		program++;

	/* handle command line options, don't take arguments */
	while (--argc > 0 && **++argv == '-') {
		for (ap = ++*argv; *ap; ap++)
			switch (*ap) {
			case 'd':	/* database file */
				if (--argc < 1 || *++argv == NULL) {
					fprintf(stderr, USAGE, program);
					exit(1);
				}
				dbfile = *argv;
				break;
			case 'o':	/* source file */
				if (--argc < 1 || *++argv == NULL) {
					fprintf(stderr, USAGE, program);
					exit(1);
				}
				srcfile = *argv;
				break;
			case 'v':	/* verbose mode */
				verbose = 1;
				break;
			default:	/* bad option */
				fprintf(stderr, USAGE, program);
				exit(1);
			}
	}
	if (argc > 0) {
		fprintf(stderr, USAGE, program);
		exit(1);
	}

	/* if stdout was explicitly specified */
	if (srcfile != NULL && (*srcfile == '\0' || !strcmp(srcfile, "-")))
		srcfile = NULL;

	/* open dbm database */
	if ((dbmp = dbm_open(dbfile, O_RDONLY, DBMODE)) == NULL) {
		fprintf(stderr, "%s: Can't open database ", program);
		perror(dbfile);
		exit(1);
	}

	/* open output file pointer */
	if (srcfile == NULL) {
		/* use standard output */
		outfp = stdout;
	} else if ((outfp = fopen(srcfile, "w")) == NULL) {
		fprintf(stderr, "%s: Can't create source file ", program);
		perror(srcfile);
		dbm_close(dbmp);
		exit(1);
	}

	/* read all symbols from database and sort them */
	status = getsyms(dbmp, &elist);
	dbm_close(dbmp);

	/* output list of symbols to source file */
	if (status == 0)
		status = putsyms(elist, outfp) != 0;
	fclose(outfp);

	exit(status);
}

/*
 *  We read in all the symbol references from the dbm
 *  database and build our sorted list of entry structs.
 *  we sort the entry structs by the en_cname field, so
 *  that folding multiple pointers to the same symbol will
 *  be easy later on.
 */

getsyms(dbmp, elistp)
	DBM		*dbmp;
	struct entry	**elistp;
{
	struct entry	*next, *last, *new;
	register char	*name;
	datum		key, content;
	register int	nfound = 0;

	for (key = dbm_firstkey(dbmp); key.dptr != NULL;
	     key = dbm_nextkey(dbmp)) {
		/* get the actual data for this key */
		content = dbm_fetch(dbmp, key);
		if (dbm_error(dbmp)) {
			fprintf(stderr,
			    "%s: Read error for content ", program);
			perror(dbfile);
			return (-1);
		}
		nfound++;

		/* copy this entry to safe storage */
		new = (struct entry *)alloc(sizeof (struct entry));
		bcopy(content.dptr, new, sizeof (struct entry));

		/* insertion sort this entry into the list */
		last = NULL;
		name = new->en_name;
		for (next = *elistp; next != NULL; next = next->en_next) {
			if (strcmp(next->en_name, name) >= 0)
				break;
			last = next;
		}
		if (last == NULL)
			*elistp = new;
		else
			last->en_next = new;
		new->en_next = next;
	}
	if (dbm_error(dbmp)) {
		fprintf(stderr, "%s: Read error for key ", program);
		perror(dbfile);
		return (-1);
	} else {
		if (verbose)
			error("Found %d symbol entries in database.", nfound);
		return (0);
	}
}

/*
 *  We dump the sorted list of entry structs as C source
 *  code to compile into an array of symbol structs which
 *  is to be compiled into VorTeX.  At startup time these
 *  symbols will be entered into the symbol table.  We
 *  first need to extern all the (struct value) references.
 *  As we do this, we want to fold multiple references to
 *  same symbols.  Since the list is sorted by these C
 *  names, we can just check to see if the last one was the
 *  same as the current one.
 */
char	FILECOMMENT[] = "/*\n\
 *  VorTeX -- Visually ORiented TeX\n\
 *\n\
 *  Peehong Chen, John Coker, Jeff McCarrell, Steve Procter\n\
 *  for Prof. Michael Harrison of the Computer Science Division\n\
 *  University of California, Berkeley\n\
 *\n\
 *  This file was created automatically from the builtin\n\
 *  symbols extracted from the C source files that make up\n\
 *  VorTeX.  This file is expendable; it will be re-created\n\
 *  every time VorTeX is linked.  Some part of the system should\n\
 *  call the functions builtin_values() and builtin_symbols()\n\
 *  created here during early initialization.\n\
 *\n\
 *  See the exsym/mksym utilities for a description of the\n\
 *  method by which the builtin symbol database is created\n\
 *  and scanned to produce this file, which is linked into\n\
 *  VorTeX to define the basic symbols (builtin functions\n\
 *  and standard variables).\n\
 *\n\
 *  Created: %s\
 */\n";
char	FILEHEADER[] = "\n\
#include \"value.h\"\n\
#include \"symtab.h\"\n";
char	NEWDECLHEADER[] = "\nstruct value";
char	DEXTERNHEADER[] = "\nextern unsigned long";
char	VEXTERNHEADER[] = "\nextern struct value";
char	SEXTERNHEADER[] = "\nextern struct string";
char	EXTERNTRAILER[] = ";\n";
char	VFUNCTHEADER[] = "\nbuiltin_values()\n\
{\n";
char	TFUNCTHEADER[] = "\nbuiltin_strings()\n\
{\n\
\tstruct string	*save_string();\n";
char	SFUNCTHEADER[] = "\nbuiltin_symbols()\n\
{\n\
\tstruct value	fix_init_value();\n\
\tstruct string	*str;\n";
char	FUNCTTRAILER[] = "\n\treturn (0);\n}\n";

#define MAXCOL	70
#define TABWID	8

putsyms(elist, outfp)
	struct entry	*elist;
	register FILE	*outfp;
{
	struct entry	*next;
	register int	col, len, first;
	register char	*name;
	long		clock;

	/* output the source file header */
	if (time(&clock) == -1) {
		fprintf(stderr, "%s: Can't get the time: ", program);
		perror(NULL);
		return (-1);
	}
	fprintf(outfp, FILECOMMENT, ctime(&clock));
	fputs(FILEHEADER, outfp);

	/* output the data extern declarations */
	fputs(DEXTERNHEADER, outfp);
	col = sizeof (DEXTERNHEADER);
	first = 1;
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_VALUE)
			continue;
		next->en_externed = 1;
		if (isdigit(*next->en_value))
			continue;
		if (first) {
			/* no longer the first */
			first = 0;
		} else {
			putc(',', outfp);
			col++;
		}
		if (col + (len = strlen(next->en_value)) > MAXCOL) {
			fputs("\n\t", outfp);
			col = TABWID;
		} else {
			putc(' ', outfp);
			col++;
		}
		fputs(next->en_value, outfp);
		col += len;
	}
	fputs(EXTERNTRAILER, outfp);

	/* output the value definitions */
	fputs(NEWDECLHEADER, outfp);
	col = sizeof (NEWDECLHEADER);
	first = 1;
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_VALUE)
			continue;
		next->en_externed = 1;
		if (first) {
			/* no longer the first */
			first = 0;
		} else {
			putc(',', outfp);
			col++;
		}
		if (col + (len = strlen(next->en_name)) > MAXCOL) {
			fputs("\n\t", outfp);
			col = TABWID;
		} else {
			putc(' ', outfp);
			col++;
		}
		fputs(next->en_name, outfp);
		col += len;
	}
	fputs(EXTERNTRAILER, outfp);

	/* output the string definitions */
	fputs(SEXTERNHEADER, outfp);
	col = sizeof (SEXTERNHEADER);
	first = 1;
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_STRING)
			continue;
		next->en_externed = 1;
		if (first) {
			/* no longer the first */
			first = 0;
		} else {
			putc(',', outfp);
			col++;
		}
		if (col + (len = strlen(next->en_name)) > MAXCOL) {
			fputs("\n\t", outfp);
			col = TABWID;
		} else {
			putc(' ', outfp);
			col++;
		}
		putc('*', outfp);
		fputs(next->en_name, outfp);
		col += len;
	}
	fputs(EXTERNTRAILER, outfp);

	/* output the value extern declarations */
	fputs(VEXTERNHEADER, outfp);
	col = sizeof (VEXTERNHEADER);
	first = 1;
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_SYMBOL || next->en_externed)
			continue;
		if (first) {
			/* no longer the first */
			first = 0;
		} else {
			putc(',', outfp);
			col++;
		}
		if (col + (len = strlen(next->en_value)) > MAXCOL) {
			fputs("\n\t", outfp);
			col = TABWID;
		} else {
			putc(' ', outfp);
			col++;
		}
		fputs(next->en_value, outfp);
		col += len;
	}
	fputs(EXTERNTRAILER, outfp);

	/* output code for initializing builtin strings */
	fputs(TFUNCTHEADER, outfp);
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_STRING)
			continue;
#ifdef FILENAMES
		fprintf(outfp, "\n\t/* source file \"%s\" */\n",
			next->en_sfile);
#endif FILENAMES
		fprintf(outfp, "\t%s = save_string(\"%s\", %s);\n",
			next->en_name, next->en_value, next->en_flags);
		fprintf(outfp, "\t%s->st_perm = 1;\n", next->en_name);
	}
	fputs(FUNCTTRAILER, outfp);

	/* output code for initializing builtin values */
	fputs(VFUNCTHEADER, outfp);
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_VALUE)
			continue;
#ifdef FILENAMES
		fprintf(outfp, "\n\t/* source file \"%s\" */\n",
			next->en_sfile);
#endif FILENAMES
		fprintf(outfp, "\t%s.vl_type = %s;\n",
			next->en_name, next->en_flags);
		fprintf(outfp, "\t%s.vl_data = %s;\n",
			next->en_name, next->en_value);
	}
	fputs(FUNCTTRAILER, outfp);

	/* output code for initializing builtin symbols */
	fputs(SFUNCTHEADER, outfp);
	for (next = elist; next != NULL; next = next->en_next) {
		if (next->en_what != ENT_SYMBOL)
			continue;
#ifdef FILENAMES
		fprintf(outfp, "\n\t/* source file \"%s\" */\n",
			next->en_sfile);
#endif FILENAMES
		fprintf(outfp, "\tstr = save_string(\"%s\", %d);\n",
			next->en_name, strlen(next->en_name));
		fprintf(outfp, "\tsetglobal(str, fix_init_value(%s), %s);\n",
			next->en_value, next->en_flags);
	}
	fputs(FUNCTTRAILER, outfp);

	return (0);
}
