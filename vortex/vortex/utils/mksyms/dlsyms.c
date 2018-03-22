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
 *  RCS Info: $Header: dlsyms.c,v 0.1 87/02/22 01:17:35 john Locked $
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
 *  dlsyms.c - delete symbols from data base
 */
 
#include "syms.h"

char	*program;
char	USAGE[] = "usage: %s [ -v ] [ -d database ] symbol ...\n";

int	verbose = 0;		/* print debugging information */
char	*dbfile = "SYMS";	/* name of database (no ".dir" or ".pag") */

main(argc, argv)
	char	*argv[];
{
	extern char	*rindex();
	register char	*ap;
	int		ret, status, nfound;
	char		dfile[1024], pfile[1024];
	DBM		*dbmp;

	/* get program name from argv[0] */
	program = rindex(*argv, '/');
	if (program == NULL)
		program = *argv;
	else
		program++;

	/* handle command line options */
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
			case 'v':	/* verbose mode */
				verbose = 1;
				break;
			default:	/* bad option */
				fprintf(stderr, USAGE, program);
				exit(1);
			}
	}

	/* open dbm database for writing */
	if (dbfile == NULL || *dbfile == '\0') {
		error("Bad database file name given!");
		exit(1);
	}
	sprintf(dfile, "%s.dir", dbfile);
	sprintf(pfile, "%s.pag", dbfile);
	if ((dbmp = dbm_open(dbfile, O_RDWR, DBMODE)) == NULL) {
		fprintf(stderr, "%s: Can't write to database ", program);
		perror(dbfile);
		exit(1);
	}

	if (argc < 1) {
		fprintf(stderr, USAGE, program);
		exit(1);
	}
	status = nfound = 0;
	while (argc > 0 && *argv != NULL) {
		ret = deletesym(*argv, dbmp);
		if (ret < 0)
			status++;
		else if (ret == 0)
			nfound++;
		argc--; argv++;
	}

	if (verbose)
		error("Deleted %d symbols from \"%s\".", nfound, dbfile);

	exit(status);
}

deletesym(name, dbmp)
	char	*name;
	DBM	*dbmp;
{
	datum		key, content;
	int		status;
	struct entry	*entp;

	/* set up key to be symbol name */
	key.dptr = name;
	key.dsize = strlen(name);

	/* check for existing key with same name */
	content = dbm_fetch(dbmp, key);
	entp = (struct entry *)content.dptr;
	if (entp == NULL)
		return (1);

	/* delete this entry by lisp symbol name */
	status = dbm_delete(dbmp, key);
	if (status < 0) {
		error("Delete of key from database failed!");
		return (-1);
	}

	if (verbose) {
#ifdef FILENAMES
		error("Deleted \"%s\" (type %d) from file \"%s\".",
		      entp->en_name, entp->en_what, entp->en_sfile);
#else !FILENAMES
		error("Deleted \"%s\" (type %d).",
		      entp->en_name, entp->en_what);
#endif FILENAMES
	}
	return (0);
}
