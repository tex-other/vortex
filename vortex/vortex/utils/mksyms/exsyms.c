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
 *  RCS Info: $Header: exsyms.c,v 0.1 87/02/22 01:18:00 john Locked $
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
 *  exsyms.c - extract symbols from C files into data base
 */
 
#include "syms.h"

char	*program;
char	USAGE[] = "usage: %s [ -cv ] [ -d database ] [ source ]\n";

int	verbose = 0;		/* print debugging information */
int	recreate = 0;		/* recreate database from scratch */
char	*dbfile = "SYMS";	/* name of database (no ".dir" or ".pag") */

char	VALUEMAC[] = "DEFVALUE";
char	SYMBOLMAC[] = "DEFSYMBOL";
char	STRINGMAC[] = "DEFSTRING";

main(argc, argv)
	char	*argv[];
{
	extern char	*rindex();
	register char	*ap;
	int		oflags, nfound;
	char		dfile[1024], pfile[1024];
	DBM		*dbmp;
	FILE		*infp;

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
			case 'c':	/* create database */
				recreate = 1;
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
	if (recreate || access(dfile, F_OK) != 0 || access(pfile, F_OK) != 0) {
		/* re-create database from scratch */
		(void)unlink(dfile);
		(void)unlink(pfile);
		if (verbose) {
			error("Creating database \"%s\" (%s and %s)%s.",
			    dbfile, dfile, pfile, recreate ? " anew" : "");
		}
		oflags = O_CREAT|O_RDWR;
	} else {
		/* open existing database for writing */
		oflags = O_RDWR;
	}
	if ((dbmp = dbm_open(dbfile, oflags, DBMODE)) == NULL) {
		fprintf(stderr, "%s: Can't write to database ", program);
		perror(dbfile);
		exit(1);
	}

	switch (argc) {
	case 0:		/* process standard input */
		errfile = "(stdin)";
		errline = 1;
		nfound = findsyms(stdin, dbmp);
		break;
	case 1:		/* process give file name */
		if ((infp = fopen(*argv, "r")) == NULL) {
			fprintf(stderr, "%s: Can't open ", program);
			perror(*argv);
			nfound = -1;
		} else {
			/* extract symbols from this file */
			errfile = *argv;
			errline = 1;
			nfound = findsyms(infp, dbmp);
			close(infp);
		}
		break;
	default:	/* too many arguments */
		fprintf(stderr, USAGE, program);
		exit(1);
	}
	errfile = NULL;
	errline = -1;

	if (verbose && nfound == 0)
		error("Found no %s/%s references at all.",
		    VALUEMAC, SYMBOLMAC);
	else if (verbose && nfound > 0)
		error("Found %d %s/%s references.",
		    nfound, VALUEMAC, SYMBOLMAC);

	exit(nfound < 0);
}

/*
 *  Scan through the C source file looking for macro calls of
 *  the form ``DEFVALUE(C-symbol, vlisp-type, C-symbol)'' or
 *  ``DEFSYMBOL("vlisp-name", vlisp-flags, C-symbol)'' or
 *  ``DEFSTRING(C-symbol, string, length)''.  We don't
 *  interpret any of these fields, just copy them out
 *  into the entry struct which is inserted into the database.
 *  We read the C source code in by lines, which must not be
 *  longer than 8k each.  If a line overflows, we fail and
 *  complain bitterly.  This is actually a feature--real C
 *  source code written by a reasonable person shouldn't have
 *  lines anywhere near as long as 8k bytes.  We're also a
 *  filter for the source, we insert ``/** /'' befor all lines
 *  beginning with ``# '' in the input and remove the DEFSYMBOL
 *  macro calls.  This filtered output goes to stdout.
 */
#define MACROCHAR	'#'
#define STRINGCHAR	'"'
#define ESCAPECHAR	'\\'
#define OPENPAREN	'('
#define CLOSEPAREN	')'

static char	NULLCOMMENT[] = "/**/";

findsyms(infp, dbmp)
	register FILE	*infp;
	DBM		*dbmp;
{
	struct entry	entb;
	datum		key, content;
	char		lbuf[8*1024], *lend = lbuf + sizeof (lbuf) - 1;
	register char	*lp;
	register int	next, last, depth;
	int		fcount = 0;
	register FILE	*outfp = stdout;

	last = '\n';
	next = getc(infp);
	while (next != EOF) {
		if (last == '\n' && next == MACROCHAR) {
			/* collect line, this is a cpp line */
			lp = lbuf;
			while (next != '\n' && next != EOF && lp < lend) {
				*lp++ = next;
				last = next;
				next = getc(infp);
			}
			*lp = '\0';
			if (next == EOF)
				goto badeof;
			if (next != '\n')
				goto toolong;

			if (lbuf[1] == ' ') {
				/* this is a ``# line file'' directive */
				geterrpos(lbuf);
				fputs(NULLCOMMENT, outfp);
				/* these lines don't really exist */
				errline--;
			}

			/* output original source line */
			fputs(lbuf, outfp);
			putc('\n', outfp);

			/* advance past newline, this line isn't counted */
			errline++;
			last = next;
			next = getc(infp);
		} else if (isspace(last) &&
			   (next == *SYMBOLMAC || next == *VALUEMAC ||
			    next == *STRINGMAC)) {
			/* collect macro name */
			lp = lbuf;
			while (next != EOF && isupper(next) && lp < lend) {
				*lp++ = next;
				last = next;
				next = getc(infp);
			}
			*lp = '\0';
			if (next == EOF)
				goto badeof;
			if (!strcmp(lbuf, VALUEMAC) && next == OPENPAREN) {
				entb.en_what = ENT_VALUE;
			} else if (!strcmp(lbuf, SYMBOLMAC) &&
				   next == OPENPAREN) {
				entb.en_what = ENT_SYMBOL;
			} else if (!strcmp(lbuf, STRINGMAC) &&
				   next == OPENPAREN) {
				entb.en_what = ENT_STRING;
			} else {
				/* spit it back out, this isn't one */
				fputs(lbuf, outfp);
				continue;
			}
			/* now read in the entry itself */
			depth = -1;
			while ((next != CLOSEPAREN || depth > 0) &&
			       next != EOF && lp < lend) {
				/* check for special characters */
				if (next == OPENPAREN)
					depth++;
				else if (next == CLOSEPAREN)
					depth--;
				/* assign and read next character */
				*lp++ = next;
				last = next;
				next = getc(infp);
			}
			if (next == EOF)
				goto badeof;
			if (next != CLOSEPAREN)
				goto toolong;

			/* add trailing close paren and move on */
			*lp++ = next;
			*lp = '\0';
			last = next;
			next = getc(infp);

			/* extract entry from macro call */
			if (getentry(lbuf, &entb) != 0)
				return (-1);
			if (putentry(&entb, dbmp) != 0)
				return (-1);
			fcount++;
			/* next and last are okay where they are */
		} else {
			if (next == '\n' || next == '\r')
				errline++;
			putc(next, outfp);
			last = next;
			next = getc(infp);
		}
	}
	return (fcount);

badeof:
	error("Premature end-of-file in C source code.");
	return (-1);
toolong:
	error("Input line too long, maximum is %d chars.", sizeof (lbuf));
	return (-1);
}

/*
 *  Scan the entry name from the given string.  It must all be
 *  here, we've already separated it out from the source file.
 *  Each entry should look like:
 *
 *	DEFVALUE(name, flags, value)
 *
 *  or
 *
 *	DEFSYMBOL("name", flags, value)
 *
 *  or
 *
 *	DEFSTRING(name, flags, value)
 *
 *  where name is any text (hopefully without spaces, though),
 *  value is a C identifier, and flags is one or more numbers
 *  separated by pipe signs.
 *
 *  We store these three fields into the current (struct entry)
 *  and return an indication of success.  The current filename
 *  and line number will be inserted later into the entry.  We
 *  also assign the appropriate entry type to en_what above.
 */

#define iscname(c)	(isalnum(c) || (c) == '_')
#define islname(c)	((c) != '\0' && (c) != STRINGCHAR)
#define isflags(c)	((c) != '\0' && (c) != ',')
#define isargsep(c)	((c) == ',')

getentry(text, entp)
	char		*text;
	struct entry	*entp;
{
	char		buf[1024], *end = buf + sizeof (buf) - 1;
	register char	*tp, *bp, *cp;
	register int	inquote, level, length;

	/* skip macro name, open paren, and leading spaces */
	if (entp->en_what == ENT_VALUE)
		tp = text + sizeof (VALUEMAC);
	else if (entp->en_what == ENT_STRING)
		tp = text + sizeof (STRINGMAC);
	else
		tp = text + sizeof (SYMBOLMAC);
	while (isspace(*tp))
		tp++;

	if (entp->en_what == ENT_SYMBOL) {
		/* scan lisp name */
		if (*tp++ != STRINGCHAR) {
			error("Expected string lisp name as first argument!");
			return (-1);
		}
		bp = buf;
		while (islname(*tp) && bp < end) {
			if (*tp == ESCAPECHAR && tp[1] == STRINGCHAR)
				*bp++ = *tp++;
			*bp++ = *tp++;
		}
		*bp = '\0';
		if (*buf == '\0' || *tp++ != STRINGCHAR) {
			error("Malformed string name for first argument!");
			return (-1);
		}
	} else {
		/* scan C identifier */
		bp = buf;
		if (!iscname(*tp)) {
			error("Expected C identifier as first argument!");
			return (-1);
		}
		while (iscname(*tp) && bp < end)
			*bp++ = *tp++;
		*bp = '\0';
		if (*buf == '\0' || !isargsep(*tp)) {
			error("Malformed C identifier for first argument!");
			return (-1);
		}
	}
	if (bp - buf >= sizeof (entp->en_name)) {
		error("Name field (first argument) is too long!");
		return (-1);
	}
	strncpy(entp->en_name, buf, sizeof (entp->en_name));
	entp->en_name[sizeof (entp->en_name) - 1] = '\0';

	/* scan flags (length) string */
	while (isargsep(*tp) || isspace(*tp))
		tp++;
	bp = buf;
	level = 0;
	while ((!isargsep(*tp) || level) && bp < end) {
		switch (*tp) {
		case ESCAPECHAR:
			*bp++ = *tp++;
			break;
		case STRINGCHAR:
			level = !level;
			break;
		}
		*bp++ = *tp++;
	}
	*bp = '\0';
	if (*buf == '\0' || !isargsep(*tp)) {
		error("Malformed flags/length value for second argument!");
		return (-1);
	}
	if (entp->en_what == ENT_STRING && !strncmp(buf, "sizeof", 6)) {
		/* simplify the expression */
		for (cp = buf; *cp != '\0' && *cp != STRINGCHAR; cp++)
			;
		if (*cp == STRINGCHAR) {
			cp++;
			length = 0;
			while (*cp != '\0' && *cp != STRINGCHAR) {
				switch (*cp) {
				case ESCAPECHAR:
					cp++;
					if (isdigit(*cp)) {
						cp++;
						if (isdigit(*cp))
							cp++;
						if (isdigit(*cp))
							cp++;
					}
					break;
				}
				length++;
				cp++;
			}
			if (*cp == STRINGCHAR) {
				sprintf(buf, "%d", length);
				for (bp = buf; *bp != '\0'; bp++)
					;
			}
		}
	}
	if (bp - buf >= sizeof (entp->en_flags)) {
		error("Flags field (second argument) is too long!");
		return (-1);
	}
	strncpy(entp->en_flags, buf, sizeof (entp->en_flags));
	entp->en_flags[sizeof (entp->en_flags) - 1] = '\0';

	/* scan value (string or identifier) argument */
	while (isargsep(*tp) || isspace(*tp))
		tp++;
	if (entp->en_what == ENT_STRING) {
		/* scan C string value */
		if (*tp++ != STRINGCHAR) {
			error("Expected string value as third argument!");
			return (-1);
		}
		bp = buf;
		while (islname(*tp) && bp < end) {
			if (*tp == ESCAPECHAR && tp[1] == STRINGCHAR)
				*bp++ = *tp++;
			*bp++ = *tp++;
		}
		*bp = '\0';
		if (*tp != STRINGCHAR) {
			error("Malformed string value for third argument!");
			return (-1);
		}
	} else {
		/* scan C identifier */
		bp = buf;
		if (!iscname(*tp)) {
			error("Expected C identifier as third argument!");
			return (-1);
		}
		while (iscname(*tp) && bp < end)
			*bp++ = *tp++;
		*bp = '\0';
		if (*buf == '\0' || *tp != CLOSEPAREN) {
			error("Malformed C identifier for third argument!");
			return (-1);
		}
	}
	if (bp - buf >= sizeof (entp->en_value)) {
		error("Value field (third argument) is too long!");
		return (-1);
	}
	strncpy(entp->en_value, buf, sizeof (entp->en_value));
	entp->en_value[sizeof (entp->en_value) - 1] = '\0';

	if (verbose) {
		error("Read %s entry { \"%s\", %s, %s }.",
		      entp->en_what == ENT_VALUE ? VALUEMAC :
			entp->en_what == ENT_SYMBOL ? SYMBOLMAC : STRINGMAC,
		    entp->en_name, entp->en_flags, entp->en_value);
	}
	return (0);
}

putentry(entp, dbmp)
	struct entry	*entp;
	DBM		*dbmp;
{
	datum		key, content;
	struct entry	*old;
	int		status = 0;
	int		modified = 0;

#ifdef FILENAMES
	/* set up current file name, line number */
	strncpy(entp->en_sfile, errfile, sizeof (entp->en_sfile));
#endif FILENAMES
	entp->en_externed = 0;

	/* set up key to be lisp symbol name */
	key.dptr = entp->en_name;
	key.dsize = strlen(entp->en_name);

	/* check for existing key with same name */
	content = dbm_fetch(dbmp, key);
	old = (struct entry *)content.dptr;

	if (old == NULL) {
		modified = 1;
	} else {
		modified = 0;
#ifdef FILENAMES
		if (strncmp(old->en_sfile, entp->en_sfile,
			     sizeof (entp->en_sfile))) {
			if (verbose)
				error("Re-defining \"%s\" from \"%s\".",
				      old->en_name, old->en_sfile);
			modified = 1;
		}
#endif FILENAMES
		if (entp->en_what != old->en_what) {
			if (verbose)
				error("Changing type of \"%s\" to %d.",
				      entp->en_name, entp->en_what);
			modified = 1;
		}
		if (strncmp(old->en_name, entp->en_name,
			     sizeof (entp->en_name))) {
			if (verbose)
				error("Changing name of \"%s\" from \"%s\".",
				      old->en_name, entp->en_name);
			modified = 1;
		}
		if (strncmp(old->en_value, entp->en_value,
			     sizeof (entp->en_value))) {
			if (verbose)
				error("Changing value of \"%s\" from \"%s\".",
				      old->en_name, old->en_value);
			modified = 1;
		}
	}

	if (modified) {
		/* set up content to be whole (struct entry) */
		content.dptr = (char *)entp;
		content.dsize = sizeof (struct entry);

		/* store away this entry by lisp symbol name */
		status = dbm_store(dbmp, key, content, DBM_REPLACE);
		if (status < 0)
			error("Store of entry into database failed!");
	}
	return (status);
}

/*
 *  Extract the current file name and next line number from the
 *  C preprocessor directive.  It is of the form ``# line "file"'',
 *  where an empty file means standard input.  We update the
 *  errline and errfile variables so that intelligent error messages
 *  can be printed.
 */

geterrpos(line)
	char	*line;
{
	register char	*lp, *bp;
	static char	fname[1024];
	static int	fline;

	for (lp = line + 1; isspace(*lp); lp++)
		;
	if (!isdigit(*lp))
		return (1);
	fline = atoi(lp);
	while (isdigit(*lp) || isspace(*lp))
		lp++;
	if (*lp++ != STRINGCHAR)
		return (1);
	strncpy(fname, lp, sizeof (fname) - 1);
	for (lp = fname; *lp != '"' && *lp != '\0'; lp++)
		;
	*lp = '\0';

	/* assign newly read values if we got anything */
	if (fline > 0)
		errline = fline;
	if (*fname != '\0')
		errfile = fname;
	return (0);
}
