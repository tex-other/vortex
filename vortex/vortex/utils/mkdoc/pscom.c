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
 *  lcom.c - scan a lisp comment for documentation
 */

#include "mkdoc.h"

/*
 *  Scan a PostScript comment for special documentation information.
 *  This comment documentation format is described in the doc
 *  directory in a file called ``doc-format''.  The fields of
 *  this special documentation format are supposed to occur
 *  in the source comment in a certain form.  The main difference
 *  between lisp_comment here and c_comment is the way they handle
 *  searching for the end of a comment.  C comments are much
 *  easier to find the end of, because they have an explicit
 *  comment-end sequence.
 *
 *  To be a candidate for a documentation comment, the PostScript
 *  comment must have the comment-start character (usually
 *  semicolon) appear at the beginning of the line (except for
 *  white space).  The comment character may appear as many times
 *  as necessary, but must appear at least once (otherwise the
 *  comment is terminated.  Since PostScript comments are
 *  line-oriented, a comment is a series of lines that begin
 *  (after white space) with the comment-start character.  Blank
 *  lines or any other text break the comment.
 *
 *  When this routine is called, it should be positioned in the
 *  input so that the first comment-start character is next to
 *  be read (except for white space).  This routine fills out
 *  the passed (struct docstr), if this is a documentation
 *  comment, and returns 0.  If this is not a documentation
 *  comment (does not start with DOC), 1 is returned.  If an
 *  error occurs, -1 is returned.  Special constraints here are
 *  that a line may be no longer than STRBUF characters long and
 *  no field may total more than BIGBUF characters.  This routine
 *  is responsible for gobbling the entire comment, unless there
 *  has been an error.
 */
#define iscomment(c)	((c) == '%')

postscript_comment(filep, docp)
	register FILE	*filep;
	struct docstr	*docp;
{
	extern char	*strsave();
	char		line[STRBUF], data[BIGBUF], field[5];
	char		*lend = line + sizeof (line) - 1;
	char		*dend = data + sizeof (data) - 1;
	register char	*lp, *dp;
	register int	in, found, eoc, ended;

	/* zero (struct docstr) and fill up later */
	bzero(docp, sizeof (struct docstr));

	/* read comment lines looking for a ``DOC'' line */
	for (;;) {
		/* is this still a comment line? */
		for (in = input(); isblank(in); in = input())
			;
		if (!iscomment(in)) {
			unput(in);
			return (1);
		}
		while (isblank(in) || iscomment(in))
			in = input();

		/* read in a line of input */
		lp = line;
		while (in != EOF && in != '\n' && lp < lend) {
			*lp++ = in;
			in = input();
		}
		if (in == EOF)
			goto badeof;
		if (in != '\n')
			goto toolong;
		*lp = '\0';

		/* look for first word, is it "DOC"? */
		if (!strncmp(line, "DOC", 3))
			break;
	}

	/* read documentation until "END" or end-of-comment */
	eoc = ended = FALSE;
	field[0] = field[4] = '\0';
	found = 0;
	dp = data;
	for (;;) {
		/* is this still a comment line? */
		for (in = input(); isblank(in); in = input())
			;
		if (!iscomment(in)) {
			unput(in);
			eoc = TRUE;
			if (*field != '\0')
				goto finish;
			break;
		}
		while (isblank(in) || iscomment(in))
			in = input();

		/* read in a line of input */
		lp = line;
		while (in != EOF && in != '\n' && lp < lend) {
			*lp++ = in;
			in = input();
		}
		if (in == EOF)
			goto badeof;
		if (in != '\n')
			goto toolong;
		*lp++ = in;
		*lp = '\0';

		/* look for first word, is it "END"? */
		if (!strncmp(line, "END", 3)) {
			ended = TRUE;
			goto finish;
		}

		lp = line;
		while (*lp != '\0' && !isspace(*lp) && *lp != ':')
			lp++;
		if (*lp == ':' && *field != '\0') {
finish:			/* this is a new field, handle old one first */
			*dp = '\0';
			/* strip off leading and trailing spaces */
			for (dp--; isspace(*dp); dp--)
				*dp = '\0';
			for (dp = data; isspace(*dp); dp++)
				;
			/* if this is an empty field, skip it */
			if (*dp == '\0' && (eoc || ended))
				break;
			else if (*dp == '\0')
				goto skip;

			/* decide what this field means */
			if (!strcmp(field, "NAME")) {
				docp->ds_name = strsave(dp);
			} else if (!strcmp(field, "CALL")) {
				docp->ds_call = strsave(dp);
			} else if (!strcmp(field, "RETU")) {
				docp->ds_retu = strsave(dp);
			} else if (!strcmp(field, "DESC")) {
				docp->ds_desc = strsave(dp);
			} else if (!strcmp(field, "SIDE")) {
				docp->ds_side = strsave(dp);
			} else if (!strcmp(field, "ERRS")) {
				docp->ds_errs = strsave(dp);
			} else if (!strcmp(field, "XREF")) {
				register char	*bp, *ep;
				register int	nxref = 0;

				bp = dp;
				while (*bp != '\0') {
					for (ep = bp; *ep != '\0' &&
					     !isspace(*ep) && *ep != ','; ep++)
						;
					if (*ep != '\0')
						*ep++ = '\0';
					if (nxref >= MAXXREF) {
						error(
			    "Too many cross reference entries; only %d used.",
						    MAXXREF);
						break;
					}
					docp->ds_xref[nxref++] = strsave(bp);
					for (bp = ep; isspace(*bp) ||
					     *bp == ','; bp++)
						;
				}
				if (nxref < MAXXREF)
					docp->ds_xref[nxref] = NULL;
			} else if (!strcmp(field, "SEEA")) {
				register char	*bp, *ep;
				register int	nseea = 0;

				bp = dp;
				while (*bp != '\0') {
					for (ep = bp; *ep != '\0' &&
					     !isspace(*ep) && *ep != ','; ep++)
						;
					if (*ep != '\0')
						*ep++ = '\0';
					if (nseea >= MAXSEEA) {
						error(
			    "Too many ``see also'' entries; only %d used.",
						    MAXSEEA);
						break;
					}
					docp->ds_seea[nseea++] = strsave(bp);
					for (bp = ep; isspace(*bp) ||
					     *bp == ','; bp++)
						;
				}
				if (nseea < MAXSEEA)
					docp->ds_seea[nseea] = NULL;
			} else {
				error("Bad field type, don't know about `%s'.",
				    field);
				return (-1);
			}
			found++;

			/* if we were just finishing up, stop */
			if (eoc || ended)
				break;
		}

skip:		if (*lp == ':') {
			register char	*cp;

			/* now set up for new field we just saw */
			strncpy(field, line, 4);
			for (cp = field; *cp != '\0'; cp++) {
				if (islower(*cp))
					*cp = toupper(*cp);
			}

			/* copy rest of line as data */
			dp = data;
			while (isspace(*lp) || *lp == ':')
				lp++;
			while (*lp != '\0')
				*dp++ = *lp++;
		} else if (dp < dend) {
			/* this is a continuation line */
			for (lp = line; *lp != '\0' && dp < dend; )
				*dp++ = *lp++;
			if (dp >= dend) {
				error(
			    "Entry too long, maximum %d chars; truncated.",
				    sizeof (data) - 1);
				/* rest of field will be skipped */
			}
		}
	}

	/* make sure mandatory fields are present */
	if (found > 0 &&
	    docp->ds_name == NULL || docp->ds_desc == NULL) {
		error("Must have both NAME and DESC fields; ignored.");
		found = 0;
	}

	/* skip the rest of this comment */
	while (!eoc) {
		/* is this still a comment line? */
		for (in = input(); isblank(in); in = input())
			;
		if (!iscomment(in)) {
			unput(in);
			eoc = TRUE;
		} else {
			/* skip past rest of line in input */
			while ((in = input()) != EOF && in != '\n')
				;
		}
	}

	/* return proper status */
	if (found > 0)
		return (0);
	else
		return (1);

badeof:
	error("EOF encountered inside lisp comment; that's no good!");
	return (-1);

toolong:
	error("Comment line too long, maximum %d chars.", sizeof (line));
	return (-1);
}
