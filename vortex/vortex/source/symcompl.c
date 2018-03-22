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
 *  RCS Info: $Header: symcompl.c,v 0.1 87/05/01 12:29:06 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the source editor/user interface written
 *  by John Coker for the VorTeX project under the direction of
 *  Prof. Michael A. Harrison of the University of California at
 *  Berkeley.
 *
 *  Copyright (c) 1987 John L. Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  John Coker and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  symcompl.c - internal symbol completion list routines
 */
static char _ID[] = "@(#)symcompl.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "symtab.h"
#include "function.h"

extern int	symtab_changed;

/*
 *  Make a sorted list of all the current buffers and return it for
 *  completion purposes.  We make one optimization here, we don't
 *  rebuild the list until the buffer list changes somehow.  This
 *  means that the above routines must set buflist_changed when they
 *  change the buffer list.
 */
static int
compare(sp1, sp2)
	char	**sp1, **sp2;
{
	register char	*p1, *p2;

	ASSERT(sp1 != NULL && (p1 = *sp1) != NULL);
	ASSERT(sp1 != NULL && (p2 = *sp2) != NULL);

	while (*p1 != '\0' && *p2 != '\0' && *p1 == *p2)
		p1++, p2++;
	return (*p1 - *p2);
}

/*
 *  For the list of symbol names, we keep a table of strings and
 *  a buffer of character storage for list storage.  This means
 *  that if there are more than MAXSTRINGS symbols and/or their
 *  names add up to more than MAXSTRLEN characters (terminating
 *  '\0' characters are also stored), not all the symbol names
 *  will appear in the list.
 */
#define MAXSTRINGS	2000
#define MAXSTRLEN	20840

char **
match_symbols(partial)
	char	*partial;
{
	static char	*strings[MAXSTRINGS + 1];
	static char	strbuf[MAXSTRLEN];
	static char	*matches[MAXSTRINGS+1];
	register int	ind, s, slen;
	struct symtab	*next;
	struct string	*name;
	register char	*sbuf, *send;
	register char	**str;
	register int	m, len;

	/* make sure the list isn't out-of-date */
	if (strings[0] != NULL && symtab_changed)
		strings[0] = NULL;
	symtab_changed = FALSE;

	if (strings[0] == NULL) {
		PROTECT();
		/* zero out the string table for safety */
		bzero(strings, sizeof (strings));

		/* set up string buffer pointers */
		sbuf = strbuf;
		send = strbuf + sizeof (strbuf);

		/* make up the new list of buffer names */
		s = 0;
		for (ind = 0; ind < sym_tabsize; ind++) {
			for (next = sym_table[ind]; next != NULL;
			     next = next->st_next) {
				if (s >= MAXSTRINGS)
					goto done;
				name = next->st_name;
				if (name != NULL && name->st_length > 0) {
					slen = name->st_length + 1;
					if (sbuf + slen >= send)
						goto done;
					strings[s] = sbuf;
					sbuf += slen;
					makecstring(name, strings[s], slen);
					s++;
				}
			}
		}
done:		/* done making the list of symbols */
		if (s > 0)
			qsort(strings, s, sizeof (char **), compare);
		UNPROTECT();
	}

	if (*strings == NULL || partial == NULL || *partial == '\0')
		return (strings);

	/* find the matches to the given symbol name */
	for (str = strings; *str != NULL && **str < *partial; str++)
		;
	len = strlen(partial);
	m = 0;
	for ( ; *str != NULL && **str == *partial && m < MAXSTRINGS; str++) {
		if (!strncmp(*str, partial, len))
			matches[m++] = *str;
	}
	matches[m] = NULL;
	return (matches);
}

compl_symbols(partial, result, maxlen)
	char	*partial, *result;
{
	extern char	**match_symbols();
	char		**matches;
	char		bigbuf[2048];
	int		status;

	ASSERT(result != NULL && maxlen > 1);

	matches = match_symbols(partial);
	if (matches == NULL || *matches == NULL)
		return (COMPL_NOMATCH);

	*bigbuf = '\0';
	status = compl_expand(partial, matches, bigbuf);
	if (status == COMPL_NOMATCH || status == COMPL_NOTUNIQ) {
		/* no completion could have been done */
		*result = '\0';
	} else if (*bigbuf == '\0') {
		/* no completion was done */
		*result = '\0';
	} else {
		strncpy(result, bigbuf, maxlen - 1);
		bigbuf[maxlen - 1] = '\0';
	}
	return (status);
}
