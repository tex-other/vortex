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
 *  RCS Info: $Header: strings.c,v 0.1 87/05/01 12:27:52 john Locked $
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
 *  strings.c - internal string storage and handling routines
 */
static char _ID[] = "@(#)strings.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  Actual storage of strings.  Things which point to strings
 *  don't point to (char *)s, the point to (struct string *)s.
 *  All the (struct string)s which are available for allocation
 *  are kept in str_table, and point to storage in the large
 *  character buffer str_buffer.  When new storage is requested
 *  for a string, we check to make sure no previous string matches
 *  it and then allocate new space if necessary.  This may call
 *  for garbage collection, in which case strings may need to
 *  be moved around.  However, since we have this doulb level
 *  of indirection, we can change the (char *)s in the (struct
 *  string)s without invalidating the (struct string *)s that
 *  outside routines might have.
 *
 *  We use a straight-forward hash into the array of strings to
 *  speed up the lookup from their contents.  The search starts
 *  from the hash point and wraps around the end back to the
 *  beginning and on to the start point again.  For this reason,
 *  the initial size of the string table should be prime.
 */
int		aagrow_strings = 0;	/* automatically grow strings */
int		aagrow_buffer = 0;	/* automatically grow buffer */
static int	minfree_strings = 0;	/* actual number that must be free */
static int	minfree_buffer = 0;	/* actual amount that must be free */
static int	curfree_strings = 0;	/* actual number that are free */
static int	curfree_buffer = 0;	/* actual amount that is free */
int		softlim_strings = 0;	/* percentage that must be free */
int		softlim_buffer = 0;	/* percentage that must be free */
int		cursize_strings = 0;	/* current number of strings */
int		cursize_buffer = 0;	/* current bytes of buffer */
int		overflow_strings = 0;	/* have run out of strings */
int		overflow_buffer = 0;	/* have run out of buffer space */

static struct string	*str_table = NULL, *str_tabend;
static int		str_tabinit = 2131;
static unsigned char	*str_buffer = NULL, *str_bufend;
static unsigned char	*str_avail = NULL;
static int		str_bufinit = 20480;
static int		str_haveholes = FALSE;

#define BUFFER_MINGROW	512

struct string *
save_string(str, len)
	register char	*str;
	register int	len;
{
	register struct string	*strp;
	struct string		*start;

	if (len < 0)
		ierror("save_string: String length %d requested!", len);

	if (str_table == NULL || str_buffer == NULL) {
		/* initial table allocation */
		grow_strings(str_tabinit);
		grow_buffer(str_bufinit);
	}

	if (len < 1) {
		/* try to find another empty string */
		start = str_table;
		if (str_haveholes) {
			/* search throughout table just in case */
			for (strp = start; strp < str_tabend; strp++)
				if (!strp->st_free && strp->st_length == 0)
					return (strp);
		} else {
			/* only search until first free string */
			for (strp = start;
			     strp < str_tabend && !strp->st_free; strp++)
				if (strp->st_length == 0)
					return (strp);
		}
	} else if (str == NULL) {
		/* allocate new string toward the beginning */
		start = str_table;
	} else {
		/* try to find an eq string already here in the table */
		start = str_table + hashstring(str, len);
		if (str_haveholes) {
			/* search throughout table just in case */
			for (strp = start; strp < str_tabend; strp++) {
				if (!strp->st_free && strp->st_length == len &&
				    bcmp(strp->st_buffer, str, len) == 0)
					return (strp);
			}
			for (strp = str_table; strp < start; strp++) {
				if (!strp->st_free && strp->st_length == len &&
				    bcmp(strp->st_buffer, str, len) == 0)
					return (strp);
			}
		} else {
			/* only search until first free string */
			for (strp = start;
			     strp < str_tabend && !strp->st_free; strp++) {
				if (strp->st_length == len &&
				    bcmp(strp->st_buffer, str, len) == 0)
					return (strp);
			}
			for (strp = str_table;
			     strp < start && !strp->st_free; strp++) {
				if (strp->st_length == len &&
				    bcmp(strp->st_buffer, str, len) == 0)
					return (strp);
			}
		}
		/* no exact match found in string table */
	}

	/* try to find a place for this string */
	for (strp = start; strp < str_tabend; strp++)
		if (strp->st_free)
			goto found;
	for (strp = str_table; strp < start; strp++)
		if (strp->st_free)
			goto found;

	/* this shouldn't happen */
	debug(DALLOC, "String size: %d, curfree: %d, minfree: %d.",
	      cursize_strings, curfree_strings, minfree_strings);
	error("Help!  Ran completely out of string storage!");

found:	if (str_avail + len > str_bufend) {
		/* the string is too long to fit */
		if (len < BUFFER_MINGROW)
			grow_buffer(BUFFER_MINGROW);
		else
			grow_buffer(len);
	} else if ((!overflow_strings && curfree_strings < minfree_strings) ||
		   (!overflow_buffer && curfree_buffer < minfree_buffer)) {
		debug(DALLOC, "String size: %d, curfree: %d, minfree: %d.",
		      cursize_strings, curfree_strings, minfree_strings);
		/* garbage collection time ... */
		gcunmark_strings();
		gc_markall();
		gcsweep_strings();
		debug("String size: %d, curfree: %d, minfree: %d.",
		      cursize_strings, curfree_strings, minfree_strings);

		/* if stil not enough, keep trying */
		if (curfree_strings < minfree_strings) {
			if (aagrow_strings > 0) {
				grow_strings(aagrow_strings);
			} else {
				overflow_strings++;
				cerror(
		    "Too little string storage left (%d used, %d free).",
				    cursize_strings - curfree_strings,
				    curfree_strings);
			}
		}
		if (curfree_buffer < minfree_buffer) {
			if (aagrow_buffer > 0) {
				grow_buffer(aagrow_buffer);
			} else {
				overflow_buffer++;
				cerror(
		    "Too little string buffer space left (%d used, %d free).",
				    cursize_buffer - curfree_buffer,
				    curfree_buffer);
			}
		}
	}

	/* we found some storage to use */
	strp->st_free = FALSE;
	strp->st_perm = FALSE;
	if (len > 0 && str != NULL) {
		strp->st_buffer = str_avail;
		strp->st_length = len;
		bcopy(str, strp->st_buffer, len);
		str_avail += len;
		curfree_buffer -= len;
	} else if (len > 0 && str == NULL) {
		strp->st_buffer = str_avail;
		strp->st_length = len;
		bzero(strp->st_buffer, len);
		str_avail += len;
		curfree_buffer -= len;
	} else {
		/* empty string, no buffer required */
		strp->st_buffer = NULL;
		strp->st_length = 0;
	}
	curfree_strings--;
	return (strp);
}

/*
 *  Check if two strings are equal.  We first check to see if they're
 *  eq (by comparing their storage positions).  If not, we make a
 *  comparison of the text characters.
 */

sequal(sone, stwo)
	struct string	*sone, *stwo;
{
	/* if eq, they must be equal */
	if (sone == stwo)
		return (TRUE);
	/* they must be the same length */
	if (sone->st_length != stwo->st_length)
		return (FALSE);
	if (sone->st_length == 0)
		return (TRUE);
	/* return byte comparison over length */
	return !bcmp(sone->st_buffer, stwo->st_buffer, sone->st_length);
}

#define STRING_HARDLIM	5

grow_strings(incr)
{
	struct string		*tabp;
	register struct string	*strp;

	if (incr > 0) {
		/* allocate storage for new, larger, table */
		tabp = (struct string *)
		    valloc((cursize_strings + incr) * sizeof (struct string));
	
		/* copy old table into bottom of new space and initialize */
		if (str_table != NULL) {
			bcopy(str_table, tabp,
			    cursize_strings * sizeof (struct string));
			vfree(str_table);
		}
		strp = tabp + (str_tabend - str_table);
		str_table = tabp;
		cursize_strings += incr;
		curfree_strings += incr;
		str_tabend = str_table + cursize_strings;
		while (strp < str_tabend) {
			strp->st_free = TRUE;
			strp++;
		}

		overflow_strings = 0;
	}

	/* update minimum free amount variable */
	minfree_strings = ROUND((double)cursize_strings *
				((double)softlim_buffer / 100.0));
	if (minfree_strings > cursize_strings - STRING_HARDLIM)
		minfree_strings = cursize_strings - STRING_HARDLIM;

	return (cursize_strings);
}

#define BUFFER_HARDLIM		512

grow_buffer(incr)
{
	unsigned char	*bufp;

	if (incr > 0) {
		/* allocate storage for new, larger, buffer */
		bufp = (unsigned char *)
		    valloc((cursize_buffer + incr) * sizeof (unsigned char));

		/* copy old buffer into bottom of new space and initialize */
		if (str_buffer != NULL) {
			bcopy(str_buffer, bufp,
			    cursize_buffer * sizeof (unsigned char));
			vfree(str_buffer);
		}
		str_avail = bufp + (str_avail - str_buffer);
		str_buffer = bufp;
		cursize_buffer += incr;
		curfree_buffer += incr;
		str_bufend = str_buffer + cursize_buffer;

		overflow_buffer = 0;
	}

	/* update minimum free amount variable */
	minfree_buffer = ROUND((double)cursize_buffer *
			       ((double)softlim_buffer / 100.0));
	if (minfree_buffer > cursize_buffer - BUFFER_HARDLIM)
		minfree_buffer = cursize_buffer - BUFFER_HARDLIM;

	return (cursize_buffer);
}

gcunmark_strings()
{
	return (0);
}

gcsweep_strings()
{
	return (0);
}

static int
hashstring(text, len)
	register unsigned char	*text;
{
	register int	count, pos, inx;

	count = 0;
	pos = len + 2;
	for (inx = 0; inx < len; inx++)
		count += *text++ * pos--;

	return (count % cursize_strings);
}
