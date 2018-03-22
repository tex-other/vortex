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
 *  RCS Info: $Header: modeline.c,v 0.1 87/05/01 12:21:32 john Locked $
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
 *  modeline.c - internal mode line formatting routines
 */
static char _ID[] = "@(#)modeline.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: global-mode-string
 *  Desc: This variable is set once, globally, for all buffers of
 *	an instance of the VorTeX editor.  That is, the only instance
 *	of this variable normally exists in the global oblist.
 *
 *	This is generally used by editor routines to pass a value,
 *	such as the current time, into the name stripe (mode line)
 *	of all windows.
 *	This variable is changed by the automatic ``time and load''
 *	printing system, \sym{current-time}.
 *  Side: When this variable changes value, modes lines may change.
 *	Wherever the escape \lit{%M} appears in the mode line of a
 *	buffer, a text representation of the value of
 *	\lit{global-mode-string}.
 *  Xref: mode-string
 *  SeeA: mode-line-format local-mode-string global-user-mode-string
 */
MKSTRING(GLOBALMODE_NAME, "global-mode-string");

/*
 *  DOCUMENTATION
 *
 *  Name: mode-string
 *  Desc: This variable is set once for each buffer, locally.
 *	So changing it will only affect the current buffer.
 *
 *	This variable is usually used by mode ``packages'', such as
 *	\sym{c-mode} which customize the editor for one particular
 *	type of document file.  It is usually set the a short string
 *	naming the mode type (type of the source file).
 *  Side: When this variable changes value, modes lines may change.
 *	Wherever the escape \lit{%m} appears in the mode line of a
 *	buffer, a text representation of the value of
 *	\lit{local-mode-string}.
 *  SeeA: mode-line-format global-mode-string
 */
MKSTRING(LOCALMODE_NAME, "mode-string");

/*
 *  Generate a string from the mode line format and whatever
 *  data it specifies.  We understand the printf(3) escapes
 *  specified in the documentation for ``mode-line-format''.
 *  This structure holds the pieces of the expanded line, before
 *  we expand the ``fill formats'' (like ``%-'').
 */
struct onefmt {
	short		fm_what;
	short		fm_length;
	unsigned char	*fm_buf;
	unsigned char	*fm_end;
};
#define FM_TEXT		1	/* normal text string */
#define FM_SPACE	2	/* fill with spaces */
#define FM_DASH		3	/* fill with dashes */
#define FM_UNDER	4	/* fill with underscores */
#define FM_EQUAL	5	/* fill with equal signs */

static struct onefmt	fmt_buf[25];
static struct onefmt	*fmt_end = fmt_buf + NITEMS(fmt_buf);

static char		NOFILE[] = "[none]";

unsigned char *
text_modeline(win, width)
	struct window	*win;
	int		*width;
{
	extern struct string	*MODEFMT_NAME;
	unsigned char		buf[STRBUF], tbuf[1025];
	register unsigned char	*fp, *fend;
	unsigned char		*bp, *bend;
	struct onefmt		*fmt;
	struct string		*format, *str;
	struct value		val;
	struct buffer		*bufp;
	struct source		*srcp;
	register int		max;
	int			pos;
	register unsigned char	*cp, *end;

	/* get buffer associated with window */
	bufp = win->wi_buffer;
	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* get the format string from oblist */
	val = get_variable(MODEFMT_NAME, bufp);
	if (stringp(val))
		format = gstring(val.vl_data);
	else
		format = gstring(v_null.vl_data);

	fend = format->st_buffer + format->st_length;
	bp = buf;
	bend = buf + sizeof (buf);
	fmt = fmt_buf;
	for (fp = format->st_buffer; fp < fend && bp < bend; fp++) {
		if (*fp == '%') {
			/* scan max field width */
			fp++;
			if (isdigit(*fp)) {
				max = atoi(*fp);
				while (isdigit(*fp) && fp < fend)
					fp++;
			} else {
				/* maximum length is rest of string */
				max = bend - bp;
			}
			if (fp >= fend) {
				/* end-of-format too soon */
				*bp++ = '%';
				break;
			}

			/* handle format type */
			switch (*fp) {
			case 'b':	/* bufer name */
				ASSERT(bufp->bu_name != NULL);
				scopy(bufp->bu_name, &bp, max);
				break;
			case '*':	/* modified flag */
				if (bufp->bu_flags & BUFF_MODIFIED)
					*bp++ = '*';
				else if (bufp->bu_flags & BUFF_READONLY)
					*bp++ = '%';
				else
					*bp++ = '-';
				break;
			case 'f':	/* full pathname of file */
				if (srcp->sb_file == NULL) {
					/* not a file-visiting buffer */
					ccopy(NOFILE, &bp, max);
				} else {
					/* copy it in there */
					scopy(srcp->sb_file, &bp, max);
				}
				break;
			case 'F':	/* last component of file path */
				if (srcp->sb_file == NULL) {
					/* not a file-visiting buffer */
					ccopy(NOFILE, &bp, max);
				} else {
					makecstring(srcp->sb_file,
						    tbuf, sizeof (tbuf));
					cp = (unsigned char *)basename(tbuf);
					ccopy(cp, &bp, max);
				}
				break;
			case '~':	/* file path relative to user's home */
				if (srcp->sb_file == NULL) {
					/* not a file-visiting buffer */
					ccopy(NOFILE, &bp, max);
				} else {
					makecstring(srcp->sb_file,
						    tbuf, sizeof (tbuf));
					cp = (unsigned char *)fromhome(tbuf);
					ccopy(cp, &bp, max);
				}
				break;
			case '[':	/* recursive edit open bracket */
				break;
			case ']':	/* recursive edit close bracket */
				break;
			case 'w':	/* window number */
				sprintf(tbuf, "%d", win->wi_index);
				ccopy(tbuf, &bp, max);
				break;
			case 'M':	/* global mode string */
				val = get_variable(GLOBALMODE_NAME, NULL);
				if (stringp(val)) {
					str = gstring(val.vl_data);
					scopy(str, &bp, max);
				}
				break;
			case 'm':	/* local mode string */
				val = get_variable(LOCALMODE_NAME, bufp);
				if (stringp(val)) {
					str = gstring(val.vl_data);
					scopy(str, &bp, max);
				}
				break;
			case 's':	/* size of buffer */
				sprintf(tbuf, "%d", srcp->sb_length);
				ccopy(tbuf, &bp, max);
				break;
			case 'p':	/* position in buffer */
				if (srcp->sb_length <= 0) {
					/* can't be anywhere else */
					pos = 0;
				} else {
					pos = (srcp->sb_point * 100) /
						srcp->sb_length;
				}
				if (pos <= 0)
					strcpy(tbuf, "top");
				else if (pos >= 100)
					strcpy(tbuf, "bot");
				else
					sprintf(tbuf, "%2d%%", pos);
				ccopy(tbuf, &bp, max);
				break;
			case ' ':	/* fill with spaces */
			case '-':	/* fill with dashes */
			case '=':	/* fill with equal signs */
			case '_':	/* fill with underscores */
				end = buf + *width;
				if (bp + max < end)
					end = bp + max;
				while (bp < end)
					*bp++ = *fp;
				break;
			default:	/* just the next character */
				*bp++ = *fp;
				break;
			}
		} else {
			/* just copy in the next character */
			*bp++ = *fp;
		}
	}

	*width = bp - buf;
	return (buf);
}

unsigned char *
dvi_modeline(winp, lenp)
	struct window	*winp;
	int		*lenp;
{
	extern struct string	*MODEFMT_NAME;
	unsigned char		buf[STRBUF], tbuf[1025];
	register unsigned char	*fp, *fend;
	unsigned char		*bp, *bend;
	struct onefmt		*fmt;
	struct string		*format, *str;
	struct value		val;
	struct buffer		*bufp;
	struct proof		*prfp;
	register int		max;
	register unsigned char	*end;

	/* get buffer associated with window */
	bufp = winp->wi_buffer;
	ASSERT(bufp != NULL);
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* get the format string from oblist */
	val = get_variable(MODEFMT_NAME, bufp);
	if (stringp(val))
		format = gstring(val.vl_data);
	else
		format = gstring(v_null.vl_data);

	fend = format->st_buffer + format->st_length;
	bp = buf;
	bend = buf + sizeof (buf);
	fmt = fmt_buf;
	for (fp = format->st_buffer; fp < fend && bp < bend; fp++) {
		if (*fp == '%') {
			/* scan max field width */
			fp++;
			if (isdigit(*fp)) {
				max = atoi(*fp);
				while (isdigit(*fp) && fp < fend)
					fp++;
			} else {
				/* maximum length is rest of string */
				max = bend - bp;
			}
			if (fp >= fend) {
				/* end-of-format too soon */
				*bp++ = '%';
				break;
			}

			/* handle format type */
			switch (*fp) {
			case 'b':	/* bufer name */
				ASSERT(bufp->bu_name != NULL);
				scopy(bufp->bu_name, &bp, max);
				break;
			case '*':	/* modified flag */
				if (bufp->bu_flags & BUFF_MODIFIED)
					*bp++ = '*';
				else if (bufp->bu_flags & BUFF_READONLY)
					*bp++ = '%';
				else
					*bp++ = '-';
				break;
			case 'f':	/* full pathname of file */
			case 'F':	/* last component of file path */
			case '~':	/* pathname from home */
				break;
			case '[':	/* recursive edit open bracket */
				break;
			case ']':	/* recursive edit close bracket */
				break;
			case 'w':	/* window number */
				sprintf(tbuf, "%d", winp->wi_index);
				ccopy(tbuf, &bp, max);
				break;
			case 'M':	/* global mode string */
				val = get_variable(GLOBALMODE_NAME, NULL);
				if (stringp(val)) {
					str = gstring(val.vl_data);
					scopy(str, &bp, max);
				}
				break;
			case 'm':	/* local mode string */
				val = get_variable(LOCALMODE_NAME, bufp);
				if (stringp(val)) {
					str = gstring(val.vl_data);
					scopy(str, &bp, max);
				}
				break;
			case 's':	/* size of page */
				sprintf(tbuf, "%dx%d",
					prfp->pb_Xtotal, prfp->pb_Ytotal);
				ccopy(tbuf, &bp, max);
				break;
			case 'p':	/* current position */
				sprintf(tbuf, "%dx%d",
					prfp->pb_Xoffset, prfp->pb_Yoffset);
				ccopy(tbuf, &bp, max);
				break;
			case ' ':	/* fill with spaces */
			case '-':	/* fill with dashes */
			case '=':	/* fill with equal signs */
			case '_':	/* fill with underscores */
				end = buf + *lenp;
				if (bp + max < end)
					end = bp + max;
				while (bp < end)
					*bp++ = *fp;
				break;
			default:	/* just the next character */
				*bp++ = *fp;
				break;
			}
		} else {
			/* just copy in the next character */
			*bp++ = *fp;
		}
	}

	*lenp = bp - buf;
	return (buf);
}

extern char	*pstrings[];

static int
scopy(str, bptr, max)
	struct string	*str;
	unsigned char	**bptr;
{
	register unsigned char	*sp, *send;
	register unsigned char	*bp, *bend;
	register char		*p;

	bp = *bptr;
	bend = *bptr + max;
	sp = str->st_buffer;
	send = sp + str->st_length;

	while (bp < bend && sp < send) {
		for (p = pstrings[*sp]; bp < bend && *p != '\0'; p++)
			*bp++ = (unsigned char)*p;
		sp++;
	}

	/* advance passed pointer to end of string */
	*bptr = bp;
}

static int
ccopy(cstr, bptr, max)
	unsigned char	*cstr;
	unsigned char	**bptr;
{
	register unsigned char	*bp, *bend;
	register char		*p;

	bp = *bptr;
	bend = *bptr + max;
	while (bp < bend && *cstr != '\0') {
		for (p = pstrings[*cstr]; bp < bend && *p != '\0'; p++)
			*bp++ = (unsigned char)*p;
		cstr++;
	}

	/* advance passed pointer to end of string */
	*bptr = bp;
}
