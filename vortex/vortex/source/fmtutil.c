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
 *  RCS Info: $Header: fmtutil.c,v 0.1 87/05/01 12:12:54 john Locked $
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
 *  fmtutil.c - format various objects into strings
 */
static char _ID[] = "@(#)fmtutil.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "format.h"
#include "parser.h"

char	NULLPTR[] = "<null>";

scanfield(ptrp, len, adjp, minp, maxp)
	char	**ptrp;
	int	*adjp, *minp, *maxp;
{
	register char	*cp, *pend;
	char		minbuf[SMALLBUF], maxbuf[SMALLBUF];

	/* make sure there are at least two characters to read */
	pend = *ptrp + len;
	if (**ptrp != FMT_FORMAT || *ptrp + 1 >= pend)
		return (-1);

	/* skip percent character */
	(*ptrp)++;

	/* look for format field */
	if (isadjust(**ptrp))
		*adjp = *(*ptrp)++;
	else
		*adjp = DEF_ADJUST;
	if (*ptrp >= pend)
		return (-1);

	if (isdigit(**ptrp)) {
		/* read digits of minimum field width */
		cp = minbuf;
		while (isdigit(**ptrp) && *ptrp < pend)
			*cp++ = *(*ptrp)++;
		*cp = '\0';
		*minp = atoi(minbuf);
	} else if (**ptrp == FMT_READARG) {
		/* read minimum field width */
		*minp = WID_GETARG;
		(*ptrp)++;
	} else {
		/* no minimum field width */
		*minp = WID_UNSPEC;
	}

	if (**ptrp == FMT_WIDSEP) {
		/* read a maximum width specification */
		(*ptrp)++;
		if (isdigit(**ptrp)) {
			cp = maxbuf;
			while (isdigit(**ptrp) && *ptrp < pend)
				*cp++ = *(*ptrp)++;
			*cp = '\0';
			*maxp = atoi(maxbuf);
		} else if (**ptrp == FMT_READARG) {
			/* read minimum field width */
			*maxp = WID_GETARG;
			(*ptrp)++;
		} else {
			/* no maximum width field */
			*maxp = WID_UNSPEC;
		}
	} else {
		/* maximum width field completely absent */
		*maxp = WID_UNSPEC;
	}

	/* return the next character, must be the code */
	if (*ptrp >= pend)
		return (-1);
	else
		return (*(*ptrp)++);
}

prtfield(inbuf, ilen, outbuf, blen, adj, min, max)
	unsigned char	*inbuf;
	unsigned char	*outbuf;
{
	register unsigned char	*bp, *bend, *ip, *iend;
	register int		count, hlen;

	/* make sure we have valid pointers */
	ASSERT(inbuf != NULL);
	ASSERT(outbuf != NULL);
	/* set up end-of-string pointers */
	iend = inbuf + ilen;
	bend = outbuf + blen;

	/* make sure min <= max if max >= 0 */
	if (max >= 0 && min > max)
		min = max;

	ip = inbuf;
	bp = outbuf;
	if (min >= 0 && ilen < min) {
		count = min - ilen;
		if (adj == ADJ_LEFT) {
			/* left adjust this field */
			while (ip < iend && bp < bend)
				*bp++ = *ip++;
			while (count-- > 0 && bp < bend)
				*bp++ = ' ';
		} else if (adj == ADJ_CENTER) {
			/* center this field */
			hlen = count / 2;
			while (count-- > hlen && bp < bend)
				*bp++ = ' ';
			while (ip < iend && bp < bend)
				*bp++ = *ip++;
			while (count-- >= 0 && bp < bend)
				*bp++ = ' ';
		} else {
			/* right adjust this field */
			while (count-- > 0 && bp < bend)
				*bp++ = ' ';
			while (ip < iend && bp < bend)
				*bp++ = *ip++;
		}
	} else if (max >= 0 && ilen > max) {
		/* print the first max characters */
		for (count = 0; count < max && ip < iend && bp < bend; count++)
			*bp++ = *ip++;
	} else {
		/* just the right size */
		while (ip < iend && bp < bend)
			*bp++ = *ip++;
	}

	return (bp - outbuf);
}


/*
 *  Below are routines to convert various numbers into C strings
 *  (null-terminated) so that one can print them.  These are mostly
 *  used by our format routines, which is why they're here, but
 *  may be called by any routine which needs a printable version
 *  of some machine structure.
 *
 *  We maintain a small pool of buffers for the return strings so
 *  that the user can call more than one of these routines ``at the
 *  same time'' without overwriting the static storage.  The macro
 *  getbuf() returns a (char *) which points to a short buffer to
 *  write into.
 */
#define NBUFS	10
#define BSIZE	64
static char	_buflist[NBUFS][BSIZE];
static int	_bufindx = 0;

#define getbuf()	((_bufindx >= NBUFS) ? \
			 _buflist[_bufindx = 0] : _buflist[_bufindx++])

static struct romconv {
	short		rc_char;	/* character for this number */
	short		rc_value;	/* value of this digit */
} roman_digits[] = {
	{ 'm',		1000 },
	{ 'd',		500 },
	{ 'c',		100 },
	{ 'l',		50 },
	{ 'x',		10 },
	{ 'v',		5 },
	{ 'i',		1 },
	{ '\0',		0 }
};

char *
itor(number)
	register int	number;
{
	register char	*obuf, *op;
	struct romconv	*rp, *start;

	/* the Romans had no zero or negative numbers */
	if (number < 0)
		number *= -1;
	else if (number == 0)
		return ("zero");

	obuf = getbuf();
	op = obuf;
	start = roman_digits;
	while (number > 0) {
		/* extract highest possible roman digit */
		for (rp = start; rp->rc_char != '\0'; rp++)
			if (rp->rc_value <= number) {
				number -= rp->rc_value;
				*op++ = rp->rc_char;
				break;
			} else {
				/* won't need this digit again */
				start = rp + 1;
			}
	}
	*op = '\0';

	return (obuf);
}

static char	digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";

char *
itoa(ival, radix)
	register int	ival, radix;
{
	char		*sbuf;
	register char	*sp;
	int		neg = FALSE;

	if (ival < 0) {
		neg = TRUE;
		ival *= -1;
	}
	if (radix < 2)
		radix = 2;
	if (radix > 36)
		radix = 36;

	/* get the buffer and move to end */
	sbuf = getbuf();
	sp = sbuf + BSIZE - 1;

	*sp = '\0';
	do {
		*--sp = digits[ival % radix];
		ival /= radix;
	} while  (ival > 0 && sp > sbuf);
	if (neg && sp > sbuf)
		*--sp = '-';

	return (sp);
}

char *
utoa(ival, radix)
	unsigned int	ival;
	register int	radix;
{
	char		*sbuf;
	register char	*sp;

	if (radix < 2)
		radix = 2;
	if (radix > 36)
		radix = 36;

	/* get the buffer and move to end */
	sbuf = getbuf();
	sp = sbuf + BSIZE - 1;

	*sp = '\0';
	do {
		*--sp = digits[ival % radix];
		ival /= radix;
	} while  (ival > 0 && sp > sbuf);

	return (sp);
}

char *
ftoa(fval)
	double	fval;
{
	register char	*nbuf, *np;

	nbuf = getbuf();
	sprintf(nbuf, "%.20f", fval);
	for (np = nbuf; *np != '\0'; np++)
		;
	for (--np; np > nbuf && *np == '0'; np--)
		;
	if (*np == '.')
		np++;
	*++np = '\0';

	return (nbuf);
}

static char	*kstrings[] = {
	"NUL", "C-a", "C-b", "C-c", "C-d", "C-e", "C-f", "C-g", "C-h",
	"TAB", "LFD", "C-k", "C-l", "RET", "C-n", "C-o", "C-p", "C-q",
	"C-r", "C-s", "C-t", "C-u", "C-v", "C-w", "C-x", "C-y", "C-z",
	"ESC", "C-\\", "C-]", "C-^", "C-_", "SPC", "!", "\"", "#",
	"$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1",
	"2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
	"@", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
	"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "[",
	"\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~", "DEL"
};
static char	*kname[] = {
	"LFT", "MID", "RHT", "MS3", "MS4", "MS5", "MS6", "MS7"
};
static char	kshft[] = "S-";
static char	kmeta[] = "M-";
static char	kctrl[] = "C-";

char *
pkeycode(code)
	unsigned int	code;
{
	char		*kbuf;
	int		but;

	code &= 0377;
	kbuf = getbuf();
	*kbuf = '\0';
	if (code > CHAR_MAXCODE) {
		/* this is a mouse button event */
		if (code & MOUSE_META)
			strcat(kbuf, kmeta);
		if (code & MOUSE_CTRL)
			strcat(kbuf, kctrl);
		if (code & MOUSE_SHIFT)
			strcat(kbuf, kshft);
		but = code & BUTTON_MASK;
		strcat(kbuf, kname[but]);
	} else {
		/* this is a keyboard event */
		strcpy(kbuf, kstrings[code]);
	}
	return (kbuf);
}

char *
pkeyseq(str)
	struct string	*str;
{
	char		*kbuf, *cp;
	register char	*kp, *kend;
	unsigned int	code, but;
	unsigned char	*sp, *send;

	/* get new buffer and set end marker */
	kbuf = getbuf();
	kend = kbuf + BSIZE;
	kp = kbuf;

	/* extract string's buffer */
	if (str == NULL || str->st_length < 1)
		return ("");
	send = str->st_buffer + str->st_length;

#define CAT(s)	for (cp = s; *cp != '\0' && kp < kend; *kp++ = *cp++)

	/* handle each key in the string */
	for (sp = str->st_buffer; sp < send && kp < kend; sp++) {
		if (kp > kbuf)
			*kp++ = ' ';
		code = *sp & 0377;
		if (code > CHAR_MAXCODE) {
			/* this is a mouse button event */
			if (code & MOUSE_META)
				CAT(kmeta);
			if (code & MOUSE_CTRL)
				CAT(kctrl);
			if (code & MOUSE_SHIFT)
				CAT(kshft);
			but = code & BUTTON_MASK;
			CAT(kname[but]);
		} else {
			/* this is a keyboard event */
			CAT(kstrings[code]);
		}
	}

	/* make sure string is null terminated and return */
	if (kp < kend)
		*kp = '\0';
	else
		*(kend - 1) = '\0';
	return (kbuf);
}
