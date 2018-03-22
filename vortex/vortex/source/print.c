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
 *  RCS Info: $Header: print.c,v 0.1 87/05/01 12:24:15 john Locked $
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
 *  print.c - internal output functions
 */
static char _ID[] = "@(#)print.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"
#include "channel.h"
#include "parser.h"
#include "function.h"
#include "format.h"

static struct channel	*curchan;	/* the channel currently in use */

#define output(c)	(((c) < 0) ? -1 : \
			 curchan->ch_outptr >= curchan->ch_output + CHANBUF ?\
			  cflush(curchan) : 0, \
			 *curchan->ch_outptr++ = (c) & 0377)

/*
 *  Make sure we can always print something.  We always have the
 *  default character to print in case the user has made one
 *  one of the syntax functions undefined.  This isn't as efficient
 *  as just looking at the variables, but we hafta do it...
 */
#define blistc		(p_blistc == -1 ? '(' : p_blistc)
#define elistc		(p_elistc == -1 ? ')' : p_elistc)
#define quotec		(p_quotec == -1 ? '\'' : p_quotec)
#define bquotec		(p_bquotec == -1 ? '`' : p_bquotec)
#define dtprc		(p_dtprc == -1 ? '.' : p_dtprc)
#define stringc		(p_stringc == -1 ? '"' : p_stringc)
#define barrayc		(p_barrayc == -1 ? '[' : p_barrayc)
#define earrayc		(p_earrayc == -1 ? ']' : p_earrayc)
#define literalc	(p_literalc == -1 ? '?' : p_literalc)
#define escapec		(p_escapec == -1 ? '\\' : p_escapec)
#define commentc	(p_commentc == -1 ? ';' : p_commentc)
#define delimc		(p_delimc == -1 ? '|' : p_delimc)

char	NULLSTR[] = "<null>";

static
_prsexpr(sexpr)
	struct value	sexpr;
{
	extern char	*pquick(), *psymbol(), *pstring(), *pfixnum();
	struct ccell	*elt;
	struct value	val;
	struct function	*func;
	register char	*cp;
	register int	end;
	struct array	*arr;
	struct value	*vp, *vend;
	char		sbuf[SMALLBUF];

	switch (sexpr.vl_type) {
	case LISP_NIL:
		output('n'), output('i'), output('l');
		break;
	case LISP_FIXNUM:
		for (cp = pfixnum(gfixnum(sexpr.vl_data)); *cp != '\0'; cp++)
			output(*cp);
		break;
	case LISP_STRING:
		for (cp = pstring(gstring(sexpr.vl_data)); *cp != '\0'; cp++)
			output(*cp);
		break;
	case LISP_SYMBOL:
		for (cp = psymbol(gsymbol(sexpr.vl_data)->sy_pname);
		     *cp != '\0'; cp++)
			output(*cp);
		break;
	case LISP_FUNC:
		if ((func = gfunct(sexpr.vl_data)) == NULL)
			ierror("_prsexpr: Null function value!");
		if (func->fn_pname == NULL) {
			/* nothing really to print */
			for (cp = "<function>"; *cp != '\0'; cp++)
				output(*cp);
		} else {
			/* builtin function print name */
			for (cp = psymbol(func->fn_pname); *cp != '\0'; cp++)
				output(*cp);
		}
		break;
	case LISP_CONS:
		output(blistc);
		elt = glist(sexpr.vl_data);
		end = FALSE;
		do {
			val.vl_type = elt->cc_tcar;
			val.vl_data = elt->cc_car;
			_prsexpr(val);
			if (elt->cc_tcdr == LISP_NIL) {
				/* this is the end */
				end = TRUE;
			} else if (elt->cc_tcdr == LISP_CONS) {
				/* there's another list element */
				output(' ');
			} else {
				output(' '), output(dtprc), output(' ');
				val.vl_type = elt->cc_tcdr;
				val.vl_data = elt->cc_cdr;
				_prsexpr(val);
				end = TRUE;
			}
		} while (!end && (elt = glist(elt->cc_cdr)) != NULL);
		output(elistc);
		break;
	case LISP_ARRAY:
		output(barrayc);
		arr = garray(sexpr.vl_data);
		vend = arr->ar_array + arr->ar_length;
		for (vp = arr->ar_array; vp < vend; vp++) {
			_prsexpr(*vp);
			if (vp < vend - 1)
				output(' ');
		}
		output(earrayc);
		break;
	default:
		sprintf(sbuf, "<type=%d,data=%x>",
		    sexpr.vl_type, sexpr.vl_data);
		for (cp = sbuf; *cp != '\0'; cp++)
			output(*cp);
		break;
	}

	return (0);
}

static char	NOWRITE[] = "Channel %d isn't open for writing!";
static char	ISCLOSED[] = "Channel %d can't be written on anymore!";

cprinc(ch, chan)
	struct channel	*chan;
{
	if ((chan->ch_flags & CHAN_WRITE) == 0)
		error(NOWRITE, chan->ch_number);
	if ((chan->ch_flags & CHAN_CLOSED) != 0)
		error(ISCLOSED, chan->ch_number);

	curchan = chan;
	output(ch);
	curchan = NULL;
}

cpatom(str, chan)
	struct string	*str;
	struct channel	*chan;
{
	register unsigned char	*cp, *send;

	if ((chan->ch_flags & CHAN_WRITE) == 0)
		error(NOWRITE, chan->ch_number);
	if ((chan->ch_flags & CHAN_CLOSED) != 0)
		error(ISCLOSED, chan->ch_number);

	curchan = chan;
	send = str->st_buffer + str->st_length;
	for (cp = str->st_buffer; cp < send; cp++)
		output(*cp);
	curchan = NULL;
}

scpatom(str, chan)
	unsigned char	*str;
	struct channel	*chan;
{
	struct string	sbuf;

	sbuf.st_length = strlen(str);
	sbuf.st_buffer = str;
	return cpatom(&sbuf, chan);
}

cprint(sexpr, chan)
	struct value	sexpr;
	struct channel	*chan;
{
	if ((chan->ch_flags & CHAN_WRITE) == 0)
		error(NOWRITE, chan->ch_number);
	if ((chan->ch_flags & CHAN_CLOSED) != 0)
		error(ISCLOSED, chan->ch_number);

	curchan = chan;
	_prsexpr(sexpr);
	curchan = NULL;
}

#define FAKEOCHAN	-1

fprint(sexpr, fd)
	struct value	sexpr;
{
	struct channel	chan;

	chan.ch_number = FAKEOCHAN;
	chan.ch_type = CHAN_FILE;
	*chan.ch_path = '\0';
	chan.ch_flags = CHAN_WRITE;
	chan.ch_iofd = fd;
	chan.ch_outptr = chan.ch_input;
	chan.ch_next = NULL;

	cprint(sexpr, &chan);
	cflush(&chan);
}

/* VARARGS */
cprintf(va_alist)
	va_dcl
{
#include "fmtdecl.h"
	struct channel	*chan;
	struct string	sbuf;

	STARTVA();
	chan = GETVARG(struct channel *);

#include "fmtcode.h"

	sbuf.st_length = msglen;
	sbuf.st_buffer = (unsigned char *)msgbuf;
	return cpatom(&sbuf, chan);
}

char *
pquick(sexpr)
	struct value	sexpr;
{
	struct function	*func;
	char		buf[48];

	switch (sexpr.vl_type) {
	case LISP_NIL:
		return ("nil");
	case LISP_CONS:
		return ("(...)");
	case LISP_SYMBOL:
		return psymbol(gsymbol(sexpr.vl_data)->sy_pname);
	case LISP_STRING:
		return pstring(gstring(sexpr.vl_data));
	case LISP_FIXNUM:
		return pfixnum(gfixnum(sexpr.vl_data));
	case LISP_FUNC:
		func = gfunct(sexpr.vl_data);
		ASSERT(func != NULL);
		if (func->fn_pname == NULL)
			return ("<function>");
		else
			return psymbol(func->fn_pname);
	case LISP_ARRAY:
		return ("[...]");
	default:
		sprintf(buf, "<type=%d,data=%x>",
			sexpr.vl_type, sexpr.vl_data);
		return (buf);
	}
}

char *
psexpr(sexpr)
	struct value	sexpr;
{
	static char	buf[STRBUF];
	struct function	*func;
	int		len;

	switch (sexpr.vl_type) {
	case LISP_NIL:
		return ("nil");
	case LISP_CONS:
	case LISP_ARRAY:
		len = precurse(sexpr, buf, sizeof (buf) - 1);
		buf[len] = '\0';
		return (buf);
	case LISP_SYMBOL:
		return psymbol(gsymbol(sexpr.vl_data)->sy_pname);
	case LISP_STRING:
		return pstring(gstring(sexpr.vl_data));
	case LISP_FIXNUM:
		return pfixnum(gfixnum(sexpr.vl_data));
	case LISP_FUNC:
		func = gfunct(sexpr.vl_data);
		ASSERT(func != NULL);
		if (func->fn_pname == NULL)
			return ("<function>");
		else
			return psymbol(func->fn_pname);
	default:
		sprintf(buf, "<type=%d,data=%x>",
		    sexpr.vl_type, sexpr.vl_data);
		return (buf);
	}
}

static int
precurse(sexpr, buf, size)
	struct value	sexpr;
	char		*buf;
{
	register char	*bp, *bend, *fp;
	char		sbuf[30];
	struct function	*func;
	struct ccell	*elt;
	struct array	*arr;
	struct value	val, *vp, *vend;
	register int	lend, len;

	/* make sure we can do anything at all */
	if (size <= 0)
		return (0);

	/* mark end of buffer passed in */
	bend = buf + size;

	switch (sexpr.vl_type) {
	case LISP_NIL:
		fp = "nil";
		for (bp = buf; *fp != '\0' && bp < bend; bp++)
			*bp = *fp++;
		break;
	case LISP_CONS:
		bp = buf;
		*bp++ = blistc;
		elt = glist(sexpr.vl_data);
		lend = FALSE;
		do {
			ASSERT(elt != NULL);
			val.vl_type = elt->cc_tcar;
			val.vl_data = elt->cc_car;
			len = precurse(val, bp, bend - bp);
			bp += len;
			if (bp >= bend)
				break;
			if (elt->cc_tcdr == LISP_NIL) {
				/* the end of the list is here */
				lend = TRUE;
			} else if (elt->cc_tcdr == LISP_CONS) {
				/* there is another list element */
				*bp++ = ' ';
			} else {
				/* second part of a dotted pair */
				if (bp + 2 >= bend)
					break;
				*bp++ = ' ';
				*bp++ = dtprc;
				*bp++ = ' ';
				val.vl_type = elt->cc_tcdr;
				val.vl_data = elt->cc_cdr;
				len = precurse(val, bp, bend - bp);
				bp += len;
				lend = TRUE;
			}
		} while (!lend && (elt = glist(elt->cc_cdr)) != NULL);
		if (bp < bend)
			*bp++ = elistc;
		break;
	case LISP_SYMBOL:
		fp = psymbol(gsymbol(sexpr.vl_data)->sy_pname);
		for (bp = buf; *fp != '\0' && bp < bend; bp++)
			*bp = *fp++;
		break;
	case LISP_STRING:
		fp = pstring(gstring(sexpr.vl_data));
		for (bp = buf; *fp != '\0' && bp < bend; bp++)
			*bp = *fp++;
		break;
	case LISP_FIXNUM:
		fp = pfixnum(gfixnum(sexpr.vl_data));
		for (bp = buf; *fp != '\0' && bp < bend; bp++)
			*bp = *fp++;
		break;
	case LISP_FUNC:
		func = gfunct(sexpr.vl_data);
		ASSERT(func != NULL);
		if (func->fn_pname == NULL)
			fp = "<function>";
		else
			fp = psymbol(func->fn_pname);
		for (bp = buf; *fp != '\0' && bp < bend; bp++)
			*bp = *fp++;
		break;
	case LISP_ARRAY:
		bp = buf;
		*bp++ = barrayc;
		arr = garray(sexpr.vl_data);
		ASSERT(arr != NULL);
		vend = arr->ar_array + arr->ar_length;
		for (vp = arr->ar_array; vp < vend; vp++) {
			len = precurse(*vp, bp, bend - bp);
			bp += len;
			if (bp >= bend)
				break;
			if (vp + 1 < vend)
				*bp++ = ' ';
		}
		if (bp < bend)
			*bp++ = earrayc;
		break;
	default:
		sprintf(sbuf, "<type=%d,data=%x>",
		    sexpr.vl_type, sexpr.vl_data);
		fp = sbuf;
		for (bp = buf; *fp != '\0' && bp < bend; bp++)
			*bp = *fp++;
		break;
	}

	return (bp - buf);
}

char *
psymbol(str)
	struct string	*str;
{
	static char		tbuf[BIGBUF];
	char			*tend = tbuf + sizeof (tbuf) - 2;
	register unsigned char	*fp, *fend;
	register char		*tp;
	register int		code, let;
	int			uglyc = FALSE;

	if (str == NULL)
		return (NULLSTR);

	if (str->st_length < 1)
		uglyc = TRUE;
	fend = str->st_buffer + str->st_length;
	tp = tbuf + 1;
	for (fp = str->st_buffer; fp < fend && tp < tend; fp++) {
		code = *fp & 0377;
		if (code == delimc) {
			*tp++ = escapec;
			*tp++ = delimc;
			uglyc = TRUE;
		} else if (code == escapec) {
			*tp++ = escapec;
			*tp++ = escapec;
			uglyc = TRUE;
		} else if (code < ' ') {
			*tp++ = escapec;
			*tp++ = '^';
			let = code + '@';
			if (isupper(let))
				*tp++ = tolower(let);
			else
				*tp++ = let;
			uglyc = TRUE;
		} else if (code == 0177) {
			*tp++ = escapec;
			*tp++ = '^';
			*tp++ = '?';
			uglyc = TRUE;
		} else if (code > 0177) {
			sprintf(tp, "%c%03.3o", escapec, code), tp += 4;
			uglyc = TRUE;
		} else {
			/* just a normal character */
			*tp++ = (char)code;
			if (code == ' ' || code == '\t')
				uglyc = TRUE;
		}
	}
	if (uglyc) {
		*tbuf = delimc;
		*tp++ = delimc;
		*tp = '\0';
		return (tbuf);
	} else {
		/* don't delimit it */
		*tp = '\0';
		return (tbuf + 1);
	}
}

char *
pstring(str)
	struct string	*str;
{
	static unsigned char	sbuf[BIGBUF];
	register unsigned char	*fp, *tp, *send;
	register int		code, let;

	if (str == NULL)
		return (NULLSTR);

	tp = sbuf;
	*tp++ = stringc;
	send = str->st_buffer + str->st_length;
	for (fp = str->st_buffer; fp < send; fp++) {
		code = *fp & 0377;
		if (code == stringc) {
			*tp++ = escapec;
			*tp++ = stringc;
		} else if (code == escapec) {
			*tp++ = escapec;
			*tp++ = escapec;
		} else if (code < ' ') {
			*tp++ = escapec;
			*tp++ = '^';
			let = code + '@';
			if (isupper(let))
				*tp++ = tolower(let);
			else
				*tp++ = let;
		} else if (code == 0177) {
			*tp++ = escapec;
			*tp++ = '^';
			*tp++ = '?';
		} else if (code > 0177) {
			sprintf(tp, "%c%03.3o", escapec, code);
			tp += 4;
		} else {
			*tp++ = code;
		}
	}
	*tp++ = stringc;
	*tp = '\0';

	return ((char *)sbuf);
}
