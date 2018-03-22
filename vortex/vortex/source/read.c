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
 *  RCS Info: $Header: read.c,v 0.1 87/05/01 12:26:04 john Locked $
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
 *  read.c - internal input routines
 */
static char _ID[] = "@(#)read.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "parser.h"
#include "channel.h"

static struct channel	*curchan;	/* the channel currently in use */
static int		_in;		/* the current input character */

#define input()		(ceof(curchan) ? EOF : \
			 ((_in = (curchan->ch_inptr >= curchan->ch_inend) ? \
			   fillchannel(curchan) : *curchan->ch_inptr++), \
			  ((_in == '\n' || _in == '\r') ? \
			   (errline++, '\n') : _in)))
#define unput(c)	(*--curchan->ch_inptr = (c))
#define ineof()		(curchan->ch_flags & CHAN_EOF)

extern int	errline;
extern char	*errfile;

MKSTRING(QUOTESTR, "quote");
MKSTRING(BQUOTESTR, "backquote");

static struct value
_rdsexpr()
{
	extern struct value	_rdatom(), readmacro();
	struct value		done, expr;
	register struct ccell	*last, *head, *new;
	register int		ch;
	int			lastdtpr = FALSE;

	/* skip past leading spaces */
	do
		ch = input();
	while (isspace(ch));

	/* check for end-of-file */
	if (ineof())
		return (NOVALUE);

	while (iscomment(ch)) {
		while (ch != EOF && ch != '\n')
			ch = input();
		if (ineof())
			return (NOVALUE);
		while (isspace(ch))
			ch = input();
	}

	/* check if the atom or list is quoted */
	if (isquote(ch)) {
		/* read next s-expression to quote */
		expr = _rdsexpr();
		if (eq(expr, NOVALUE))
			expr = v_nil;
		/* expand quote read macro */
		done = readmacro(expr, QUOTESTR);
	} else if (isbquote(ch)) {
		/* read next s-expr to backquote */
		expr = _rdsexpr();
		if (eq(expr, NOVALUE))
			expr = v_nil;
		/* expand backquote read macro */
		done = readmacro(expr, BQUOTESTR);
	} else if (isblist(ch)) {
		/* start of a list */
		last = head = NULL;
		while (!iselist(ch)) {
			/* skip past leading spaces */
			for (ch = input(); isspace(ch); ch = input())
				;
			if (ineof()) {
				error(
			    "EOF occured in the middle of an s-expression.");
			}

			/* check for this being a dotted pair */
			if (isdtpr(ch)) {
				ch = input();
				if (!isspace(ch)) {
					/* not a dotted pair */
					unput(p_dtprc);
					/* continue with normal case */
				} else if (head == NULL) {
					error(
				    "No first element for the dotted pair!");
				} else {
					expr = _rdsexpr();
					if (eq(expr, NOVALUE))
						error(
				   "EOF occurred when an atom was expected!");
					last->cc_tcdr = expr.vl_type;
					last->cc_cdr = expr.vl_data;
					lastdtpr = TRUE;
					continue;
				}
			}

			/* handle next atom in list */
			if (!iselist(ch)) {
				unput(ch);
				if (lastdtpr) {
					error(
		    "Second element of dotted pair not last element in list!");
				}

				/* add this element to the list */
				expr = _rdsexpr();
				ASSERT(!eq(expr, NOVALUE));
				new = save_ccell();
				new->cc_tcar = expr.vl_type;
				new->cc_car = expr.vl_data;
				if (last == NULL) {
					/* this is the head of the list */
					head = new;
				} else {
					/* sppend to existing list */
					last->cc_tcdr = LISP_CONS;
					slist(last->cc_cdr, new);
				}
				last = new;
			}
		}
		if (head == NULL) {
			/* no elements in this list, it's nil */
			done = v_nil;
		} else if (lastdtpr) {
			/* assign the list read so far */
			done.vl_type = LISP_CONS;
			slist(done.vl_data, head);
		} else {
			/* terminate the list with nil */
			last->cc_tcdr = LISP_NIL;
			slist(last->cc_cdr, NULL);
			/* assign the list read so far */
			done.vl_type = LISP_CONS;
			slist(done.vl_data, head);
		}
	} else {
		/* an atom, just read and return it */
		unput(ch);
		done = _rdatom();
	}

	return (done);
}

static struct value
_rdatom()
{
	struct value		done;
	unsigned char		token[BIGBUF], *tend = token + sizeof (token);
	register int		tlen = 0;
	register int		ch, type;
	struct string		*pname;
	int			fixnum, code;
	register unsigned char	*tp;

	/* read in first token character, skipping spaces */
	ch = input();
	while (isspace(ch))
		ch = input();

	/* check for EOF one last time before we dive into it */
	if (ineof())
		return (NOVALUE);

	if (isstring(ch)) {
		tp = token;
		ch = input();
		while (!ineof() && !isstring(ch) && tp < tend) {
			if (isescape(ch)) {
				/* magical escape sequences */
				code = magic();
				if (code > 0377) {
					error(
				   "Illegal character code %o read in string.",
					      code);
				}
				*tp = code;
			} else {
				/* just a normal character */
				*tp = ch;
			}
			tp++;
			tlen++;
			ch = input();
		}
		if (tp >= tend) {
			/* token is too long, something is screwy */
			error(
		    "String too long for reader, maximum %d characters!",
			    tend - token);
		} else if (!isstring(ch)) {
			/* EOF in string */
			error("EOF occurred before the string ended!");
		}
		done.vl_type = LISP_STRING;
		sstring(done.vl_data, save_string(token, tlen));
		return (done);
	} else {
		/* check for the literal read macro */
		if (isliteral(ch)) {
			ch = input();
			if (ch == EOF)
				error("EOF read during a character literal!");
			if (isescape(ch))
				ch = magic();
			done.vl_type = LISP_FIXNUM;
			sfixnum(done.vl_data, ch);
			return (done);
		}

		type = LISP_FIXNUM;
		tp = token;
		while (!ineof() && !isspace(ch) &&
		       (!islist(ch) || tp == token) &&
		       tp < tend) {
			if (isescape(ch)) {
				ch = input();
				if (ch == EOF)
					ch = p_escapec;
			}
			*tp = ch;
			if (*tp == '\0')
				*tp = '\200';
			if (type == LISP_FIXNUM &&
			    !isdigit(ch) && (ch != '-' || tp > token))
				type = LISP_SYMBOL;
			tp++;
			tlen++;
			ch = input();
		}
		*tp = '\0';
		ASSERT(tlen > 0);
		tend = token + tlen;

		/* unput next character if not space */
		if (ch != EOF && !isspace(ch))
			unput(ch);

		/* make sure we understand the proper type */
		if (type == LISP_FIXNUM) {
			/* make sure there are some digits in number */
			for (tp = token; *tp == '-' && tp < tend; tp++)
				;
			if (tp >= tend || tp > token + 1) {
				/* no digits; or too many minus signs */
				type = LISP_SYMBOL;
			} else {
				/* it is a number */
				if (*token == '0' ||
				    (*token == '-' && token[1] == '0'))
					sscanf(token, "%o", &fixnum);
				else
					fixnum = atoi(token);
				done.vl_type = LISP_FIXNUM;
				sfixnum(done.vl_data, fixnum);
				return (done);
			}
		}

		/* this must be a symbol */
		if (tlen == 3 && !strncmp(token, "nil", 3)) {
			/* the special type nil */
			return (v_nil);
		} else if (tlen == 1 && *token == 't') {
			/* the special symbol t */
			return (v_t);
		} else {
			/* a symbol (function/variable name) */
			done.vl_type = LISP_SYMBOL;
			pname = save_string(token, tlen);
			ssymbol(done.vl_data, save_symbol(pname));
			return (done);
		}
	}
	/* NOTREACHED */
}

static struct value
readmacro(expr, macro)
	struct value	expr;
	struct string	*macro;
{
	struct ccell	*one, *two;
	struct value	done;

	two = save_ccell();
	two->cc_tcar = expr.vl_type;
	two->cc_car = expr.vl_data;
	two->cc_tcdr = LISP_NIL;
	slist(two->cc_cdr, NULL);

	one = save_ccell();
	one->cc_tcar = LISP_SYMBOL;
	ssymbol(one->cc_car, save_symbol(macro));
	one->cc_tcdr = LISP_CONS;
	slist(one->cc_cdr, two);

	done.vl_type = LISP_CONS;
	slist(done.vl_data, one);
	return (done);
}

static char	NOREAD[] = "Channel %d isn't open for reading!";

creadc(chan)
	struct channel	*chan;
{
	int	ch;

	if ((chan->ch_flags & CHAN_READ) == 0)
		error(NOREAD, chan->ch_number);

	curchan = chan;
	ch = input();
	curchan = NULL;

	return (ch);
}

struct value
cratom(chan)
	struct channel	*chan;
{
	struct value	sexpr;

	if ((chan->ch_flags & CHAN_READ) == 0)
		error(NOREAD, chan->ch_number);
	curchan = chan;
	sexpr = _rdatom();
	curchan = NULL;

	return (sexpr);
}

struct value
cread(chan)
	struct channel	*chan;
{
	struct value	sexpr;

	if ((chan->ch_flags & CHAN_READ) == 0)
		error(NOREAD, chan->ch_number);

	curchan = chan;
	sexpr = _rdsexpr();
	curchan = NULL;

	return (sexpr);
}

#define FAKEICHAN	-1

struct value
fread(fd)
{
	struct channel	chan;

	/* use a file channel, but don't place it in the list */
	chan.ch_number = FAKEICHAN;
	chan.ch_type = CHAN_FILE;
	chan.ch_flags = CHAN_READ;
	chan.ch_iofd = fd;
	chan.ch_inptr = chan.ch_inend = chan.ch_input;
	chan.ch_next = NULL;

	return cread(&chan);
}

struct value
sread(str)
	struct string	*str;
{
	struct channel	chan;

	/* make sure there is something to do */
	if (str == NULL || str->st_length < 1 || str->st_buffer == NULL)
		return (v_nil);

	/* use a file channel with the string as the buffer */
	chan.ch_number = FAKEICHAN;
	chan.ch_type = CHAN_FILE;
	chan.ch_flags = CHAN_READ;
	chan.ch_iofd = -1;
	chan.ch_inptr = str->st_buffer;
	chan.ch_inend = chan.ch_inptr + str->st_length;
	chan.ch_next = NULL;

	return cread(&chan);
}

static int
magic()
{
	unsigned char	number[10], *cp;
	unsigned int	n, ch;

	ch = input();
	if (ch == EOF) {
		/* EOF in escape sequence */
		return (p_escapec);
	} else if (isdigit(ch)) {
		cp = number;
		while (cp - number <= 3 && isdigit(ch)) {
			if (ch > '7')
				error(
			    "Non octal digit in octal character code!");
			*cp++ = ch;
			ch = input();
		}
		if (ch != EOF && !isdigit(ch))
			unput(ch);
		*cp = '\0';
		sscanf(number, "%o", &n);
		return (n);
	} else {
		switch (ch) {
		case 'n':	/* newline */
			return ('\n');
		case 'r':	/* carriage return */
			return ('\r');
		case 't':	/* tab */
			return ('\t');
		case 'b':	/* backspace */
			return ('\b');
		case 'f':	/* form feed */
			return ('\f');
		case 'e':	/* escape */
			return ('\033');
		case '^':	/* control */
			ch = input();
			if (ch == EOF) {
				/* EOF in escape sequence */
				return ('^');
			} else if (ch == '"') {
				/* premature end-of-string */
				error(
			    "Unfinished control character specification!");
			} else if (ch == '?') {
				/* the delete character */
				return ('\177');
			} else {
				/* make a control character */
				return (ch & 037);
			}
		case '@':	/* mouse button */
		case 'm':
			return rmouse();
		default:	/* regular character */
			return (ch);
		}
	}
}

static int
rmouse()
{
	unsigned int	ch, code;

	code = 0;
	ch = input();
	while (ch == p_escapec) {
		ch = input();
		if (ch == '^')
			code |= MOUSE_CTRL;
		else if (ch == 'm')
			code |= MOUSE_META;
		else if (ch == 's')
			code |= MOUSE_SHIFT;
		else
			error("Malformed mouse button specification.");
		ch = input();
	}
	if (isupper(ch)) {
		ch = tolower(ch);
		code |= MOUSE_SHIFT;
	}
	if (isoctal(ch)) {
		/* mouse button specified by number */
		code |= MOUSE_BUTTON(ch - '0');
	} else {
		switch (ch) {
		case 'l':
			code |= MOUSE_LEFT;
			break;
		case 'm':
			code |= MOUSE_MIDDLE;
			break;
		case 'r':
			code |= MOUSE_RIGHT;
			break;
		default:
			error("Invalid mouse button specification.");
		}
	}

	return (code);
}
