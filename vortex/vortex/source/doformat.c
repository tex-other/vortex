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
 *  RCS Info: $Header: doformat.c,v 0.1 87/05/01 11:47:22 john Locked $
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
 *  doformat.c - format arguments into a string
 */
static char _ID[] = "@(#)doformat.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "format.h"

/*
 *  DOCUMENTATION
 *
 *  Name: format
 *  Call: (format 'string [ 'arg ... ])
 *  Retu: string
 *  Desc: This function formats a list of arguments into a string,
 *	which is returned, according to the instructions in the first
 *	argument.  It needs at least one argument, the format.
 *	This first argument must evaluate to a string, which specifies
 *	how the returned string is to be produced.  This format string
 *	should contain normal characters, which are copied to the
 *	output string, and field character, which specifies how a
 *	matching evaluated argument is to be formatted in the output
 *	string.
 *
 *	An argument is expected for each field specification in the
 *	format string, all of which are evaluated.  Format specifications
 *	appear as a percent sign (\lit{%}) optionally follwed by a
 *	number specifying the minimum field with, then an optional
 *	period and another number specifying the maximum field width.
 *	Then, one of the characters below must appear.  These field
 *	specification characters specify how the matching argument
 *	is to be formatted.  Note that it's illegal to have characters
 *	other than one of these after a percent sign in the format
 *	string.
 *
 *	\tab{The field specifiers recognized are:
 *	d	fixnum as a decimal number
 *	o	fixnum as a octal number
 *	x	fixnum as a hexadecimal number
 *	b	fixnum as a binary number
 *	r	fixnum as a roman numeral
 *	s	string or symbol as text
 *	v	any value as its print image
 *	c	fixnum as an \sc{ASCII} character
 *	C	fixnum as character in printable form
 *	k	fixnum as a key stroke
 *	K	string as a key sequence}
 *
 *	Between the format character (\lit{%}) and the format letter
 *	there may be a number, a period and another number.  These
 *	specify the minimum and maximum field widths of the entry
 *	in the output.  Either number may be missing, which means
 *	that dimension is unbounded  The number may be replaced
 *	with a single asterisk (\lit{*}, which specifies that the next
 *	argument will specify that width.  Normally, all fields are
 *	right adjusted, if a minus sign (\lit{-}) appears before the
 *	field width numbers, it causes the field to be left-adjusted,
 *	similarly, if a pipe symbol (\lit{|}) appears, it causes the
 *	contents of the field to be centered.  A plus sign (\lit{+})
 *	specifies right adjustment, which is the default.
 *
 *  END
 *
 *  Note that the maximum field width is 128 characters, fields
 *  longer than this are truncated.  So, this is not a complete
 *  alternative to print.  This maximum width can be increased
 *  by increasing the size of the static buffers, which are
 *  currently of size SMALLBUF.
 */

DEFUN(doformat, "format", FLAG_NONE, NULL)
{
	extern char		*psexpr(), *nthname();
	struct value		ret, arg;
	register unsigned char	*tp;
	unsigned char		text[BIGBUF], *tend = text + sizeof (text);
	int			adj, min, max, code;
	register int		acount, fcount;
	struct string		*str;
	unsigned char		*fp, *fend;
	char			*cptr;

	CHECKAC(1, -1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a format string");
	str = gstring(arg.vl_data);
	fend = str->st_buffer + str->st_length;
	fp = str->st_buffer;
	tp = text;
	acount = 1;
	fcount = 0;
	while (fp < fend && tp < tend) {
		if (*fp == '%') {
			fcount++;
			/* extract field information from format */
			code = scanfield(&fp, fend - fp, &adj, &min, &max);
			if (code < 0) {
				error(
	    "The format ended before the %s field specification was complete!",
				      nthname(fcount));
			}
			if (min == WID_GETARG) {
				acount++;
				arg = EVALARGN(acount);
				if (!fixnump(arg))
					goto badarg;
				min = gfixnum(arg.vl_data);
				if (min < 0)
					min = 0;
			}
			if (max == WID_GETARG) {
				acount++;
				arg = EVALARGN(acount);
				if (!fixnump(arg))
					goto badarg;
				max = gfixnum(arg.vl_data);
				if (max < 0)
					max = 0;
			}
			if (code == '%') {
				*tp++ = code;
				continue;
			}

			/* go to next argument for value */
			acount++;
			arg = EVALARGN(acount);

			/* format value for output */
			switch (code) {
			case FMT_DECIMAL:
				if (!fixnump(arg))
					goto badarg;
				cptr = itoa(gfixnum(arg.vl_data), 10);
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_OCTAL:
				if (!fixnump(arg))
					goto badarg;
				cptr = utoa(gfixnum(arg.vl_data), 8);
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_HEX:
				if (!fixnump(arg))
					goto badarg;
				cptr = utoa(gfixnum(arg.vl_data), 16);
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_BINARY:
				if (!fixnump(arg))
					goto badarg;
				cptr = utoa(gfixnum(arg.vl_data), 2);
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_ROMAN:
				if (!fixnump(arg))
					goto badarg;
				cptr = itor(gfixnum(arg.vl_data));
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_STRING:
				if (symbolp(arg))
					str = gsymbol(arg.vl_data)->sy_pname;
				else if (stringp(arg))
					str = gstring(arg.vl_data);
				else
					goto badarg;
				if (str->st_length > 0) {
					tp += prtfield(str->st_buffer,
						       str->st_length,
						       tp, tend - tp,
						       adj, min, max);
				}
				break;
			case FMT_VALUE:
				cptr = psexpr(arg);
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_CHAR:
				if (!fixnump(arg))
					goto badarg;
				*tp++ = gfixnum(arg.vl_data) & 0377;
				break;
			case FMT_PNTCHAR:
				if (!fixnump(arg))
					goto badarg;
				cptr = pchar(gfixnum(arg.vl_data));
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_KEYCODE:
				if (!fixnump(arg))
					goto badarg;
				cptr = pkeycode(gfixnum(arg.vl_data));
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			case FMT_KEYSEQ:
				if (!stringp(arg))
					goto badarg;
				str = gstring(arg.vl_data);
				cptr = pkeyseq(str);
				tp += prtfield(cptr, strlen(cptr),
					       tp, tend - tp, adj, min, max);
				break;
			default:	/* some regular type */
				error(
			"Unknown format specification type %%%s for %s field!",
				      pchar(code), nthname(fcount));
			}
		} else {
			/* just copy character into output */
			*tp++ = *fp++;
		}
	}

	/* return the string we generated */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, save_string(text, tp - text));
	return (ret);

badarg:	/* bad value type for field */
	error("Argument type mismatch for the %s field specification.",
	      nthname(fcount));
	/* NOTREACHED */
}
