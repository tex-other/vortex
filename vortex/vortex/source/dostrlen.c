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
 *  RCS Info: $Header: dostrlen.c,v 0.1 87/05/01 12:04:18 john Locked $
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
 *  dostrlen.c - various string handling functions
 */
static char _ID[] = "@(#)dostrlen.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: strlen
 *  Call: (strlen 'string)
 *  Retu: fixnum
 *  Desc: This function returns the number of characters in the
 *	string to which its argument must evaluate.  This length
 *	may not be the same as the print length of the string,
 *	since ``funny'' characters in the string still only add
 *	one to the length, but will be printed as two or more
 *	characters on output.
 *  SeeA: substr stridx stridxr stringp
 */

DEFUN(dostrlen, "strlen", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct string	*str;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string");
	str = gstring(arg.vl_data);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, str->st_length);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: substr
 *  Call: (substr 'position 'length 'string)
 *  Retu: string
 *  Desc: This function extracts part of a string from the longer
 *	to which its last argument must evaluate to.  The first
 *	and second argument must evaluate to fixnums and specify
 *	the portion of the string to select.  The first argument
 *	is the position to start, zero based.  The second is the
 *	number of characters to extract.
 *  SeeA: stridx stridxr strlen
 */

DEFUN(dosubstr, "substr", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	int		pos, len;
	struct string	*str, *new;
	unsigned char	*first;

	CHECKAC(3, 3);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a position fixnum");
	pos = gfixnum(arg.vl_data);
	arg = EVALARGN(2);
	if (!fixnump(arg) || (len = gfixnum(arg.vl_data)) < 1)
		BADARGN(2, "a positive length fixnum");
	arg = EVALARGN(3);
	if (!stringp(arg))
		BADARGN(3, "a string");
	str = gstring(arg.vl_data);

	if (pos < 0) {
		if (-pos > str->st_length)
			error("Negative position larger than string is!");
		if (len > -pos)
			error("Length greater than negative position!");
		first = str->st_buffer + (str->st_length + pos);
	} else {
		if (pos + len > str->st_length)
			error("String not long enough for that substring!");
		first = str->st_buffer + pos;
	}
	new = save_string(first, len);

	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, new);
	return(ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: stridx
 *  Call: (stridx 'pattern 'string)
 *  Retu: string or nil
 *  Desc: This function searches for the first string pattern in
 *	the second string, and returns a new string which begins
 *	with the given pattern and continues with the rest of the
 *	original string (which may contain more instances of the
 *	pattern).  Both arguments must evaluate to string, and
 *	neither may be the empty string.  If the pattern is not
 *	found at all, nil is returned.
 *  Xref: index
 *  SeeA: substr stridxr strlen
 */

DEFUN(dostridx, "stridx", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, ret;
	struct string	*pat, *str, *new;
	unsigned char	*sp, *ssp, *send;
	unsigned char	*pp, *pend;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!stringp(arg1))
		BADARGN(1, "a non-empty string");
	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(2, "a string");
	pat = gstring(arg1.vl_data);
	str = gstring(arg2.vl_data);
	if (pat->st_length < 1)
		error("Pattern string can't be empty!");

	pend = pat->st_buffer + pat->st_length;
	send = str->st_buffer + str->st_length;
	for (sp = str->st_buffer; sp < send; sp++) {
		pp = pat->st_buffer;
		ssp = sp;
		while (pp < pend && ssp < send) {
			/* compare next character */
			if (*pp != *ssp)
				break;
			pp++; ssp++;
		}
		if (ssp >= send) {
			/* remaining string isn't long enough */
			break;
		}
		if (pp >= pend) {
			/* found the match here */
			new = save_string(sp, send - sp);
			ret.vl_type = LISP_STRING;
			sstring(ret.vl_data, new);
			return (ret);
		}
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: stridxr
 *  Call: (stridxr 'pattern 'string)
 *  Retu: string or nil
 *  Desc: This function searches for the last string pattern in
 *	the second string, and returns a new string which begins
 *	with the given pattern and continues with the rest of the
 *	original string (which will not contain another match of
 *	the pattern).  Both arguments must evaluate to string,
 *	and neither may be the empty string.  If the pattern is
 *	not found at all, nil is returned.
 *  Xref: rindex
 *  SeeA: substr stridx strlen
 */

DEFUN(dostridxr, "stridxr", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, ret;
	struct string	*pat, *str, *new;
	unsigned char	*sp, *ssp, *send;
	unsigned char	*pp, *pend;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!stringp(arg1))
		BADARGN(1, "a non-empty string");
	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(2, "a string");
	pat = gstring(arg1.vl_data);
	str = gstring(arg2.vl_data);
	if (pat->st_length < 1)
		error("Pattern string can't be empty!");

	pend = pat->st_buffer + pat->st_length;
	send = str->st_buffer + str->st_length;
	sp = send - pat->st_length;
	while (sp >= str->st_buffer) {
		pp = pat->st_buffer;
		ssp = sp;
		while (pp < pend) {
			/* compare next character */
			if (*pp != *ssp)
				break;
			pp++; ssp++;
		}
		if (pp >= pend) {
			/* found the match here */
			new = save_string(sp, send - sp);
			ret.vl_type = LISP_STRING;
			sstring(ret.vl_data, new);
			return (ret);
		}
		sp--;
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: strmapc
 *  Call: (strmapc 'string 'function)
 *  Retu: count
 *  Desc: This function invokes the specified function on every
 *	character in the given string.  The function will be
 *	called repeatedly with its single argument bound to the
 *	\sc{ASCII} character value of that element of the string.
 *  Side: Any side effects of the function to be invoked on the
 *	characters in the string will occur as many times as the
 *	length of the string.  In fact, since one doesn't have
 *	access to the results of the individual evaluations, only
 *	the side effects of the function are of any practical use.
 *  SeeA: strlen mapc
 */

DEFUN(dostrmapc, "strmapc", FLAG_NONE, NULL)
{
	struct value	arg, fval;
	struct string	*str;
	unsigned char	*cp, *cend;

	CHECKAC(2, 2);

	/* get the function, but also set up print name */
	arg = GETARGN(1);
	fval = evalsexpr(arg);
	if (!funcp(fval))
		BADARGN(1, "a function");

	/* get second argument, must be a string */
	arg = EVALARGN(2);
	if (!stringp(arg))
		BADARGN(2, "a string");
	str = gstring(arg.vl_data);

	/* call the function with each character */
	arg.vl_type = LISP_FIXNUM;
	cend = str->st_buffer + str->st_length;
	for (cp = str->st_buffer; cp < cend; cp++) {
		sfixnum(arg.vl_data, *cp);
		call_function(arg, fval, arg);
	}
	sfixnum(arg.vl_data, str->st_length);
	return (arg);
}
