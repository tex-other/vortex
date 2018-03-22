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
 *  RCS Info: $Header: doprint.c,v 0.1 87/05/01 11:58:38 john Locked $
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
 *  doprint.c - vLisp output functions
 */
static char _ID[] = "@(#)doprint.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: print
 *  Call: (print 'any [ 'channel ])
 *  Retu: t
 *  Desc: This function writes the image of a lisp expression
 *	on the given I/O channel.  The first argument should
 *	evaluate to a lisp expression, which is printed.  The
 *	optional second argument should evaluate to a channel
 *	open for writing.  If the argument is left out, the
 *	special channel \sym{stdout} is used, which normally
 *	send characters to the terminal without synchronization
 *	with the editor.
 *
 *	This function prints out lisp symbols just as one would
 *	type them in.  Quoted symbols are preceded with a single
 *	quote, lists are surrounded by parenthesis, strings are
 *	surrounded with double quotes.  Fixnums print as one would
 *	expect, flonums always have at least one number after the
 *	decimal point to distinguish them from fixnums.  All ``funny''
 *	characters in strings or symbols are escaped and symbols
 *	containing funny characters or spaces are surrounded by
 *	pipe symbols (\lit{|}).
 *  Xref: write
 *  SeeA: patom princ terpri channel
 */

DEFUN(doprint, "print", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct channel	*chan = cstdout;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (GETACOUNT() > 1) {
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a channel number");
		chan = get_channel(gfixnum(arg2.vl_data));
	}

	cprint(arg1, chan);

	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: patom
 *  Call: (patom 'atom [ 'channel ])
 *  Retu: t
 *  Desc: This function puts a single string out to the given channel,
 *	(\sym{stdout} default), without doing the escaping done by
 *	\sym{print}.  The first argument must evaluate to a string
 *	or a symbol, which gives the characters to be printed
 *	exactly.  No escaping of ``funny'' characters or spaces is done.
 *	If the optional second argument is given, it specifies the
 *	output channel to write the string onto, otherwise it will
 *	be sent to the terminal, without any cooperation of the
 *	editor.
 *  SeeA: print princ terpri channel
 */

DEFUN(dopatom, "patom", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct channel	*chan = cstdout;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (GETACOUNT() > 1) {
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a channel number");
		chan = get_channel(gfixnum(arg2.vl_data));
	}

	switch (arg1.vl_type) {
	case LISP_SYMBOL:
		cpatom(gsymbol(arg1.vl_data)->sy_pname, chan);
		break;
	case LISP_STRING:
		cpatom(gstring(arg1.vl_data), chan);
		break;
	default:
		cprint(arg1, chan);
		break;
	}

	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: princ
 *  Call: (princ 'fixnum [ 'channel ])
 *  Retu: t
 *  Desc: This function sends a single \sc{ASCII} character to
 *	the given output channel (\sym{stdout} default).  This
 *	character can either be a fixnum, corespoinding to the
 *	\sc{ASCII} character code, or a string of which the first
 *	character will be printed.  If the optional channel
 *	argument is given, it specifies the channel on which to
 *	write this character, otherwise the character will be sent
 *	to the termianl with no editor synchronization.
 *  SeeA: print patom terpri channel
 */

DEFUN(doprinc, "princ", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct channel	*chan = cstdout;
	int		code;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (GETACOUNT() > 1) {
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a channel number");
		chan = get_channel(gfixnum(arg2.vl_data));
	}

	if (!fixnump(arg1))
		BADARGN(1, "a character code");
	code = gfixnum(arg1.vl_data) & 0377;
	cprinc(code, chan);

	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: terpri
 *  Call: (terpri [ 'channel ])
 *  Retu: t
 *  Desc: This function sends a newline character (\sc{ASCII} 10)
 *	to the given output channel (\sym{stdout} default).  This
 *	usually means that a new line is created in the output file
 *	at this point.  Also, most files should end with a newline.
 *	If the optional channel argument is given, it specifies an
 *	output port to write the newline to, otherwise it will go
 *	to the terminal, without any screen coordination.
 *  SeeA: print patom princ channel
 */

DEFUN(doterpri, "terpri", FLAG_NONE, NULL)
{
	struct value	arg;
	struct channel	*chan = cstdout;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a channel number");
		chan = get_channel(gfixnum(arg.vl_data));
	}

	cprinc('\n', chan);
	cflush(chan);

	return (v_t);
}
