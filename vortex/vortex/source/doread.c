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
 *  RCS Info: $Header: doread.c,v 0.1 87/05/01 11:59:51 john Locked $
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
 *  doread.c - vLisp input functions
 */
static char _ID[] = "@(#)doread.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: read
 *  Call: (read [ 'channel ])
 *  Retu: any
 *  Desc: This function returns the next lisp expression that
 *	can be read on the given channel (\sym{stdin} default).
 *	The characters are examined, and if the first non-space
 *	character is an open parenthesis, a list is expected,
 *	otherwise a symbol is read.  The lisp expression read
 *	is then returned.  All read functions return nil to signal
 *	end-of-file, which implies that nil should not appear in
 *	the outer level of a file to be processed with \sym{read}.
 *	If the optional argument is given, it should evaluate to
 *	a channel number, on which the characters are to be read.
 *	Otherwise, the characters are expected to come from the
 *	terminal (standard input) without editor synchronization.
 *
 *	Read determines the syntax of the characters it finds
 *	on the input stream according to a syntax specified
 *	(and changed) by the \sym{syntax} function.  See \sym{syntax}
 *	for a description of what each syntax character means.
 *	For example, the fact that \lit{(} begins a list is a
 *	consequence of the fact that the syntax \lit{blist} has
 *	been assigned the character \lit{(}, \sc{ASCII} 40.
 *  SeeA: ratom readc syntax channel
 */

DEFUN(doread, "read", FLAG_NONE, NULL)
{
	struct value	arg1, val;
	struct channel	*chan = cstdin;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg1 = EVALARGN(1);
		if (!fixnump(arg1))
			BADARGN(1, "a channel number");
		chan = get_channel(gfixnum(arg1.vl_data));
	}

	val = cread(chan);
	if (eq(val, NOVALUE))
		val = v_nil;
	return (val);
}

/*
 *  DOCUMENTATION
 *
 *  Name: ratom
 *  Call: (ratom [ 'channel ])
 *  Retu: atom
 *  Desc: This function reads the next atomic token from the
 *	given channel (\sym{stdin} default) and returns it.
 *	This atom may be anything except a list or nil.  In fact,
 *	a nil return means end-of-input.  If the optional argument
 *	is given, it should evaluate to a channel number, from
 *	which the atom is to be read.  Otherwise, the characters
 *	are expected to come from the terminal, with no editor
 *	synchronization.
 *  SeeA: read readc channel
 */

DEFUN(doratom, "ratom", FLAG_NONE, NULL)
{
	struct value	arg1, val;
	struct channel	*chan = cstdin;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg1 = EVALARGN(1);
		if (!fixnump(arg1))
			BADARGN(1, "a channel number");
		chan = get_channel(gfixnum(arg1.vl_data));
	}

	/* do actual read */
	val = cratom(chan);
	if (eq(val, NOVALUE))
		val = v_nil;
	return (val);
}

/*
 *  DOCUMENTATION
 *
 *  Name: readc
 *  Call: (readc [ 'channel ])
 *  Retu: fixnum
 *  Desc: This function reads the next character off the given
 *	input channel (\sym{stdin} default) and returns it as
 *	an \sc{ASCII} code.  A return value of nil signals the
 *	end-of-input.  If the optional argument is given, it should
 *	evaluate to a channel number, on which to read.  Otherwise,
 *	the character is read from the standard input, without
 *	editor synchronization.
 *  SeeA: read ratom channel
 */

DEFUN(doreadc, "readc", FLAG_NONE, NULL)
{
	struct value	arg1, val;
	struct channel	*chan = cstdin;
	int		got;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg1 = EVALARGN(1);
		if (!fixnump(arg1))
			BADARGN(1, "a channel number");
		chan = get_channel(gfixnum(arg1.vl_data));
	}

	got = creadc(chan);
	if (got == EOF) {
		/* read end-of-file on this channel */
		return (v_nil);
	} else {
		val.vl_type = LISP_FIXNUM;
		sfixnum(val.vl_data, got);
		return (val);
	}
}
