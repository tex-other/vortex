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
 *  RCS Info: $Header: dosopen.c,v 0.1 87/05/01 12:02:37 john Locked $
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
 *  dosopen.c - function to open a ``string channel''
 */
static char _ID[] = "@(#)dosopen.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: sopen
 *  Call: (sopen 'symbol 'mode)
 *  Retu: channel
 *  Desc: This function opens an I/O channel into a lisp string
 *	value.  That is, one can use the \sym{read} and \sym{print}
 *	functions to change the string value of a symbol.  The first
 *	argument must evaluate to the name of a symbol, whose bound value
 *	is to be changed by these actions.  The second argument should
 *	evaluate to a string mode, which describes how the string is to
 *	be accessed.  The mode should be one of \lit{"r"}, \lit{"w"} or
 *	\lit{a}; string channels may not be opened both for reading
 *	and writing.  If one is reading from the symbol, it must be
 *	currently bound to a string value.
 *
 *	Normally, I/O channels are buffered for the sake of efficiency,
 *	but this may be disabled by specifying \lit{"u"} in the mode
 *	string, which makes the channel unbuffered, or by manually
 *	\sym{flush}ing the channel when the string symbol should be
 *	brought up-to-date.
 *
 *	A string channel opened for reading with \lit{"r"} yields
 *	up the characters in its \sym{print} image when the channel
 *	is read from.  When writing to a string channel, characters
 *	are appended to the end.  If the channel was opened for
 *	writing with \lit{"w"}, the string is truncated first, if
 *	it was opened with \lit{"a"} it is not truncated.
 *  Side: There is a possibly nasty side effect when using this type
 *	of channel.  Since writing to a string in this way modifies
 *	a lisp symbol, other instances of this symbol (such as through
 *	local binding) may be changed unexpectedly.  It is best to use
 *	a symbol name unlikely to be used elsewhere or to create a
 *	unique name using \sym{gensym}.
 *  SeeA: read print close gensym channel
 */

DEFUN(dosopen, "sopen", FLAG_NONE, NULL)
{
	struct channel	*new;
	struct value	arg1, arg2, ret;
	struct string	*str;
	unsigned char	*sp, *send;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a symbol name");
	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(2, "a string mode");
	str = gstring(arg2.vl_data);

	/* make up flags and channel struct */
	new = (struct channel *)valloc(sizeof (struct channel));
	new->ch_type = CHAN_SYMBOL;
	new->ch_flags = FLAG_NONE;
	send = str->st_buffer + str->st_length;
	for (sp = str->st_buffer; sp < send; sp++) {
		switch (*sp) {
		case 'r':
			new->ch_flags |= CHAN_READ;
			break;
		case 'a':
			new->ch_flags |= CHAN_APPEND;
			/* fall through to 'w' case */
		case 'w':
			new->ch_flags |= CHAN_WRITE;
			break;
		case 'u':
			new->ch_flags |= CHAN_UNBUF;
			break;
		default:
			error("Bad characters in mode string to sopen.");
		}
	}
	if ((new->ch_flags & (CHAN_READ|CHAN_WRITE)) == 0)
		error("Must specify r, w or a in mode string.");
	if ((new->ch_flags & CHAN_READ) != 0 &&
	    (new->ch_flags & CHAN_WRITE) != 0)
		error("Can only either read or write a string, not both.");

	/* finish up channel struct now that we have a mode */
	new->ch_line = 1;
	new->ch_number = newchan();
	new->ch_symbol = gsymbol(arg1.vl_data)->sy_pname;
	new->ch_inptr = new->ch_inend = new->ch_input;
	new->ch_outptr = new->ch_output;
	set_channel(new);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, new->ch_number);
	return (ret);
}
