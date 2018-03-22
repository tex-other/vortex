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
 *  RCS Info: $Header: debug.c,v 0.1 87/05/01 11:28:52 john Locked $
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
 *  debug.c - internal debugging routines and functions
 */
static char _ID[] = "@(#)debug.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: backtrace
 *  Call: (backtrace [ 'count ])
 *  Retu: nil
 *  Desc: This function prints a function call stack backtrace into
 *	the buffer \lit{*backtrace*}.  Normally the entire stack is
 *	traced, but if the optional argument is given, only that many
 *	levels down are shown.
 *  SeeA: debug
 */
MKSTRING(BACKTRACE_BUF, "*backtrace*");

DEFUN(dobacktrace, "backtrace", FLAG_NONE, "")
{
	struct value	arg;
	struct stack	*stack;
	register int	level, bottom;
	struct buffer	*bufp;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		bottom = call_depth - gfixnum(arg.vl_data);
		if (bottom < 1)
			bottom = 1;
	} else {
		/* whole stack */
		bottom = 1;
	}

	/* write the stack backtrace into the buffer */
	bufp = buffer_create(BACKTRACE_BUF, BUFF_SOURCE, BUFF_KILLOK);
	bappend(bufp, "Lisp call frame backtrace\n");
	bappend(bufp, "-------------------------\n");
	level = call_depth;
	for (stack = stack_ptr; level >= bottom; stack--) {
		bappend(bufp, "Level %-4d cmd: %v, list: %v\n",
			level, stack->st_value, stack->st_argl);
		level--;
	}
	if (level == 0) {
		/* now print the top-level pseudo-call frame */
		bappend(bufp, "Level zero cmd: --%s top level--\n",
			havedisplay ? "VorTeX" : "vLisp");
	}

	/* set the buffer read-only and pop it up on the display */
	bufp->bu_flags &= ~BUFF_MODIFIED;
	bufp->bu_flags |= BUFF_CHANGED|BUFF_NEWMODE|BUFF_READONLY;
	popto_buffer(bufp, FALSE);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: debug
 *  Call: (debug [ 'state ... ])
 *  Retu: fixnum
 *  Desc: This function enables and disables debugging of the editor
 *	itself.  It is meant for the program maintainer, and is probably
 *	not useful to the user.
 *
 *	There are several different debugging ``states'', any of
 *	which may be active at once.  To enable a certain debugging
 *	state, its name should be given as an argument, in the form
 *	of a string.  To disable a debugging state, the name should
 *	be given preceded with a dash.
 *	
 *	The known debugging state names are:
 *	\tab{alloc	storage allocation
 *	paint	painting buffers in windows
 *	xio	I/O with X window system
 *	call	all function calls
 *	stack	changes to lisp call stack
 *	proc	\sc{UNIX} sub-process management
 *	read	the vlisp reader
 *	input	visual editor keyboard input
 *	throw	catch/throw manipulations
 *	buffer	editor buffer manipulations
 *	window	editor window manipulations
 *	conn	interprocess communications
 *	iTeX	interactive \TeX{} connection
 *	proof	proof editor connection
 *	all	all states at once}
 *
 *	Case is insensitive when specifying these debugging state
 *	names.  For example, to enable all debugging states, give
 *	the argument \lit{all} and to disable all debugging states,
 *	give \lit{-all}.
 *
 *	States may also be specified by fixnum arguments.  These
 *	set the states to the single argument, rather than adding
 *	or subtracting single states.  Bits set in the fixnum
 *	represent debugging states internally.
 *
 *	The function returns a fixnum representing the bit flags of
 *	the currently set debugging state.
 *  Side: Messages will be printed to the tty standard error output
 *	at various times as debugging messages are triggerd by the
 *	current state of debugging.
 */

struct dbgnam {
	char		*dn_name;
	int		dn_bitop;
	long		dn_bits;
};
#define BITOP_SET	1
#define BITOP_CLR	0

static struct dbgnam	debug_names[] = {
	{ "alloc",	BITOP_SET,	DALLOC },
	{ "-alloc",	BITOP_CLR,	DALLOC },
	{ "paint",	BITOP_SET,	DPAINT },
	{ "-paint",	BITOP_CLR,	DPAINT },
	{ "xio",	BITOP_SET,	DXIO },
	{ "-xio",	BITOP_CLR,	DXIO },
	{ "call",	BITOP_SET,	DCALL },
	{ "-call",	BITOP_CLR,	DCALL },
	{ "stack",	BITOP_SET,	DSTACK },
	{ "-stack",	BITOP_CLR,	DSTACK },
	{ "proc",	BITOP_SET,	DPROC },
	{ "-proc",	BITOP_CLR,	DPROC },
	{ "read",	BITOP_SET,	DREAD },
	{ "-read",	BITOP_CLR,	DREAD },
	{ "input",	BITOP_SET,	DINPUT },
	{ "-input",	BITOP_CLR,	DINPUT },
	{ "throw",	BITOP_SET,	DTHROW },
	{ "-throw",	BITOP_CLR,	DTHROW },
	{ "buffer",	BITOP_SET,	DBUFFER },
	{ "-buffer",	BITOP_CLR,	DBUFFER },
	{ "window",	BITOP_SET,	DWINDOW },
	{ "-window",	BITOP_CLR,	DWINDOW },
	{ "conn",	BITOP_SET,	DCONN },
	{ "-conn",	BITOP_CLR,	DCONN },
	{ "itex",	BITOP_SET,	DITEX },
	{ "-itex",	BITOP_CLR,	DITEX },
	{ "format",	BITOP_SET,	DITEX },
	{ "-format",	BITOP_CLR,	DITEX },
	{ "proof",	BITOP_SET,	DPROOF },
	{ "-proof",	BITOP_CLR,	DPROOF },
	{ "all",	BITOP_SET,	DALL },
	{ "-all",	BITOP_CLR,	DALL },
	{ NULL,		0,		0 },
};

DEFUN(dodebug, "debug", FLAG_NONE, "sWhat: ")
{
	struct value	arg, ret;
	struct string	*str;
	register int	n, count;
	char		name[28];

	count = GETACOUNT();
	for (n = 1; n <= count; n++) {
		arg = EVALARGN(n);
		switch (arg.vl_type) {
		case LISP_FIXNUM:
			debug_states = gfixnum(arg.vl_data);
			break;
		case LISP_STRING:
			str = gstring(arg.vl_data);
			makecstring(str, name, sizeof (name));
			int_debug(name);
			break;
		case LISP_SYMBOL:
			str = gsymbol(arg.vl_data)->sy_pname;
			makecstring(str, name, sizeof (name));
			int_debug(name);
			break;
		default:
			BADARGN(n, "a debug state");
		}
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, debug_states);
	return (ret);
}

#define isdsep(c)	(isspace(c) || (c) == ',' || (c) == '|')

int_debug(name)
	char	*name;
{
	struct dbgnam	*dptr;
	register char	*cp;
	char		nbuf[128];

	/* copy given string into private storage and lower case it */
	strncpy(nbuf, name, sizeof (nbuf) - 1);
	nbuf[sizeof (nbuf) - 1] = '\0';
	for (cp = nbuf; *cp != '\0'; cp++) {
		if (isupper(*cp))
			*cp = tolower(*cp);
	}
	name = nbuf;

	while (isdsep(*name))
		name++;
	while (*name != '\0') {
		for (cp = name; *cp != '\0' && !isdsep(*cp); cp++)
			;
		if (*cp != '\0')
			*cp++ = '\0';
	
		/* look for this name in our list of known state names */
		for (dptr = debug_names; dptr->dn_name != NULL; dptr++)
			if (!strcmp(dptr->dn_name, nbuf))
				break;
		if (dptr->dn_name == NULL)
			error("Unknown debug state name %s!", name);
	
		/* perform the appropriate operation */
		if (dptr->dn_bitop == BITOP_SET)
			debug_states |= dptr->dn_bits;
		else
			debug_states &= ~dptr->dn_bits;

		/* skip any separator characters to next word */
		name = cp;
		while (isdsep(*name))
			name++;
	}

	return (debug_states);
}
