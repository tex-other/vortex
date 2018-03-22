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
 *  RCS Info: $Header: doforall.c,v 0.1 87/05/01 11:47:05 john Locked $
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
 *  doforall.c - forall-variables forall-buffers and forall-windows functions
 */
static char _ID[] = "@(#)doforall.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "symtab.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: forall-variables
 *  Call: (forall-variables 'function)
 *  Retu: fixnum
 *  Desc: This function executes the specified function once for
 *	each symbol in the global oblist.  The given function should
 *	take one argument, the symbol name as a symbol, and may
 *	return any value.  Then, \sym{forall-variables} returns a
 *	count of times the it was called (thus the number of bound
 *	symbols) as a fixnum.
 *
 *	This has the effect of calling the function given as the
 *	first argument for each element in the list with that
 *	element as the argument.  Thus, evaluating the expression
 *	\lit{(forall-variables 'patom)} would call the function
 *	\sym{patom} once for every bound symbol in the oblist, each
 *	time with that variable's name as the single argument.
 *  Side: This function is necessary since there is no way to get all
 *	the names of the bound symbols (of course, this could be written
 *	in terms of \sym{forall-variables}).  This function is much more
 *	efficient than a \lit{(mapcar function (oblist))} would be since
 *	no list of symbols need be made.
 *  Xref: oblist variable-list
 *  SeeA: set remob make-local-variable
 */

DEFUN(doforallvars, "forall-variables", FLAG_NONE, NULL)
{
	struct value	arg, fval, val;
	register int	count;
	struct symtab	*sym;
	register int	ind;

	CHECKAC(1, 1);

	/* get the function, but also set up print name */
	arg = GETARGN(1);
	fval = evalsexpr(arg);
	if (!funcp(fval))
		BADARGN(1, "a function");

	/* call the function repeatedly on symbol names */
	val.vl_type = LISP_SYMBOL;
	count = 0;
	for (ind = 0; ind < sym_tabsize; ind++)
		for (sym = sym_table[ind]; sym != NULL; sym = sym->st_next) {
			/* make the symbol from the name *
			name = save_symbol(sym->st_name);
			ssymbol(val.st_data, name);
			/* evaluate this function call */
			call_function(arg, fval, val);
			count++;
		}

	val.vl_type = LISP_FIXNUM;
	sfixnum(val.vl_data, count);
	return (val);
}

/*
 *  DOCUMENTATION
 *
 *  Name: forall-buffers
 *  Call: (forall-buffers 'function)
 *  Retu: count
 *  Desc: This function causes the specified function to be called once
 *	for each buffer currently active in the editor.  The function is
 *	called with one argument, the name of the buffer.  The buffers
 *	are accessed in order, from most to least recently touched.
 *
 *	A count of the number of buffers seen is returned.
 *  Side: Any side effects of the user-specified function for each
 *	current buffer.  Each buffer is guaranteed to have a unique name
 *	by the buffer manager.
 *  Xref: buffer-list
 *  SeeA: switch-to-buffer kill-buffer
 */

DEFUN(doforallbufs, "forall-buffers", FLAG_NONE, NULL)
{
	struct value	arg0, fval, arg, ret;
	struct buffer	*bufp;
	register int	count;

	CHECKAC(1, 1);
	arg0 = GETARGN(1);
	fval = evalsexpr(arg0);
	if (!funcp(fval))
		BADARGN(1, "a function");

	count = 0;
	arg.vl_type = LISP_STRING;
	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
		ASSERT(bufp->bu_name != NULL);
		sstring(arg.vl_data, bufp->bu_name);
		call_function(arg0, fval, arg);
		count++;
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: forall-windows
 *  Call: (forall-windows 'function)
 *  Retu: count
 *  Desc: This function calls the user specified function (given by the
 *	first argument) with each active window's handle (a fixnum) one by one.
 *	This allows the user to perform some simple action on each window.
 *	\sym{forall-windows} itself returns the number of windows found.
 *
 *	The function specified by the user should take one argument, the
 *	window number.  It may return any value, which will be thrown
 *	away.
 *  Side: Any side effects of the user-specified function will occur
 *	once for each active window as the user's function is called.
 *  Xref: window-list
 *  SeeA: current-window
 */

DEFUN(doforallwins, "forall-windows", FLAG_NONE, NULL)
{
	struct value	arg0, fval, arg, ret;
	struct window	*win;
	register int	count;

	CHECKAC(1, 1);
	arg0 = GETARGN(1);
	fval = evalsexpr(arg0);
	if (!funcp(fval))
		BADARGN(1, "a function");

	count = 0;
	arg.vl_type = LISP_FIXNUM;
	for (win = window_list; win != NULL; win = win->wi_next) {
		if ((win->wi_flags & WIN_ACTIVE) != 0) {
			sfixnum(arg.vl_data, win->wi_index);
			call_function(arg0, fval, arg);
			count++;
		}
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}
