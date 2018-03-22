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
 *  RCS Info: $Header: doremob.c,v 0.1 87/05/01 12:00:12 john Locked $
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
 *  doremob.c - delete a symbol from symbol table
 */
static char _ID[] = "@(#)doremob.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: remob
 *  Call: (remob 'symbol [ 'buffer ])
 *  Retu: t
 *  Desc: This function removes a binding at any level to the symbol
 *	named by the evaluated argument.  All bindings, no matter how
 *	deeply nested, are removed, but only in the global oblist.
 *	It is as if this symbol was never bound globally.
 *
 *	If a buffer name is given as the second argument, the symbol
 *	is removed in that buffer's local list, not in the global
 *	oblist.
 *
 *	\sym{Remob} evaluates its argument, so you must quote it
 *	if the name of the symbol if it is given directly.
 *	For example, to delete \em{sym}, you would use
 *	\lit{(remob 'sym)}.
 *  SeeA: set make-local-variable forall-variables forall-buffers
 */

DEFUN(doremob, "remob", FLAG_NONE, NULL)
{
	struct value	arg;
	struct string	*str;
	struct buffer	*bufp = NULL;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	str = gsymbol(arg.vl_data)->sy_pname;
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
		remlocal(str, &bufp->bu_locals);
	} else {
		/* remove the symbol globally */
		remglobal(str);
	}
	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: remove-local-variables
 *  Call: (remove-local-variables [ 'buffer ])
 *  Retu: nil
 *  Desc: This function removes all buffer-local bindings for the
 *	current buffer (or the buffer specified by the optional
 *	argument, if given).  No local bindings will exist in
 *	this buffer at all.
 *  Side: If a symbol is marked as local (i.e., with
 *	\sym{declare-variable-local}) and set from this
 *	buffer, that variable will be created local to this
 *	buffer again when set.
 *  Xref: kill-all-local-variables
 *  SeeA: remob set make-local-variable declare-variable-local
 */

DEFUN(doremovelocals, "remove-local-variables", FLAG_NONE, NULL)
{
	struct value	arg;
	struct buffer	*bufp = current_buffer;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}
	remlocal(NULL, &bufp->bu_locals);
	return (v_nil);
}
