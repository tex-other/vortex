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
 *  RCS Info: $Header: doset.c,v 0.1 87/05/01 12:01:50 john Locked $
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
 *  doset.c - vLisp set and similar functions
 */
static char _ID[] = "@(#)doset.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: set
 *  Call: (set 'symbol 'value)
 *  Retu: value
 *  Desc: This function associates the given symbol name with the
 *	given value.  Both argument are evaluated, the first must
 *	evaluate to a symbol, the second may be of any type.
 *
 *	Veriable names can have various values at ``different levels''
 *	of the interpreter.  For example, if there is a global symbol
 *	called \sym{foo}, bound to \lit{(a b c)}, and one calls a
 *	function which has an argument named \sym{foo}, inside the
 *	execution of that function the symbol \sym{foo} will evaluate
 *	to the value given as that particular argument to the function,
 *	not to the globally bound value.  Once the function returns,
 *	the globally bound value, \lit{(a b c)}, will be restored.
 *
 *	If a veriable is bound at any depth which has never before
 *	been bound, it becomes a global symbol.  This is a special
 *	case of the fact that the closest upward variable binding
 *	is changed whenever \sym{set}.
 *
 *	The user can implicitly create new binding levels by naming
 *	function arguments as described above, or explicitly declare
 *	local variables with the function \sym{let}.  This function
 *	creates local variables which only have the initialized values
 *	until that invocation of \sym{let} returns.
 *
 *	Note that \sym{et} (like \sym{setq}) will bind the variable
 *	locally in the current buffer if that variable has already been
 *	defined local to the buffer and global (as described above)
 *	otherwise.
 *  SeeA: setq let setenv
 */

DEFUN(doset, "set", FLAG_NONE, NULL)
{
	struct value	name, what;
	struct symbol	*sym;

	CHECKAC(2, 2);
	name = EVALARGN(1);
	if (!symbolp(name))
		BADARGN(1, "a symbol name");
	what = EVALARGN(2);
	sym = gsymbol(name.vl_data);
	ASSERT(sym != NULL);
	set_variable(sym->sy_pname, what, FLAG_NONE);

	return (what);
}

/*
 *  Set the variable according to the scoping rules taking buffer
 *  local bindings into account.  If the variable exists locally,
 *  set it there.  If it doesn't, but the flag STAB_LOCAL is set
 *  in the global binding, set it locally otherwise, set it
 *  globally.
 */
set_variable(name, value, flags)
	struct string	*name;
	struct value	value;
{
	struct symtab	*sym = NULL;
	struct buffer	*buf = current_buffer;

	ASSERT(name != NULL);
	if (buf != NULL) {
		/* check if this variable is local to the buffer */
		sym = getlocal(name, buf->bu_locals);
		if (sym != NULL || (flags & STAB_LOCAL) != 0) {
			setlocal(name, value, flags & ~STAB_LOCAL,
				 &buf->bu_locals);
			return (0);
		}
	}

	/* check if this variable is globally buffer local */
	sym = getglobal(name);
	if (sym != NULL && (sym->st_flags & STAB_LOCAL) != 0 && buf != NULL) {
		/* create this variable local to this buffer */
		setlocal(name, value, flags, &buf->bu_locals);
		return (0);
	}

	/* otherwise set the variable globally */
	setglobal(name, value, flags);
	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: setq
 *  Call: (setq symbol 'value)
 *  Retu: value
 *  Desc: This function binds the given symbol name to the given
 *	vlisp value.  The symbol name argument is not evaluated,
 *	but the value is.  Other than this, this function is
 *	entirely the same as \sym{set}.
 *
 *	Also, \sym{setq} has a buffer local variable binder
 *	\sym{local-setq} just as \sym{set} has \sym{local-set}.
 *  SeeA: set let
 */

DEFUN(dosetq, "setq", FLAG_NONE, "vSet variable: '\nlto value: ")
{
	struct value	name, what;
	struct symbol	*sym;

	CHECKAC(2, 2);
	name = GETARGN(1);
	if (!symbolp(name))
		BADARGN(1, "a symbol name");
	what = EVALARGN(2);
	sym = gsymbol(name.vl_data);
	ASSERT(sym != NULL);
	set_variable(sym->sy_pname, what, FLAG_NONE);

	return (what);
}

/*
 *  DOCUMENTATION
 *
 *  Name: make-local-variable
 *  Call: (make-local-variable 'symbol [ 'buffer ])
 *  Retu: t
 *  Desc: This function creates an instance of the variable named
 *	by the first argument local to the current buffer (or the
 *	specified buffer if the second argument is given).  If the
 *	variable is not set locally to the buffer, nothing is done
 *	otherwise it is initialized locally to the global value or
 *	nil if not set globally.
 *  Side: In future calls to \sym{set}, etc. in this buffer, the
 *	variable will be changed local to the buffer, not globally.
 *  SeeA: set let declare-variable-local
 */

DEFUN(domakelocalvar, "make-local-variable", FLAG_NONE, "vVariable name: ")
{
	struct value	arg;
	struct buffer	*buf = current_buffer;
	struct string	*name;
	struct symtab	*sym;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	name = gsymbol(arg.vl_data)->sy_pname;
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a string buffer name");
		buf = buffer_get(gstring(arg.vl_data), TRUE);
	}

	sym = getlocal(name, buf->bu_locals);
	if (sym != NULL)
		return (v_t);
	sym = getglobal(name);
	if (sym != NULL) {
		/* set the variable locally with the global value */
		setlocal(name, sym->st_value,
			 sym->st_flags & ~STAB_PERM, &buf->bu_locals);
	} else {
		/* set the variable locally to nil */
		setlocal(name, v_nil, FLAG_NONE, &buf->bu_locals);
	}
	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: declare-variable-local
 *  Call: (declare-variable-local 'symbol)
 *  Retu: t
 *  Desc: This function specifies that the given variable will
 *	always be local to buffers when set in the future.  If
 *	the variable does not exist globally, it is created and
 *	initialized to nil there.
 *  Side: The variable is made local to the appropriate buffer
 *	when next set, \em{not} when \sym{declare-variable-local}
 *	is called.
 *  SeeA: set make-local-variable
 */

DEFUN(dodeclarevarloc, "declare-variable-local", FLAG_NONE, "vVariable name: ")
{
	struct value	arg;
	struct symtab	*sym;
	struct string	*name;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	name = gsymbol(arg.vl_data)->sy_pname;

	sym = getglobal(name);
	if (sym == NULL) {
		/* set the variable globally to nil marking it local */
		setglobal(name, v_nil, STAB_LOCAL);
	} else {
		/* just add the local flag */
		sym->st_flags |= STAB_LOCAL;
	}
	return (v_t);
}
