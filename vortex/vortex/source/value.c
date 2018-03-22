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
 *  RCS Info: $Header: value.c,v 0.1 87/05/01 12:32:12 john Locked $
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
 *  value.c - standard vLisp value initializations
 */
static char _ID[] = "@(#)value.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "value.h"
#include "function.h"

/*
 *  Here are the special values that are used often enough that
 *  a value for them is worth keeping around.  We need to set these
 *  up at run time because the C compiler doesn't properly
 *  initialize bit fields.  Also, we can't use the standard
 *  DEFVALUE mechanism from "vlisp.h" because v_t needs more
 *  processing than just assigning to the data field, we need
 *  to intern the symbol it points to.
 *
 *  These values are all safe from the user's machinations,
 *  except for v_t (the symbol t).  If the user ever does a
 *  (remob t), the internal reference v_t will be probably be
 *  invalidated, since the string buffer pointed to can then
 *  be garbage.
 */
struct value	v_t, v_nil, v_zero, v_one, v_four, v_neg, v_null;
struct value	NOVALUE;

standard_values()
{
	struct string	*str;

	str = save_string("t", 1);
	str->st_perm = TRUE;
	v_t.vl_type = LISP_SYMBOL;
	ssymbol(v_t.vl_data, save_symbol(str));

	v_nil.vl_type = LISP_NIL;
	slist(v_nil.vl_data, NULL);

	v_zero.vl_type = LISP_FIXNUM;
	sfixnum(v_zero.vl_data, 0);

	v_one.vl_type = LISP_FIXNUM;
	sfixnum(v_one.vl_data, 1);

	v_four.vl_type = LISP_FIXNUM;
	sfixnum(v_four.vl_data, 4);

	v_neg.vl_type = LISP_FIXNUM;
	sfixnum(v_neg.vl_data, -1);

	v_null.vl_type = LISP_STRING;
	str = save_string(NULL, 0);
	str->st_perm = TRUE;
	sstring(v_null.vl_data, str);

	return (0);
}

/*
 *  This function is called for each value at the time the
 *  builtin symbols are being inserted into the symbol table.
 *  All it needs to do is fix up the function values, as the
 *  other types should be statically initialized properly.
 *  This function is called by builtin_symbols(), which is
 *  generated automatically by mksyms.
 */

struct value
fix_init_value(val)
	struct value	val;
{
	struct function	*func;
	char		buf[50];

	if (funcp(val)) {
		/* do the right things with this function */
		if ((func = gfunct(val.vl_data)) == NULL)
			panic("Null function struct for builtin function!");
		if (func->fn_pname == NULL) {
			/* not yet converted */
			sprintf(buf, "#%x-nlambda", (unsigned)func->fn_funct);
			func->fn_pname = save_string(buf, strlen(buf));
			/* handle interactive arguments, if present */
			if (*func->fn_prompt != NULL)
				parse_istring((char *)*func->fn_prompt, func);
		}
	}
	return (val);
}
