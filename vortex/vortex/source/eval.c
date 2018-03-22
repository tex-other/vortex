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
 *  RCS Info: $Header: eval.c,v 0.1 87/05/01 12:10:28 john Locked $
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
 *  eval.c - internal sexpr evaluation routine
 */
static char _ID[] = "@(#)eval.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"
#include "buffer.h"

struct value
evalsexpr(sexpr)
	struct value	sexpr;
{
	struct value	call_function();
	struct value	cerror();
	struct value	cmd, fval, args, ret;
	struct string	*sname;

	if (dtprp(sexpr)) {
		/* get command name, or something for error message */
		cmd = car(sexpr);
		args = cdr(sexpr);
feval:		/* evaluate first element in list for the function */
		if (symbolp(cmd)) {
			sname = gsymbol(cmd.vl_data)->sy_pname;
			fval = get_variable(sname, current_buffer);
			if (eq(fval, NOVALUE)) {
				cmd = cerror("Unbound symbol %Y as function.",
					     sname);
				/* try for a function again */
				goto feval;
			}
		} else {
			/* evaluate this as a list */
			fval = evalsexpr(cmd);
		}
		while (!funcp(fval)) {
			fval = cerror("Non-function %v called as function.",
				      cmd);
			/* go around again */
		}

		/* run this as a lisp function */
		ret = call_function(cmd, fval, args);
	} else if (symbolp(sexpr)) {
		/* evaluate the symbol as a variable */
		sname = gsymbol(sexpr.vl_data)->sy_pname;
		ret = get_variable(sname, current_buffer);
		if (eq(ret, NOVALUE)) {
			ret = cerror("Unbound symbol %Y to evaluate.", sname);
			/* if we get here, user continued */
		}
		/* else we got a value */
	} else {
		/* just the value of the s-expression */
		ret = sexpr;
	}
	return (ret);
}
