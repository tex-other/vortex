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
 *  RCS Info: $Header: dopp.c,v 0.1 87/05/01 11:58:15 john Locked $
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
 *  dopp.c - vLisp pretty printer function
 */
static char _ID[] = "@(#)dopp.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: pp
 *  Call: (pp 'function [ 'channel ])
 *  Retu: t
 *  Desc: This function prints out a vlisp value in a format
 *	that looks nice.  User defined functions are printed
 *	specially and all other values are printed prettily.
 *	The first argument is evaluated, and that expression
 *	is printed.  If it is a list, its elements are printed
 *	properly indented recursively.
 *
 *	The optional second argument specifies a channel to write
 *	the output to.  By default the channel \sym{stdout} is used,
 *	which just prints onto the terminal with no screen
 *	synchronization.
 *
 *	If a function value is given to print, but that function
 *	is a builtin (written in C and compiled), it can't be
 *	pretty printed so a message to this effect is printed
 *	instead (but not onto the given output channel).
 *  SeeA: defun print
 */

DEFUN(dopp, "pp", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, exp;
	struct channel	*chan = cstdout;
	struct function	*func;
	register int	i, len, count;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (GETACOUNT() > 1) {
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a channel number");
		chan = get_channel(gfixnum(arg2.vl_data));
		if ((chan->ch_flags & CHAN_WRITE) == 0)
			error("Channel %d isn't open for writing!",
			    chan->ch_number);
	}
	if (funcp(arg1)) {
		func = gfunct(arg1.vl_data);
		if ((func->fn_flags & FUNC_BUILTIN) != 0) {
			message("Function is a builtin, can't pretty print.");
			return (v_nil);
		}
		ASSERT(!eq(func->fn_body, NOVALUE));

		/* print out function discipline */
		switch (func->fn_disc) {
		case DISC_LAMBDA:
			scpatom("(lambda", chan);
			break;
		case DISC_NLAMBDA:
			scpatom("(nlambda", chan);
			break;
		case DISC_LEXPR:
			scpatom("(lexpr", chan);
			break;
		case DISC_MACRO:
			scpatom("(macro", chan);
			break;
		default:
			scpatom("(unknown", chan);
			break;
		}

		/* print out argument list */
		if (func->fn_argc == 0) {
			/* no arguments, just use nil */
			scpatom(" nil", chan);
		} else if (func->fn_argc == 1 &&
			   func->fn_disc != DISC_LAMBDA) {
			cprinc(' ', chan);
			cpatom(func->fn_alist[0], chan);
		} else {
			scpatom(" (", chan);
			for (i = 0; i < func->fn_argc; i++) {
				cpatom(func->fn_alist[i], chan);
				if (i < func->fn_argc - 1)
					cprinc(' ', chan);
			}
			cprinc(')', chan);
		}
		cprinc('\n', chan);

		/* print out the body s-exprs */
		if (!dtprp(func->fn_body)) {
			/* this shouldn't be the case */
			scpatom("  ", chan);
			ppform(func->fn_body, 2, chan);
		} else {
			len = length(func->fn_body);
			for (count = 0; count < len; count++) {
				exp = nth(count, func->fn_body);
				scpatom("  ", chan);
				ppform(exp, 2, chan);
				cprinc('\n', chan);
			}
		}
		cprinc(')', chan);
		cprinc('\n', chan);
	} else {
		/* print out the given s-expr, whatever it is */
		ppform(arg1, 0, chan);
		cprinc('\n', chan);
	}

	return (v_t);
}

static int	pp_cols = 79, pp_indent = 2;

ppform(sexpr, margin, chan)
	struct value	sexpr;
	struct channel	*chan;
{
	register int	i, cnt, len, current;
	struct value	elt;

	if (sexpr.vl_type != LISP_CONS || pntlen(sexpr) < pp_cols - margin) {
		/* just use the standard printing function */
		return cprint(sexpr, chan);
	}

	cprinc('(', chan);
	current = margin + 1;
	len = length(sexpr);
	for (cnt = 0; cnt < len; cnt++) {
		elt = nth(cnt, sexpr);
		if (cnt > 0 &&
		    (listp(elt) || pntlen(elt) >= pp_cols - current)) {
			current = margin + pp_indent;
			cprinc('\n', chan);
			for (i = 1; i < current; i++)
				cprinc(' ', chan);
		} else if (cnt > 0) {
			/* just output a single space */
			cprinc(' ', chan);
			current++;
		}
		ppform(elt, current, chan);
		current += pntlen(elt);
	}
	cprinc(')', chan);

	return (0);
}

static int
pntlen(sexpr)
	struct value	sexpr;
{
	extern char	*psexpr();
	char		*str;
	register int	n, total, count;

	if (dtprp(sexpr)) {
		total = 0;
		count = length(sexpr);
		for (n = 0; n < count; n++)
			total += pntlen(nth(n, sexpr));
		/* count parenthesis and spaces */
		total += count + 1;
	} else if (dtprp(sexpr)) {
		total = 0;
		count = length(sexpr);
		for (n = 0; n < count; n++)
			total += pntlen(aindex(sexpr, n));
		/* count brackets and spaces */
		total += count + 1;
	} else {
		str = psexpr(sexpr);
		total = strlen(str);
	}
	return (total);
}
