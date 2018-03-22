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
 *  RCS Info: $Header: args.c,v 0.1 87/05/01 11:23:45 john Locked $
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
 *  args.c - vLisp function argument handling
 */
static char _ID[] = "@(#)args.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  This function makes sure that the number of arguments given
 *  is in the range we expected.  If not, it generates an error
 *  with a nice message, otherwise it returns the number of
 *  arguments it got.  This is meant to be called with an
 *  nlambda argument list as passed in the lisp call frame.
 *  If there is no maximum number of arguments, max should be
 *  given as -1, if min and max are equal, only that number
 *  of arguments are allowed, otherwise they specify a range
 *  of arguments permissible.  In every case, a good error
 *  message is generated, this function should be used as
 *  much as possible, also to save repeated strings for
 *  error messages.
 */

checkacount(fname, given, min, max)
	char	*fname;
{
	extern char	*numname();

	if ((min > max && max >= 0) || min < 0 || given < 0) {
		ierror("checkacount(\"%s\", %d, %d, %d): Bad arguments.",
		    fname, given, min, max);
	}

	if (min == max && given != min) {
		switch (min) {
		case 0:
			error("Function %s takes no arguments.", fname);
		case 1:
			error("Function %s takes just one argument.", fname);
		default:
			error("Function %s takes %s arguments.",
			    fname, numname(min));
		}
	} else if (given < min && max < 0) {
		error("Function %s takes at least %s argument%s.",
		    fname, numname(min), PLURAL(min));
	} else if (given > max && min <= 0) {
		error("Function %s takes no more than %s argument%s.",
		    fname, numname(max), PLURAL(max));
	} else if (given < min || (max > 0 && given > max)) {
		if (min + 1 == max) {
			error("Function %s takes %s or %s argument%s.",
			    fname, numname(min), numname(max), PLURAL(max));
		} else {
			error("Function %s takes %s to %s arguments.",
			    fname, numname(min), numname(max));
		}
	} else {
		/* proper number of arguments */
		return (given);
	}
	/* NOTREACHED */
}

/*
 *  This function returns the nth argument in the given list,
 *  raising an appropriate error if the argument doesn't exist.
 *  This is meant to be called through the macros defined in
 *  "vlisp.h", not directly.
 */

struct value
getntharg(fname, args, n)
	char		*fname;
	struct value	args;
{
	extern char	*nthname();

	if (fname == NULL || eq(args, NOVALUE) || n <= 0) {
		ierror("getntharg(0x%x, 0x%x, %d): Bad arguments!",
		    fname, args, n);
	}

	if (dtprp(args)) {
		if (length(args) < n - 1)
			error("Missing %s argument for function %s.",
			    nthname(n), fname);
		return nth(n - 1, args);
	} else if (nullp(args)) {
		/* there's no argument we can get */
		error("No arguments passed to %s at all!", fname);
	} else if (n == 1) {
		/* return the value, not a list */
		return (args);
	} else {
		/* can't get more than first argument of non-list */
		error("Only a single argument passed to %s!", fname);
	}
	/* NOTREACHED */
}

/*
 *  This function returns the nth argument in the given list,
 *  raising an appropriate error if the argument doesn't exist
 *  or the evaluation fails. This is meant to be called through
 *  the macros defined in "vlisp.h", not directly.
 */

struct value
evalntharg(fname, args, n)
	char		*fname;
	struct value	args;
{
	extern char	*nthname();
	struct value	arg;

	if (fname == NULL)
		fname = "<null>";
	if (n <= 0)
		ierror("evalntharg(\"%s\", %v, %d): Bad argument number!",
		    fname, args, n);

	/* use GETARGN function getntharg to check for errors */
	arg = getntharg(fname, args, n);
	return evalsexpr(arg);
}

/*
 *  This function reports that the nth argument to fname is wrong,
 *  it should be should.  The should string should contain the
 *  type expected along with the appropriate article; example:
 *  "a mode string".  This is meant to be called through the
 *  macros defined in "vlisp.h", not directly.
 */

badntharg(fname, n, should)
	char	*fname, *should;
{
	extern char	*nthname();

	error("The %s argument to function %s should be %s.",
	    nthname(n), fname, should);
}
