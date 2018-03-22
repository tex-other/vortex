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
 *  RCS Info: $Header: doerror.c,v 0.1 87/05/01 11:43:52 john Locked $
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
 *  doerror.c - basic error reporting routines and functions
 */
static char _ID[] = "@(#)doerror.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"
#include "catch.h"
#include "format.h"
#include "channel.h"

struct value	haderror();	/* internal error handler */

/*
 *  This is the error message passed to ierror() when an assertion
 *  (using the ASSERT macro) fails.  This message should expect two
 *  arguments, the file name and line number in the source.
 */
char	ASSERT_MSG[] = "Assertion failed! Source file \"%s\", line %d.";

/*
 *  DOCUMENTATION
 *
 *  Name: error
 *  Call: (error 'message)
 *  Desc: This function prints an error message on the minibuf
 *	window and aborts back to the top level.  Its argument
 *	must evaluate to a string, the text of the error message,
 *	or nil to mean no message.  This function never returns;
 *	control aborts back up to the top-level.
 *
 *	If the variable \sym{break-on-error} is set non-nil, then
 *	instead of \sym{throw}ing to the catch tag \lit{error},
 *	a break loop is entered.  The break loop is a special
 *	read/eval/print loop with a different prompt.  One can
 *	examine the state at the time of the error.  To exit the
 *	break loop and return to the top level, one calls the
 *	function \sym{reset}.
 *  SeeA: cerror message break-on-error debug-on-error
 */

DEFUN(doerror, "error", FLAG_NONE, NULL)
{
	struct value	arg;
	char		msg[STRBUF];

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (stringp(arg))
		makepstring(gstring(arg.vl_data), msg, sizeof (msg));
	else if (nullp(arg))
		*msg = '\0';
	else
		BADARGN(1, "a string message");

	/* handle error; can break, can't continue */
	haderror(msg, TRUE, FALSE);
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: cerror
 *  Call: (error 'message)
 *  Retu: any
 *  Desc: This function prints an error message on the minibuf
 *	window and aborts back to the top level.  Its argument
 *	must evaluate to a string, the text of the error message,
 *	or nil to mean no message.  This type of error is different
 *	from a regular \sym{error} in that it is \em{continuable}.
 *	This means that one can specify a value to return from a
 *	break loop to continue the interrupted function.
 *
 *	If the variable \sym{break-on-error} is set non-nil, then
 *	instead of \sym{throw}ing to the catch tag \lit{error},
 *	a break loop is entered.  The break loop is a special
 *	read/eval/print loop with a different prompt.  One can
 *	examine the state at the time of the error.  To exit the
 *	break loop and return to the top level, one calls the
 *	function \sym{reset}, of, because this is a continuable
 *	error, one can call the function \sym{cont} to specify a
 *	value to return as the value of this call to \sym{cerror}.
 *  SeeA: error message break-on-error debug-on-error
 */

DEFUN(docerror, "cerror", FLAG_NONE, NULL)
{
	struct value	arg;
	char		msg[STRBUF];

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (stringp(arg))
		makepstring(gstring(arg.vl_data), msg, sizeof (msg));
	else if (nullp(arg))
		*msg = '\0';
	else
		BADARGN(1, "a string message");

	/* handle error; can break and cant continue */
	return (haderror(msg, TRUE, TRUE));
}

/*
 *  These routines are called by the C programmer directly to
 *  report problems with VorTeX.  They all take variable number
 *  of arguments in the style of printf(3).  The error functions
 *  do a (throw 'error message), and the error catch is responsible
 *  for doing the right thing.
 *
 *  If we're reading a file, errfile and errline will be set up
 *  properly with the current file name and line number so that we
 *  can give a more informative error message.  In this case these
 *  values are prepended to the normal error message.
 *
 *  In general, we throw to the error catch, which is at top-level.
 *  However, we sometimes perform a break loop, for debugging.  If
 *  the variable "break-on-error" is set non-nil, we do a break loop
 *  from the error handler.  And, if the error is a continuable one,
 *  a cerror(), we can return a different value to the interpreter
 *  and continue.  Or, if the variable "debug-on-error" is set non-nil
 *  we write a backtrace into a buffer and do the break loop.
 */
char	*errfile = NULL;		/* file name for error printing */
int	errline = -1;			/* line number for error printing */

extern struct string	*ERRORCATCH;

/* VARARGS */
error(va_alist)
	va_dcl
{
#include "fmtdecl.h"

	if (errfile != NULL) {
		sprintf(msgbuf, "\"%s\", line %d: ",
			basename(errfile), errline);
		mp = msgbuf + strlen(msgbuf);
	}

#include "fmtcode.h"

	/* handle error; can break but can't continue */
	haderror(msgbuf, TRUE, FALSE);
	ASSERT(FALSE);
}

/* VARARGS */
perror(va_alist)
	va_dcl
{
	extern int	errno, sys_nerr;
	extern char	*sys_errlist[];
#include "fmtdecl.h"
	char		*err;

	if (errfile != NULL) {
		sprintf(msgbuf, "\"%s\", line %d: ",
			basename(errfile), errline);
		mp = msgbuf + strlen(msgbuf);
	}

#include "fmtcode.h"

	mend--;
	err = errno < sys_nerr ? sys_errlist[errno] : "Unknown error";
	if (strlen(err) + 2 <= mend - mp) {
		*mp++ = ':';
		*mp++ = ' ';
		while (*err != '\0')
			*mp++ = *err++;
	}
	if (mp < mend)
		*mp++ = '.';
	*mp = '\0';

	/* handle error; can break but can't continue */
	haderror(msgbuf, TRUE, FALSE);
	ASSERT(FALSE);
}

/* VARARGS */
struct value
cerror(va_alist)
	va_dcl
{
#include "fmtdecl.h"

	if (errfile != NULL) {
		sprintf(msgbuf, "\"%s\", line %d: ",
			basename(errfile), errline);
		mp = msgbuf + strlen(msgbuf);
	}

#include "fmtcode.h"

	/* handle error; can break and can continue */
	return haderror(msgbuf, TRUE, TRUE);
}

static char	ERROR_STR[] = "ERROR: ";

/* VARARGS */
ierror(va_alist)
	va_dcl
{
#include "fmtdecl.h"

	strcpy(msgbuf, ERROR_STR);
	mp = msgbuf + sizeof (ERROR_STR) - 1;
	if (errfile != NULL) {
		sprintf(mp, "\"%s\", line %d: ",
			basename(errfile), errline);
		mp = msgbuf + strlen(msgbuf);
	}

#include "fmtcode.h"

	/* handle error; can't break and can't continue */
	haderror(msgbuf, FALSE, FALSE);
	ASSERT(FALSE);
}

static char	PANIC_STR[] = "PANIC: ";

/* VARARGS */
panic(va_alist)
	va_dcl
{
	static int	panicing = FALSE;
#include "fmtdecl.h"

	/* make sure we don't get interrupted */
	alarm(0);

	/* make sure we aren't called recursively */
	if (panicing)
		exit(1);
	panicing = TRUE;

	strcpy(msgbuf, PANIC_STR);
	mp = msgbuf + sizeof (PANIC_STR) - 1;
	if (errfile != NULL) {
		sprintf(mp, "\"%s\", line %d: ",
			basename(errfile), errline);
		mp = msgbuf + strlen(msgbuf);
	}

#include "fmtcode.h"

	write(STDERR, msgbuf, strlen(msgbuf));
	write(STDERR, "\r\n", 2);
	int_exit(TRUE);

	/* shouldn't get to here! */
	exit(1);
}

overflow(what, max)
	char	*what;
{
	static char	msgbuf[128];

	if (max > 0) {
		sprintf(msgbuf,
	    "Whoops!  Overflowed %s (maximum is %d).  Can't continue.",
		    what, max);
	} else {
		/* not given a maximum amount */
		sprintf(msgbuf,
		    "Whoops!  Overflowed %s.  Can't continue.", what);
	}
	
	/* handle error; can't break and can't continue */
	haderror(msgbuf, FALSE, FALSE);
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: break-on-error
 *  Desc: This variable, if set non-nil, causes the thread of control
 *	to pass to a break loop when an error occurs.  The break loop
 *	is just like the top-level read/eval/print loop except that
 *	all the state, up to the point of error, is still available
 *	for inspection.
 *
 *	If the error which invoked the break loop is a continuable one
 *	(called \sym{cerror} rather than \sym{error}), the additional
 *	command \sym{cont} is available, to return to the main thread
 *	of control.  Otherwise, the only way to exit the break loop is
 *	with \sym{reset}.
 *  SeeA: error cerror debug-on-error
 */
MKSTRING(BREAKVAR, "break-on-error");

extern struct value	break_loop();

static char	SWARN[] = "VorTeX: Error during initialization; aborting.";
static char	QWARN[] = "VorTeX: Error during cleanup processing; aborting.";
static char	PWARN[] = "VorTeX: Error in a sub-process; child is aborting.";
static char	EWARN[] = "VorTeX: Error without error handling; aborting.";

#define ERRPRINT(s)	write(STDERR, (s), strlen(s)); \
			write(STDERR, ELINE, sizeof (ELINE) - 1)
static char	ELINE[] = "\n\r";

struct value
haderror(message, canbreak, cancont)
	char	*message;
{
	struct value	sval;
	int		dobr = FALSE;

	/* make sure we don't get interrupted by the abort check */
	alarm(0);

	if (message == NULL || *message == '\0')
		message = "<unknown error occurred>";

	if (!initialized) {
		ERRPRINT(message);
		ERRPRINT(SWARN);
		exit(1);
	}
	if (quitting) {
		ERRPRINT(message);
		ERRPRINT(QWARN);
		exit(1);
	}
	if (subprocess) {
		ERRPRINT(message);
		ERRPRINT(PWARN);
		exit(1);
	}

	if (truevar(BREAKVAR))
		dobr = TRUE;

	/* print the error message if we're debugging the display */
	debug(DPAINT, "(error) %s", message);

	/* handle the error as appropriate */
	if (canbreak && dobr) {
		/* perform a break loop */
		err_message(message);
		return break_loop(cancont, message);
	} else if (get_catch(ERRORCATCH) == NULL) {
		/* this shouldn't happen */
		ERRPRINT(message);
		ERRPRINT(EWARN);
		exit(1);
	} else {
		/* throw to error catch */
		sval.vl_type = LISP_STRING;
		sstring(sval.vl_data, save_string(message, strlen(message)));
		int_throw(ERRORCATCH, sval);
	}
	/* NOTREACHED */
}

longjmperror()
{
	panic("Whoops, longjmp just failed somewhere!");
	exit(1);
}
