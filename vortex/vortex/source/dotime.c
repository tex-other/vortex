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
 *  RCS Info: $Header: dotime.c,v 0.1 87/05/01 12:05:11 john Locked $
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
 *  dotime.c - functions to report the UNIX time
 */
static char _ID[] = "@(#)dotime.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/param.h>
#include <sys/time.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: time
 *  Call: (time)
 *  Retu: symbol
 *  Desc: This function returns the current time as a symbol
 *	(created by \sym{gensym}) whose property list is set
 *	so that property/value pairs correspond to the fields
 *	of the time struct returned by the \em{localtime(3)}
 *	library routine.
 *
 *	The fields of the time structure (and the names of
 *	the returned symbol's properties) are:
 *
 *	\tab{sec	the number of seconds in current minute
 *	min	the number of minutes in current hour
 *	hour	the number of hours in current day
 *	mday	the day of the month
 *	mon	the current month (a zero-based index)
 *	year	the year since 1900
 *	wday	the day of the week (a zero-based index)
 *	yday	the day of the year (1 to 366)
 *	isdst	t if daylight savings time, otherwise nil}
 *
 *	All the fields above have as their value a fixnum, except
 *	for \lit{isdst}, which is t or nil.  Below is code which
 *	defines a function which returns the day of the week, as
 *	a symbol.
 *
 *	\tab{\lit{(setq day-names '(Sunday Monday Tuesday Wednesday}
 *	\lit{                   Thursday Friday Saturday)}
 *	\lit{(defun day-of-week ()}
 *	\lit{  (let ((tret (time))}
 *	\lit{        (wday 0))}
 *	\lit{   (setq wday (getprop tret 'wday))}
 *	\lit{   (nth wday day-names)))}}
 *
 *	The most interesting thing here is that we're using the
 *	property list facilities of vlisp to emulate the aggregate
 *	data structures of other languages.
 *  Side: The substantial property list of the new symbol takes a
 *	fair amount of space to store.
 *  SeeA: getprop getplist gensym
 */

DEFUN(dotime, "time", FLAG_NONE, NULL)
{
	struct value	vtime();
	unsigned long	clock;

	CHECKAC(0, 0);

	if (time(&clock) == -1)
		perror("Can't get the current time");

	return vtime(clock);
}

/*
 *  We take the UNIX time given in the passed word and expand
 *  it into the (struct tm) from which we make the property
 *  list of the symbol to return as the result of this function.
 *  This stuff is described above in the documentation to the
 *  vlisp function ``time''.
 */
MKSTRING(TIMELEADER, "%time");

static struct tm	tmbuf;

static struct sandv {
	char		*sv_name;
	short		sv_nlen;
	short		sv_predp;
	int		*sv_addr;
} time_pnames[] = {
	{ "isdst",	5,	TRUE,		&tmbuf.tm_isdst },
	{ "yday",	4,	FALSE,		&tmbuf.tm_yday },
	{ "wday",	4,	FALSE,		&tmbuf.tm_wday },
	{ "year",	4,	FALSE,		&tmbuf.tm_year },
	{ "mon",	3,	FALSE,		&tmbuf.tm_mon },
	{ "mday",	4,	FALSE,		&tmbuf.tm_mday },
	{ "hour",	4,	FALSE,		&tmbuf.tm_hour },
	{ "min",	3,	FALSE,		&tmbuf.tm_min },
	{ "sec",	3,	FALSE,		&tmbuf.tm_sec },
	{ NULL,		0,	FALSE,		NULL }
};

struct value
vtime(clock)
	long	clock;
{
	struct string	*sgensym();
	struct value	ret, plist;
	struct value	pname, value;
	struct tm	*tmptr;
	struct symbol	*sym;
	struct string	*name;
	struct sandv	*svp;

	/* get the broken-out fields of the time */
	if ((tmptr = localtime(&clock)) == NULL)
		perror("Can't get the local time");
	bcopy(tmptr, &tmbuf, sizeof (struct tm));

	/* make up the property list now */
	plist = v_nil;
	for (svp = time_pnames; svp->sv_name != NULL; svp++) {
		/* make up value part of property pair */
		if (svp->sv_predp) {
			if (svp->sv_addr == 0)
				value = v_nil;
			else
				value = v_t;
		} else {
			value.vl_type = LISP_FIXNUM;
			sfixnum(value.vl_data, *svp->sv_addr);
		}
		plist = cons(value, plist);
		/* make up name of property */
		pname.vl_type = LISP_SYMBOL;
		name = save_string(svp->sv_name, svp->sv_nlen);
		ssymbol(pname.vl_data, save_symbol(name));
		plist = cons(pname, plist);
	}

	/* make up the new symbol to return */
	name = sgensym(TIMELEADER);
	sym = save_symbol(name);
	sym->sy_plist = plist;

	/* return this new symbol */
	ret.vl_type = LISP_SYMBOL;
	ssymbol(ret.vl_data, sym);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: timecmp
 *  Call: (timecmp 'symbol 'symbol)
 *  Retu: fixnum
 *  Desc: This function takes two time values, both of which must
 *	evaluate to symbols whose property lists are fields of
 *	a time (see the \sym{time} function).  It returns -1, zero,
 *	or 1 if the first is before, the same or later than the
 *	second time.
 *  SeeA: time stat
 */

DEFUN(dotimecmp, "timecmp", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct symbol	*atm1, *atm2;
	int		tmv1, tmv2;

	CHECKAC(2, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a time symbol");
	atm1 = gsymbol(arg1.vl_data);
	arg2 = EVALARGN(2);
	if (!symbolp(arg2))
		BADARGN(2, "a time symbol");
	atm2 = gsymbol(arg2.vl_data);

	return (v_zero);
}
