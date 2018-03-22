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
 *  RCS Info: $Header: dosearch.c,v 0.1 87/05/01 12:01:01 john Locked $
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
 *  dosearch.c - editor searching functions
 */
static char _ID[] = "@(#)dosearch.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: search-forward
 *  Call: (search-forward 'pattern [ 'repeat [ 'bound [ 'failok ] ] ])
 *  Retu: t or nil
 *  Desc: This function searches for the given string in the current
 *	buffer and returns t if the pattern is found, moving point to
 *	the end of the matched text of the buffer.  If the pattern
 *	isn't found, an error occurs.
 *
 *	If the optional second argument is given, it specifies a repeat
 *	count.  The search is done that many times, each search starting
 *	from the matched point of the previous one.  This repeat count
 *	should be a positive fixnum.
 *
 *	If the optional third argument is given, it specifies a bounding
 *	point for the search.  The normal end point is the end of the
 *	buffer (searces do not ``wrap around'').  The starting point of
 *	the search is always point in the buffer.  This must be a fixnum
 *	representing a point past the current position of point.
 *
 *	If the optional fourth argument is given and evaluates non-nil,
 *	nil is returned if the string is not found.  Normally an error
 *	message is printed if the pattern is not found to the bottom
 *	of the buffer.
 *  Side: If the string is found, point is changed in the buffer,
 *	otherwise an error occurs unless the fourth argument is given
 *	non-nil.
 *  SeeA: search-backward search-forward-regexp
 */

DEFUN(dosearchforwd, "search-forward", FLAG_NONE, "sSearch for: \np")
{
	struct value	arg;
	int		repeat = 1, failok = FALSE, bound;

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: search-backward
 *  Call: (search-backward 'pattern [ 'repeat [ 'bound [ 'failok ] ] ])
 *  Retu: t or nil
 *  Desc: This function searches for the given string in the current
 *	buffer and returns t if the pattern is found, moving point to
 *	the beginning of the matched text in the buffer.  If the pattern
 *	isn't found, an error occurs.
 *
 *	If the optional second argument is given, it specifies a repeat
 *	count.  The search is done that many times, each search starting
 *	from the matched point of the previous one.  This repeat count
 *	should be a positive fixnum.
 *
 *	If the optional third argument is given, it specifies a bounding
 *	point for the search.  The normal end point is the beginning of the
 *	buffer (searces do not ``wrap around'').  The starting point of
 *	the search is always point in the buffer.  This must be a fixnum
 *	representing an offset before the current position of point.
 *
 *	If the optional fourth argument is given and evaluates non-nil,
 *	nil is returned if the string is not found.  Normally an error
 *	message is printed if the pattern is not found to the bottom
 *	of the buffer.
 *  Side: If the string is found, point is changed in the buffer,
 *	otherwise an error occurs unless the fourth argument is given
 *	non-nil.
 *  SeeA: search-forward search-backward-regexp
 */

DEFUN(dosearchbackwd, "search-backward", FLAG_NONE, "sSearch back for: \np")
{
	struct value	arg;
	int		repeat = 1, failok = FALSE, bound;

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: search-forward-regexp
 *  Call: (search-forward-regexp 'pattern [ 'repeat [ 'bound [ 'failok ] ] ])
 *  Retu: t or nil
 *  Desc: This function searches for a match of the given regular expression
 *	in the current buffer and returns t if the pattern matches, moving
 *	point to the end of the matched text of the buffer.  If the pattern
 *	isn't matched, an error occurs.
 *
 *	If the optional second argument is given, it specifies a repeat
 *	count.  The search is done that many times, each search starting
 *	from the matched point of the previous one.  This repeat count
 *	should be a positive fixnum.
 *
 *	If the optional third argument is given, it specifies a bounding
 *	point for the search.  The normal end point is the end of the
 *	buffer (searces do not ``wrap around'').  The starting point of
 *	the search is always point in the buffer.  This must be a fixnum
 *	representing a point past the current position of point.
 *
 *	If the optional fourth argument is given and evaluates non-nil,
 *	nil is returned if the string is not found.  Normally an error
 *	message is printed if the pattern is not found to the bottom
 *	of the buffer.
 *  Side: If the string is found, point is changed in the buffer,
 *	otherwise an error occurs unless the fourth argument is given
 *	non-nil.
 *  SeeA: search-backward-regexp search-forward
 */

DEFUN(dosrchfwdregex, "search-forward-regexp", FLAG_NONE, "sSearch for: \np")
{
	struct value	arg;
	int		repeat = 1, failok = FALSE, bound;

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: search-backward-regexp
 *  Call: (search-backward 'pattern [ 'repeat [ 'bound [ 'failok ] ] ])
 *  Retu: t or nil
 *  Desc: This function searches for a match of the given regular
 *	expression in the current buffer and returns t if the pattern is
 *	matched, moving point to the beginning of the matched text in the
 *	buffer.  If the pattern isn't matched, an error occurs.
 *
 *	If the optional second argument is given, it specifies a repeat
 *	count.  The search is done that many times, each search starting
 *	from the matched point of the previous one.  This repeat count
 *	should be a positive fixnum.
 *
 *	If the optional third argument is given, it specifies a bounding
 *	point for the search.  The normal end point is the beginning of the
 *	buffer (searces do not ``wrap around'').  The starting point of
 *	the search is always point in the buffer.  This must be a fixnum
 *	representing an offset before the current position of point.
 *
 *	If the optional fourth argument is given and evaluates non-nil,
 *	nil is returned if the string is not found.  Normally an error
 *	message is printed if the pattern is not found to the bottom
 *	of the buffer.
 *  Side: If the string is found, point is changed in the buffer,
 *	otherwise an error occurs unless the fourth argument is given
 *	non-nil.
 *  SeeA: search-forward-regexp search-backward
 */

DEFUN(dosearchbkregexp, "search-backward-regexp", FLAG_NONE,
      "sSearch back for: \np")
{
	struct value	arg;
	int		repeat = 1, failok = FALSE, bound;

	return (v_nil);
}
