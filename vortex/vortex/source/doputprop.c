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
 *  RCS Info: $Header: doputprop.c,v 0.1 87/05/01 11:59:11 john Locked $
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
 *  doputprop.c - vLisp property list management
 */
static char _ID[] = "@(#)doputprop.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: putprop
 *  Call: (putprop 'symbol 'value 'property)
 *  Retu: value
 *  Desc: This function creates or changes the property named by
 *	the argument property of the given symbol symbol to the
 *	given value.  The function returns the value placed under
 *	the given property indicator.  The first and third arguments
 *	must evaluate to symbol names, the second may be of any type.
 *
 *	This property may be retrieved by the \sym{getprop} or
 *	deleted by \sym{remprop}.  Every atom with the same print
 *	name has shares a property list.  This is a side effect of
 *	the fact that all symbols are \sym{eq} if they are \sym{equal}.
 *  Side: When a property is added, two elements are appended
 *	to the property list; the list itself is changed.  So,
 *	anything \sym{eq} to the property list will also change.
 *  Xref: put defprop
 *  SeeA: getprop remprop setplist
 */
static char	_BADPLIST[] = "Bad property list value; must be a list!";
#define	BADPLIST()	error(_BADPLIST)
static char	_BADPROP[] = "Bad property list; property element garbled!";
#define	BADPROP()	error(_BADPROP)

DEFUN(doputprop, "putprop", FLAG_NONE, NULL)
{
	struct value	arg, sym, val;
	struct ccell	*last, *next;
	struct symbol	*atom;
	register int	count, len;

	CHECKAC(3, 3);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	atom = gsymbol(arg.vl_data);
	val = EVALARGN(2);
	arg = EVALARGN(3);
	if (!symbolp(arg))
		BADARGN(3, "a property name symbol");

	/* search for the given property name */
	if (nullp(atom->sy_plist)) {
		/* make a two element list for this */
		atom->sy_plist.vl_type = LISP_CONS;
		last = save_ccell();
		slist(atom->sy_plist.vl_data, last);
		last->cc_tcar = LISP_SYMBOL;
		last->cc_car = arg.vl_data;
		last->cc_tcdr = LISP_CONS;
		next = save_ccell();
		slist(last->cc_cdr, next);
		next->cc_tcdr = LISP_NIL;
		slist(next->cc_cdr, NULL);
	} else if (dtprp(atom->sy_plist)) {
		len = length(atom->sy_plist);
		if (len % 2 != 0)
			BADPROP();
		for (count = 0; count < len; count += 2) {
			sym = nth(count, atom->sy_plist);
			if (!symbolp(sym))
				BADPROP();
			if (eq(sym, arg))
				break;
		}
		if (count >= len) {
			/* extend the list for this property */
			sym = nthcdr(len - 1, atom->sy_plist);
			if (!listp(sym))
				BADPROP();
			last = glist(sym.vl_data);
			next = save_ccell();
			last->cc_tcdr = LISP_CONS;
			slist(last->cc_cdr, next);
			next->cc_tcar = LISP_SYMBOL;
			next->cc_car = arg.vl_data;
			last = next;
			next = save_ccell();
			last->cc_tcdr = LISP_CONS;
			slist(last->cc_cdr, next);
			next->cc_tcdr = LISP_NIL;
			slist(next->cc_cdr, NULL);
		} else {
			sym = nthcdr(count + 1, atom->sy_plist);
			if (!dtprp(sym))
				BADPROP();
			next = glist(sym.vl_data);
		}
	} else {
		/* this shouldn't happen */
		BADPLIST();
	}

	/* change this cons cell's car to point to new value */
	next->cc_tcar = val.vl_type;
	next->cc_car = val.vl_data;

	return (val);
}

/*
 *  DOCUMENTATION
 *
 *  Name: getprop
 *  Call: (getprop 'symbol 'property)
 *  Retu: any
 *  Desc: This function returns the value stored under the property
 *	indicator for the given symbol.  If there is no such property
 *	for the given symbol, nil is returned.  Both arguments must
 *	evaluate to symbol names.
 *  Xref: get
 *  SeeA: putprop remprop getplist
 */

DEFUN(dogetprop, "getprop", FLAG_NONE, NULL)
{
	struct value	arg;
	struct symbol	*atom;
	struct string	*pname;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	atom = gsymbol(arg.vl_data);
	arg = EVALARGN(2);
	if (!symbolp(arg))
		BADARGN(2, "a property name symbol");
	pname = gsymbol(arg.vl_data)->sy_pname;

	return getprop(atom, pname);
}

struct value
getprop(atom, pname)
	struct symbol	*atom;
	struct string	*pname;
{
	struct value	sym;
	struct string	*sname;
	register int	count, len;

	/* search for the given property name */
	if (eq(atom->sy_plist, v_nil))
		return (v_nil);
	if (!dtprp(atom->sy_plist))
		error("Property list for symbol %Y isn't a list!", atom);

	len = length(atom->sy_plist);
	if (len % 2 != 0)
		BADPROP();
	for (count = 0; count < len; count += 2) {
		sym = nth(count, atom->sy_plist);
		if (!symbolp(sym))
			BADPROP();
		sname = gsymbol(sym.vl_data)->sy_pname;
		if (sequal(sname, pname))
			return nth(count + 1, atom->sy_plist);
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: remprop
 *  Call: (remprop 'symbol 'property)
 *  Retu: t or nil
 *  Desc: This function removes the named property indicator (and
 *	any stored value) from the given symbol.  The function
 *	returns t if there was such a property to remove and nil
 *	if not.  Both arguments must evaluate to symbol names.
 *  Side: When a property is removed, two elements are removed
 *	from the property list, the list itself is changed.  So,
 *	anything \sym{eq} to the property list will also change.
 *  SeeA: putprop getprop setplist
 */

DEFUN(doremprop, "remprop", FLAG_NONE, NULL)
{
	struct value	arg, sym, val, rest;
	struct symbol	*atom;
	struct ccell	*last;
	register int	count, len;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	atom = gsymbol(arg.vl_data);
	arg = EVALARGN(2);
	if (!symbolp(arg))
		BADARGN(2, "a property name symbol");

	/* search for the given property name */
	if (eq(atom->sy_plist, v_nil))
		return (v_nil);
	if (!dtprp(atom->sy_plist))
		error("Property list for symbol %Y isn't a list!",
		      atom->sy_pname);

	len = length(atom->sy_plist);
	if (len % 2 != 0)
		BADPROP();
	for (count = 0; count < len; count += 2) {
		sym = nth(count, atom->sy_plist);
		if (!symbolp(sym))
			BADPROP();
		if (eq(sym, arg))
			break;
	}
	if (count >= len)
		return (v_nil);

	/* remove these next two elements from the plist */
	if (count + 2 >= len)
		rest = v_nil;
	else
		rest = nthcdr(count + 2, atom->sy_plist);
	if (count == 0) {
		/* assign plist to rest of list */
		atom->sy_plist = rest;
	} else {
		val = nthcdr(count - 1, atom->sy_plist);
		if (!dtprp(val))
			BADPLIST();
		last = glist(val.vl_data);
		last->cc_tcdr = rest.vl_type;
		last->cc_cdr = rest.vl_data;
	}
	return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: setplist
 *  Call: (setplist 'symbol 'list)
 *  Retu: list
 *  Desc: This function sets the property list associated
 *	with the named symbol to the given list.  This property
 *	list, if not nil, must contain alternating symbols
 *	(which are the property indicators to set) and any values
 *	(which are values to be stored under the preceding indicator).
 *	These property values are accessed with \sym{getprop} or
 *	\sym{getplist}.
 *  Side: Now the property list for this symbol is set to the
 *	user specified list.  At the time \sym{setprop} is called,
 *	the property list is checked for correct format (alternating
 *	symbols and values).  However, if the user causes this list
 *	to lose this structure (perhaps with \sym{replacd}), later
 *	calls to \sym{getprop} may fail because the property list will
 *	become garbled.
 *
 *	Also, this property list is changed by calls to \sym{putprop}
 *	or \sym{remprop} will change this list, so anything \sym{eq}
 *	will also be changed.  It is probably best to call \sym{setplist}
 *	with a list gotten from \sym{copy}, which will make a non-\sym{eq}
 *	copy of the given list.
 *  Xref: plist
 *  SeeA: getplist remprop copy
 */

DEFUN(dosetplist, "setplist", FLAG_NONE, NULL)
{
	extern char	*nthname();
	struct value	arg, plist, sym;
	struct symbol	*atom;
	register int	count, len;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	atom = gsymbol(arg.vl_data);

	plist = EVALARGN(2);
	if (!listp(plist))
		BADARGN(2, "a list");

	if (!nullp(plist)) {
		/* make sure the list has en even number of elements */
		len = length(plist);
		if (len % 2 != 0)
			error("Property lists must have pairs of elements!");
	
		/* loop through list once checking for errors */
		for (count = 0; count < len; count += 2) {
			sym = nth(count, plist);
			if (!symbolp(sym))
				error("The %s list element %v isn't a symbol!",
				      nthname(count), sym);
		}
	}

	/* set this atom's property list to this list */
	atom->sy_plist = plist;
	return (plist);
}

/*
 *  DOCUMENTATION
 *
 *  Name: getplist
 *  Call: (getplist 'symbol)
 *  Retu: list
 *  Desc: This function returns the property list associated
 *	with the named symbol.  This property list, if not nil,
 *	will contain alternating symbols (which are the property
 *	indicators) and any values (which are current values
 *	stored under the preceding indicator).  These property
 *	values are stored with \sym{putprop} or \sym{setplist}.
 *  SeeA: setplist putprop
 */

DEFUN(dogetplist, "getplist", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!symbolp(arg))
		BADARGN(1, "a symbol name");
	return (gsymbol(arg.vl_data)->sy_plist);
}
