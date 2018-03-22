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
 *  RCS Info: $Header: dogc.c,v 0.1 87/05/01 11:48:18 john Locked $
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
 *  dogc.c - vLisp garbage collection interface
 */
static char _ID[] = "@(#)dogc.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  These are the external functions and variables that control
 *  the garbage collection and automatic allocation of different
 *  storage types.
 */
extern int	aagrow_symbols,		softlim_symbols,	cursize_symbols,
		aagrow_ccells,		softlim_ccells,		cursize_ccells,
		aagrow_functs,		softlim_functs,		cursize_functs,
		aagrow_strings,		softlim_strings,	cursize_strings,
		aagrow_buffer,		softlim_buffer,		cursize_buffer,
		aagrow_arrays,		softlim_arrays,		cursize_arrays;
		aagrow_tblocks,		softlim_tblocks,	cursize_tblocks;

extern int	/* don't garbage collect symbols */		grow_symbols(),
		gcunmark_ccells(),	gcsweep_ccells(),	grow_ccells(),
		gcunmark_functs(),	gcsweep_functs(),	grow_functs(),
		gcunmark_strings(),	gcsweep_strings(),	grow_strings(),
		/* buffer garbage collects with strings */	grow_buffer(),
		gcunmark_arrays(),	gcsweep_arrays(),	grow_arrays();
		gcunmark_tblocks(),	gcsweep_tblocks(),	grow_tblocks();

struct gcstat {
	struct string	gc_name;		/* storage type name */
	char		*gc_cname;		/* initial C string */
	int		(*gc_gcunmark)();	/* unmark function */
	int		(*gc_gcsweep)();	/* sweep function */
	int		(*gc_growfun)();	/* grow function */
	int		*gc_cursize;		/* current allocation */
	int		*gc_growamt;		/* amount to auto-allocate */
	int		*gc_softlim;		/* soft allocation limit */
};

static struct gcstat	gcstat_list[] = {
	{ { 0 }, "symbol",
		NULL,			NULL,
		grow_symbols,		&cursize_symbols,
		&aagrow_symbols,	&softlim_symbols },
	{ { 0 }, "cons",
		gcunmark_ccells,	gcsweep_ccells,
		grow_ccells,		&cursize_ccells,
		&aagrow_ccells,		&softlim_ccells },
	{ { 0 }, "function",
		gcunmark_functs,	gcsweep_functs,
		grow_functs,		&cursize_functs,
		&aagrow_functs,		&softlim_functs },
	{ { 0 }, "string",
		gcunmark_strings,	gcsweep_strings,
		grow_strings,		&cursize_strings,
		&aagrow_strings,	&softlim_strings },
	{ { 0 }, "buffer",
		NULL,			NULL,
		grow_buffer,		&cursize_buffer,
		&aagrow_buffer,		&softlim_buffer },
	{ { 0 }, "array",
		gcunmark_arrays,	gcsweep_arrays,
		grow_arrays,		&cursize_arrays,
		&aagrow_arrays,		&softlim_arrays },
	{ { 0 }, "tblock",
		gcunmark_tblocks,	gcsweep_tblocks,
		grow_tblocks,		&cursize_tblocks,
		&aagrow_tblocks,	&softlim_tblocks },
},	*gcstat_end = gcstat_list + NITEMS(gcstat_list);

/*
 *  DOCUMENTATION
 *
 *  Name: gc
 *  Call: (gc [ 'type ])
 *  Retu: number
 *  Desc: This function invokes the internal garbage collection
 *	mechanism to search for values that are no longer used
 *	by any vlisp data structures and place them on the free
 *	list.  If the optional argument is present, it must
 *	evaluate to a symbol, specifying the type of values to
 *	garbage collect.  If the argument is not present, all
 *	types are subject.  This function returns the number
 *	of values it was able to retrieve.
 *
 *	The value type specified by the optional argument should
 *	be one of: \lit{cons}, \lit{function}, \lit{string},
 *	\lit{array} or \lit{tblock}.
 *	Any non-symbol argument or a symbol with a name other than
 *	one of these type specifiers will cause an error.  When a
 *	specific type is given to collect, the number returned is
 *	the count of recovered values of only that type.
 *
 *	Vlisp values are allocated in large chunks, and
 *	obtained when needed from several free lists (one free
 *	list for each type of value).  Since vlisp dynamically
 *	allocates so many values, and uses most of them for a
 *	short time, there will be many values lying around unused.
 *	Garbage collection finds these and places them back on
 *	the free list for later reuse.
 *
 *	Garbage collection is invoked automatically when there are
 *	fewer available values of a certain type than specified by
 *	the soft limit for that type (see \sym{soft-limit}).  However,
 *	only values of that type are garbage collected when \sym{gc} is
 *	invoked automatically in this way.
 *
 *	The garbage collector itself never creates new storage, that
 *	is done at a higher level; possibly when garbage collection
 *	can find no new values (see \sym{auto-allocate}) or
 *	explicitly by the user with the \sym{allocate} function.
 *	To reduce the amount of garbage collection needed (but decrease
 *	the system free memory) one may \sym{allocate} more storage
 *	for specific value types.
 *  Side: Vlisp values are reogranized, memory is recovered.  In some
 *	cases, existing vlisp values may be moved in memory, which
 *	should not affect existing data structures at the user level.
 *	Garbage collection takes an appreciable amount of time, and
 *	is not interruptable.
 *  SeeA: allocate auto-allocate soft-limit
 */
extern struct gcstat	*getgcname();

DEFUN(dogc, "gc", FLAG_NONE, NULL)
{
	struct gcstat	*gcp;
	struct value	arg, ret;
	struct string	*strp;
	int		found;

	CHECKAC(0, 1);
	if (GETACOUNT() == 0) {
		for (gcp = gcstat_list; gcp < gcstat_end; gcp++) {
			if (gcp->gc_gcunmark != NULL)
				(*gcp->gc_gcunmark)();
		}
		gc_markall();
		found = 0;
		for (gcp = gcstat_list; gcp < gcstat_end; gcp++) {
			if (gcp->gc_gcsweep != NULL)
				found += (*gcp->gc_gcsweep)();
		}
	} else {
		arg = EVALARGN(1);
		if (!symbolp(arg))
			BADARGN(1, "a symbol");
		strp = gsymbol(arg.vl_data)->sy_pname;
		gcp = getgcname(strp);
		if (gcp->gc_gcunmark == NULL)
			error("We don't garbage collect %Y storage.",
			      gcp->gc_name);
		(*gcp->gc_gcunmark)();
		gc_markall();
		found = (*gcp->gc_gcsweep)();
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, found);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: allocate
 *  Call: (allocate 'type [ 'number ])
 *  Retu: number
 *  Desc: This function increases the number of values of a specific
 *	type allocated, or returns the current allocation for that
 *	type.  The first arguent must evaluate to a symbol, which
 *	should be one of \lit{symbol}, \lit{function}, \lit{cons},
 *	\lit{string}, \lit{buffer}, \lit{array} or \lit{tblock}.
 *	The second argument,
 *	if present, must evaluate to a fixnum which specifies the number
 *	of entries of that type to additionally allocate.  If no second
 *	argument is given, the number currently allocated is returned.
 *
 *	When the storage manager of vlisp needs a new value, and fewer
 *	such values exist than specified by the soft limit for that
 *	type (see \sym{soft-limit}), it calls the garbage collector to
 *	find values that are no longer being used, and may be used again.
 *	If too few unused values are found, vlisp is forced to
 *	allocate mew values (if there is an automatic allocation
 *	specified by \sym{auto-allocate}) or an error occurs.
 *  Side: Creates additional storage for the given type; from then
 *	on the garbage collector should be called less frequently.
 *	However, one should be careful about allocating too much
 *	storage for vlisp values, there must be enough room for the
 *	other parts of the editor to use or errro will occur later.
 *	There is no way to ``deallocate'' storage.
 *
 *	Allocation of a particular type increments the soft limit
 *	(see the function \sym{soft-limit}) for that value type by
 *	the amount allocated.  If this isn't desired, the function
 *	\sym{soft-limit} should be called explicitly to change this
 *	limit again.
 *  SeeA: gc auto-allocate soft-limit
 */

DEFUN(doallocate, "allocate", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, ret;
	struct gcstat	*gcp;
	struct string	*strp;
	int		add;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a symbol");
	strp = gsymbol(arg1.vl_data)->sy_pname;
	gcp = getgcname(strp);

	if (GETACOUNT() > 1) {
		/* actually grow that table */
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a positive number");
		add = gfixnum(arg2.vl_data);
		if (add < 0)
			error("Can't allocate negative amounts!");
		(*gcp->gc_growfun)(add);
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, *gcp->gc_cursize);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: auto-allocate
 *  Call: (auto-allocate 'type [ amount ])
 *  Retu: amount
 *  Desc: This function sets the amount for a specific storage
 *	type that will be automatically allocated when the
 *	interpreter runs out of storage for that type.  The
 *	first argument must evaluate to a symbol, one of
 *	\lit{symbol}, \lit{cons}, \lit{function}, \lit{string},
 *	\lit{buffer}, \lit{array} or \lit{tblock}.  The second
 *	argument, if present, should evaluate to a positive fixnum.
 *
 *	If the second argument is given, the amount to automatically
 *	allocate when memory for that storage type is exhausted
 *	is set to the value given  In any case, the current value
 *	is returned.
 *  Side: When the amount of free values of the given type becomes
 *	less than the soft limit (as set by the \sym{soft-limit}
 *	function) for that type, one of two things happens.
 *	If a non-zero amount to automatically allocate has been
 *	specified (by this function), that much more space is
 *	allocated.  Otherwise, an error occurs.  If there is an
 *	automatic allocation amount, this allocation has the same
 *	effect as an asynchronous call to \sym{allocate} when
 *	storage for the type becomes low.
 *  SeeA: gc allocate soft-limit
 */

DEFUN(doautoalloc, "auto-allocate", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, ret;
	struct gcstat	*gcp;
	struct string	*strp;
	int		amt;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a symbol");
	strp = gsymbol(arg1.vl_data)->sy_pname;
	gcp = getgcname(strp);

	if (GETACOUNT() > 1) {
		/* actually set the growth amount */
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a positive number");
		amt = gfixnum(arg2.vl_data);
		if (amt < 0)
			amt = 0;
		*gcp->gc_growamt = amt;
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, *gcp->gc_growamt);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: soft-limit
 *  Call: (soft-limit 'type [ percentage ])
 *  Retu: percentage
 *  Desc: This function gets or sets the soft limit for allocation
 *	of a particular type of storage to the value given, which
 *	is considered a percentage of the total available.  The
 *	first argument must evaluate to a symbol; one of
 *	\lit{symbol}, \lit{cons}, \lit{function}, \lit{string},
 *	\lit{buffer}, \lit{array} or \lit{tblock}.  The second
 *	argument, if present, should evaluate to a non-zero fixnum.
 *  Side: When the amount of free values of the given type falls
 *	below this percentage of the total number of allocated
 *	values, one of two things happens.  If there is a positive
 *	automatic allocation amount (which may be set with
 *	\sym{auto-allocate}), then that number of new values are
 *	allocated.  Otherwise, an appropriate error occurs.
 *  SeeA: gc allocate auto-allocate
 */

DEFUN(dosoftlim, "soft-limit", FLAG_NONE, NULL)
{
	struct value	arg1, arg2, ret;
	struct gcstat	*gcp;
	struct string	*strp;
	int		amt;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a symbol");
	strp = gsymbol(arg1.vl_data)->sy_pname;
	gcp = getgcname(strp);

	if (GETACOUNT() > 1) {
		/* actually grow that table */
		arg2 = EVALARGN(2);
		if (!fixnump(arg2))
			BADARGN(2, "a positive number");
		amt = gfixnum(arg2.vl_data);
		if (amt <= 0 || amt > 100)
			error("The soft limit must be a non-zero percentage.");
		*gcp->gc_softlim = amt;
		if (gcp->gc_growfun != NULL)
			(*gcp->gc_growfun)(0);
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, *gcp->gc_softlim);
	return (ret);
}

static struct gcstat *
getgcname(name)
	struct string	*name;
{
	struct gcstat	*gcp;

	for (gcp = gcstat_list; gcp < gcstat_end; gcp++) {
		if (gcp->gc_name.st_buffer == NULL) {
			gcp->gc_name.st_buffer = (unsigned char *)
						 gcp->gc_cname;
			gcp->gc_name.st_length = strlen(gcp->gc_cname);
		}
		if (sequal(&gcp->gc_name, name))
			break;
	}
	if (gcp == NULL)
		error("Unknown internal storage type %Y!", name);
	return (gcp);
}
