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
 *  RCS Info: $Header: doarray.c,v 0.1 87/05/01 11:33:51 john Locked $
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
 *  doarray.c - array handling functions
 */
static char _ID[] = "@(#)doarray.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: array
 *  Call: (array 'length)
 *  Retu: array
 *  Desc: This function creates an array of the specified length
 *	an returns the new value.  The single argument must evaluate
 *	to a fixnum which is the number of elements in the array.
 *
 *	Vlisp arrays are very flexible.  Values of any type may be
 *	stored in any element of an array (when created, all the
 *	elements of the array are set to nil).  An array element is
 *	retrieved from an array with \sym{index} and a value replaced
 *	with \sym{store}.
 *  Side: Creating an array uses up a block of available values,
 *	an many as the length of the array.  Thus, arrays use
 *	a large amount of space.  However, arrays have a faster
 *	access time than lists; much faster for long lists.  Arrays
 *	are a fixed size which cannot be changed once allocated.
 *
 *	Two arrays are never \sym{eq} unless they were inherited
 *	from the same object.  In other words, the result of two
 *	calls to \sym{array} can never be \sym{eq}, even if \sym{equal}.
 *  Xref: marray
 *  SeeA: index store length listarray
 */

DEFUN(doarray, "array", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct array	*arr;
	int		len;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a positive fixnum");
	len = gfixnum(arg.vl_data);
	if (len <= 0)
		error("Array lengths must be greater than zero!");
	arr = save_array(len);
	ret.vl_type = LISP_ARRAY;
	sarray(ret.vl_data, arr);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: index
 *  Call: (index 'array 'index)
 *  Retu: any
 *  Desc: This function accesses the element of an array subscripted
 *	by the index (which is zero based).  The first argument must
 *	evaluate to an array and the second to a fixnum.  The function
 *	returns the value of the element at the given index.
 *  SeeA: array store
 */

DEFUN(doindex, "index", FLAG_NONE, NULL)
{
	struct value	arr, val;
	int		ind;

	CHECKAC(2, 2);
	arr = EVALARGN(1);
	if (!arrayp(arr))
		BADARGN(1, "an array value");
	val = EVALARGN(2);
	if (!fixnump(val))
		BADARGN(2, "a positive fixnum");
	ind = gfixnum(val.vl_data);

	return aindex(arr, ind);
}

struct value
aindex(aval, ind)
	struct value	aval;
{
	struct array	*aptr;

	if (!arrayp(aval))
		ierror("Trying to take the index of a non-array value!");

	aptr = garray(aval.vl_data);
	if (ind < 0 || ind >= aptr->ar_length)
		error("Bad index for array; must be in range 0 to %d.",
		    aptr->ar_length - 1);
	return (aptr->ar_array[ind]);
}

/*
 *  DOCUMENTATION
 *
 *  Name: store
 *  Call: (store 'array 'index 'value)
 *  Retu: value
 *  Desc: This function sets the element in the array indexed by
 *	index to the given value.  The first argument must evaluate
 *	to an array and the second to a fixnum.  The third argument
 *	is evaluated and the result becomes the value of the indexed
 *	element in the array.
 *  Side: This whole thing is a side effect; any other references to
 *	this array are also changed.
 *  SeeA: array index
 */

DEFUN(dostore, "store", FLAG_NONE, NULL)
{
	struct value	arr, val;
	int		ind;

	CHECKAC(3, 3);
	arr = EVALARGN(1);
	if (!arrayp(arr))
		BADARGN(1, "an array value");
	val = EVALARGN(2);
	if (!fixnump(val))
		BADARGN(2, "a positive fixnum");
	ind = gfixnum(val.vl_data);
	val = EVALARGN(3);

	store(arr, ind, val);
	return (val);
}

store(aval, ind, val)
	struct value	aval, val;
{
	struct array	*aptr;

	if (!arrayp(aval))
		ierror("index: Called on a non-array value!");

	aptr = garray(aval.vl_data);
	if (ind < 0 || ind >= aptr->ar_length)
		error("Bad index for array; must be in range 0 to %d.",
		    aptr->ar_length - 1);
	aptr->ar_array[ind] = val;
	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: fillarray
 *  Call: (fillarray 'array 'value)
 *  Retu: array
 *  Desc: This function fills the given array with the value that
 *	second argument evaluates to.  The first argument must
 *	evaluate to an array, the second may evaluate to any type.
 *	This function returns the value put into the last element
 *	in the array.
 *
 *	If the second argument evaluates to a non-nil list, then
 *	the element of that list are used to fill the array.  If
 *	the list is too short, the last element of the list is
 *	repeated until the end of the list.  Otherwise, if the
 *	value is a non-list (or nil) each element of the array
 *	is set to that value.
 *  Side: The action a side effect, the array passed in is
 *	modified itself, so each reference to that array will
 *	find its array changed.
 *  SeeA: array store
 */

DEFUN(dofillarray, "fillarray", FLAG_NONE, NULL)
{
	struct value	arg, val;
	struct ccell	*cell;
	struct array	*arr;
	struct value	*vp, *vend;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (!arrayp(arg))
		BADARGN(1, "an array value");
	val = EVALARGN(2);

	if (dtprp(val)) {
		arr = garray(arg.vl_data);
		vend = arr->ar_array + arr->ar_length;
		cell = glist(val.vl_data);
		val.vl_type = cell->cc_tcar;
		val.vl_data = cell->cc_car;
		for (vp = arr->ar_array; vp < vend; vp++) {
			*vp = val;
			if (cell->cc_tcdr == LISP_CONS) {
				cell = glist(cell->cc_cdr);
				val.vl_type = cell->cc_tcar;
				val.vl_data = cell->cc_car;
			}
		}
	} else {
		/* fill entire list with same value */
		fillarray(arg, val);
	}
	return (arg);
}

fillarray(aval, fval)
	struct value	aval, fval;
{
	struct array	*arr;
	struct value	*vp, *vend;

	if (!arrayp(aval))
		ierror("Non array type to fillarray!");

	arr = garray(aval.vl_data);
	vend = arr->ar_array + arr->ar_length;
	for (vp = arr->ar_array; vp < vend; vp++)
		*vp = fval;

	return (0);
}
