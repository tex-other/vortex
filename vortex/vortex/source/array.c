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
 *  RCS Info: $Header: array.c,v 0.1 87/05/01 11:24:03 john Locked $
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
 *  array.c - vLisp array storage and handling
 */
static char _ID[] = "@(#)array.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "value.h"

/*
 *  These routines manage the storage of array structs for the
 *  vlisp array values.  We maintain a table of arrays at
 *  any one time and allocate from that table.  Multiple values
 *  may point to the same array, when garbage collection time
 *  is at hand, things will be resolved.  Save_array takes one
 *  argument, the length, and returns an array all of whose
 *  elements are nil.
 */
int		aagrow_arrays = 0;	/* automatically grow arrays */
static int	minfree_arrays = 0;	/* actual amount that must be free */
static int	curfree_arrays = 0;	/* actual amount that is free */
int		softlim_arrays = 10;	/* percentage that must be free */
int		cursize_arrays = 0;	/* current number allocated */
int		overflow_arrays = 0;	/* have run low on array structs */

static struct array	*array_table = NULL, *array_tabend;
static int		array_initsize = 50;

struct array *
save_array(len)
{
	extern struct value	cerror();
	register struct array	*arp;
	register struct value	*vp, *vend;

	if (array_table == NULL) {
		/* allocate initial array storage */
		grow_arrays(array_initsize);
	}

	/* need to find a new array struct */
	for (arp = array_table; arp < array_tabend; arp++)
		if (arp->ar_free)
			break;
	if (arp >= array_tabend) {
		/* this shouldn't happen */
		ierror("Help!  Ran completely out of array storage!");
	} else if (!overflow_arrays && curfree_arrays < minfree_arrays) {
		/* garbage collection time ... */
		gcunmark_arrays();
		gc_markall();
		gcsweep_arrays();

		/* if still not enough, keep trying */
		if (curfree_arrays < minfree_arrays) {
			if (aagrow_arrays > 0) {
				/* automatically allocate more storage */
				grow_arrays(aagrow_arrays);
			} else {
				overflow_arrays++;
				cerror(
		    "Too little array storage left (%d used, %d free).",
				       cursize_arrays - curfree_arrays,
				       curfree_arrays);
			}
		}
	}

	/* make up the new array */
	if (len <= 0) {
		arp->ar_length = 0;
		arp->ar_array = NULL;
	} else {
		arp->ar_length = len;
		arp->ar_array = (struct value *)
		    valloc(len * sizeof (struct value));
		vend = arp->ar_array + arp->ar_length;
		for (vp = arp->ar_array; vp < vend; vp++) {
			vp->vl_type = LISP_NIL;
			slist(vp->vl_data, NULL);
		}
	}

	arp->ar_free = FALSE;
	curfree_arrays--;
	return (arp);
}

#define ARRAYS_HARDLIM		2

grow_arrays(incr)
{
	struct array	*new, *arp;

	if (incr > 0) {
		new = (struct array *)
		    valloc((cursize_arrays + incr) * sizeof (struct array));
		if (array_table != NULL) {
			bcopy(array_table, new,
			    cursize_arrays * sizeof (struct array));
			vfree(array_table);
		}
		arp = new + cursize_arrays;
		array_table = new;
		cursize_arrays += incr;
		curfree_arrays += incr;
		array_tabend = array_table + cursize_arrays;
		while (arp < array_tabend) {
			arp->ar_free = TRUE;
			arp++;
		}

		overflow_arrays = 0;
	}

	/* update minimum free amount variable */
	minfree_arrays = ROUND((double)cursize_arrays *
			       ((double)softlim_arrays / 100.0));
	if (minfree_arrays > cursize_arrays - ARRAYS_HARDLIM)
		minfree_arrays = cursize_arrays - ARRAYS_HARDLIM;

	return (cursize_arrays);
}

gcunmark_arrays()
{
	return (0);
}

gcsweep_arrays()
{
	register int	nfound = 0;

	return (nfound);
}

aequal(arr1, arr2)
	struct array	*arr1, *arr2;
{
	struct value	*vp1, *vp2, *vend;

	if (arr1->ar_length != arr2->ar_length)
		return (FALSE);
	vend = arr1->ar_array + arr1->ar_length;
	vp1 = arr1->ar_array;
	vp2 = arr2->ar_array;
	while (vp1 < vend) {
		if (!eq(*vp1, *vp2))
			return (FALSE);
		vp1++; vp2++;
	}
	return (TRUE);
}
