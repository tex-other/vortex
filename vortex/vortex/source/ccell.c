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
 *  RCS Info: $Header: ccell.c,v 0.1 87/05/01 11:26:17 john Locked $
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
 *  ccell.c - vLisp cons cell storage and handling
 */
static char _ID[] = "@(#)ccell.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "value.h"

/*
 *  These routines manage the storage of ccell structs for the
 *  vlisp ccell values.  We maintain a table of ccells at
 *  any one time and allocate from that table.  Multiple values
 *  may point to the same ccell, when garbage collection time
 *  is at hand, things will be resolved.  Save_ccell takes no
 *  arguments, it always returns a new cons cell.
 */
int		aagrow_ccells = 0;	/* automatically grow ccells */
static int	minfree_ccells = 0;	/* actual amount that must be free */
static int	curfree_ccells = 0;	/* actual amount that is free */
int		softlim_ccells = 5;	/* percentage that must be free */
int		cursize_ccells = 0;	/* current number allocated */
int		overflow_ccells = 0;	/* have run low on cons cells */

static struct ccell	*ccell_table = NULL, *ccell_tabend;
static int		ccell_initsize = 5000;
static int		ccell_haveholes = FALSE;
static struct ccell	*ccell_curalloc = NULL;

struct ccell *
save_ccell()
{
	extern struct value	cerror();
	register struct ccell	*ccp;

	if (ccell_table == NULL) {
		/* allocate initial ccell storage */
		grow_ccells(ccell_initsize);
		ccell_curalloc = ccell_table;
	}

	/* need to find a new ccell struct */
	if (ccell_haveholes) {
		for (ccp = ccell_table; ccp < ccell_tabend; ccp++)
			if (ccp->cc_free)
				break;
	} else {
		for (ccp = ccell_curalloc; ccp < ccell_tabend; ccp++)
			if (ccp->cc_free)
				break;
	}
	ccell_curalloc = ccp + 1;

	if (ccp >= ccell_tabend) {
		/* this shouldn't happen */
		ierror("Help!  Ran completely out of cons cell storage!");
	} else if (!overflow_ccells && curfree_ccells < minfree_ccells) {
		/* garbage collection time ... */
		gcunmark_ccells();
		gc_markall();
		gcsweep_ccells();

		/* if stil not enough, keep trying */
		if (curfree_ccells < minfree_ccells) {
			if (aagrow_ccells > 0) {
				/* automatically allocate more storage */
				grow_ccells(aagrow_ccells);
			} else {
				overflow_ccells++;
				cerror(
		    "Too little cons cell storage left (%d used, %d free).",
				       cursize_ccells - curfree_ccells,
				       curfree_ccells);
			}
		}
	}

	ccp->cc_free = FALSE;
	curfree_ccells--;
	return (ccp);
}

#define CCELLS_HARDLIM		50

grow_ccells(incr)
{
	struct ccell	*new, *ccp;

	if (incr > 0) {
		new = (struct ccell *)
		    valloc((cursize_ccells + incr) * sizeof (struct ccell));
		if (ccell_table != NULL) {
			bcopy(ccell_table, new,
			    cursize_ccells * sizeof (struct ccell));
			vfree(ccell_table);
		}
		ccp = new + cursize_ccells;
		ccell_table = new;
		cursize_ccells += incr;
		curfree_ccells += incr;
		ccell_tabend = ccell_table + cursize_ccells;
		while (ccp < ccell_tabend) {
			ccp->cc_free = TRUE;
			ccp++;
		}

		overflow_ccells = 0;
	}

	/* update minimum free amount variable */
	minfree_ccells = ROUND((double)cursize_ccells *
			       ((double)softlim_ccells / 100.0));
	if (minfree_ccells > cursize_ccells - CCELLS_HARDLIM)
		minfree_ccells = cursize_ccells - CCELLS_HARDLIM;

	return (cursize_ccells);
}

gcunmark_ccells()
{
	return (0);
}

gcsweep_ccells()
{
	register int	nfound = 0;

	return (nfound);
}
