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
 *  RCS Info: $Header: tblock.c,v 0.1 87/05/01 12:30:41 john Locked $
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
 *  tblock.c - source buffer text block storage and management routines
 */
static char _ID[] = "@(#)tblock.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "value.h"
#include "buffer.h"

/*
 *  These routines manage the storage of tblock structs for the
 *  buffer text storage.  We maintain a table of tblocks at
 *  any one time and allocate from that table.  Because of the
 *  way text is handled, we may get significant fragmentation,
 *  so garbage collection is responsible for consolidating the
 *  buffer text into the fewest blocks possible.
 */
int		aagrow_tblocks = 50;	/* automatically grow tblocks */
static int	minfree_tblocks = 0;	/* actual amount that must be free */
static int	curfree_tblocks = 0;	/* actual amount that is free */
int		softlim_tblocks = 2;	/* percentage that must be free */
int		cursize_tblocks = 0;	/* current number allocated */
int		overflow_tblocks = 0;	/* have run low on text blocks */

static struct tblock	*tblock_table = NULL, *tblock_tabend;
static int		tblock_initsize = 1000;

struct tblock *
save_tblock()
{
	register struct tblock	*tbp;

	if (tblock_table == NULL) {
		/* allocate initial tblock storage */
		grow_tblocks(tblock_initsize);
	}

	/* need to find a new tblock struct */
	for (tbp = tblock_table; tbp < tblock_tabend; tbp++)
		if (tbp->tb_free)
			break;
	if (tbp >= tblock_tabend) {
		/* this shouldn't happen */
		ierror("Help!  Ran completely out of text block storage!");
	} else if (!overflow_tblocks && curfree_tblocks < minfree_tblocks) {
		/* garbage collection time ... */
		gcunmark_tblocks();
		gc_markall();
		gcsweep_tblocks();

		/* if stil not enough, keep trying */
		if (curfree_tblocks < minfree_tblocks) {
			if (aagrow_tblocks > 0) {
				/* automatically allocate more storage */
				grow_tblocks(aagrow_tblocks);
			} else {
				overflow_tblocks++;
				cerror(
		    "Too little text block storage left (%d used, %d free).",
				       cursize_tblocks - curfree_tblocks,
				       curfree_tblocks);
			}
		}
	}

	tbp->tb_free = FALSE;
	tbp->tb_flags = FLAG_NONE;
	tbp->tb_offset = -1;
	tbp->tb_length = 0;
	curfree_tblocks--;
	return (tbp);
}

#define TBLOCK_HARDLIM		10

grow_tblocks(incr)
{
	struct tblock	*new, *tbp;

	if (incr > 0) {
		new = (struct tblock *)
		    valloc((cursize_tblocks + incr) * sizeof (struct tblock));
		if (tblock_table != NULL) {
			bcopy(tblock_table, new,
			    cursize_tblocks * sizeof (struct tblock));
			vfree(tblock_table);
		}
		tbp = new + cursize_tblocks;
		tblock_table = new;
		cursize_tblocks += incr;
		curfree_tblocks += incr;
		tblock_tabend = tblock_table + cursize_tblocks;
		while (tbp < tblock_tabend) {
			tbp->tb_free = TRUE;
			tbp++;
		}

		overflow_tblocks = 0;
	}

	/* update minimum free amount variable */
	minfree_tblocks = ROUND((double)cursize_tblocks *
			       ((double)softlim_tblocks / 100.0));
	if (minfree_tblocks > cursize_tblocks - TBLOCK_HARDLIM)
		minfree_tblocks = cursize_tblocks - TBLOCK_HARDLIM;

	return (cursize_tblocks);
}

gcunmark_tblocks()
{
	return (0);
}

gcsweep_tblocks()
{
	register int	nfound = 0;

	return (nfound);
}
