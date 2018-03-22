/* 
 * Copyright (c) 1986-1992 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* Copyright (c) 1992 Regents of the University of California
 * All rights reserved.
 */
#define EXT_LASTSTATE	"lastchkpt"
#define MAX_QSC_CHECKS 10
#define MaxQSCMsgs 4
#define EXPLAINIT explain && (n_qsc_msgs<MaxQSCMsgs)
#define TALK (showq || (EXPLAINIT))

extern short	*global_chset,
		*eqtb_chset,
		*hash_chset,
		*xeq_chset;
extern int	explain,
		n_qsc_msgs,
		showq;

extern DIFF_CHUNK
		*fch_ptr,	/* ptr into fdim_list */
		*fchset,	/* says whether entry has been checked */
		*fchset_end,
		*fchset_ptr;
		/* fdim_list is already declared: font addr list */
		/* keep fchset_ptr, fch_ptr in lock step */
extern int	fcount;

#define SET_ELEMBAD -2
#define SET_CHECKBAD -1
#define ITEM_CLEAR 0
#define ITEM_CHANGED 1
#define ITEM_EQUIV_CURR 2
#define ITEM_EQUIV_INIT 3
#define ITEM_UNEQUIV_CURR 4
#define ITEM_UNEQUIV_INIT 5
#define ITEM_MAXVAL ITEM_UNEQUIV_INIT
