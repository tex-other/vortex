/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifndef PAGE_
#define PAGE_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		page.h
 */

global	ptr		last_page_glue;

#define	active_height	active_width
#define	cur_height	active_height[1]

ptr			prune_page_top();

global	scal		best_height_plus_depth;
#define	DEPLORABLE				100000

ptr			vert_break();
ptr			vsplit();

global	ptr		page_tail;
global	int		page_contents;

#define	INSERTS_ONLY				1
#define	BOX_THERE				2

global	ptr		best_page_break;

global	scal		best_size;
global	scal		page_max_depth;
global	val		least_page_cost;

#define	INSERTING				0
#define	SPLIT_UP				1
#define	broken_ptr(N)				link(N + 1)
#define	broken_ins(N)				info(N + 1)
#define	last_ins_ptr(N)				link(N + 2)
#define	best_ins_ptr(N)				info(N + 2)
#define	PAGE_INS_NODE_SIZE			4

global	scal		page_so_far[];

#define	page_goal				page_so_far[0]
#define	page_total				page_so_far[1]
#define	page_shrink				page_so_far[6]
#define	page_depth				page_so_far[7]

global	ptr		last_glue;
global	val		last_penalty;
global	scal		last_kern;
global	val		insert_penalties;

int			print_totals();

#define	start_new_page() \
	{page_contents = EMPTY; \
	page_tail = page_head; \
	link(page_head) = NULL; \
	last_glue = MAX_HALFWORD; \
	last_penalty = 0; \
	last_kern = 0; \
	page_depth = 0; \
	page_max_depth = 0;}

int		freeze_page_specs();

global	bool	output_active;

int		ensure_vbox();
int		box_error();
int		build_page();
int		fire_up();

#define	contrib_tail			nest[0].tail_field

#endif
