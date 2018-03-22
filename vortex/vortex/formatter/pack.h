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

#ifndef PACK_
#define PACK_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		pack.h
 */

#define	EXACTLY				0
#define	ADDITIONAL			1
#define	NATURAL				0L, ADDITIONAL

global	ptr	adjust_tail;
global	scal	total_stretch[];
global	scal	total_shrink[];

global	long	pack_begin_line;

#define	make_char_from_lig() \
	{mem[lig_trick] = mem[lig_char(p)]; \
	link(lig_trick) = link(p); \
	p = lig_trick;}

#define	get_stretch_order() \
	{if (total_stretch[FILLL] != 0) o = FILLL; \
	else if (total_stretch[FILL] != 0) o = FILL; \
	else if (total_stretch[FIL] != 0) o = FIL; \
	else o = NORMAL;}
			
#define	get_shrink_order() \
	{if (total_shrink[FILLL] != 0) o = FILLL; \
	else if (total_shrink[FILL] != 0) o = FILL; \
	else if (total_shrink[FIL] != 0) o = FIL; \
	else o = NORMAL;}

#define	vpack(P, H) 		vpackage(P, H, MAX_DIMEN)
ptr		vpackage();
ptr		hpack();

#endif
