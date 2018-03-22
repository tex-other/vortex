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

#ifndef EXPAND_
#define EXPAND_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		expand.h
 */

int		get_x_token();
int		expand();
int		insert_relax();

#define	TOP_MARK_CODE				0
#define	FIRST_MARK_CODE				1
#define	BOT_MARK_CODE				2
#define	SPLIT_FIRST_MARK_CODE			3
#define	SPLIT_BOT_MARK_CODE			4

#define	top_mark				cur_mark[TOP_MARK_CODE]
#define	first_mark				cur_mark[FIRST_MARK_CODE]
#define	bot_mark				cur_mark[BOT_MARK_CODE]
#define	split_first_mark			cur_mark[SPLIT_FIRST_MARK_CODE]
#define	split_bot_mark				cur_mark[SPLIT_BOT_MARK_CODE]

global	ptr	cur_mark[];

global	int	long_state;
global	ptr	pstack[];

int		macro_call();
int		x_token();

#endif
