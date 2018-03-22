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

#ifndef STR_
#define STR_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		string.h
 */

global	ptr		str_start[];
global	str		str_ptr;

global	ascii		str_pool[];
global	ptr		pool_ptr;

global	str		null_str;

#define	length(S)		(str_start[S + 1] - str_start[S])
#define	cur_length()		(pool_ptr - str_start[str_ptr])

#define	append_char(C) 		{str_pool[pool_ptr] = C; incr(pool_ptr);}
#define	flush_char()		decr(pool_ptr)

#define	str_room(S) \
	{if (pool_ptr + S >= POOL_SIZE) \
		overflow("pool_size", POOL_SIZE);}

str		make_string();
str		make_string_given();

#define	flush_string() \
	{decr(str_ptr); pool_ptr = str_start[str_ptr];}

bool	str_eq_buf();
bool	str_eq_str();

int	init_strings();

#endif
