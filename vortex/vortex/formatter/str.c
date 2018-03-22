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


/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		str.c
 */

#include "tex.h"
#include "io.h"
#include "file.h"
#include "error.h"
#include "str.h"

extern ascii 	str_pool[];
extern ptr	pool_ptr;
extern ptr	str_start[];
extern str	str_ptr;
extern str	null_str;

bool
str_eq_buf (s, k)
	str		s;
	int		k;
{
	int		j;
	
	j = str_start[s];
	while (j < str_start[s + 1]) {
		if (str_pool[j] != buffer[k])
			return FALSE;
		incr(j); incr(k);
	}
	return TRUE;
}

bool
str_eq_str (s, t)
	str		s;
	str		t;
{
	int		j;
	int		k;

	if (length(s) != length(t))
		return FALSE;
	j = str_start[s];
	k = str_start[t];
	while (j < str_start[s + 1]) {
		if (str_pool[j] != str_pool[k])
			return FALSE;
		incr(j); incr(k);
	}
	return TRUE;
}

str
make_string ()
{
	incr(str_ptr);
	if (str_ptr == MAX_STRINGS)
		overflow("number of strings", MAX_STRINGS);
	str_start[str_ptr] = pool_ptr;
	return (str_ptr - 1);
}

str
make_string_given (s)
	char* 	s;
{
	while (*s != NUL) {
		append_char(*s);
		incr(s);
	}
	return (make_string());
}

init_strings ()
{
	int 	k;

	str_ptr = pool_ptr = 0;
	for (k = 0; k <= 127; incr(k)) {
		if (k < ' ') {
			append_char('^');
			append_char('^');
			append_char(k + 0100);
		} else if (k == 127)
			make_string_given("^^?");
		else
			append_char(k);
		make_string();
	}
	null_str = make_string();
}
