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

/* This file is part of IncTeX 1.0
 *
 * Copyright (C) 1992 by Regents of the University of California
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/* @(#)str.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include "tex.h"

#ifdef INCTEX
extern	str     str_ptr;
ptr     str_start[MAX_STRINGS];
ascii   str_pool[POOL_SIZE];
extern	ptr     pool_ptr;
#else
str     str_ptr;
ptr     str_start[MAX_STRINGS];
ascii   str_pool[POOL_SIZE];
ptr     pool_ptr;
#endif

str     null_str;

bool
str_eq_buf (s, k)
    str     s;
    int     k;
{
    int     j;
    
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
    str     s;
    str     t;
{
    int     j;
    int     k;

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
make_str ()
{
    incr(str_ptr);
    if (str_ptr == MAX_STRINGS)
        overflow("number of strings", MAX_STRINGS);
    str_start[str_ptr] = pool_ptr;
    return (str_ptr - 1);
}

str
make_str_given (s)
    chrs    s;
{
    while (*s != NUL) {
        append_char(*s);
        incr(s);
    }
    return (make_str());
}

init_strings ()
{
    int     k;

    str_ptr = pool_ptr = 0;
    for (k = 0; k <= 255; incr(k)) {
        if (k < ' ') {
            append_char('^');
            append_char('^');
            append_char(k + 0100);
        } else if (k == 127) {
            make_str_given("^^?");
			continue;
		} else {
			append_char(k);
		}
        make_str();
    }
    null_str = make_str();
}
