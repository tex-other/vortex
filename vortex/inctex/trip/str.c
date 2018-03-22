
/*
 * @(#)str.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include "tex.h"

extern	str     str_ptr;
ptr     str_start[MAX_STRINGS];
ascii   str_pool[POOL_SIZE];
extern	ptr     pool_ptr;

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
