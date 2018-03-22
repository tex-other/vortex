
/*
 * @(#)str.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  str     str_ptr;
global  ptr     str_start[];
global  ascii   str_pool[];
global  ptr     pool_ptr;

global  str     null_str;

#define length(S)           (str_start[S + 1] - str_start[S])
#define cur_length()        (pool_ptr - str_start[str_ptr])

#define append_char(C)      {str_pool[pool_ptr] = C; incr(pool_ptr);}
#define flush_char()        {decr(pool_ptr);}

#define str_room(S) \
    {if (pool_ptr + S >= POOL_SIZE) \
        overflow("pool_size", POOL_SIZE);}

str     make_str();
str     make_str_given();

#define flush_string() \
    {decr(str_ptr); pool_ptr = str_start[str_ptr];}

bool    str_eq_buf();
bool    str_eq_str();

int     init_strings();
