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
/*
 * @(#)tokenlists.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

ptr     str_toks();
ptr     the_toks();
int     conv_toks();

#define NUMBER_CODE         0
#define ROMAN_NUMERAL_CODE  1
#define STRING_CODE         2
#define MEANING_CODE        3
#define FONT_NAME_CODE      4
#define JOB_NAME_CODE       5

#define token_ref_count(T)  token(T)

ptr     scan_toks();
int     read_toks();
int     ins_the_toks();
int     print_meaning();

int     flush_list();

#define add_token_ref(T) incr(token_ref_count(T))

#ifdef	INCTEX
#define delete_token_ref(T) \
    {if (token_ref_count(T) == NULL) { \
	if (T < premac_lo || T > premac_hi) \
	        flush_list(T); \
    } else decr(token_ref_count(T));}
#else
#define delete_token_ref(T) \
    {if (token_ref_count(T) == NULL) \
        flush_list(T); \
    else decr(token_ref_count(T));}
#endif

#define store_new_token(T) \
    {q = new_token(); token_link(p) = q; token(q) = T; p = q;}

#define fast_store_new_token(T) \
    {fast_new_token(q); token_link(p) = q; token(q) = T; p = q;}

int     show_token_list();
int     token_show();
