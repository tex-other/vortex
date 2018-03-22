
/*
 * @(#)tokenlists.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
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
