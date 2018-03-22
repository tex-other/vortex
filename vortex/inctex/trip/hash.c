
/*
 * @(#)hash.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include	"tex.h"
#include	"box.h"
#include	"scan.h"
#include	"math.h"
#include	"boxlists.h"

#ifdef INCTEX
extern	ptr     hash_used;
extern	bool    no_new_control_sequence;
extern	int     cs_count;
extern	hh	hash[UNDEFINED_CONTROL_SEQUENCE+1];
#else
ptr		hash_used			= FROZEN_CONTROL_SEQUENCE;
bool		no_new_control_sequence		= TRUE;
int		cs_count			= 0;
hh		hash[UNDEFINED_CONTROL_SEQUENCE+1];
#endif


ptr
id_lookup (j, l)
    int     j;
    int     l;
{
    int     h;
    int     k;
    ptr     p;

    h = buffer[j];
    for (k = j + 1; k < j + l; incr(k)) {
        h = h + h + buffer[k];
        while (h >= HASH_PRIME)
            h -= HASH_PRIME;
    }
    for (p = h + HASH_BASE; ; p = next(p)) {
        if (text(p) > 0 && length(text(p)) == l)
            if (str_eq_buf(text(p), j))
                return p;
        if (next(p) == 0) {
            if (no_new_control_sequence)
                return UNDEFINED_CONTROL_SEQUENCE;
            if (text(p) > 0) {
                do
                    if (hash_is_full)
                        overflow("hash size", HASH_SIZE);
                    else decr(hash_used);
                while (text(hash_used) != 0);
                next(p) = hash_used;
                p = hash_used;
            }
            str_room(l);
            for (k = j; k < j + l; incr(k))
                append_char(buffer[k]);
            text(p) = make_str();
#ifdef STAT
            incr(cs_count);
#endif
            return p;
        }
    }
}

print_cs (p)
    ptr     p;
{   
    if (p < HASH_BASE) {
        if (p >= SINGLE_BASE) {
            if (p == NULL_CS) {
                print_esc("csname");
                print_esc("endcsname");
            } else {
                print_esc(""); 
                print_str(p - SINGLE_BASE); 
                if (cat_code(p - SINGLE_BASE) == LETTER)
                    print_char(' ');
            }
        } else if (p < ACTIVE_BASE)
            print_esc("IMPOSSIBLE.");
        else print_str(p - ACTIVE_BASE);
    } else if (p >= UNDEFINED_CONTROL_SEQUENCE) {
        print_esc("IMPOSSIBLE.");
    } else if (text(p) < 0 || text(p) >= str_ptr) {
        print_esc("NONEXISTENT.");
    } else {
        print_esc("");
        slow_print(text(p));
        print_char(' ');
    }
}

sprint_cs (p)
    ptr     p;
{
    if (p < HASH_BASE) {
        if (p < SINGLE_BASE) {
            print_str(p - ACTIVE_BASE);
        } else if (p < NULL_CS) {
            print_esc("");
            print_str(p - SINGLE_BASE);
        } else {
            print_esc("csname");
            print_esc("endcsname");
        }
    } else {
        print_esc("");
        slow_print(text(p));
    }
}

#ifdef INIT
primitive (s, code, order)
    chrs    s;
    qword   code;
    hword   order;
{
    ascii   c;
    int     j;
    int     k;
    int     l;
    str     new_str;

    if (s[1] == NUL)
        cur_val = s[0] + SINGLE_BASE;
    else {
        new_str = make_str_given(s);
        k = str_start[new_str];
        l = length(new_str);
        for (j = 0; j < l; incr(j))
            buffer[j] = str_pool[k + j];
        cur_val = id_lookup(0, l);
        flush_string();
        text(cur_val) = new_str;
    }
    eq_level(cur_val) = LEVEL_ONE;
    eq_type(cur_val) = code;
    equiv(cur_val) = order;
}
#endif
