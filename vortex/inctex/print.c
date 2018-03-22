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
 * This file has been modified, with permission from Pat Monardo, for IncTeX
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/* @(#)print.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"texext.h"

#ifdef INCTEX
extern	int     selector;
/* define another selector mode */
#define CHECK_FILE 15 
extern
FILE	*parse_check_file;
#define wcheck_cr() putc('\n', parse_check_file);
#define wcheck(c)   putc(c, parse_check_file);
#else
int 		selector			= TERM_ONLY;
#endif
int		file_offset;
int		term_offset;

alpha_file 	log_file;
char		dig[23];
val		first_count;
val		tally;
ascii		trick_buf[ERROR_LINE];
val		trick_count;

print_ln ()
{
    switch (selector)
    {
#ifdef INCTEX
    case CHECK_FILE:
        wcheck_cr();
        file_offset = 0;
        break;

#endif
    case TERM_AND_LOG:
        wterm_cr();
        term_offset = 0;
        wlog_cr();
        file_offset = 0;
        break;

    case LOG_ONLY:
        wlog_cr();
        file_offset = 0;
        break;

    case TERM_ONLY:
        wterm_cr();
        term_offset = 0;
        break;

    case NO_PRINT:
    case PSEUDO:
    case NEW_STRING:
        break;

    default:
        wfile_cr();
        break;
    }
}   

print_char (c)
    ascii   c;
{
    if (c == new_line_char && selector < PSEUDO) {
        print_ln();
        return;
    }
    switch (selector)
    {
#ifdef INCTEX
    case CHECK_FILE:
        wcheck(xchr[c]);
        break;

#endif
    case TERM_AND_LOG:
        wterm(xchr[c]);
        incr(term_offset);
        wlog(xchr[c]);
        incr(file_offset);
        if (term_offset == MAX_PRINT_LINE) {
            wterm_cr();
            term_offset = 0;
        }
        if (file_offset == MAX_PRINT_LINE) {
            wlog_cr();
            file_offset = 0;
        }
        break;

    case LOG_ONLY:
        wlog(xchr[c]);
        incr(file_offset);
        if (file_offset == MAX_PRINT_LINE)
            print_ln();
        break;

    case TERM_ONLY:
        wterm(xchr[c]);
        incr(term_offset);
        if (term_offset == MAX_PRINT_LINE)
            print_ln();
        break;

    case NO_PRINT:
        return;

    case PSEUDO:
        if (tally < trick_count) 
            trick_buf[tally % ERROR_LINE] = c;
        break;

    case NEW_STRING:
        if (pool_ptr < POOL_SIZE) 
            append_char(c);
        break;

    default:
        wfile(xchr[c]);
        break;
    }
    incr(tally);
}

print_ASCII (c)
    int      c;
{
    if (c >= 0 && c <= 127) {
        print_str(c);
    } else {
        print_char('[');
        if (c < 0)
            print_int(c);
        else print_hex((val) c);
        print_char(']');
    }
}

print_str (s)
    str     s;
{
    int     j;

    if (s >= str_ptr) {
        s = make_str_given("???");
    } else if (s < 128) {
        if (s < 0) {
            s = make_str_given("???");
        } else if (s == new_line_char && selector < PSEUDO) {
            print_ln();
            return;
        }
    }
    j = str_start[s];
    while (j < str_start[s + 1]) {
        print_char(str_pool[j]);
        incr(j);
    }
}

slow_print (s)
    str     s;
{
    int     j;

    if (s >= str_ptr) {
        s = make_str_given("???");
    } else if (s < 128) {
        if (s < 0) {
            s = make_str_given("???");
        } else if (s == new_line_char && selector < PSEUDO) {
            print_ln();
            return;
        }
    }
    j = str_start[s];
    while (j < str_start[s + 1]) {
        print_str(str_pool[j]);
        incr(j);
    }
}

print (s)
    chrs    s;
{
    while (*s) { 
        print_char(*s);
        incr(s);
    }
}

print_nl (s)
    chrs    s;
{
    if (term_offset > 0 && odd(selector) ||
        file_offset > 0 && selector >= LOG_ONLY)
        print_ln();
    print(s);
}

print_esc (s)
    chrs    s;
{
    ascii   c;

    c = escape_char;
    if (c >= 0 && c < 128) 
        print_str(c);
    print(s);
}

print_int (n)
    int     n;
{
    int     k;
    int     m;

    k = 0;
    if (n < 0)  {
        print_char('-');
        negate(n);
    }
    do {
        dig[k] = n % 10;
        n /= 10;
        incr(k);
    } while (n != 0);
    print_the_digs(k);
}

print_val (n)
    val     n;
{
    int     k;
    val     m;

    k = 0;
    if (n < 0)  {
        print_char('-');
        if (n > -100000000) {
            negate(n);
        } else {
            m = -1 - n;
            n = m / 10;
            m = m % 10 + 1;
            k = 1;
            if (m < 10)
                dig[0] = m;
            else {
                dig[0] = 0;
                incr(n);
            }
        }
    }
    do {
        dig[k] = n % 10;
        n /= 10;
        incr(k);
    } while (n != 0);
    print_the_digs(k);
}

print_hex (v)
    val     v;
{
    int     k;

    k = 0;
    print_char('"');
    do {
        dig[k] = v&15;
        v >>= 4;
        incr(k);
    } while (v != 0);
    print_the_digs(k);
}

print_the_digs (k)
    int      k;
{
    while (k > 0) {
        decr(k);
        if (dig[k] < 10)
            print_char('0' + dig[k]);
        else print_char('A' - 10 + dig[k]);
    }
}

print_two (n)
    int     n;
{
    n = abs(n) % 100;
    print_char('0' + n / 10);
    print_char('0' + n % 10);
}

print_roman_int (n)
    val     n;
{
    chrs    j = "m2d5c2l5x2v5i";
    chrs    k;
    val     u;
    val     v = 1000;

    loop {
        while (n >= v) {
            print_char(*j);
            n -= v;
        }
        if (n <= 0) return;
        k = j + 2;
        u = v / (*(k - 1) - '0');
        if (*(k - 1) == '2')  {
            k += 2;
            u /= *(k - 1) - '0';
        }
        if (n + u >= v)  {
            print_char(*k);
            n += u;
        } else {
            j += 2;
            v /= *(j - 1) - '0';
        }
    }
}

print_cur_str ()
{
    int     j;

    j = str_start[str_ptr];
    while (j < pool_ptr) {
        print_char(str_pool[j]);
        incr(j);
    }
}
