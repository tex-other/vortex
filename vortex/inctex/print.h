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
 * @(#)print.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

#define NO_PRINT            16 
#define TERM_ONLY           17
#define LOG_ONLY            18
#define TERM_AND_LOG        19
#define PSEUDO              20
#define NEW_STRING          21
#define MAX_SELECTOR        21

global  alpha_file      log_file;

global  int         selector;

global  int         term_offset;
global  int         file_offset;

global  char        dig[];
global  val         tally;
global  ascii       trick_buf[];
global  val         trick_count;
global  val         first_count;

int     print();
int     print_char();
int     print_sym();
int     print_ln();
int     print_esc();
int     print_nl();
int     print_the_digs();
int     print_int();
int     print_val();
int     print_hex();
int     print_ASCII();
int     print_roman_int();
int     print_cur_str();

#define wterm(c)        putchar(c);
#define wterm_ln(c)     {putchar(c); putchar('\n');}
#define wterm_cr()      putchar('\n');

#define wlog(c)         putc(c, log_file)
#define wlog_ln(c)      {putc(c, log_file); putc('\n', log_file);}
#define wlog_cr()       putc('\n', log_file);

#define wfile(c)        putc(c, write_file[selector])
#define wfile_ln(c)     {putc(c, write_file[selector]); \
                        putc('\n', write_file[selector]);}
#define wfile_cr()      putc('\n', write_file[selector]);
