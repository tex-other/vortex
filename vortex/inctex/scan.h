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
 * @(#)scan.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

int     scan_left_brace();
int     scan_optional_equals();
bool    scan_keyword();

global  val     cur_val;
global  int     cur_val_level;

int     scan_something_internal();

#define INT_VAL         0
#define DIMEN_VAL       1
#define GLUE_VAL        2
#define MU_VAL          3
#define IDENT_VAL       4
#define TOK_VAL         5

int     scan_eight_bit_int();
int     scan_seven_bit_int();
int     scan_four_bit_int();
int     scan_char_num();
int     scan_fifteen_bit_int();
int     scan_twenty_seven_bit_int();

int     scan_int();
#define INFINITY    017777777777

global  int     radix;

#define PLUS_TOKEN          (OTHER_TOKEN + '+')
#define MINUS_TOKEN         (OTHER_TOKEN + '-')
#define ZERO_TOKEN          (OTHER_TOKEN + '0')
#define A_TOKEN             (LETTER_TOKEN + 'A')
#define OTHER_A_TOKEN       (OTHER_TOKEN + 'A')
#define OCTAL_TOKEN         (OTHER_TOKEN + '\'')
#define HEX_TOKEN           (OTHER_TOKEN + '"')
#define ALPHA_TOKEN         (OTHER_TOKEN + '`')
#define POINT_TOKEN         (OTHER_TOKEN + '.')
#define EURO_POINT_TOKEN    (OTHER_TOKEN + ',')

int     scan_dimen();
#define MAX_DIMEN   07777777777

#define scan_normal_dimen()     scan_dimen(FALSE, FALSE, FALSE)

global  gord    cur_order;
int     scan_glue();

#define scan_optional_space() \
    {get_x_token(); if (cur_cmd != SPACER) back_input();}

#define get_nbx_token() \
    {do get_x_token(); while (cur_cmd == SPACER);}

#define get_nbrx_token() \
    {do get_x_token(); while (cur_cmd == SPACER || cur_cmd == RELAX);}

ptr     scan_rule_spec();
