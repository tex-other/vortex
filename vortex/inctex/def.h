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
 * @(#)def.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  hword   after_token;
global  bool    long_help_seen;
global  val     mag_set;

int     get_r_token();
int     prefixed_command();

#define CHAR_DEF_CODE       0
#define MATH_CHAR_DEF_CODE  1
#define COUNT_DEF_CODE      2
#define DIMEN_DEF_CODE      3
#define SKIP_DEF_CODE       4
#define MU_SKIP_DEF_CODE    5
#define TOKS_DEF_CODE       6

int     do_register_command();
int     trap_zero_glue();
int     alter_aux();
int     alter_prev_graf();
int     alter_page_so_far();
int     alter_integer();
int     alter_box_dimen();
int     new_font();
int     prepare_mag();
int     new_interaction();
int     do_assignments();
int     clopen_stream();
int     issue_message();
int     give_err_help();
int     shift_case();
int     show_whatever();

#define SHOW_CODE           0
#define SHOW_BOX_CODE       1
#define SHOW_THE_CODE       2
#define SHOW_LISTS          3
