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
 * @(#)dvi.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  byte_file   dvi_file;
global  str         dvi_name;

#define SET_CHAR_0      0
#define SET1            128
#define SET_RULE        132
#define PUT1            133
#define PUT_RULE        137
#define NOP             138
#define BOP             139
#define EOP             140
#define PUSH            141
#define POP             142
#define RIGHT1          143
#define W0              147
#define W1              148
#define X0              152
#define X1              153
#define DOWN1           157
#define Y0              161
#define Y1              162
#define Z0              166
#define Z1              167
#define FNT_NUM_0       171
#define FNT1            235
#define XXX1            239
#define XXX4            242
#define FNT_DEF1        243
#define PRE             247
#define POST            248
#define POST_POST       249
#define ID_BYTE         2

global  qword       c;
global  qword       f;
global  ptr         g;
global  int         dead_cycles;
global  bool        doing_leaders;
global  val         last_bop;
global  val         lq;
global  val         lr;
global  val         lx;
global  scal        max_h;
global  scal        max_v;
global  int         max_push;
global  scal        rule_ht;
global  scal        rule_dp;
global  scal        rule_wd;

global  fnt         dvi_f;
global  scal        dvi_h;
global  scal        dvi_v;
global  scal        cur_h;
global  scal        cur_v;
global  int         cur_s;

global  ptr         down_ptr;
global  ptr         right_ptr;

global  byte        dvi_buf[];
global  ptr         dvi_limit; 
global  ptr         dvi_ptr;
global  val         dvi_offset;
global  val         dvi_gone;
global  bool        output_active;
global  int         total_pages;
    
#define MOVEMENT_NODE_SIZE      3
#define location(L)             mem[L + 2].i

#define synch_h() \
    {if (cur_h != dvi_h) \
        {movement(cur_h - dvi_h, RIGHT1); dvi_h = cur_h;}}

#define synch_v() \
    {if (cur_v != dvi_v) \
        {movement(cur_v - dvi_v, DOWN1); dvi_v = cur_v;}}

#define dvi_out(B) \
    {dvi_buf[dvi_ptr] = B; \
    incr(dvi_ptr); \
    if (dvi_ptr == dvi_limit) \
        dvi_swap();}

#define write_dvi(a, b) \
    {fwrite(&dvi_buf[a], sizeof(dvi_buf[0]), b - a, dvi_file);}


int     dvi_swap();
int     dvi_four();
int     dvi_pop();
int     dvi_font_def();
int     movement();
int     prune_movements();
int     hlist_out();
int     vlist_out();
int     ship_out();
