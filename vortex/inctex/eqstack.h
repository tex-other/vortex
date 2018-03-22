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
 * @(#)eqstack.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  ptr     save_ptr;
global  mword   save_stack[];
global  ptr     max_save_stack;

global  qword   cur_level;
global  group   cur_group;
global  ptr     cur_boundary;

#define save_type(S)            save_stack[S].hh.hh2.b0
#define save_level(S)           save_stack[S].hh.hh2.b1
#define save_index(S)           save_stack[S].hh.hh2.rh

#define RESTORE_OLD_VALUE       0
#define RESTORE_ZERO            1
#define INSERT_TOKEN            2
#define LEVEL_BOUNDARY          3

#define BOTTOM_LEVEL            0
#define SIMPLE_GROUP            1
#define HBOX_GROUP              2
#define ADJUSTED_HBOX_GROUP     3
#define VBOX_GROUP              4
#define VTOP_GROUP              5
#define ALIGN_GROUP             6
#define NO_ALIGN_GROUP          7
#define OUTPUT_GROUP            8
#define MATH_GROUP              9
#define DISC_GROUP              10
#define INSERT_GROUP            11
#define VCENTER_GROUP           12
#define MATH_CHOICE_GROUP       13
#define SEMI_SIMPLE_GROUP       14
#define MATH_SHIFT_GROUP        15
#define MATH_LEFT_GROUP         16
#define MAX_GROUP_CODE          16

#define saved(I)    save_stack[save_ptr + I].i

int     new_save_level();
int     eq_destroy();
int     eq_save();
int     eq_define();
int     eq_word_define();
int     geq_define();
int     geq_word_define();
int     unsave();
int     offsave();

#ifdef STAT
int     restore_trace();
#endif
