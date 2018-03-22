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
 * @(#)cond.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  ptr     cond_ptr;
global  int     cur_if;
global  int     if_limit;
global  val     if_line;

#define IF_NODE_SIZE        2
#define if_line_field(M)    mem[M + 1].i

#define IF_CODE             1
#define FI_CODE             2
#define ELSE_CODE           3
#define OR_CODE             4

#define IF_CHAR_CODE        0
#define IF_CAT_CODE         1
#define IF_INT_CODE         2
#define IF_DIM_CODE         3
#define IF_ODD_CODE         4
#define IF_VMODE_CODE       5
#define IF_HMODE_CODE       6
#define IF_MMODE_CODE       7
#define IF_INNER_CODE       8
#define IF_VOID_CODE        9
#define IF_HBOX_CODE        10
#define IF_VBOX_CODE        11
#define IFX_CODE            12
#define IF_EOF_CODE         13
#define IF_TRUE_CODE        14
#define IF_FALSE_CODE       15
#define IF_CASE_CODE        16

int     push_cond();
int     pop_cond();
int     pass_text();
int     change_if_limit();
int     conditional();

global  long    skip_line;
