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
 * @(#)align.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

int     push_alignment();
int     pop_alignment();

#define ALIGN_STACK_NODE_SIZE   5

#define u_part(L)               mem[L + HEIGHT_OFFSET].i
#define v_part(L)               mem[L + DEPTH_OFFSET].i
#define extra_info(L)           info(L + LIST_OFFSET)

#define SPAN_CODE               256
#define CR_CODE                 257
#define CR_CR_CODE              CR_CODE + 1

#define SPAN_NODE_SIZE          2

#define preamble                link(align_head)

global  ptr     cur_align;
global  ptr     cur_span;
global  ptr     cur_loop;
global  ptr     cur_head;
global  ptr     cur_tail;
global  ptr     align_ptr;

int     init_align();
int     get_preamble_token();
int     align_peek();
int     init_row();
int     init_span();
int     init_col();
bool    fin_col();
int     fin_row();
int     fin_align();
