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
 * @(#)evalstack.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

#define VMODE       1
#define HMODE       (VMODE + MAX_COMMAND + 1)
#define MMODE       (HMODE + MAX_COMMAND + 1)

int     push_nest();
int     pop_nest();
int     print_mode();

#define IGNORE_DEPTH    -65536000

typedef struct
{
    int     mode_field;
    ptr     head_field;
    ptr     tail_field;
    int     pg_field;
    val     aux_field;
    val     ml_field;
}
    list;

global  list    cur_list;
global  ptr     nest_ptr;
global  list    nest[];
global  int     max_nest_stack;

#define mode                cur_list.mode_field
#define head                cur_list.head_field
#define tail                cur_list.tail_field
#define prev_graf           cur_list.pg_field
#define aux                 cur_list.aux_field
#define prev_depth          aux
#define space_factor        aux
#define incompleat_noad     aux
#define mode_line           cur_list.ml_field

global  int     shown_mode;

int     show_activities();

#define tail_append(N) \
    {link(tail) = N; tail = link(tail);}
