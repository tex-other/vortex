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
 * @(#)boxlists.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

int     append_glue();

#define FIL_CODE            0
#define FILL_CODE           1
#define SS_CODE             2
#define FIL_NEG_CODE        3
#define SKIP_CODE           4
#define MSKIP_CODE          5

int     append_kern();

int     handle_right_brace();
int     extra_right_brace();

global  ptr     cur_box;

int     begin_box();
int     scan_box();
int     scan_spec();
int     package();

int     box_end();

#define BOX_FLAG            010000000000
#define SHIP_OUT_FLAG       (BOX_FLAG + 512)
#define LEADER_FLAG         (BOX_FLAG + 513)
#define BOX_CODE            0
#define COPY_CODE           1
#define LAST_BOX_CODE       2
#define VSPLIT_CODE         3
#define VTOP_CODE           4

int     normal_paragraph();
int     new_graf();
int     indent_in_hmode();
int     head_for_vmode();
int     end_graf();
int     append_to_vlist();
int     begin_insert_or_adjust();
int     make_mark();
int     append_penalty();
int     delete_skip();
int     unpackage();
int     append_italic_correction();
int     append_discretionary();
int     build_discretionary();
int     make_accent();
int     align_error();
int     no_align_error();
int     omit_error();
int     do_endv();
int     cs_error();
