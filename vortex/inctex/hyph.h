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
 * @(#)hyph.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  ptr     ha;
global  ptr     hb;
global  hword   hc[];
global  int     hn;
global  fnt     hf;
global  ascii   hu[];
global  int     hyf_char;

int     hyphenate();

global  byte    hyf[];
global  int     hyphen_passed;

int     reconstitute();

#define trie_link(T)        trie[T].hh2.rh
#define trie_char(T)        trie[T].hh2.b1
#define trie_op(T)          trie[T].hh2.b0

global  hh  trie[];
global  int     trie_max;

global  int     hyf_distance[];
global  int     hyf_num[];
global  qword   hyf_next[];

int     new_hyph_exceptions();

global  str     hyph_word[];
global  ptr     hyph_list[];
global  int     hyph_count;

global  qword       trie_op_ptr;
#ifdef INIT

global  qword   trie_op_hash[];
global  int     trie_min;

qword   new_trie_op();

#define trie_root       trie_l[0]

global  int     trie_ptr;
global  ascii   trie_c[];
global  qword   trie_o[];
global  int     trie_l[];
global  int     trie_r[];

global  int     trie_hash[];

int     trie_node();
int     compress_trie();

int     init_pattern_memory();

#define trie_ref            trie_hash
#define trie_back(T)        trie[T].hh1.lh

global  bool    trie_taken[];

int     init_trie_memory();

int     first_fit();
int     trie_pack();
int     trie_fix();

int     new_patterns();

#endif
