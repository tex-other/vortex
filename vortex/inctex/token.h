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
 * @(#)token.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

global  tok     cur_tok;
global  hword   cur_cmd;
global  hword   cur_chr;
global  ptr     cur_cs;

#define LEFT_BRACE_TOKEN    0400
#define LEFT_BRACE_LIMIT    01000
#define RIGHT_BRACE_TOKEN   01000
#define RIGHT_BRACE_LIMIT   01400
#define MATH_SHIFT_TOKEN    01400
#define TAB_TOKEN           02000
#define OUT_PARAM_TOKEN     02400
#define SPACE_TOKEN         05040
#define LETTER_TOKEN        05400
#define OTHER_TOKEN         06000
#define MATCH_TOKEN         06400
#define END_MATCH_TOKEN     07000
#define CS_TOKEN_FLAG       010000
#define END_TEMPLATE_TOKEN  (CS_TOKEN_FLAG + FROZEN_END_TEMPLATE)

#define NO_EXPAND_FLAG      257

global  ptr     par_loc;
global  tok     par_token;
global  bool    force_eof;

#define token(T)        tok_mem[T]
#define token_link(T)   tok_link[T]

global  tok     tok_mem[];
global  ptr     tok_link[];
global  ptr     tok_head;
global  ptr     tok_low;
global  ptr     tok_end;
global  int     tok_used;

#define temp_toks       TOK_TOP
#define align_tokens    (TOK_TOP - 1)
#define omit_template   (TOK_TOP - 2)
#define null_list       (TOK_TOP - 3)
#define backup_tokens   (TOK_TOP - 4)
#define tok_high        (TOK_TOP - 4)
#define tok_usage       5

#ifdef  STAT
#define fast_new_token(T) \
    {T = tok_head; \
    if (T == NULL) T = new_token(); \
    else {tok_head = token_link(T); token_link(T) = NULL; incr(tok_used);}}
#else
#define fast_new_token(T) \
    {T = tok_head; \
    if (T == NULL) T = new_token(); \
    else {tok_head = token_link(T); token_link(T) = NULL;}}
#endif

int     get_token();
int     get_next();
ptr     new_token();

#ifdef STAT
#define free_token(T) \
    {token_link(T) = tok_head; tok_head = T; decr(tok_used);}
#else
#define free_token(T) \
    {token_link(T) = tok_head; tok_head = T;}
#endif

int     check_outer_validity();
int     firm_up_the_line();
