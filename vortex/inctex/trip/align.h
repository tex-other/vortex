
/*
 * @(#)align.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
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
