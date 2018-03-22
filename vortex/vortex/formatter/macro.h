/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 * The above licensing information supersedes all licensing information
 * below.
 */

#ifndef MACRO_
#define MACRO_

/*
macro.h
*/


struct input_state_record {
  struct _group	*cur_group_node;
  struct _node	*token_node_last;
  struct _unode	*par_node_last;
};
  
#define UNKNOWN				0
#define	NOSKIP				0
#define	DOSKIP				1
#define	HLIST				2
#define	IGN_SP				3
#define	MATH_QUE_SIZE			3
#define	MATH_QUE_NODE			100
#define	math_char_field(M)		mem[M + 2].i
#define math_rlink(M)			mem[M + 1].hh.hh1.lh
#define math_llink(M)			mem[M + 1].hh.hh1.rh
#define CS_QUE_SIZE			3
#define	CS_QUE_NODE			101
#define cs_rlink(M)			mem[M + 1].hh.hh1.lh
#define cs_llink(M)			mem[M + 1].hh.hh1.rh
#define cs_node_field(M)		mem[M + 2].i

#endif
