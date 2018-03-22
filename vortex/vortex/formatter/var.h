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

#ifndef VAR_
#define VAR_

/*
  var.h
*/

extern short	input_flag;
extern short	input_state_level;
extern struct input_state_record	input_state_stack[];
extern short	vmode_doll;
extern short	backup_real_token;
extern short	macro_first_token;
extern short	macro_que_enable;
extern short	real_token;
extern short	real_input;
extern short	warming;
extern short    cond_skip;
extern short    token_count;
extern short	prefix_flag;
extern short    cond_level;
extern short	disp_watch;
extern char	defed_char;
extern short	catcode;
extern struct _cseq 	*macro_node_name;
extern struct _cseq	*backup_cs_node;
extern short	backup_cs_level;
extern struct _cseq	*cur_cs_node;
extern struct _group	*cur_group_node;
extern struct _node	*token_node_last;
extern struct _unode  	*par_node_last;
extern struct _unode	*par_node_top;
extern struct _char	*begin_of_space_token;
extern struct _char	*begin_of_word_token;
extern struct _char	*group_s_node_begin;
extern struct _char    *group_s_node_end;
extern struct _char	*begin_of_cs_token;
extern struct _char	*irs_ptr;
extern struct _char	*irs_next;
extern struct _char	*irs_bol;
extern struct _char	*irs_eol;
extern short	irs_read_only;
extern ptr	math_que_top;
extern ptr	math_que_end;
extern ptr	cs_que_top;
extern ptr	cs_que_end;

#endif
