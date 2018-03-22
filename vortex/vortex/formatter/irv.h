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

#ifndef IRV_
#define IRV_

/*
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Development Environment
 *
 *  This file is part of the VorTeX incremental formatter,
 *
 *  Copyright (C) 1987 by	Ikuo Minakata	(min@renoir.berkeley.edu)
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

extern	make_par_node();
extern	input_flag;
extern	input_state_level;
extern	vmode_doll;
extern	backup_real_token;
extern	macro_first_token;
extern	macro_que_enable;
extern	real_token;
extern	real_input;
extern	warming;
extern	cond_skip;
extern	token_count;
extern	prefix_flag;
extern	cond_level;
extern	disp_watch;
extern struct _cseq	*macro_node_name;
extern struct _cseq	*backup_cs_node;
extern	backup_cs_level;
extern struct _cseq	*cur_cs_node;
extern struct _group	*cur_group_node;
extern struct _node	*token_node_last;
extern struct _unode  	*par_node_last;
extern struct _unode	*par_node_top;
extern struct _char	*begin_of_space_token;
extern struct _char	*begin_of_word_token;
extern struct _char	*group_s_node_begin;
extern struct _char    *group_s_node_end;
extern struct _char	*irs_ptr;
extern struct _char	*irs_next;
extern struct _char	*begin_of_cs_token;
extern struct input_state_record	input_state_stack[MAX_IN_OPEN];

#endif
