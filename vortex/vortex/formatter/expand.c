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


/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		expand.c
 */

#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"io.h"
#include	"eq.h"
#include	"hash.h"
#include	"box.h"
#include	"tokenstack.h"
#include	"scan.h"
#include	"token.h"
#include	"tokenlists.h"
#include	"cond.h"
#include	"file.h"
#include	"print.h"
#include	"error.h"
#include	"expand.h"

#ifdef VORTEX
#include	"allir.h"
#include	"main.h"
#include	"macro.h"
#include	"var.h"
#include	"msg.h"
#endif

extern int	long_state;
extern ptr	pstack[];
extern ptr	cur_mark[];

#ifdef VORTEX
extern short	macro_param_num;	/* number of macro param. read */
extern		make_group_node(), close_group_node();
#endif

get_x_token ()
{
#ifdef VORTEX
	short  save_token_count;
	short	 real_macro_flag;
  
	real_macro_flag = FALSE;
	macro_que_enable = TRUE;
#endif

restart:
#ifdef VORTEX
	save_token_count = token_count; 
#endif
	get_next();
	if (cur_cmd <= MAX_COMMAND)
		goto done;

	if (cur_cmd >= CALL) {
		if (cur_cmd < END_TEMPLATE)
#ifdef VORTEX
			if (real_token) {
				macro_call();
				/* # of macro param. and macro itself*/
				token_count = save_token_count + macro_param_num + 1;
				real_macro_flag = TRUE;
			} else
#endif
				macro_call();
		else {
			cur_cs = FROZEN_ENDV;
			cur_cmd = ENDV;
			goto done;
		}
	} else
		expand();
	goto restart;

done:
#ifdef VORTEX
	macro_que_enable = FALSE;
	if (real_macro_flag == TRUE)
		macro_first_token = TRUE;
#endif
	if (cur_cs == 0)
		cur_tok = cur_cmd * 0400 + cur_chr;
	else
		cur_tok = CS_TOKEN_FLAG + cur_cs;
}

expand ()
{
	int		j;
	ptr		p;
	ptr		q;
	ptr		r;
	hword		t;
	int		save_scanner_status;
	val		cv_backup = cur_val;
	int		radix_backup = radix;
	int		cvl_backup = cur_val_level;
	ptr		backup_backup = link(backup_head);	

#ifdef VORTEX
	short		save_cond_skip;
#endif
	if (cur_cmd < CALL) {
		if (tracing_commands > 1)
			show_cur_cmd_chr();
#ifdef VORTEX
		save_cond_skip = cond_skip;
		cond_skip = DOSKIP;
#endif
		switch (cur_cmd) {
		case TOP_BOT_MARK:
			if (cur_mark[cur_chr] != NULL)
				begin_token_list(cur_mark[cur_chr], MARK_TEXT);
			break;

		case EXPAND_AFTER: 
			get_token();
			t = cur_tok;
			get_token();
			if (cur_cmd > MAX_COMMAND)
				expand();
			else back_input();
			cur_tok = t;
			back_input();
			break;

		case NO_EXPAND:
			save_scanner_status = scanner_status;
			scanner_status = NORMAL;
			get_token(); 
			scanner_status = save_scanner_status;
			t = cur_tok;
			back_input();
			if (t >= CS_TOKEN_FLAG) {
				p = get_avail();
				info(p) = CS_TOKEN_FLAG + FROZEN_DONT_EXPAND;
				link(p) = loc;
				start = p;
#ifdef VORTEX
				last_loc = loc;
#endif
				loc = p;
			}
			break;

		case CS_NAME:
			p = r = get_avail();
			do {
				get_x_token();
				if (cur_cs == 0)
					store_new_token(cur_tok);
			} while (cur_cs == 0);
			if (cur_cmd != END_CS_NAME) {
				print_err("Missing ");
				print_esc("endcsname");
				print(" inserted");
				help_cs();
				back_error();
			}
			j = first;
			p = link(r);
			while (p != NULL) {
				if (j >= max_buf_stack) {
					max_buf_stack = j + 1;
					if (max_buf_stack == BUF_SIZE)
						overflow("buffer size", BUF_SIZE);
				}
				buffer[j] = info(p) % 0400;
				incr(j);
				p = link(p);
			}
			if (j > first + 1) {
				no_new_control_sequence = FALSE;
				cur_cs = id_lookup(first, j - first);
				no_new_control_sequence = TRUE;
			} else if (j == first)
				cur_cs = NULL_CS;
			else cur_cs = SINGLE_BASE + buffer[first];
			flush_list(r);
			if (eq_type(cur_cs) == UNDEFINED_CS)
				eqtb[cur_cs] = eqtb[FROZEN_RELAX];
			cur_tok = cur_cs + CS_TOKEN_FLAG;
			back_input();
			break;
		
		case CONVERT:
			conv_toks();
			break;
		
		case THE:
			ins_the_toks();
			break;
		
		case IF_TEST:
			conditional();
			break;
		
		case FI_OR_ELSE:
			if (cur_chr > if_limit) {
				if (if_limit == IF_CODE) {
#ifdef VORTEX
					cur_cs_node = NIL;
#endif
					insert_relax();
				} else {
					print_err("Extra ");
					print_cmd_chr(FI_OR_ELSE, cur_chr);
					help_extra_if();
					error();
				}
			} else {
				while (cur_chr != FI_CODE)
					pass_text();
				pop_cond();
			}
			break;

		case INPUT:
			if (cur_chr > 0)
				force_eof = TRUE;
			else if (name_in_progress)
				insert_relax();
			else
				start_input();
			break;
		
		default:
			print_err("Undefined control sequence");
			help_undefd();
			error();
			break;
		}
#ifdef VORTEX
		cond_skip = save_cond_skip;
#endif
	} else if (cur_cmd < END_TEMPLATE)
		macro_call();
	else {
		cur_tok = CS_TOKEN_FLAG + FROZEN_ENDV;
		back_input();
	}
	cur_val = cv_backup;
	cur_val_level = cvl_backup;
	radix = radix_backup;
	link(backup_head) = backup_backup;
}

insert_relax ()
{
	cur_tok = CS_TOKEN_FLAG + cur_cs;
	back_input();
#ifdef VORTEX
	cur_cs_node = NIL;
#endif
	cur_tok = CS_TOKEN_FLAG + FROZEN_RELAX;
	back_input();
	token_type = INSERTED;
}

#define	runaway_arg() \
	{if (long_state == CALL) { \
		runaway(); \
		print_err("Paragraph ended before "); \
		sprint_cs(warning_index); \
		print(" was complete"); \
		help_runaway(); \
		back_error();} \
	pstack[n] = link(temp_head); \
	align_state -= unbalance; \
	for (m = 0; m <= n; incr(m)) \
		flush_list(pstack[m]); \
	goto local_exit;}

macro_call ()
{
	int		m;
	int		n;
	ptr		p;
	ptr		q;
	ptr		r;
	ptr		s;
	ptr		t;
	ptr		u;
	ptr		v;
	ascii		match_chr;
	ptr		ref_count;
	ptr		rbrace_ptr;
	ptr		save_warning_index;
	int		save_scanner_status;
	int		unbalance;

#ifdef VORTEX
	struct _cseq	*cur_macro_node;
	char		*tmp;
	short		save_token_count;
	short		save_cond_skip;
	
	cur_macro_node = NIL;
	if (macro_node_name != NIL) {	/* if there's unprocessed real macro */
		cur_macro_node = macro_node_name;
		macro_node_name = NIL;
	}
	macro_param_num = 0;
#endif
	n = 0;
	ref_count = cur_chr;
	r = link(ref_count);
	save_scanner_status = scanner_status;
	save_warning_index = warning_index;
	warning_index = cur_cs;
	if (tracing_macros > 0) {
		begin_diagnostic();
		print_ln();
		print_cs(warning_index);
		token_show(ref_count);
		end_diagnostic(FALSE);
	}
	if (info(r) != END_MATCH_TOKEN) {
#ifdef VORTEX
		save_cond_skip = cond_skip;
		cond_skip = DOSKIP;
#endif
		scanner_status = MATCHING;
		unbalance = 0;
		long_state = eq_type(cur_cs);
		if (long_state >= OUTER_CALL)
			long_state -= 2;
		do {
			if (info(r) > MATCH_TOKEN + 127 ||
				info(r) < MATCH_TOKEN) {
				s = NULL;
			} else {
				match_chr = info(r) - MATCH_TOKEN;
				s = link(r);
				r = s;
				p = temp_head;
				link(p) = NULL;
				m = 0;
			}

		contin:
			get_token();
#ifdef VORTEX
			if (cur_macro_node != NIL)
				set_param_extent(cur_macro_node);
			macro_param_num++;
#endif
			if (cur_tok == info(r)) {
				r = link(r);
				if (info(r) >= MATCH_TOKEN &&
					info(r) <= END_MATCH_TOKEN) {
						if (cur_tok < LEFT_BRACE_LIMIT)
							decr(align_state);
						goto found;
				} else goto contin;
			}
			if (s != r) {
				if (s == NULL) {
					print_err("Use of ");
					sprint_cs(warning_index);
					print(" doesn't match its definition");
					help_match();
					error();
					goto local_exit;
				} else {
					t = s;
					do {
						store_new_token(info(t));
						incr(m);
						u = link(t);
						v = s;
						loop {
							if (u == r) {
								if (cur_tok != info(v))
									break;
								else {
									r = link(v);
									goto contin;
								}
							}
							if (info(u) != info(v))
								break;
							u = link(u);
							v = link(v);
						}
						t = link(t);
					} while (t != r);
					r = s;
				}
			}
			if (cur_tok == par_token && long_state != LONG_CALL)
				runaway_arg();
			if (cur_tok < RIGHT_BRACE_LIMIT) {
				if (cur_tok < LEFT_BRACE_LIMIT) {
#ifdef VORTEX
					if (begin_of_space_token != NIL) {
						make_space_node(begin_of_space_token, irs_ptr->_lt);
						begin_of_space_token = NIL;
					}
					if (begin_of_word_token != NIL &&
					    begin_of_word_token != group_s_node_begin) {
						if (begin_of_word_token == irs_ptr)
							make_word_node(begin_of_word_token, irs_ptr);
						else
							make_word_node(begin_of_word_token, irs_ptr->_lt);
						begin_of_word_token = NIL;
					}
					if (real_token && group_s_node_begin != NIL) {
						make_group_node(group_s_node_begin);
						group_s_node_begin = NIL;
					}
#endif
					unbalance = 1;
					loop {
						fast_store_new_token(cur_tok);
						get_token();
#ifdef VORTEX
						if (cur_macro_node != NIL)
							set_param_extent(cur_macro_node);
						macro_param_num++;
#endif
						if (cur_tok == par_token && 
						    long_state != LONG_CALL) {
							runaway_arg();
						}
						if (cur_tok < RIGHT_BRACE_LIMIT) {
							if (cur_tok < LEFT_BRACE_LIMIT) 
#ifdef VORTEX
								{
									if (begin_of_space_token != NIL) {
										make_space_node(begin_of_space_token, irs_ptr->_lt);
										begin_of_space_token = NIL;

									}
									if (begin_of_word_token != NIL 
									    && begin_of_word_token != irs_ptr) {
										make_word_node(begin_of_word_token, irs_ptr->_lt);
										begin_of_word_token = NIL;
									}
									if (real_token && group_s_node_begin != NIL) {
										make_group_node(group_s_node_begin);
										group_s_node_begin = NIL;
									}
#endif
									incr(unbalance);
#ifdef VORTEX
					  }
#endif
					else {
#ifdef VORTEX
					  if (begin_of_space_token != NIL) {
					    make_space_node(begin_of_space_token, irs_ptr->_lt);
					    begin_of_space_token = NIL;
					  }
					  if (begin_of_word_token != NIL 
					      && begin_of_word_token != irs_ptr) {
					    make_word_node(begin_of_word_token, irs_ptr->_lt);
					    begin_of_word_token = NIL;
					  }
					  if (real_token && group_s_node_end != NIL) {
					    close_group_node(group_s_node_end);
					    group_s_node_end = NIL;
					  }
#endif
								decr(unbalance);
								if (unbalance == 0)
									break;
							}
						}
					}
					rbrace_ptr = p;
					store_new_token(cur_tok);
				} else {
					back_input();
					print_err("Argument of ");
					sprint_cs(warning_index);
					print(" has an extra `}'");
					help_match_xtra();
					incr(align_state);
					long_state = CALL;
					cur_tok = par_token;
					ins_error();
				}
			} else {
				if (cur_tok == SPACE_TOKEN &&
					info(r) <= END_MATCH_TOKEN &&
					info(r) >= MATCH_TOKEN)
					goto contin;
				store_new_token(cur_tok);
			}
			incr(m);
			if (info(r) > END_MATCH_TOKEN || info(r) < MATCH_TOKEN)
				goto contin;

		found:
			if (s != NULL) {
				if (m == 1 &&
					info(p) < RIGHT_BRACE_LIMIT &&
					p != temp_head) {
					link(rbrace_ptr) = NULL;
					free_avail(p);
					p = link(temp_head);
					pstack[n] = link(p);
					free_avail(p);
				} else pstack[n] = link(temp_head);
				incr(n);
				if (tracing_macros > 0) {
					begin_diagnostic();
					print_nl("");
					print_char(match_chr);
					print_int(n);
					print("<-");
					show_token_list(pstack[n - 1], NULL, 1000L);
					end_diagnostic(FALSE);
				}
			}
		} while (info(r) != END_MATCH_TOKEN);
#ifdef VORTEX
			cond_skip = save_cond_skip;
#endif
	}
	while (state == TOKEN_LIST && loc == NULL)
		end_token_list();
	begin_token_list(ref_count, MACRO);
	name = warning_index;
#ifdef VORTEX
	last_loc = loc;
#endif
	loc = link(r);
	if (n > 0) {
		if (param_ptr + n > max_param_stack) {
			max_param_stack = param_ptr + n;
			if (max_param_stack >= PARAM_SIZE)
				overflow("parameter stack size", PARAM_SIZE);
		}
		for (m = 0; m < n; incr(m))
			param_stack[param_ptr + m] = pstack[m];
		param_ptr += n;
	}

local_exit:
#ifdef VORTEX
	if (cur_macro_node != NIL) {
		last_cs = cur_macro_node;
		if (cur_macro_node->_boc == NIL)
			ir_cur = cur_macro_node->_eon->nd_rt;
		else
			ir_cur = cur_macro_node->_boc->nd_rt;
	}
#endif
	scanner_status = save_scanner_status; 
	warning_index = save_warning_index; 
}

x_token ()
{
	while (cur_cmd > MAX_COMMAND) {
		expand();
		get_next();
	}
	if (cur_cs == 0)
		cur_tok = cur_cmd * 0400 + cur_chr;
	else
		cur_tok = CS_TOKEN_FLAG + cur_cs;
}

/*
 *	Help text
 */

help_runaway ()
{
	help3("I suspect you've forgotten a `}', causing me to apply this",
	"control sequence to too much text. How can we recover?",
	"My plan is to forget the whole thing and hope for the best.");
}

help_match ()
{
	help4("If you say, e.g., `\\def\\a1{...}', then you must always",
	"put `1' after `\\a', since control sequence names are",
	"made up of letters only. The macro here has not been",
	"followed by the required stuff, so I'm ignoring it.");
}

help_match_xtra ()
{
	help6("I've run across a `}' that doesn't seem to match anything.",
	"For example, `\\def\\a#1{...}' and `\\a}' would produce",
	"this error. If you simply proceed nw, the `\\par' that",
	"I've just inserted will cause me to report a runaway",
	"argument that might be the root of the problem. But if",
	"your `}' was spurious, just type `2' and it will go away.");
}

help_undefd ()
{
	help5("The control sequence at the end of the top line",
	"of your error message was never \\def'ed. If you have",
	"misspelled it (e.g., `\\hobx'), type `I' and the correct",
	"spelling (e.g., `I\\hbox'). Otherwise just continue,",
	"and I'll forget about whatever was undefined.");
}

help_cs ()
{
	help2("The control sequence marked <to be read again> should",
	"not appear between \\csname and \\endcsname");
}

help_extra_if ()
{
	help1("I'm ignoring this; it doesn't match any \\if.");
}
