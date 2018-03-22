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
 *		token.c
 */

#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"eq.h"
#include	"hash.h"
#include	"scan.h"
#include	"io.h"
#include	"char.h"
#include	"box.h"
#include	"cond.h"
#include	"print.h"
#include	"error.h"
#include	"expand.h"
#include	"align.h"
#include	"tokenstack.h"
#include	"token.h"

#ifdef VORTEX

#include    	<stdio.h>
#include    	"allir.h"
#include    	"main.h"
#include    	"macro.h"
#include    	"var.h"
#include	"msg.h"

extern		make_space_node();
extern		make_word_node();
extern ptr	save_hi_mem_min;
struct _cseq	*make_cs_node();
struct _char	*get_irs_ptr();
struct _char	*get_next_irs();
struct _char	*get_prev_irs();

#endif


/*
 *	this is the TeX segment
 */

extern hword		cur_chr;
extern byte		cur_cmd;
extern ptr		cur_cs;
extern hword		cur_tok;
extern ptr		par_loc;
extern hword		par_token;
extern bool		force_eof;

get_token ()
{
	no_new_control_sequence = FALSE;
	get_next();
	no_new_control_sequence = TRUE;
	if (cur_cs == 0)
		cur_tok = cur_cmd * 0400 + cur_chr;
	else
		cur_tok = CS_TOKEN_FLAG + cur_cs;
}

#ifndef VORTEX
#define	reduce_expanded_cc() { \
	if (buffer[k] == cur_chr && cat == SUP_MARK && k < limit) { \
		cur_chr = buffer[k + 1]; \
		if (cur_chr < 0100) \
			buffer[k - 1] = cur_chr + 0100; \
		else buffer[k - 1] = cur_chr - 0100; \
		limit -= 2; \
		first -= 2; \
		while (k <= limit) { \
			buffer[k] = buffer[k + 2]; \
			incr(k); \
		} \
		goto start_cs; \
	} \
}
#else
#define	reduce_expanded_cc() { \
	if (buffer[k] == cur_chr && cat == SUP_MARK && k < limit) { \
		tmp = get_prev_irs(irs_tmp); \
		tmp1 = irs_tmp; \
		cur_chr = buffer[k + 1]; \
		if (cur_chr < 0100) \
			buffer[k - 1] = cur_chr + 0100; \
		else \
			buffer[k - 1] = cur_chr - 0100; \
		tmp2 = (struct _char *) get_next_irs(irs_tmp)->_lt; \
		reduce_sum += 2; \
		if ((k - loc <= 1) || (cat_code(buffer[k-1]) == LETTER)) { \
			tmp = tmp2; \
			tmp1 = NIL; \
		} \
		limit -= 2; \
		first -= 2; \
		while (k <= limit) { \
			buffer[k] = buffer[k + 2]; \
			incr(k); \
		} \
		goto start_cs; \
	} else { \
		if (tmp == NIL) { \
			tmp = get_prev_irs(irs_tmp); \
			tmp1 = NIL; \
		} \
	} \
}
#endif

#ifdef VORTEX
#define	advance_irs() \
	if (irs_bol != NIL) { \
		irs_ptr = irs_next; \
		irs_next = get_next_irs(irs_next); \
	}
#endif

#define	any_state(CAT) \
	case MID_LINE + CAT: \
	case SKIP_BLANKS + CAT: \
	case NEW_LINE + CAT
			
#ifndef VORTEX
#define	delims(CAT) \
	case MATH_SHIFT + CAT: \
	case TAB_MARK + CAT: \
	case MAC_PARAM + CAT: \
	case SUB_MARK + CAT: \
	case LETTER + CAT: \
	case OTHER_CHAR + CAT
#else			
#define	delims(CAT) \
	case TAB_MARK + CAT: \
	case MAC_PARAM + CAT: \
	case SUB_MARK + CAT: \
	case LETTER + CAT: \
	case OTHER_CHAR + CAT
#endif

#define	mid_line(CAT) \
	case MID_LINE + CAT

#define	new_line(CAT) \
	case NEW_LINE + CAT

#define	skip_blanks(CAT) \
	case SKIP_BLANKS + CAT


#ifdef VORTEX
#define pop_input_state { \
	input_state_level--; \
	cur_group_node = input_state_stack[input_state_level].cur_group_node; \
	token_node_last = input_state_stack[input_state_level].token_node_last; \
	par_node_last = input_state_stack[input_state_level].par_node_last; \
}
#endif

get_next ()
{
	int		k;
	hword		t;
	int		cat;

#ifdef VORTEX
	int		reduce_sum;
	int		i;
	struct _char	*tmp, *tmp1, *tmp2;
	struct _char	*irs_tmp;
	struct _char	*irs_save_sup;
#endif

restart:

#ifdef VORTEX
	reduce_sum = 0;
	cur_cs_node = NIL;
	irs_save_sup = NIL;
#endif

	cur_cs = 0;
	if (state != TOKEN_LIST) {

#ifdef VORTEX
		real_token = TRUE;
		token_count++;
#endif

reread:
		if (loc <= limit) {
			cur_chr = buffer[loc];

#ifdef VORTEX
			advance_irs();
#endif

			incr(loc);
reswitch:
			cur_cmd = cat_code(cur_chr);
			switch (state + cur_cmd) {
			any_state(IGNORE):
			skip_blanks(SPACER):
			new_line(SPACER):
#ifdef VORTEX
				if (begin_of_space_token == NIL)
					begin_of_space_token = irs_ptr;
#endif
				goto reread;
				break;
	
			any_state(ESCAPE):
#ifdef VORTEX
				begin_of_cs_token = irs_ptr;
				if(begin_of_space_token != 0) {
					make_space_node(begin_of_space_token,
							begin_of_cs_token->_lt);
					begin_of_space_token = 0;
				}
				if(begin_of_word_token != 0) {
					make_word_node(begin_of_word_token,
						       begin_of_cs_token->_lt);
					begin_of_word_token = 0;
				}
#endif
				if (loc > limit)
					cur_cs = NULL_CS;
				else {
#ifdef VORTEX
					tmp = tmp1 = NIL;
#endif
			start_cs: 
					k = loc;
					cur_chr = buffer[k];
					cat = cat_code(cur_chr);
					incr(k);
#ifdef VORTEX
					irs_tmp = irs_next;
#endif
					if (cat == LETTER)
						state = SKIP_BLANKS;
					else if (cat == SPACER)
						state = SKIP_BLANKS;
					else
						state = MID_LINE;
					if (cat == LETTER && k <= limit) {
						do {
#ifdef VORTEX
							irs_tmp = get_next_irs(irs_tmp);
#endif
							cur_chr = buffer[k];
							incr(k);
							cat = cat_code(cur_chr);
						} while (cat == LETTER && k <= limit);
						reduce_expanded_cc();
#ifdef VORTEX
						cur_cs_node = make_cs_node(begin_of_cs_token, tmp);
	/*
						if (tmp1 != NIL)
							begin_of_word_token = tmp1;
	*/
#endif
						if (cat != LETTER)
							decr(k);
#ifdef VORTEX
						irs_next = irs_tmp;
#endif
						if (k > loc + 1) {
							cur_cs = id_lookup(loc, k-loc);
#ifdef VORTEX
							last_loc = loc;
#endif
							loc = k;
							goto found;
						}
					} else {
#ifdef VORTEX
	/*
						if (buffer[k] != cur_chr || cat != SUP_MARK)
							irs_tmp = get_next_irs(irs_tmp);
	*/
#endif
						reduce_expanded_cc();
#ifdef VORTEX
						if (k <= limit) 
							irs_tmp = get_next_irs(irs_tmp);
						cur_cs_node = make_cs_node(begin_of_cs_token,
									   irs_tmp->_lt);
						irs_next = irs_tmp;
#endif
					}
					cur_cs = SINGLE_BASE + buffer[loc];
					incr(loc);
				}
			found:
				cur_cmd = eq_type(cur_cs);
				cur_chr = equiv(cur_cs);
#ifdef  VORTEX
				if (cur_cmd >= CALL && cur_cmd < END_TEMPLATE) {
					if (macro_que_enable &&
					    scanner_status != MATCHING &&
					    cond_level == 0)
						if (!irs_read_only) {
							/* get def'ed IRi */
							if (equiv(cur_cs) >= MEM_TOP || equiv(cur_cs) <= save_hi_mem_min)
								cur_cs_node->_def 
									= (struct _node *)mem[equiv(cur_cs) + 1].i;
							macro_node_name = cur_cs_node;
						}
				} else {
					if (scanner_status != SKIPPING &&
					    cond_skip == NOSKIP &&
					    cond_level == 0)  
						cs_node_enque(cur_cs_node);
				}
#endif
				if (cur_cmd >= OUTER_CALL)
					check_outer_validity();
				break;
							
			any_state(ACTIVE_CHAR):
#ifdef VORTEX
				if (begin_of_space_token != 0) {
					make_space_node(begin_of_space_token, irs_ptr->_lt);
					begin_of_space_token = 0;
				}
				if (begin_of_word_token != 0) {
					make_word_node(begin_of_word_token, irs_ptr->_lt);
					begin_of_word_token = 0;
				}
				tmp = get_next_irs(irs_ptr);
				if (tmp != NIL)
					tmp = (struct _char *)tmp->_lt;
				else
					tmp = irs_eol;
				make_word_node(irs_ptr, tmp);
#endif
				cur_cs = cur_chr + ACTIVE_BASE;
				cur_cmd = eq_type(cur_cs);
				cur_chr = equiv(cur_cs);
				state = MID_LINE;
				if (cur_cmd >= OUTER_CALL)
					check_outer_validity();
				break;
				
			any_state(SUP_MARK):
#ifdef VORTEX
				if (begin_of_space_token != 0) {
					make_space_node(begin_of_space_token, irs_ptr->_lt);
					begin_of_space_token = 0;
				}
				if (begin_of_word_token != 0) {
					make_word_node(begin_of_word_token, irs_ptr->_lt);
					begin_of_word_token = 0;
				}
				begin_of_word_token = irs_ptr;
#endif
				if (cur_chr == buffer[loc] && loc < limit) {
					if (buffer[loc + 1] < 0100)
						cur_chr = buffer[loc + 1] + 0100;
					else
						cur_chr = buffer[loc + 1] - 0100;
#ifdef VORTEX
	/*
					advance_irs();
					advance_irs();
	*/
					/* remember '^' position */
					irs_save_sup = irs_ptr;
					irs_ptr = irs_next;
#endif
					loc += 2;
					goto reswitch;
				}
				state = MID_LINE;
				break;
	
			any_state(INVALID_CHAR):
				print_err("Text line contains an invalid character");
				help_funny();
				deletions_allowed = FALSE;
				error();
				deletions_allowed = TRUE;
				goto restart;
				break;
				
			mid_line(SPACER):
#ifdef VORTEX
				begin_of_space_token = irs_ptr;
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       begin_of_space_token->_lt);
					begin_of_word_token = NIL;
				}
#endif
				state = SKIP_BLANKS;
				cur_chr = ' ';
				break;
			      
			mid_line(CAR_RET):
#ifdef VORTEX
				begin_of_space_token = irs_ptr;
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       begin_of_space_token->_lt);
					begin_of_word_token = NIL;
				}
				last_loc = loc;
#endif
				loc = limit + 1;
				cur_cmd = SPACER;
				cur_chr = ' ';
				break;
				
			skip_blanks(CAR_RET):
			any_state(COMMENT):
#ifdef VORTEX
				if (begin_of_space_token != NIL)
					make_space_node(begin_of_space_token, irs_ptr->_lt);
				begin_of_space_token = irs_ptr;
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       begin_of_space_token->_lt);
					begin_of_word_token = NIL;
				}
				last_loc = loc;
#endif
				loc = limit + 1;
				goto reread;
				break;
	
			new_line(CAR_RET):
#ifdef VORTEX
				if(begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token, irs_ptr);
					begin_of_space_token = NIL;
				}
				if(begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token, irs_ptr->_lt);
					begin_of_word_token = NIL;
				}
				make_par_node(irs_ptr);
				last_loc = loc;
#endif
				loc = limit + 1;
				cur_cs = par_loc;
				cur_cmd = eq_type(cur_cs);
				cur_chr = equiv(cur_cs);
				if (cur_cmd >= OUTER_CALL)
					check_outer_validity();
				break;
				
			mid_line(LEFT_BRACE):
#ifdef VORTEX
				group_s_node_begin = irs_ptr;
				if (begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token,
							group_s_node_begin->_lt);
					begin_of_space_token = NIL;
				}
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       group_s_node_begin->_lt);
				}
				begin_of_word_token = group_s_node_begin; 
#endif
				incr(align_state);
				break;
	
			skip_blanks(LEFT_BRACE):
			new_line(LEFT_BRACE):
#ifdef VORTEX
				group_s_node_begin = irs_ptr;
				if (begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token,
							group_s_node_begin->_lt);
					begin_of_space_token = NIL;
				}
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       group_s_node_begin->_lt);
				}
				begin_of_word_token = group_s_node_begin; 
#endif
				state = MID_LINE;
				incr(align_state);
				break;
				
			mid_line(RIGHT_BRACE):
#ifdef VORTEX
				group_s_node_end = irs_ptr;
				if (begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token,
							group_s_node_end->_lt);
					begin_of_space_token = NIL;
				}
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       group_s_node_end->_lt);
				}
				begin_of_word_token = group_s_node_end; 
#endif
				decr(align_state);
				break;
				
			skip_blanks(RIGHT_BRACE):
			new_line(RIGHT_BRACE):
#ifdef VORTEX
				group_s_node_end = irs_ptr;
				if (begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token,
							group_s_node_end->_lt);
					begin_of_space_token = NIL;
				}
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token,
						       group_s_node_end->_lt);
				}
				begin_of_word_token = group_s_node_end; 
#endif
				state = MID_LINE;
				decr(align_state);
				break;
				
#ifdef VORTEX
			mid_line(MATH_SHIFT):
				if (begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token, irs_ptr->_lt);
					begin_of_space_token = NIL;
				}
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token, irs_ptr->_lt);
				}
				if (scanner_status == DEFINING)
					begin_of_word_token = irs_ptr; 
				else {
					begin_of_word_token = NIL;
					/* can't process '$" in macro */
					if (scanner_status != MATCHING)
						math_enque(irs_ptr);
					else
						make_word_node(irs_ptr, irs_ptr);
				}
				break;
				
			skip_blanks(MATH_SHIFT):
			new_line(MATH_SHIFT):
				if (begin_of_space_token != NIL) {
					make_space_node(begin_of_space_token, irs_ptr->_lt);
					begin_of_space_token = NIL;
				}
				if (begin_of_word_token != NIL) {
					make_word_node(begin_of_word_token, irs_ptr->_lt);
				}
				if (scanner_status == DEFINING)
					begin_of_word_token = irs_ptr; 
				else {
					begin_of_word_token = NIL;
					/* can't process '$" in macro */
					if (scanner_status != MATCHING)
						math_enque(irs_ptr);
					else
						make_word_node(irs_ptr, irs_ptr);
				}
				state = MID_LINE;
				break;
#endif
	
			delims(SKIP_BLANKS):
			delims(NEW_LINE):
#ifdef VORTEX
				if (irs_save_sup != NIL) {
				      /* in case of "^^x" used as delimiter */
					make_word_node(irs_save_sup, irs_ptr->_lt);
					begin_of_word_token = NIL;
				} else
					begin_of_word_token = irs_ptr;
				if (begin_of_space_token != NIL) {
					make_space_node (begin_of_space_token,
							 begin_of_word_token->_lt);
					begin_of_space_token = NIL;
				}
#endif
				state = MID_LINE;
				break;
	
			default:
#ifdef VORTEX
				if (cur_chr >= ' ' && cur_chr <= '~') {
					if (begin_of_word_token == NIL)
						begin_of_word_token = irs_ptr;
				}
#endif
				break;
			}
		} else {
			state = NEW_LINE; 
			if (name > 17) {
				incr(line);
				first = start;
				if (!force_eof) {
#ifdef VORTEX
					if (input_ln())
#else
					if (input_ln(cur_file, TRUE))
#endif
						firm_up_the_line();
					else force_eof = TRUE;
				}
				if (force_eof) {
					print_char(')');
					force_eof = FALSE;
					update_terminal();
					end_file_reading();
					check_outer_validity();
#ifdef VORTEX
					if (begin_of_space_token != NIL) {
						make_space_node(begin_of_space_token, irs_ptr);
						begin_of_space_token = NIL;
					}
					if (begin_of_word_token != NIL) {
						make_word_node(begin_of_word_token, irs_ptr);
						begin_of_word_token = NIL;
					}
					if (input_state_level <= 0) {
						if (input_flag)
							msg(STDERR, ">>> input stack underflow! <<<");
					} else {
						pop_input_state;
					}
/*
					irs_bol = NIL;
*/
#endif
					goto restart;
				}
				if (end_line_char < 0 || end_line_char > 127)
					decr(limit);
				else
					buffer[limit] = end_line_char;
				first = limit + 1;
#ifdef VORTEX
				last_loc = loc;
#endif
				loc = start;
#ifdef VORTEX
				irs_next = irs_bol;
#endif
			} else {
				if (!terminal_input) {
					cur_cmd = 0;
					cur_chr = 0;
					return;
				}
				if (input_ptr > 0) {
					end_file_reading();
					goto restart;
				}
				if (selector < LOG_ONLY)
					open_log_file();
				if (interaction > NONSTOP_MODE) {
					if (limit == start)
						print_nl("(Please type a command or say `\\end')");
					print_ln();
					first = start;
					prompt_input("*");
					limit = last;
					if (end_line_char < 0 || end_line_char > 127)
						decr(limit);
					else
						buffer[limit] = end_line_char;
					first = limit + 1;
#ifdef VORTEX
					last_loc = loc;
#endif
					loc = start;
#ifdef VORTEX
					irs_next = irs_bol;
#endif
				} else
					fatal_error("*** (job aborted, no legal \\end found)");
			}
			check_interrupt();
			goto reread;
		}
	} else {
		if (loc != NULL) {
			t = info(loc);
#ifdef VORTEX
			last_loc = loc;
#endif
			loc = link(loc);
#ifdef VORTEX
			if (input_ptr == backup_real_token) {
				token_count++;
				backup_real_token = 0;
				real_token = TRUE;
			} else
				real_token = FALSE;
#endif
			if (t >= CS_TOKEN_FLAG) {
				cur_cs = t - CS_TOKEN_FLAG;
				cur_cmd = eq_type(cur_cs);
				cur_chr = equiv(cur_cs);
				if (cur_cmd >= OUTER_CALL) {
					if (cur_cmd == DONT_EXPAND) {
						cur_cs = info(loc) - CS_TOKEN_FLAG;
#ifdef VORTEX
						last_loc = loc;
#endif
						loc = NULL;
						cur_cmd = eq_type(cur_cs);
						cur_chr = equiv(cur_cs);
						if (cur_cmd > MAX_COMMAND) {
							cur_cmd = RELAX;
							cur_chr = NO_EXPAND_FLAG;
						}
					} else
						check_outer_validity();
				}
#ifdef VORTEX
				if (cur_cmd <= MAX_COMMAND || cur_cmd == INPUT)
					if (token_type != BACKED_UP &&
					    token_type != INSERTED &&
					    scanner_status != SKIPPING &&
					    cond_skip == NOSKIP &&
					    align_state !=0 &&
					    cond_level == 0) {
						cs_node_enque((struct _cseq *) cur_cmd);
					} else {
						if (token_type == BACKED_UP &&
						    backup_cs_node != NIL &&
						    backup_cs_level == input_ptr - 1) {
							cur_cs_node = backup_cs_node;
							backup_cs_node = NIL;
							if (cond_skip == NOSKIP &&
							    cond_level == 0)
								cs_node_enque(cur_cs_node);
						}
					}
#endif
			} else {
				cur_cmd = t / 0400;
				cur_chr = t % 0400;
				switch (cur_cmd) {
				case LEFT_BRACE:
					incr(align_state);
					break;
		  
				case RIGHT_BRACE:
					decr(align_state);
					break;
		
				case OUT_PARAM:
					begin_token_list(param_stack[param_start + cur_chr - 1],
							 PARAMETER);
					goto restart;
					break;
#ifdef VORTEX
				case MATH_SHIFT:
					if (real_token == FALSE)
						if (disp_watch == TRUE ||
						    cond_skip != DOSKIP &&
						    vmode_doll != input_ptr
						    /*&& scanner_status != ALIGNING*/) {
							math_enque(NIL);
						}
					if (vmode_doll == input_ptr)
						vmode_doll = NULL;
					break;
#endif

				default:
					break;
				}
			}
		} else {
			end_token_list();
			goto restart;
		}
	}
#ifdef VORTEX
	if (token_type != BACKED_UP)
		macro_first_token = FALSE;
#endif
	if (cur_cmd <= CAR_RET && cur_cmd >= TAB_MARK && align_state == 0) {
		if (scanner_status == ALIGNING)
			fatal_error("(interwoven alignment preambles are not allowed)");
		cur_cmd = extra_info(cur_align);
		extra_info(cur_align) = cur_chr;
		if (cur_cmd == OMIT)
			begin_token_list(omit_template, (qword) V_TEMPLATE);
		else
			begin_token_list((ptr) v_part(cur_align), (qword) V_TEMPLATE);
		align_state = 1000000;
		goto restart;
	}
}

check_outer_validity ()
{
	ptr		p;
	ptr		q;

	if (scanner_status != NORMAL) {
		deletions_allowed = FALSE;
		if (cur_cs != 0) {
			if (state == TOKEN_LIST || name < 1 || name > 17) {
				p = get_avail();
				info(p) = CS_TOKEN_FLAG + cur_cs;
				back_list(p);
			}
			cur_cmd = SPACER;
			cur_chr = ' ';
		}
		if (scanner_status > SKIPPING) {
			runaway();
			if (cur_cs == 0) 
				print_err("File ended");
			else {
				cur_cs = 0;
				print_err("Forbidden control sequence found");
			}
			print(" while scanning ");
			p = get_avail();
			switch (scanner_status)
			{
			case DEFINING:
				print("definition");
				info(p) = RIGHT_BRACE_TOKEN + '}';
				break;

			case MATCHING:
				print("use");
				info(p) = par_token;
				long_state = OUTER_CALL;
				break;

			case ALIGNING:
				print("preamble");
				info(p) = RIGHT_BRACE_TOKEN + '}';
				q = p;
				p = get_avail();
				link(p) = q;
				info(p) = CS_TOKEN_FLAG + FROZEN_CR;
				align_state = -1000000;
				break;

			case ABSORBING:
				print("text"); 
				info(p) = RIGHT_BRACE_TOKEN + '}';
				break;
			}
			ins_list(p);
			print(" of ");
			sprint_cs(warning_index);
			help_scanner();
			error();
		} else {
			print_err("Incomplete ");
			print_cmd_chr(IF_TEST, cur_if);
			print("; all text was ignored after line ");
			print_val(skip_line);
			help_skif();
			if (cur_cs != 0)
				cur_cs = 0;
			else
				help_line[0] = 
					"The file ended while I was skipping conditional text.";
			cur_tok = CS_TOKEN_FLAG + FROZEN_FI;
			ins_error();
		}
		deletions_allowed = TRUE;
	}
}

firm_up_the_line ()
{
	int		k;

	limit = last;
	if (pausing > 0 && interaction > NONSTOP_MODE) {
		wake_up_terminal();
		print_ln();
		if (start < limit) {
			for (k = start; k < limit; incr(k))
				print_char(buffer[k]);
		}
		first = limit;
		prompt_input("=>");
		if (last > first) {
			for (k = first; k < last; incr(k))
				buffer[k + start - first] = buffer[k];
			limit = start + last - first;
		}
	}
}

/*
 *	Help text
 */

help_scanner ()
{
	help4("I suspect you have forgotten a `}', causing me",
	"to read past where you wanted me to stop.",
	"Either type `I}' to try recovering,",
	"or type 'X' and fix your file.");
}

help_funny ()
{
	help2("A funny symbol that I can't read has just been input.",
	"Continue, and I'll forget that it ever happened.");
}

help_skif ()
{
	help3("A forbidden control sequence occurred in skipped text.",
	"This kind of error happens when you say `\\if...' and forget",
	"the matching `\\fi'. I've inserted a `\\fi'; this might work.");
}
