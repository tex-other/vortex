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
 *		tokenlists.c
 */

#include		"tex.h"
#include		"cmds.h"
#include		"heap.h"
#include		"eq.h"
#include		"hash.h"
#include		"str.h"
#include		"box.h"
#include		"token.h"
#include		"expand.h"
#include		"tokenstack.h"
#include		"io.h"
#include		"scan.h"
#include		"def.h"
#include		"file.h"
#include		"tfm.h"
#include		"print.h"
#include		"error.h"
#include		"tokenlists.h"

#ifdef VORTEX
#include		"allir.h"
#include		"main.h"
#include		"macro.h"
#include		"var.h"

extern ptr		save_hi_mem_min;
extern			make_space_node();
extern			make_word_node();
extern			make_group_node();
extern			close_group_node();
#endif

ptr
str_toks ()
{
	int		k;
	ptr		p;
	ptr		q;
	hword		t;

	str_room(1);
	p = temp_head;
	link(p) = NULL;
	k = str_start[str_ptr];
	while (k < pool_ptr) {
		t = str_pool[k];
		if (t == ' ')
			t = SPACE_TOKEN;
		else
			t += OTHER_TOKEN;
		fast_store_new_token(t);
		incr(k);
	}
	pool_ptr = str_start[str_ptr];
	return p;
}
		
ptr
the_toks ()
{
	ptr		p;
	ptr		q;
	ptr		r;
	int		old_setting;

	get_x_token();
	scan_something_internal(TOK_VAL, FALSE);
	if (cur_val_level >= IDENT_VAL) {
		p = temp_head;
		link(p) = NULL;
		if (cur_val_level == IDENT_VAL)
			{store_new_token(CS_TOKEN_FLAG + cur_val);}
		else if (cur_val != NULL) {
			r = link(cur_val);
			while (r != NULL) {
				fast_store_new_token(info(r));
				r = link(r);
			}
		}
		return p;
	} else {
		old_setting = selector;
		selector = NEW_STRING;
		switch (cur_val_level)
		{
		case INT_VAL:
			print_val(cur_val);
			break;

		case DIMEN_VAL:
			print_scaled(cur_val);
			print("pt");
			break;

		case GLUE_VAL:
			print_spec((ptr) cur_val, "pt");
			delete_glue_ref((ptr) cur_val);
			break;

		case MU_VAL:
			print_spec((ptr) cur_val,"mu");
			delete_glue_ref((ptr) cur_val);
			break;
		}
		selector = old_setting;
		return (str_toks());
	}
}

ins_the_toks ()
{
	the_toks();
	ins_list(link(temp_head));
}

conv_toks ()
{
	int		c;
	int		old_setting;
	int		save_scanner_status;

	c = cur_chr;
	switch (c)
	{
	case NUMBER_CODE: 
	case ROMAN_NUMERAL_CODE:
		scan_int();
		break;

	case STRING_CODE:
	case MEANING_CODE:
		save_scanner_status = scanner_status;
		scanner_status = NORMAL;
		get_token();
		scanner_status = save_scanner_status;
		break;
	
	case FONT_NAME_CODE:
		scan_font_ident();
		break;

	case JOB_NAME_CODE:
		if (job_name == 0)
			open_log_file();
		break;
	}
	old_setting = selector;
	selector = NEW_STRING;
	switch (c) {
	case NUMBER_CODE:
		print_val(cur_val);
		break;
	
	case ROMAN_NUMERAL_CODE:
		print_roman_int(cur_val);
		break;

	case STRING_CODE:
		if (cur_cs != 0)
			sprint_cs(cur_cs);
		else 
			print_char((ascii) cur_chr);
		break;
	
	case MEANING_CODE:
		print_meaning();
		break;

	case FONT_NAME_CODE:
		print_str(font_name[cur_val]);
		if (font_size[cur_val] != font_dsize[cur_val]) {
			print(" at ");
			print_scaled(font_size[cur_val]);
			print("pt");
		}
		break;

	case JOB_NAME_CODE:
		print_str(job_name);
		break;
	}
	selector = old_setting; 
	str_toks();
	ins_list(link(temp_head));
}

ptr
scan_toks (macro_def, xpand)
	bool		macro_def;
	bool		xpand;
{
	ptr		p;
	ptr		q;
	hword		s;
	hword		t;
	int		unbalance;
	hword		hash_brace;

	if (macro_def)
		scanner_status = DEFINING;
	else 
		scanner_status = ABSORBING;
	warning_index = cur_cs;
	def_ref = get_avail();
	token_ref_count(def_ref) = NULL;
	p = def_ref;
	hash_brace = 0;
	t = ZERO_TOKEN;
	if (macro_def) {
		loop {
			get_token();
			if (cur_tok < RIGHT_BRACE_LIMIT)
				break;
			if (cur_cmd == MAC_PARAM) {
				s = MATCH_TOKEN + cur_chr;
				get_token();
				if (cur_cmd == LEFT_BRACE) {
#ifdef VORTEX
					if (real_token && (group_s_node_begin != NIL)) {
						make_group_node(group_s_node_begin);
						group_s_node_begin = NIL;
					}
#endif
					hash_brace = cur_tok; 
					store_new_token(cur_tok);
					store_new_token(END_MATCH_TOKEN);
					goto done;
				}
				if (t == ZERO_TOKEN + 9) {
					print_err("You already have nine parameters");
					help_param_count();
					error();
				} else {
					incr(t);
					if (cur_tok != t) {
						print_err("Parameters must be numbered consecutively");
						help_param_num();
						back_error();
					}
					cur_tok = s;
				}
			}
			store_new_token(cur_tok);
		}
		store_new_token(END_MATCH_TOKEN);
		if (cur_cmd == RIGHT_BRACE) {
			print_err("Missing { inserted");
			incr(align_state); 
			help_left_brace();
			error();
			goto found;
#ifdef VORTEX
		} else {
			if (real_token && (group_s_node_begin != NIL)) {
				make_group_node(group_s_node_begin);
				group_s_node_begin = NIL;
			}
#endif
		}
	} else 
		scan_left_brace();

done:
	unbalance = 1;
	loop {
		if (xpand) {
			loop {
				get_next();
				if (cur_cmd <= MAX_COMMAND)
					break;
				if (cur_cmd != THE)
					expand();
				else {
					q = the_toks(); 
					if (link(temp_head) != NULL) {
						link(p) = link(temp_head);
						p = q;
					}
				}
			}
			x_token();
		} else 
			get_token();
		if (cur_tok < RIGHT_BRACE_LIMIT) {
			if (cur_cmd < RIGHT_BRACE) {
#ifdef VORTEX
				if (real_token && (group_s_node_begin != NIL)) {
					make_group_node(group_s_node_begin);
					group_s_node_begin = NIL;
				}
#endif
				incr(unbalance);
			} else {
#ifdef VORTEX
				if (real_token && (group_s_node_end != NIL)) {
					close_group_node(group_s_node_end);
					group_s_node_end = NIL;
				}
#endif
				decr(unbalance);
				if (unbalance == 0)
					break;
			}
		} else if (cur_cmd == MAC_PARAM && macro_def) {
			s = cur_tok;
			if (xpand)
				get_x_token();
			else 
				get_token();
			if (cur_cmd != MAC_PARAM) {
				if (cur_tok <= ZERO_TOKEN || cur_tok > t) {
					print_err("Illegal parameter number in definition of ");
					sprint_cs(warning_index);
					help_param_use();
					back_error(); 
					cur_tok = s;
				} else
					cur_tok = OUT_PARAM_TOKEN + cur_chr - '0';
			}
		}
		store_new_token(cur_tok);
	}

found:
	scanner_status = NORMAL;
	if (hash_brace != 0)
		store_new_token(hash_brace);
	return p;
}

read_toks (n, r)
	int		n;
	ptr		r;
{
	int		m;
	ptr		p;
	ptr		q;
	val		s;

	scanner_status  = DEFINING;
	warning_index = r;
	def_ref = get_avail();
	token_ref_count(def_ref) = NULL;
	p = def_ref;
	store_new_token(END_MATCH_TOKEN);
	if (n < 0 || n > 15)
		m = 16;
	else 
		m = n;
	s = align_state;
	align_state = 1000000;
	do {
		begin_file_reading();
		name = m + 1;
		if (read_open[m] == CLOSED) {
			if (interaction > NONSTOP_MODE) {
				if (n < 0) {
					prompt_input("");
				} else {
					wake_up_terminal();
					print_ln();
					sprint_cs(r);
					prompt_input("=");
					n = -1;
				}
			} else 
				fatal_error
					("*** (cannot \\read from terminal in nonstop modes)");
		} else if (read_open[m] == JUST_OPENED) {
#ifdef VORTEX
			if (input_ln_vir(read_file[m], FALSE))
#else
			if (input_ln(read_file[m], FALSE))
#endif
				read_open[m] = NORMAL;
			else {	
				a_close(read_file[m]);
				read_open[m] = CLOSED;
			}
		} else {
#ifdef VORTEX
			if (!input_ln_vir(read_file[m], TRUE)) {
#else
			if (!input_ln(read_file[m], TRUE)) {
#endif
				a_close(read_file[m]);
				read_open[m] = CLOSED;
				if (align_state != 1000000) {
					runaway();
					print_err("File ended within ");
					print_esc("read");
					help_read();
					align_state = 1000000;
					error();
				}
			}
		}
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
		state = NEW_LINE;
		loop {
			get_token();
			if (cur_tok == 0)
				break; 
			store_new_token(cur_tok);
		}
		end_file_reading();
	} while (align_state != 1000000);
	cur_val = def_ref;
	scanner_status = NORMAL; 
	align_state = s;
}

show_token_list	(p, q, l)
	ptr		p;
	ptr		q;
	val		l;
{
	hword		c;
	hword		m;
	ascii		n;
	ascii		match_chr;

	match_chr = '#';
	n = '0';
	for (tally = 0; p != NULL && tally < l; p = link(p)) {
		if (p == q)
			magic_c();
	        if (p < hi_mem_min || p > mem_end) {
			print_esc("CLOBBERED.");
			return;
		}
		if (info(p) >= CS_TOKEN_FLAG)
			print_cs(info(p) - CS_TOKEN_FLAG);
		else {
			m = info(p) / 0400;
			c = info(p) % 0400;
			if (m < 0 || c > 127)
				print_esc("BAD.");
			else {
				switch (m)
				{
				case LEFT_BRACE:
				case RIGHT_BRACE:
				case MATH_SHIFT:
				case TAB_MARK:
				case SUP_MARK:
				case SUB_MARK:
				case SPACER:
				case LETTER:
				case OTHER_CHAR:
					print_char(c); 
					break;
				
				case MAC_PARAM:
					print_char(c);
					print_char(c);
					break;
				
				case OUT_PARAM:
					print_char(match_chr);
					if (c <= 9)
						print_char(c + '0');
					else {
						print_char('!');
						return;
					}
					break;
				
				case MATCH:
					match_chr = c;
					print_char(c);
					incr(n);
					print_char(n);
					if (n > '9')
						return;
					break;
				
				case END_MATCH:
					print("->");
					break;
				
				default:
					print_esc("BAD.");
					break;
				}
			}
		}
	}
	if (p != NULL)
		print_esc("ETC.");
}

token_show (p)
	ptr		p;
{ 
	if (p == NULL)	
		print("(null)");
	else 
		show_token_list(link(p), NULL, 1000L);
}

print_meaning ()
{
	print_cmd_chr(cur_cmd, cur_chr);
	if (cur_cmd >= CALL) {
		print_char(':');
		print_ln();
		token_show(cur_chr);
	} else if (cur_cmd == TOP_BOT_MARK) {
		print_char(':');
		print_ln();
		token_show(cur_mark[cur_chr]);
	}
}

flush_list (p)
	ptr		p;
{
	ptr		q;
	ptr		r;

#ifdef	VORTEX
	if (p >= MEM_TOP || p <= save_hi_mem_min)
#endif
	if (p != NULL) {
		r = p;
		do {
			q = r;
			r = link(r);
#ifdef	STAT
			decr(dyn_used);
#endif
		} while (r != NULL);
		link(q) = avail;
		avail = p;
	}
}

/*
 *	Help text
 */

help_param_num ()
{
	help2("I've inserted the digit you should have used after the #.",
	"Type `1' to delete what you did use.");
}

help_param_count ()
{
	help1("I'm going to ignore the # sign you just used.");
}

help_left_brace ()
{
	help2("Where was the left brace? You said something like `\\def\\a}',",
	"which I'm going to interpret as `\\def\\a{}'.");
}

help_param_use ()
{
	help3("You meant to type ## instead of #, right?",
	"Or maybe a } was forgotten somewhere earlier, and things",
	"are all screwed up? I'm going to assume you meant ##.");
}

help_read ()
{
	help1("This \\read has unbalanced braces.");
}
