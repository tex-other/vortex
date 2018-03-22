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
 *		boxlists.c
 */

#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"eq.h"
#include	"eqstack.h"
#include	"def.h"
#include	"box.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"scan.h"
#include	"tokenlists.h"
#include	"evalstack.h"
#include	"tfm.h"
#include	"pack.h"
#include	"page.h"
#include	"math.h"
#include	"io.h"
#include	"print.h"
#include	"error.h"
#include	"boxlists.h"

#ifdef VORTEX
#include	"main.h"
#endif

extern ptr		cur_box;

append_glue ()
{
	hword		s;
	
	s = cur_chr;
	switch (s) 
	{
	case FIL_CODE:
		cur_val = fil_glue;
		break;

	case FILL_CODE:
		cur_val = fill_glue;
		break;

	case SS_CODE:
		cur_val = ss_glue;
		break;

	case FIL_NEG_CODE:
		cur_val = fil_neg_glue;
		break;

	case SKIP_CODE:
		scan_glue(GLUE_VAL);
		break;

	case MSKIP_CODE:
		scan_glue(MU_VAL);
		break;

	default:
		break;
	}
	tail_append(new_glue((ptr) cur_val));
	if (s >= SKIP_CODE) {
		decr(glue_ref_count((ptr) cur_val));
		if (s > SKIP_CODE)
			subtype(tail) = MU_GLUE;
	}
}

append_kern ()
{
	qword		s;
	
	s = cur_chr;
	scan_dimen(s == MU_GLUE, FALSE, FALSE);
	tail_append(new_kern(cur_val));
	subtype(tail) = s;
}

handle_right_brace ()
{
	scal		d;
	val		f;
	ptr		p;
	ptr		q;
	
	switch (cur_group)
	{
	case SIMPLE_GROUP:
		unsave();
		break;
	
	case BOTTOM_LEVEL:
		print_err("Too many }'s");
		help_close_group();
		error();
		break;
	
	case SEMI_SIMPLE_GROUP:
	case MATH_SHIFT_GROUP:
	case MATH_LEFT_GROUP:
		extra_right_brace();
		break;

	case HBOX_GROUP:
		package(0);
		break;
	
	case ADJUSTED_HBOX_GROUP:
		adjust_tail = adjust_head;
		package(0);
		break;
	
	case VBOX_GROUP:
		end_graf();
		package(0);
		break;
	
	case VTOP_GROUP:
		end_graf();
		package(VTOP_CODE);
		break;

	case INSERT_GROUP:
		end_graf();
		q = split_top_skip;
		add_glue_ref(q);
		d = split_max_depth;
		f = floating_penalty;
		unsave();
		decr(save_ptr);
		p = vpack(link(head), NATURAL);
		pop_nest();
		if (saved(0) < 255) {
			tail_append(get_node(INS_NODE_SIZE));
			type(tail) = INS_NODE;
			subtype(tail) = qi(saved(0));
			height(tail) = height(p) + depth(p);
			ins_ptr(tail) = list_ptr(p);
			split_top_ptr(tail) = q;
			depth(tail) = d;
			float_cost(tail) = f;
		} else {
			tail_append(get_node(SMALL_NODE_SIZE));
			type(tail) = ADJUST_NODE;
			subtype(tail) = 0;
			adjust_ptr(tail) = list_ptr(p);
			delete_glue_ref(q);
		}
		free_node(p, BOX_NODE_SIZE);
		if (nest_ptr == 0) 
			build_page();
		break;
	
	case OUTPUT_GROUP:
		if (loc != NULL)  {
			print_err("Unbalanced output routine");
			help_output_balance();
			error();
			do get_token();
			while (loc != NULL);
		}
		end_token_list();
		end_graf();
		unsave();
		output_active = FALSE;
		insert_penalties = 0;
		if (box(255) != NULL) {
			print_err("Output routine didn't use all of ");
			print_esc("box255");
			help_output();
			box_error(255);
		}
		if (tail != head) {
			link(page_tail) = link(head);
			page_tail = tail;
		}
		if (link(page_head) != NULL) {
			if (link(contrib_head) == NULL)
				contrib_tail = page_tail;
			link(page_tail) = link(contrib_head);
			link(contrib_head) = link(page_head);
			link(page_head) = NULL;
			page_tail = page_head;
		}
		pop_nest();
		build_page();
		break;
	
	case DISC_GROUP:
		build_discretionary();
		break;
	
	case ALIGN_GROUP:
		back_input();
		cur_tok = CS_TOKEN_FLAG + FROZEN_CR;
		print_err("Missing ");
		print_esc("cr");
		print(" inserted");
		help_align_cr();
		ins_error(); 
		break;

	case NO_ALIGN_GROUP:
		end_graf();
		unsave();
		align_peek();
		break;
	
	case VCENTER_GROUP:
		end_graf();
		unsave();
		save_ptr -= 2;
		p = vpackage(link(head), saved(1), (int) saved(0), MAX_DIMEN);
		pop_nest();
		tail_append(new_noad());
		type(tail) = VCENTER_NOAD;
		math_type(nucleus(tail)) = SUB_BOX;
		info(nucleus(tail)) = p;
		break;
	
	case MATH_CHOICE_GROUP:
		build_choices();
		break;

	case MATH_GROUP:
		unsave();
		decr(save_ptr);
		math_type(saved(0)) = SUB_MLIST;
		p = fin_mlist(NULL);
		info(saved(0)) = p;
		if (p != NULL) {
			if (link(p) == NULL) {
				if (type(p) == ORD_NOAD) {
					if (math_type(subscr(p)) == EMPTY &&
						math_type(supscr(p)) == EMPTY) {
						mem[saved(0)].hh = mem[nucleus(p)].hh;
						free_node(p, NOAD_SIZE);
					}
				} else if (type(p) == ACCENT_NOAD &&
					saved(0) == nucleus(tail) &&
					type(tail) == ORD_NOAD) {
					q = head;
					while (link(q) != tail)
						q = link(q);
					link(q) = p;
					free_node(tail, NOAD_SIZE);
					tail = p;
				}
			}
		}
		break;
	
	default:
		confusion("rightbrace");
		break;
	}
}

extra_right_brace ()
{
	print_err("Extra }, or forgotten ");
	switch (cur_group) 
	{
	case SEMI_SIMPLE_GROUP:
		print_esc("endgroup");
		break;

	case MATH_SHIFT_GROUP:
		print_char('$');
		break;

	case MATH_LEFT_GROUP:
		print_esc("right");
		break;
	}
	help_group_close();
	error();
	incr(align_state);
}

package (c)
	int		c;
{
	scal		d;
	scal		h;
	ptr		p;

	d = box_max_depth;
	unsave();
	save_ptr -= 3;
	if (mode == -HMODE)
		cur_box = hpack(link(head), saved(2), (int) saved(1));
	else {
		cur_box = vpackage(link(head), saved(2), (int) saved(1), d);
		if (c == VTOP_CODE) {
			h = 0;
			p = list_ptr(cur_box);
			if (p != NULL && type(p) <= RULE_NODE)
				h = height(p);
			depth(cur_box) += height(cur_box) - h;
			height(cur_box) = h;
		}
	}
	pop_nest();
	box_end();
}

box_end ()
{
	ptr		p;

	if (saved(0) < BOX_FLAG) {
		if (cur_box != NULL) {
			shift_amount(cur_box) = saved(0);
			if (abs(mode) == VMODE) {
				append_to_vlist(cur_box);
				if (adjust_tail != NULL) {
					if (adjust_head != adjust_tail) {
						link(tail) = link(adjust_head);
						tail = adjust_tail;
					}
					adjust_tail = NULL;
				}
				if (mode > 0) 
					build_page();
			} else {
				if (abs(mode) == HMODE)
					space_factor = 1000;
				else {
					p = new_noad();
					math_type(nucleus(p)) = SUB_BOX;
					info(nucleus(p)) = cur_box;
					cur_box = p;
				}
				link(tail) = cur_box;
				tail = cur_box;
			}
		}
	} else if (saved(0) < SHIP_OUT_FLAG)
		if (saved(0) < BOX_FLAG + 256)
			eq_define((ptr)(BOX_BASE-BOX_FLAG+saved(0)), BOX_REF, cur_box);
		else
			geq_define((ptr)(BOX_BASE-BOX_FLAG-256+saved(0)), BOX_REF, cur_box);
	else if (cur_box != NULL) {
		if (saved(0) > SHIP_OUT_FLAG) {
			get_nbrx_token();
			if (cur_cmd == HSKIP && abs(mode) != VMODE ||
				cur_cmd == VSKIP && abs(mode) == VMODE ||
				cur_cmd == MSKIP && abs(mode) == MMODE) {
				append_glue();
				leader_ptr(tail) = cur_box;
				subtype(tail) = saved(0) - (LEADER_FLAG - A_LEADERS);
			} else {
				print_err("Leaders not followed by proper glue");
				help_leaders();
				back_error();
				flush_node_list(cur_box);
			}
		} else
			ship_out(cur_box);
	}
}

begin_box ()
{
	int		k;
	int		m;
	int		n;
	ptr		p;
	ptr		q;

	switch (cur_chr)
	{
	case BOX_CODE:
		scan_eight_bit_int();
		cur_box = box(cur_val);
		box(cur_val) = NULL;
		break;
	
	case COPY_CODE:
		scan_eight_bit_int();
		cur_box = copy_node_list(box(cur_val));
		break;
	
	case LAST_BOX_CODE:
		cur_box = NULL;
		if (abs(mode) == MMODE) {
			you_cant();
			help_lastbox_m();
			error();
		} else if (mode == VMODE && head == tail) {
			you_cant();
			help_lastbox_v();
			error();
		} else if (!is_char_node(tail) &&
			(type(tail) == HLIST_NODE || type(tail) == VLIST_NODE)) {
			q = head;
			do	{
				p = q;
				if (!is_char_node(q) && type(q) == DISC_NODE) {
					for (m = 1; m <= replace_count(q); incr(m)) 
						p = link(p);
					if (p == tail)
						break;
				}
				q = link(p);
			} while (q != tail);
			cur_box = tail;
			shift_amount(cur_box) = 0;
			tail = p;
			link(p) = NULL;
		}
		break;

	case VSPLIT_CODE:
		scan_eight_bit_int();
		n = cur_val;
		if (!scan_keyword("to")) {
			print_err("Missing `to' inserted");
			help_vsplit();
			error();
		}
		scan_normal_dimen();
		cur_box = vsplit(n, cur_val);
		break;
	
	default:
		k = cur_chr - VTOP_CODE;
		incr(save_ptr);
		scan_spec();
		if (k == HMODE) {
			if (saved(-3) < BOX_FLAG && abs(mode) == VMODE)
				new_save_level(ADJUSTED_HBOX_GROUP);
			else new_save_level(HBOX_GROUP);
		} else {
			if (k == VMODE)
				new_save_level(VBOX_GROUP);
			else {
				new_save_level(VTOP_GROUP);
				k = VMODE;
			}
			normal_paragraph();
		}
		push_nest();
		mode = -k;
		if (k == VMODE) {
			prev_depth = IGNORE_DEPTH;
			if (every_vbox != NULL)
				begin_token_list(every_vbox, EVERY_VBOX_TEXT);
		} else {
			space_factor = 1000;
			if (every_hbox != NULL) 
				begin_token_list(every_hbox, EVERY_HBOX_TEXT);
		}
		return;
		break;
	}
	box_end();
}

scan_spec ()
{
	if (scan_keyword("to"))
		saved(0) = EXACTLY;
	else if (scan_keyword("spread")) 
		saved(0) = ADDITIONAL;
	else {
		saved(0) = ADDITIONAL;
		saved(1) = 0;
		goto found;
	}
	scan_normal_dimen();
	saved(1) = cur_val;

found:
	save_ptr += 2;
	scan_left_brace();
}

scan_box ()
{
	get_nbrx_token();
	if (cur_cmd == MAKE_BOX)
		begin_box();
	else if (saved(0) >= LEADER_FLAG &&
			(cur_cmd == HRULE || cur_cmd == VRULE)) {
			cur_box = scan_rule_spec();
			box_end();
	} else {
		print_err("A <box> was supposed to be here");
		help_box();
		back_error();
	}
}

normal_paragraph ()
{
	if (looseness != 0)
		eq_word_define((ptr) INT_BASE + LOOSENESS_CODE, 0L);
	if (hang_indent != 0)
		eq_word_define((ptr) DIMEN_BASE + HANG_INDENT_CODE, 0L);
	if (hang_after != 1)
		eq_word_define((ptr) INT_BASE + HANG_AFTER_CODE, 1L);
	if (par_shape_ptr != NULL)
		eq_define(PAR_SHAPE_LOC, SHAPE_REF, NULL);
}

new_graf (indented)
	bool		indented;
{
	prev_graf = 0;
	if (mode == VMODE || head != tail)
		tail_append(new_param_glue(PAR_SKIP_CODE));
	push_nest();
	mode = HMODE;
	space_factor = 1000;
	if (indented) {
		tail = new_null_box();
		link(head) = tail;
		width(tail) = par_indent;
	}
	if (every_par != NULL)
		begin_token_list(every_par, EVERY_PAR_TEXT);
	if (nest_ptr == 1) 
		build_page();
}

indent_in_hmode ()
{
	ptr		p;
	ptr		q;

	if (cur_chr > 0) {
		p = new_null_box();
		width(p) = par_indent;
		if (abs(mode) == HMODE)
			space_factor = 1000;
		else {
			q = new_noad();
			math_type(nucleus(q)) = SUB_BOX;
			info(nucleus(q)) = p;
#ifdef VORTEX
			math_esc(q) = ir_char(p);
#endif
			p = q;
		}
		tail_append(p);
	}
}

head_for_vmode ()
{
	if (mode < 0) {
		if (cur_cmd != HRULE)
			off_save();
		else {
			print_err("You can't use `");
			print_esc("hrule");
			print("' here except with leaders");
			help_head_for_vmode();
			error();
		}
	} else {
		back_input();
		cur_tok = par_token;
		back_input();
		token_type = INSERTED;
	}
}
	
end_graf ()
{
	if (mode == HMODE) {
		if (head == tail)
			pop_nest();
		else
			line_break(widow_penalty);
		normal_paragraph();
		error_count = 0;
	}
}

append_to_vlist (b)
	ptr		b;
{
	scal		d;
	ptr		p;

	if (prev_depth > IGNORE_DEPTH) {
		d = width(baseline_skip) - prev_depth - height(b);
		if (d < line_skip_limit)
			p = new_param_glue(LINE_SKIP_CODE);
		else {
			p = new_skip_param(BASELINE_SKIP_CODE);
			width(temp_ptr) = d;
		}
		link(tail) = p;
		tail = p;
	}
	link(tail) = b;
	tail = b;
	prev_depth = depth(b);
}

begin_insert_or_adjust ()
{
	if (cur_cmd == VADJUST)
		cur_val = 255;
	else {
		scan_eight_bit_int();
		if (cur_val == 255) {
			print_err("You can't ");
			print_esc("insert");
			print_int(255);
			help_insert_255();
			error();
			cur_val = 0;
		}
	}
	saved(0) = cur_val;
	incr(save_ptr);
	new_save_level(INSERT_GROUP);
	scan_left_brace();
	normal_paragraph();
	push_nest();
	mode = -VMODE;
	prev_depth = IGNORE_DEPTH;
}

make_mark ()
{
	ptr		p;

	scan_toks(FALSE, TRUE);
	p = get_node(SMALL_NODE_SIZE);
	type(p) = MARK_NODE;
	subtype(p) = 0;
	mark_ptr(p) = def_ref;
	link(tail) = p;
	tail = p;
}

append_penalty ()
{
	scan_int();
	tail_append(new_penalty(cur_val));
	if (mode == VMODE) 
		build_page();
}

delete_last ()
{
	qword		m;
	ptr		p;
	ptr		q;

	if (mode == VMODE && tail == head) {
		if (cur_chr != GLUE_NODE || last_glue != MAX_HALFWORD) {
			you_cant();
			help_delete_last();
			if (cur_chr == KERN_NODE)
				help_line[1] = "Try `I\\kern-\\lastkern instead.";
			else if (cur_chr != GLUE_NODE)
				help_line[1] = "Perhaps you can make an output routine do it.";
			error();
		}
	} else if (!is_char_node(tail) && type(tail) == cur_chr) {
		q = head;
		do	{
			p = q;
			if (!is_char_node(q) && type(q) == DISC_NODE) {
				for (m = 1; m <= replace_count(q); incr(m))
					p = link(p);
				if (p == tail)
					return;
			}
			q = link(p);
		} while (q != tail);
		link(p) = NULL;
		flush_node_list(tail);
		tail = p;
	}
}

unpackage ()
{	
	int		c;
	ptr		p;
	
	c = cur_chr;
	scan_eight_bit_int();
	p = box(cur_val);
	if (p == NULL) return;
	if (abs(mode) == MMODE ||
		abs(mode) == VMODE && type(p) != VLIST_NODE ||
		abs(mode) == HMODE && type(p) != HLIST_NODE) {
		print_err("Incompatible list can't be unboxed");
		help_pandora();
		error();
		return;
	}
	if (c == COPY_CODE)
		link(tail) = copy_node_list(list_ptr(p));
	else {
		link(tail) = list_ptr(p);
		box(cur_val) = NULL;
		free_node(p, BOX_NODE_SIZE);
	}
	while (link(tail) != NULL)
		tail = link(tail);
}

append_italic_correction ()
{
	ptr		p;
	fnt		f;

 	if (tail != head) {
		if (is_char_node(tail))
			p = tail;
		else if (type(tail) == LIGATURE_NODE)
			p = lig_char(tail);
		else return;
	}
	f = font(p);
	tail_append(new_kern(char_italic(f, char_info(f, character(p)))));
	subtype(tail) = EXPLICIT;
}

append_discretionary ()
{
	int		c;

	tail_append(new_disc());
	if (cur_chr == 1) {
		c = hyphen_char[cur_font];
		if (c >= 0 && c < 256)
			pre_break(tail) = new_character(cur_font, c);
	} else {
		incr(save_ptr);
		saved(-1) = 0;
		scan_left_brace();
		new_save_level(DISC_GROUP);
		push_nest();
		mode = -HMODE;
		space_factor = 1000;
	}
}

build_discretionary ()
{
	int		n;
	ptr		p;
	ptr		q;

	unsave();
	q = head;
	p = link(q);
	n = 0;
	while (p != NULL) {
		if (!is_char_node(p) && type(p) > RULE_NODE &&
			type(p) != KERN_NODE && type(p) != LIGATURE_NODE) {
			print_err("Improper discretionary list");
			help_discretionary();
			error();
			begin_diagnostic();
			print_nl("The following discretionary sublist has been deleted:");
			show_box(p);
			end_diagnostic(TRUE);
			flush_node_list(p);
			link(q) = NULL;
			break;
		}
		q = p;
		p = link(q);
		incr(n);
	}
	p = link(head);
	pop_nest();
	switch ((int) saved(-1))
	{
	case 0:
		pre_break(tail) = p;
		break;

	case 1:
		post_break(tail) = p;
		break;

	case 2:
		if (n > 0 && abs(mode) == MMODE) {
			print_err("Illegal math ");
			print_esc("discretionary");
			help_math_disc();
			flush_node_list(p);
			n = 0;
			error();
		} else
			link(tail) = p;
		if (n <= MAX_QUARTERWORD)
			replace_count(tail) = n;
		else {
			print_err("Discretionary list is too long");
			help_disc();
			error();
		}
		if (n > 0) tail = q;
		decr(save_ptr);
		return;
	}
	incr(saved(-1));
	scan_left_brace();
	new_save_level(DISC_GROUP);
	push_nest();
	mode = -HMODE;
	space_factor = 1000;
}

make_accent ()
{
	scal		a;
	fnt		f;
	scal		h;
	fourq		i;
	ptr		p;
	ptr		q;
	ptr		r;
	float		s;
	float		t;
	scal		w;
	scal		x;
	scal		delta;

#ifdef VORTEX
	ir_cur = NIL;
#endif
	scan_char_num();
	f = cur_font;
	p = new_character(f, cur_val);
	if (p != NULL) {
		x = x_height(f);
		s = (float) slant(f) / 65536.0;
		a = char_width(f, char_info(f, character(p)));
		do_assignments();
		q = NULL;
		f = cur_font;
		if (cur_cmd == LETTER ||
			cur_cmd == OTHER_CHAR ||
			cur_cmd ==  CHAR_GIVEN) 
			q = new_character(f, cur_chr);
		else if (cur_cmd ==  CHAR_NUM) {
			scan_char_num();
			q = new_character(f, cur_val);
		} else back_input();
		if (q != NULL) {
			t = (float) slant(f) / 65536.0;
			i = char_info(f, character(q));
			w = char_width(f, i); 
			h = char_height(f, height_depth(i));
			if (h != x) {
				p = hpack(p, NATURAL);
				shift_amount(p) = x - h;
			}
			delta = round((float) (w - a) / 2.0 + h * t - x * s);
			r = new_kern(delta);
			subtype(r) = ACC_KERN;
			link(tail) = r;
			link(r) = p;
			tail = new_kern(-a - delta);
			subtype(tail) = ACC_KERN;
			link(p) = tail;
			p = q;
		}
		link(tail) = p;
		tail = p;
		space_factor = 1000;
	}
}

align_error ()
{
	if (abs(align_state) > 2) {
		print_err("Misplaced ");
		print_cmd_chr(cur_cmd, cur_chr);
		if (cur_tok == TAB_TOKEN + '&') 
			help_tab();
		else help_align_error();
		error();
	} else {
		back_input();
		if (align_state < 0) {
			print_err("Missing { inserted");
			incr(align_state);
			cur_tok = LEFT_BRACE_TOKEN + '{';
		} else {
			print_err("Missing } inserted");
			decr(align_state);
			cur_tok = RIGHT_BRACE_TOKEN + '}';
		}
		help_fix_alignment();
		ins_error();
	}
}

no_align_error ()
{
	print_err("Misplaced ");
	print_esc("noalign");
	help_noalign();
	error();
}

omit_error ()
{
	print_err("Misplaced ");
	print_esc("omit");
	help_omit();
	error();
}

do_endv ()
{
	if (cur_group == ALIGN_GROUP) {
		end_graf();
		if (fin_col())
			fin_row();
	} else off_save();
}

cs_error ()
{
	print_err("Extra ");
	print_esc("endcsname");
	help_csname();
	error();
}

/*
 *	Help text
 */


help_head_for_vmode ()
{
	help2("To put a horizontal rule in an hbox or an alignment,",
	"you should use \\leaders or \\hrulefill (see The TeXbook).");
}


help_close_group ()
{
	help2("You've closed more groups than you opened.",
	"Such booboos are generally harmless, so keep going.");
}


help_output_balance ()
{
	help2("Your sneaky output routine has fewer real {'s than }'s.",
	"I can't handle that very well; good luck.");
}


help_output ()
{
	help3("Your \\output commands should empty \\box255,",
	"e.g., by saying `\\shipout\\box255'.",
	"Proceed; I'll discard its present contents.");
}


help_group_close ()
{
	help5("I've deleted a group-closing symbol because it seems to be",
	"spurious, as in `$x}$'. But perhaps the } is legitimate and",
	"you forgot something else, as in `\\hbox{$x}'. In such cases",
	"the way to recover is to insert both the forgotten and the",
	"deleted material, e.g., by typing `I$}'.");
}


help_leaders ()
{
	help3("You should say `\\leaders <box or rule><hskip or vskip>'.",
	"I found the <box or rule>, but there's no suitable",
	"<hskip or vskip>, so I'm ignoring these leaders.");
}


help_lastbox_m ()
{
	help1("Sorry; this \\lastbox will be void.");
}


help_lastbox_v ()
{
	help2("Sorry...I usually can't take things from the current page.",
	"This \\lastbox will therefore be void.");
}


help_vsplit ()
{
	help2("I'm working on `\\vsplit<box number> to <dimen>';",
	"will look for the <dimen> next.");
}


help_box ()
{
	help3("I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
	"something like that. So you might find something missing in",
	"your output. But keep trying; you can fix this later.");
}


help_insert_255 ()
{
	help1("I'm changing to \\insert0; box 255 is special.");
}


help_space_fac ()
{
	help1("I allow only values in the range 1..65536 here.");
}


help_delete_last ()
{
	help2("Sorry...I usually can't take things from the current page.",
	"Try `I\\vskip-\\lastskip' instead.");
}


help_pandora ()
{
	help3("Sorry, Pandora. (You sneaky devil.)",
	"I refuse to unbox an \\hbox in vertical mode or vice versa.",
	"And I can't open any boxes in math mode.");
}


help_math_disc ()
{
	help2("Sorry: The third part of a discretionary break must be",
	"empty, in math formulas. I had to delete your third part.");
}


help_discretionary ()
{
	help1("Discretionary lists must contain only boxes and kerns.");
}


help_disc ()
{
	help2("Wow---I never thought anybody would tweak me here.",
	"You can't seriously need such a huge discretionary list?");
}


help_missing_the ()
{
	help3("Please dont say \\count or \\dimen or \\skip in the midst of",
	"a paragraph or formula. I'm going to assume that you",
	"meant to say `\\the\\count' or `\\the\\dimen' or `\\the\\skip',");
}


help_tab ()
{
	help6("I can't figure out why you would want to use a tab mark",
	"here. If you want an ampersand, the remedy is",
	"simple: Just type `I\\&' now. But if some right brace",
	"up above has ended a previous alignment prematurely,",
	"you're probably due for more error messages, and you",
	"might try typing `S' now just to see what is salvageable.");
}


help_align_error ()
{
	help5("I can't figure out why you would want to use a tab mark",
	"or \\cr or \\span just now. If something like a right brace",
	"up above has ended a previous alignment prematurely,",
	"you're probably due for more error messages, and you",
	"might try typing `S' now just to see what is salvageable.");
}


help_align_cr ()
{
	help1("I'm guessing that you meant to end an alignment here.");
}
	

help_fix_alignment ()
{
	help3("I've put in what seems to be necessary to fix",
	"the current column of the current alignment.",
	"Try to go on, since this might almost work.");
}


help_noalign ()
{
	help2("I expect to see \\noalign only after the \\cr of",
	"an alignment. Proceed, and I'll ignore this case.");
}


help_omit ()
{
	help2("I expect to see \\omit only after tab marks or the \\cr of",
	"an alignment. Proceed, and I'll ignore this case.");
}


help_csname ()
{
	help1("I'm ignoring this, since I wasn't doing a \\csname.");
}
