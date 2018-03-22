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
 *		mathlists.c
 */

#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"arith.h"
#include	"eq.h"
#include	"eqstack.h"
#include	"evalstack.h"
#include	"token.h"
#include	"tokenstack.h"
#include	"scan.h"
#include	"tfm.h"
#include	"box.h"
#include	"pack.h"
#include	"math.h"
#include	"mlist-hlist.h"
#include	"par.h"
#include	"page.h"
#include	"print.h"
#include	"error.h"
#include	"mathlists.h"

#ifdef VORTEX
#include	"allir.h"
#include	"main.h"
#include	"macro.h"
#include	"var.h"
#include	"msg.h"
struct _math	*make_math_node();
extern int	in_math;
#endif

push_math (c)
	group	c;
{
	push_nest();
	mode = -MMODE;
	incompleat_noad = NULL;
	new_save_level(c);
}

init_math ()
{
	scal		d;
	fnt		f;
	scal		l;
	int		n;
	ptr		p;
	ptr		q;
	scal		s;
	scal		v;
	scal		w;

#ifdef VORTEX
	struct _char	*char_ptr, *char_ptr1;
	struct _math	*tmp;
	ptr		px;
	
	if (math_que_top == NIL ) {
		msg(STDERR, "something strange in math nest!");
		goto t100_1;
	}
	px = math_que_top;
	char_ptr = (struct _char *)math_char_field(px);
	math_que_top = math_rlink(px);
	if (math_que_top != NIL)
		math_llink(math_que_top) = NIL;
	free_node(px, MATH_QUE_SIZE);
	if (char_ptr != NIL)
		tmp = make_math_node(char_ptr);
t100_1:
	if (mode >0 )
		disp_watch = TRUE;
#endif
	get_token();
#ifdef VORTEX
	disp_watch = FALSE;
	in_math = TRUE;
#endif
	if (cur_cmd == MATH_SHIFT && mode > 0) {
#ifdef VORTEX
		if (math_que_top == NIL ) {
			msg(STDERR, "something strange in display nest!");
			goto t100_2;
		}
		px = math_que_top;
		char_ptr1 = (struct _char *)math_char_field(px);
		math_que_top = math_rlink(px);
		if (math_que_top != NIL)
			math_llink(math_que_top) = NIL;
		free_node(px, MATH_QUE_SIZE);
		if (char_ptr != NIL) {
			make_display_node(tmp); /* change math to display */
			make_word_node(char_ptr1, char_ptr1);
		}
t100_2:
#endif
		if (head == tail) {
			pop_nest();
			w = -MAX_DIMEN;
		} else {
			line_break(display_widow_penalty);
			v = shift_amount(just_box) + 2 * quad(cur_font);
			w = -MAX_DIMEN;
			p = list_ptr(just_box);
			while (p != NULL) {
		reswitch:
				if (is_char_node(p)) {
					f = font(p);
					d = char_width(f, char_info(f, character(p)));
					goto found;
				}
				switch (type(p))
				{
				case HLIST_NODE:
				case VLIST_NODE:
				case RULE_NODE:
					d = width(p);
					goto found;
					break;
				
				case LIGATURE_NODE:
					make_char_from_lig();
					goto reswitch;
					break;
				
				case KERN_NODE:
				case MATH_NODE:
					d = width(p);
					break;

				case GLUE_NODE:
					q = glue_ptr(p);
					d = width(q);
					if (glue_sign(just_box) == STRETCHING) {
						if (glue_order(just_box) == stretch_order(q) &&
							stretch(q) != 0)
							v = MAX_DIMEN;
					} else if (glue_sign(just_box) == SHRINKING)  {
						if (glue_order(just_box) == shrink_order(q) &&
							shrink(q) != 0) 
							v = MAX_DIMEN;
					}
					if (subtype(p) >= A_LEADERS)	
						goto found;
					break;

				case WHATSIT_NODE: 
					d = 0;
					break;

				default:
					d = 0;
					break;
				}
				if (v < MAX_DIMEN)
					v += d;
				goto not_found;
			
			found:
				if (v < MAX_DIMEN) {
					v += d;
					w = v;
				} else {
					w = MAX_DIMEN;
					break;
				}

			not_found:
				p = link(p);
			}
		}
		if (par_shape_ptr == NULL) {
			if (hang_indent != 0 &&
				(hang_after >= 0 &&
				prev_graf + 2 > hang_after ||
				prev_graf + 1 < -hang_after)) {
				l = hsize - abs(hang_indent);
				s = (hang_indent > 0) ? hang_indent : 0;
			} else {
				l = hsize;
				s = 0;
			}
		} else {
			n = info(par_shape_ptr);
			if (prev_graf + 2 >= n)
				p = par_shape_ptr + 2 * n;
			else p = par_shape_ptr + 2 * (prev_graf + 2);
			s = mem[p - 1].sc;
			l = mem[p].sc;
		}
		push_math(MATH_SHIFT_GROUP);
		mode = MMODE;
		eq_word_define(INT_BASE + CUR_FAM_CODE, -1L);
		eq_word_define(DIMEN_BASE + PRE_DISPLAY_SIZE_CODE, w);
		eq_word_define(DIMEN_BASE + DISPLAY_WIDTH_CODE, l);
		eq_word_define(DIMEN_BASE + DISPLAY_INDENT_CODE, s);
		if (every_display != NULL)
			begin_token_list(every_display, EVERY_DISPLAY_TEXT);
		if (nest_ptr == 1)
			build_page();
	} else {
		back_input(); 
		push_math(MATH_SHIFT_GROUP);
		eq_word_define(INT_BASE + CUR_FAM_CODE, -1L);
		if (every_math != NULL)
			begin_token_list(every_math, EVERY_MATH_TEXT);
	}
}

start_eq_no ()
{
	saved(0) = cur_chr;
	incr(save_ptr);
	push_math(MATH_SHIFT_GROUP);
	eq_word_define(INT_BASE + CUR_FAM_CODE, -1L);
	if (every_math != NULL)
		begin_token_list(every_math, EVERY_MATH_TEXT);
}

#define	fam_in_range()	 (cur_fam >= 0 && cur_fam < 16)

scan_math (p)
	ptr		p;
{
	int		c;

restart:
	get_nbrx_token();

reswitch:
	switch (cur_cmd)
	{
	case LETTER:
	case OTHER_CHAR:
	case CHAR_GIVEN:
		if (cur_chr >= 128)
			c = cur_chr;
		else {
			c = ho(math_code(cur_chr));
			if (c == 0100000) {
				cur_cs = cur_chr + ACTIVE_BASE;
				cur_cmd = eq_type(cur_cs);
				cur_chr = equiv(cur_cs);
				x_token();
				back_input();
				goto restart;
			}
		}
		break;
	
	case CHAR_NUM:
		scan_char_num();
		cur_chr = cur_val;
		cur_cmd = CHAR_GIVEN;
		goto reswitch;
		break;
	
	case MATH_CHAR_NUM:
		scan_fifteen_bit_int();
		c = cur_val;
		break;

	case MATH_GIVEN:
		c = cur_chr;
		break;
		
	case DELIM_NUM:
		scan_twenty_seven_bit_int();
		c = cur_val / 010000;
		break;

	default:
		back_input();
		scan_left_brace();
		saved(0) = p;
		incr(save_ptr);
		push_math(MATH_GROUP);
		return;
		break;
	}
	math_type(p) = MATH_CHAR;
	character(p) = c % 256;
	if (c >= VAR_CODE && fam_in_range())
		fam(p) = cur_fam;
	else fam(p) = (c / 256) % 16;
}

set_math_char (c)
	val		c;
{
	ptr		p;

	if (c >= 0100000) {
		cur_cs = cur_chr + ACTIVE_BASE;
		cur_cmd = eq_type(cur_cs);
		cur_chr = equiv(cur_cs);
		x_token();
		back_input();
	} else {
		p = new_noad();
		math_type(nucleus(p)) = MATH_CHAR;
		character(nucleus(p)) = c % 256;
		fam(nucleus(p)) = (c / 256) % 16;
		if (c >= VAR_CODE) {
			if (fam_in_range())
				fam(nucleus(p)) = cur_fam;
			type(p) = ORD_NOAD;
		} else
			type(p) = ORD_NOAD + (c / 010000);
		link(tail) = p;
		tail = p;
	}
}

math_limit_switch ()
{
	if (head != tail && type(tail) == OP_NOAD) {
		subtype(tail) = cur_chr;
		return;
	}
	print_err("Limit controls must follow a math operator");
	help_limits();
	error();
}

scan_delimiter (p, r)
	ptr		p;
	bool	r;
{
	if (r)
		scan_twenty_seven_bit_int();
	else {
		get_nbrx_token();
		switch (cur_cmd) 
		{
		case LETTER:
		case OTHER_CHAR:
			cur_val = del_code(cur_chr);
			break;

		case DELIM_NUM:
			scan_twenty_seven_bit_int();
			break;

		default:
			cur_val = -1;
			break;
		}
	}
	if (cur_val < 0) {
		print_err("Missing delimiter (. inserted)");
		help_delimiter();
		back_error();
		cur_val = 0;
	}
	small_fam(p) = (cur_val / 04000000) % 16;
	small_char(p) = qi((cur_val / 010000) % 256);
	large_fam(p) = (cur_val / 256) % 16;
	large_char(p) = qi(cur_val % 256);
}

math_radical ()
{
	tail_append(get_node(RADICAL_NOAD_SIZE));
	type(tail) = RADICAL_NOAD;
	subtype(tail) = NORMAL;
	mem[nucleus(tail)].hh = empty_field;
	mem[supscr(tail)].hh = empty_field;
	mem[subscr(tail)].hh = empty_field;
	scan_delimiter(left_delimiter(tail), TRUE);
	scan_math(nucleus(tail));
}

math_ac ()
{
	if (cur_cmd == ACCENT) {
		print_err("Please use ");
		print_esc("mathaccent");
		print(" for accents in math mode");
		help_math_accent();
		error();
	}
	tail_append(get_node(ACCENT_NOAD_SIZE));
	type(tail) = ACCENT_NOAD;
	subtype(tail) = NORMAL;
	mem[nucleus(tail)].hh = empty_field;
	mem[subscr(tail)].hh = empty_field;
	mem[supscr(tail)].hh = empty_field;
	math_type(accent_chr(tail)) = MATH_CHAR;
	scan_fifteen_bit_int();
	character(accent_chr(tail)) = qi(cur_val % 256);
	if (cur_val >= VAR_CODE && fam_in_range())
		fam(accent_chr(tail)) = cur_fam;
	else fam(accent_chr(tail)) = (cur_val / 256) % 16;
	scan_math(nucleus(tail));
}

append_choices ()
{
	tail_append(new_choice());
	incr(save_ptr);
	saved(-1) = 0;
	scan_left_brace();
	push_math(MATH_CHOICE_GROUP);
}

build_choices ()
{
	ptr		p;

	unsave();
	p = fin_mlist(NULL);
	switch ((int) saved(-1))
	{
	case 0:
		display_mlist(tail) = p;
		break;

	case 1:
		text_mlist(tail) = p;
		break;

	case 2:
		script_mlist(tail) = p;
		break;

	case 3:
		script_script_mlist(tail) = p;
		decr(save_ptr);
		return;
	}
	incr(saved(-1));
	scan_left_brace();
	push_math(MATH_CHOICE_GROUP);
}

sub_sup ()
{
	ptr		p = NULL;
	short	t = EMPTY;

	if (tail != head && scripts_allowed(tail)) {
		p = supscr(tail) + cur_cmd - SUP_MARK;
		t = math_type(p);
	}
	if (p == NULL || t != EMPTY) {
		tail_append(new_noad());
		p = supscr(tail) + cur_cmd - SUP_MARK;
		if (t != EMPTY) {
			if (cur_cmd == SUP_MARK) {
				print_err("Double superscript");
				help_double_sup();
			} else {
				print_err("Double subscript");
				help_double_sub();
			}
			error();
		}
	}
	scan_math(p);
}

math_fraction ()
{
	int		c;

	c = cur_chr;
	if (incompleat_noad != NULL) {
		if (c >= DELIMITED_CODE) {
			scan_delimiter(garbage, FALSE);
			scan_delimiter(garbage, FALSE);
		}
		if (c % DELIMITED_CODE == ABOVE_CODE)
			scan_normal_dimen();
		print_err("Ambiguous; you need another { and }");
		help_fraction();
		error();
	} else {
		incompleat_noad = get_node(FRACTION_NOAD_SIZE);
		type(incompleat_noad) = FRACTION_NOAD;
		subtype(incompleat_noad) = NORMAL;
		math_type(numerator(incompleat_noad)) = SUB_MLIST;
		info(numerator(incompleat_noad)) = link(head);
		mem[denominator(incompleat_noad)].hh = empty_field;
		mem[left_delimiter(incompleat_noad)].qqqq = null_delimiter;
		mem[right_delimiter(incompleat_noad)].qqqq = null_delimiter;
		link(head) = NULL;
		tail = head;
		if (c >= DELIMITED_CODE) {
			scan_delimiter(left_delimiter(incompleat_noad), FALSE);
			scan_delimiter(right_delimiter(incompleat_noad), FALSE);
		}
		switch (c % DELIMITED_CODE)
		{
		case ABOVE_CODE:
			scan_normal_dimen();
			thickness(incompleat_noad) = cur_val;
			break;

		case OVER_CODE:
			thickness(incompleat_noad) = DEFAULT_CODE;
			break;

		case ATOP_CODE:
			thickness(incompleat_noad) = 0;
			break;
		}
	}
}

ptr
fin_mlist (p)
	ptr		p;
{
	ptr		q;

	if (incompleat_noad != NULL) {
		math_type(denominator(incompleat_noad)) = SUB_MLIST;
		info(denominator(incompleat_noad)) = link(head);
		if (p == NULL)
			q = incompleat_noad;
		else {
			q = info(numerator(incompleat_noad));
			if (type(q) != LEFT_NOAD)
				confusion("right");
			info(numerator(incompleat_noad)) = link(q);
			link(q) = incompleat_noad;
			link(incompleat_noad) = p;
		}
	} else {
		link(tail) = p;
		q = link(head);
	}
	pop_nest();
	return q;
}

math_left_right ()
{
	ptr		p;
	int		t;

	t = cur_chr;
	if (t == RIGHT_NOAD && cur_group != MATH_LEFT_GROUP) {
		if (cur_group == MATH_SHIFT_GROUP) {
			scan_delimiter(garbage, FALSE);
			print_err("Extra ");
			print_esc("right");
			help_xtra_right();
			error();
		} else
			off_save();
	} else {
		p = new_noad();
		type(p) = t;
		scan_delimiter(delimiter(p), FALSE);
		if (t == LEFT_NOAD) {
			push_math(MATH_LEFT_GROUP);
			link(head) = p;
			tail = p;
		} else {
			p = fin_mlist(p);
			unsave();
			tail_append(new_noad());
			type(tail) = INNER_NOAD;
			math_type(nucleus(tail)) = SUB_MLIST;
			info(nucleus(tail)) = p;
		}
	}
}

after_math ()
{
	ptr		a;
	ptr		b;
	scal		d;
	scal		e;
	bool		l;
	int		m;
	ptr		p;
	scal		q;
	ptr		r;
	scal		s;
	scal		t;
	scal		w;
	scal		z;
	int		g1;
	int		g2;
	bool		danger;

	danger = FALSE;
	if (font_params[fam_fnt(2 + TEXT_SIZE)] < TOTAL_MATHSY_PARAMS ||
		font_params[fam_fnt(2 + SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS ||
		font_params[fam_fnt(2 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHSY_PARAMS) {
		print_err("Math formula deleted: Insufficient symbol fonts");
		help_math_sy();
		error();
		flush_math();
		danger = TRUE;
	} else if (font_params[fam_fnt(3 + TEXT_SIZE)] < TOTAL_MATHEX_PARAMS ||
		font_params[fam_fnt(3 + SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS ||
		font_params[fam_fnt(3 + SCRIPT_SCRIPT_SIZE)] < TOTAL_MATHEX_PARAMS) {
		print_err("Math formula deleted: Insufficient extension fonts");
		help_math_ex();
		error();
		flush_math();
		danger = TRUE;
	}
	m = mode;
	l = FALSE;
	p = fin_mlist(NULL);
	if (mode == -m) {
		cur_mlist = p;
		cur_style = TEXT_STYLE;
		mlist_penalties = FALSE;
		mlist_to_hlist();
		a = hpack(link(temp_head), NATURAL);
		unsave();
		decr(save_ptr);
		if (saved(0) == 1)
			l = TRUE;
		if (danger)
			flush_math();
		m = mode;
		p = fin_mlist(NULL);
	} else
		a = NULL;
	if (m < 0) {
#ifdef VORTEX
		close_math_node();
#endif
		tail_append(new_math(math_surround, BEFORE));
		cur_mlist = p;
		cur_style = TEXT_STYLE;
		mlist_penalties = (mode > 0);
		mlist_to_hlist();
		link(tail) = link(temp_head);
		while (link(tail) != NULL)
			tail = link(tail);
		tail_append(new_math(math_surround, AFTER));
		space_factor = 1000;
		unsave();
	} else {
		get_x_token();
		if (cur_cmd != MATH_SHIFT) {
			print_err("Display math should end with $$");
			help_doldol();
			back_error();
		}
#ifdef VORTEX
		close_display_node();
#endif
		cur_mlist = p;
		cur_style = DISPLAY_STYLE;
		mlist_penalties = FALSE;
		mlist_to_hlist();
		p = link(temp_head);
		adjust_tail = adjust_head;
		b = hpack(p, NATURAL);
		t = adjust_tail;
		adjust_tail = NULL;
		w = width(b);
		z = display_width;
		s = display_indent;
		if (a == NULL || danger)
			e = q = 0;
		else {
			e = width(a);
			q = e + math_quad(TEXT_SIZE);
		}
		if (w + q > z) {
			if (e != 0 &&
				(w - total_shrink[NORMAL] + q <= z ||
				total_shrink[FIL] != 0 ||
				total_shrink[FILL] != 0 ||
				total_shrink[FILLL] != 0)) {
				free_node(b, BOX_NODE_SIZE);
				b = hpack(p, z - q, EXACTLY);
			} else {
				e = 0;
				if (w > z) {
					free_node(b, BOX_NODE_SIZE);
					b = hpack(p, z, EXACTLY);
				}
			}
			w = width(b);
		}
		d = half(z - w);
		if (e > 0 && d < 2 * e) {
			d = half(z - w - e);
			if (p != NULL && type(p) == GLUE_NODE)
				d = 0;
		}
		tail_append(new_penalty(pre_display_penalty));
		if (d + s <= pre_display_size || l) {
			g1 = ABOVE_DISPLAY_SKIP_CODE;
			g2 = BELOW_DISPLAY_SKIP_CODE;
		} else {
			g1 = ABOVE_DISPLAY_SHORT_SKIP_CODE;
			g2 = BELOW_DISPLAY_SHORT_SKIP_CODE;
		}
		if (l && e == 0) {
			shift_amount(a) = s;
			append_to_vlist(a);
			tail_append(new_penalty(INF_PENALTY));
		} else
			tail_append(new_param_glue(g1));
		if (e != 0) {
			r = new_kern(z - w - e - d);
			if (l) {
				link(a) = r;
				link(r) = b;
				b = a;
				d = 0;
			} else {
				link(b) = r;
				link(r) = a;
			}
			b = hpack(b, NATURAL);
		}
		shift_amount(b) = s + d;
		append_to_vlist(b);
		if (t != adjust_head) {
			link(tail) = link(adjust_head);
			tail = t;
		}
		if (a != NULL && e == 0 && !l) {
			tail_append(new_penalty(INF_PENALTY));
			shift_amount(a) = s + z - width(a);
			append_to_vlist(a);
			tail_append(new_penalty(post_display_penalty));
		} else {
			tail_append(new_penalty(post_display_penalty));
			tail_append(new_param_glue(g2));
		}
		resume_after_display();
	}
#ifdef VORTEX
	in_math = FALSE;
#endif
}

resume_after_display ()
{
	if (cur_group != MATH_SHIFT_GROUP)
		confusion("display");
	unsave();
	prev_graf += 3;
	push_nest();
	mode = HMODE;
	space_factor = 1000;
	scan_optional_space();
	if (nest_ptr == 1) 
		build_page();
}


/*
 *	Help text
 */

help_math_accent ()
{
	help2("I'm changing \\accent to \\mathaccent here; wish me luck.",
	"(Accents are not the same in formulas as they are in text.)" );
}

help_math_sy ()
{
	help3("Sorry, but I can't typeset math unless \\textfont 2",
	"and \\scriptfont 2 and \\scriptscriptfont 2 have all",
	"the \\fontdimen values needed in math symbol fonts." );
}

help_math_ex ()
{
	help3("Sorry, but I can't typeset math unless \\textfont 3",
	"and \\scriptfont 3 and \\scriptscriptfont 3 have all",
	"the \\fontdimen values needed in math extension fonts." );
}

help_limits ()
{
	help1("I'm ignoring this misplaced \\limits or \\nolimits command.");
}

help_delimiter ()
{
	help6("I was expecting to see something like `(' or `\\{' or",
	"`\\}' here. If you typed, e.g., `{' instead of `\\{', you",
	"should probably delete the `{' by typing `1' now, so that",
	"braces don't get unbalanced. Otherwise just proceed.",
	"Acceptable delimiters are characters whose \\delcode is",
	"nonnegative, or you can use `\\delimiter <delimiter code>'.");
}

help_fraction ()
{
	help3("I'm ignoring this fraction specification, since I don't",
	"know whether a construction like `x \\over y \\over z'",
	"means `{x \\over y} \\over z' or `x \\over {y \\over z}'.");
}

help_xtra_right ()
{
	help1("I'm ignoring a \\right that had no matching \\left.");
}

help_doldol ()
{
	help2("The `$' that I just saw supposedly matches a previous `$$'.",
	"So I shall assume that you typed `$$' both times.");
}

help_double_sub ()
{
	help1("I treat `x_1_2' essentially like `x_1{}_2'.");
}

help_double_sup ()
{
	help1("I treat `x^1^2' essentially like `x^1{}^2'.");
}
