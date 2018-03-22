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
 *		box.c
 */

#include	"tex.h"
#include	"texext.h"
#include	"arith.h"
#include	"heap.h"
#include	"char.h"
#include	"str.h"
#include	"eq.h"
#include	"hash.h"
#include	"tfm.h"
#include	"print.h"
#include	"math.h"
#include	"box.h"
#include	"token.h"
#include	"tokenlists.h"
       
#ifdef VORTEX
#include	"allir.h"
#include	"main.h"
#endif

extern val		depth_threshold;
extern val		breadth_max;
extern fnt		font_in_short_display;

ptr
new_null_box ()
{
	ptr		p;

	p = get_node(BOX_NODE_SIZE);
	type(p) = HLIST_NODE;
	subtype(p) = MIN_QUARTERWORD;
	width(p) = 0;
	depth(p) = 0;
	height(p) = 0;
	shift_amount(p) = 0;
	list_ptr(p) = NULL;
	glue_sign(p) = NORMAL;
	glue_order(p) = NORMAL;
	glue_set(p) = 0.0;

	return p;
}

ptr
new_rule ()
{
	ptr		 p;

	p = get_node(RULE_NODE_SIZE);
	type(p) = RULE_NODE;
	subtype(p) = 0;
	width(p) = NULL_FLAG;
	depth(p) = NULL_FLAG;
	height(p) = NULL_FLAG;

	return p;
}

ptr
new_ligature (f, c, q)
	qword	f;
	qword	c;
	ptr		q;
{
	ptr	 	p;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = LIGATURE_NODE;
	subtype(p) = 0;
	font(lig_char(p)) = f;
	character(lig_char(p)) = c;
	lig_ptr(p) = q;

	return p;
}

ptr
new_disc ()
{
	ptr		p;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = DISC_NODE;
	replace_count(p) = 0;
	pre_break(p) = NULL;
	post_break(p) = NULL;

	return p;
}

ptr
new_math (w, s)
	scal	w;
	int		s;
{
	ptr		p;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = MATH_NODE;
	subtype(p) = s;
	width(p) = w;

	return p;
}

ptr
new_spec (p)
	ptr		p;
{
	ptr		q;

	q = get_node(GLUE_SPEC_SIZE);
	mem[q] = mem[p];
	glue_ref_count(q) = NULL;
	width(q) = width(p);
	stretch(q) = stretch(p);
	shrink(q) = shrink(p);

	return q;
}

ptr
new_param_glue (n)
	int		n;
{
	ptr		p;
	ptr		q;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = GLUE_NODE;
	subtype(p) = n + 1;
	leader_ptr(p) = NULL;
	q = glue_par(n);
	glue_ptr(p) = q;
	incr(glue_ref_count(q));

	return p;
}

ptr
new_glue (q)
	ptr		q;
{
	ptr		p;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = GLUE_NODE;
	subtype(p) = NORMAL;
	leader_ptr(p) = NULL;
	glue_ptr(p) = q;
	incr(glue_ref_count(q));

	return p;
}

ptr
new_skip_param (n)
	int		n;
{
	ptr		p;

	temp_ptr = new_spec(glue_par(n));
	p = new_glue(temp_ptr);
	glue_ref_count(temp_ptr) = NULL;
	subtype(p) = n + 1;

	return p;
}

ptr
new_kern (w)
	scal	w;
{
	ptr		p;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = KERN_NODE;
	subtype(p) = NORMAL;
	width(p) = w;

	return p;
}

ptr
new_penalty (m)
	val		m;
{
	ptr		p;

	p = get_node(SMALL_NODE_SIZE);
	type(p) = PENALTY_NODE;
	subtype(p) = 0;
	penalty(p) = m;

	return p;
}

print_font_and_char (p)
	ptr		p;
{
	if (p > mem_end)
		print_esc("CLOBBERED.");
	else {
		if (font(p) < FONT_BASE || font(p) > FONT_MAX)
			print_char('*');
		else {
			print_esc("");
			print_str(font_id_text(font(p)));
			print_char(' ');
			print_ASCII(qo(character(p)));
		}
	}
}

print_mark (p)
	ptr		p;
{
	print_char('{');
	if (p < hi_mem_min || p > mem_end)
		print_esc("CLOBBERED.");
	else
		show_token_list(link(p), NULL, (val) MAX_PRINT_LINE - 10);
	print_char('}');
}

print_rule_dimen (d)
	scal	d;
{
	if (is_running(d))
		print_char('*');
	else print_scaled(d);
}

print_glue (d, o, s)
	scal	d;
	qword	o;
	char*	s;
{
	print_scaled(d);
	if (o < NORMAL || o > FILLL)
		print("foul");
	else if (o > NORMAL) {
		print("fil");
		while  (o >  FIL) {
			print_char('l');
			decr(o);
		}
	} else if (s)
		print(s);
}

print_spec (p, s)
	ptr		p;
	char*	s;
{
	if (p < MEM_MIN || p >= hi_mem_min)
		print_char('*');
	else {
		print_scaled(width(p));
		print(s);
		if (stretch(p) != 0) {
			print(" plus ");
			print_glue(stretch(p), stretch_order(p), s);
		}
		if (shrink(p) != 0) {
			print(" minus ");
			print_glue(shrink(p), shrink_order(p), s);
		}
	}
}

short_display (p)
	ptr		p;
{
	int		n;

	for (p; p > NULL; p = link(p)) {
		if (is_char_node(p) && p <= mem_end) {
			if (font(p) != font_in_short_display) {
				if (font(p) < FONT_BASE || font(p) > FONT_MAX) 
					print_char('*');
				else {
					print_esc("");
					print_str(font_id_text(font(p)));
				}
				print_char(' ');
				font_in_short_display = font(p);
			}
			print_ASCII(qo(character(p)));
		} else {
			switch (type(p))
			{
			case HLIST_NODE:
			case VLIST_NODE: 
			case INS_NODE:
			case WHATSIT_NODE:
			case MARK_NODE:
			case ADJUST_NODE:
			case UNSET_NODE:
				print("[]");
				break;

			case RULE_NODE:
				print_char('|');
				break;

			case GLUE_NODE:
				if (glue_ptr(p) != zero_glue)
					print_char(' ');
				break;

			case MATH_NODE:
				print_char('$');
				break;

			case LIGATURE_NODE:
				short_display(lig_ptr(p));
				break;

			case DISC_NODE:
				short_display(pre_break(p));
				short_display(post_break(p));
				n = replace_count(p);
				while (n > 0) {
					if (link(p) != NULL)
						p = link(p);
					decr(n);
				}
				break;

			default:
				break;
			}
		}
	}
}

show_box (p)
	ptr		p;
{
	depth_threshold = show_box_depth;
	breadth_max = show_box_breadth;
	if (breadth_max <= 0)
		breadth_max = 5;
	if (pool_ptr + depth_threshold >= POOL_SIZE)
		depth_threshold = POOL_SIZE - pool_ptr - 1;
	show_node_list(p);
	print_ln();
}

show_box1 (p)
	ptr		p;
{
	if (type(p) == HLIST_NODE)
		print_esc("h");
	else if (type(p) == VLIST_NODE)
		print_esc("v");
	else print_esc("unset");
	print("box(");
	print_scaled(height(p));
	print_char('+')	;
	print_scaled(depth(p));
	print(")x")	;
	print_scaled(width(p));
	if (type(p) == UNSET_NODE) {
		if (span_count(p) != MIN_QUARTERWORD) {
			print(" (");
			print_int(qo(span_count(p))+1);
			print(" columns)");
		}
		if (glue_stretch(p) != 0) {
			print(", stretch ");
			print_glue(glue_stretch(p), glue_order(p), "");
		}
		if (glue_shrink(p) != 0) {
			print(", shrink ");
			print_glue(glue_shrink(p), glue_sign(p), "");
		}
	} else {
		show_glue_set(p);
		if (shift_amount(p) != 0) {
			print(", shifted ");
			print_scaled(shift_amount(p));
		}
	}
	node_list_display(list_ptr(p));
}

show_glue_set (p)
	ptr		p;
{
	if (glue_set(p) != 0 && glue_sign(p) != NORMAL) {
		print(", glue set ");
		if (glue_sign(p) == SHRINKING)
			print("- ");
		if (abs(glue_set(p)) > 20000.0) {
			if (glue_set(p) > 0)
				print_char('>');
			else print("< -");
			print_glue(20000 * UNITY, glue_order(p), "");
		} else
			print_glue(round(glue_set(p) * UNITY), glue_order(p), "");
	}
}

show_rule (p)
	ptr		p;
{
	print_esc("rule(");
	print_rule_dimen(height(p));
	print_char('+');
	print_rule_dimen(depth(p));
	print(")x");
	print_rule_dimen(width(p));
}

show_insertion (p)
	ptr		p;
{
	print_esc("insert");
	print_int(qo(subtype(p)));
	print(", natural size ");
	print_scaled(height(p));
	print("; split(");
	print_spec(split_top_ptr(p), "");
	print_char(',');
	print_scaled(depth(p));
	print("); float cost ");
	print_val(float_cost(p));
	node_list_display(ins_ptr(p));
}

show_leaders (p)
	ptr		p;
{
	print_esc("");
	if (subtype(p) == C_LEADERS)
		print_char('c');
	else if (subtype(p) == X_LEADERS)
		print_char('x');
	print("leaders ");
	print_spec(glue_ptr(p), "");
	node_list_display(leader_ptr(p));
}

show_glue (p)
	ptr		p;
{
	if (subtype(p) >= A_LEADERS)
		show_leaders(p);
	else {
		print_esc("glue");
		if (subtype(p) != NORMAL) {
			print_char('(');
			if (subtype(p) < COND_MATH_GLUE)
				print_skip_param(subtype(p) - 1);
			else if (subtype(p) == COND_MATH_GLUE)
				print_esc("nonscript");
			else print_esc("mskip");
			print_char(')');
		}
		if (subtype(p) != COND_MATH_GLUE) {
			print_char(' ');
			if (subtype(p) < COND_MATH_GLUE)
				print_spec(glue_ptr(p), "");
			else print_spec(glue_ptr(p), "mu");
		}
	}
}

show_kern (p)
	ptr		p;
{
	if (subtype(p) != MU_GLUE) {
		print_esc("kern");
		if (subtype(p) != NORMAL)
			print_char(' ');
		print_scaled(width(p));
		if (subtype(p) == ACC_KERN)
			print(" (for accent)");}
	else {
		print_esc("mkern");
		print_scaled(width(p));
		print("mu");
	}
}

show_math (p)
	ptr		p;
{
	print_esc("math");
	if (subtype(p) == BEFORE)
		print("on");
	else print("off");
	if (width(p) != 0) {
		print(", surrounded ");
		print_scaled(width(p));
	}
}
	
show_ligature (p)
	ptr		p;
{
	print_font_and_char(lig_char(p));
	print(" (ligature ");
	font_in_short_display = font(lig_char(p));
	short_display(lig_ptr(p));
	print_char(')');
}

show_discretionary (p)
	ptr		p;
{
	print_esc("discretionary");
	if (replace_count(p) > 0) {
		print(" replacing ");
		print_int(replace_count(p));
	}
	node_list_display(pre_break(p));
	append_char('|');
	show_node_list(post_break(p));
	flush_char();
}

show_penalty (p)
	ptr		p;
{
	print_esc("penalty ");
	print_val(penalty(p));
}

show_mark (p)
	ptr		p;
{
	print_esc("mark");
	print_mark(mark_ptr(p));
}

show_adjust (p)
	ptr		p;
{
	print_esc("vadjust");
	node_list_display(adjust_ptr(p));
}

show_node_list (p)
	ptr 	p;
{
	int		n;

	if (cur_length() > depth_threshold) {
		if (p > NULL)
			print(" []");
		return;
	}
	n = 0;
	while (p > NULL) {
		print_ln();
		print_current_string();
		if (p > mem_end) {
			print("Bad link, display aborted.");
			return;
		}
		incr(n);
		if (n > breadth_max) {
			print("etc.");
			return;
		}
		if (is_char_node(p))
			print_font_and_char(p);
		else {
			switch (type(p))
			{
			case HLIST_NODE:
			case VLIST_NODE:
			case UNSET_NODE:
				show_box1(p);
				break;

			case RULE_NODE:
				show_rule(p);
				break;
			
			case INS_NODE:
				show_insertion(p);
				break;
			
			case WHATSIT_NODE:
				show_whatsit(p);
				break;
			
			case GLUE_NODE:
				show_glue(p);
				break;
			
			case KERN_NODE:
				show_kern(p);
				break;
			
			case MATH_NODE:
				show_math(p);
				break;
			
			case LIGATURE_NODE:
				show_ligature(p);
				break;

			case PENALTY_NODE:
				show_penalty(p);
				break;
		
			case DISC_NODE:
				show_discretionary(p);
				break;
			
			case MARK_NODE:
				show_mark(p);
				break;
			
			case ADJUST_NODE:
				show_adjust(p);
				break;
			
			case STYLE_NODE:
				print_style(subtype(p));
				break;
			
			case CHOICE_NODE:
				show_choice_node(p);
				break;

			case INNER_NOAD:
			case ORD_NOAD:
			case OP_NOAD:
			case BIN_NOAD:
			case REL_NOAD:
			case OPEN_NOAD:
			case CLOSE_NOAD:
			case PUNCT_NOAD:
			case RADICAL_NOAD:
			case OVER_NOAD:
			case UNDER_NOAD:
			case VCENTER_NOAD:
			case ACCENT_NOAD:
			case LEFT_NOAD:
			case RIGHT_NOAD:
				show_normal_noad(p);
				break;
			
			case FRACTION_NOAD:
				show_fraction_noad(p);
				break;

			default:
				print("Unknown node type!");
				break;
			}
		}
		p = link(p);
	}
}

show_info ()
{
	show_node_list(info(temp_ptr));
}

delete_glue_ref (p)
	ptr		p;
{
	if (glue_ref_count(p) == NULL)
		free_node(p, GLUE_SPEC_SIZE);
	else decr(glue_ref_count(p));
}

flush_node_list (p)
	ptr		p;
{
	ptr		q;

	while (p != NULL) {
		q = link(p);
		if (is_char_node(p)) {
			free_avail(p);
		} else {
			switch (type(p))
			{
			case HLIST_NODE:
			case VLIST_NODE:
			case UNSET_NODE:
				flush_node_list(list_ptr(p));
				free_node(p, BOX_NODE_SIZE);
				goto done;
				break;
					
			case RULE_NODE:
				free_node(p, RULE_NODE_SIZE);
				goto done;
				break;

			case INS_NODE:
				flush_node_list(ins_ptr(p));
				delete_glue_ref(split_top_ptr(p));
				free_node(p, INS_NODE_SIZE);
				goto done;
				break;

			case WHATSIT_NODE:
				free_whatsit(p);
				goto done;
				break;
			
			case GLUE_NODE:
				fast_delete_glue_ref(glue_ptr(p));
				if (leader_ptr(p) != NULL)
					flush_node_list(leader_ptr(p));
				break;

			case KERN_NODE:
			case MATH_NODE:
			case PENALTY_NODE:
				break;

			case LIGATURE_NODE:
				flush_node_list(lig_ptr(p));
				break;

			case MARK_NODE:
				delete_token_ref(mark_ptr(p));
				break;
			
			case DISC_NODE:
				flush_node_list(pre_break(p));
				flush_node_list(post_break(p));
				break;

			case ADJUST_NODE:
				flush_node_list(adjust_ptr(p));
				break;

			case STYLE_NODE:
				free_node(p, STYLE_NODE_SIZE);
				goto done;
				break;

			case CHOICE_NODE:
				flush_node_list(display_mlist(p));
				flush_node_list(text_mlist(p));
				flush_node_list(script_mlist(p));
				flush_node_list(script_script_mlist(p));
				free_node(p, STYLE_NODE);
				goto done;
				break;

			case ORD_NOAD:
			case OP_NOAD:
			case BIN_NOAD:
			case REL_NOAD:
			case OPEN_NOAD:
			case CLOSE_NOAD:
			case PUNCT_NOAD:
			case INNER_NOAD:
			case RADICAL_NOAD:
			case OVER_NOAD:
			case UNDER_NOAD:
			case VCENTER_NOAD:
			case ACCENT_NOAD:
				if (math_type(nucleus(p)) >= SUB_BOX)
					flush_node_list(info(nucleus(p)));
				if (math_type(supscr(p)) >= SUB_BOX)
					flush_node_list(info(supscr(p)));
				if (math_type(subscr(p)) >= SUB_BOX)
					flush_node_list(info(subscr(p)));
				if (type(p) == RADICAL_NOAD)
					free_node(p, RADICAL_NOAD_SIZE);
				else if (type(p) == ACCENT_NOAD)
					free_node(p, ACCENT_NOAD_SIZE);
				else free_node(p, NOAD_SIZE);
				goto done;
				break;
			
			case LEFT_NOAD:
			case RIGHT_NOAD:
				free_node(p, NOAD_SIZE);
				goto done;
				break;
			
			case FRACTION_NOAD:
				flush_node_list(info(numerator(p)));
				flush_node_list(info(denominator(p)));
				free_node(p, FRACTION_NOAD_SIZE);
				goto done;
				break;

			default:
				confusion("flushing");
				break;
			}
			free_node(p, SMALL_NODE_SIZE);
			done:;	
		}
		p = q;
	}
}

ptr
copy_node_list (p)
	ptr		p;
{
	ptr		h;
	ptr		q;
	ptr		r;
	int		words;

	h = get_avail();
	q = h;
	while (p != NULL) {
		words = 1;
		if (is_char_node(p)) {
			r = get_avail();
#ifdef VORTEX
			incr(words);
#endif
		} else {
			switch (type(p)) {
			case HLIST_NODE:
			case VLIST_NODE:
			case UNSET_NODE:
				r = get_node(BOX_NODE_SIZE);
				mem[r + 6] = mem[p + 6];
				mem[r + 5] = mem[p + 5];
				list_ptr(r) = copy_node_list(list_ptr(p));
				words = 5;
				break;
			
			case RULE_NODE:
				r = get_node(RULE_NODE_SIZE);
				words = RULE_NODE_SIZE;
				break;

			case INS_NODE:
				r = get_node(INS_NODE_SIZE);
				mem[r + 4] = mem[p + 4];
				add_glue_ref(split_top_ptr(p));
				ins_ptr(r) = copy_node_list(ins_ptr(p));
				words = INS_NODE_SIZE - 1;
				break;

			case WHATSIT_NODE:
				r = copy_whatsit(p);
				break;
			
			case GLUE_NODE:
				r = get_node(SMALL_NODE_SIZE);
				add_glue_ref(glue_ptr(p));
				glue_ptr(r) = glue_ptr(p);
				leader_ptr(r) = copy_node_list(leader_ptr(p));
				break;
			
			case KERN_NODE:
			case MATH_NODE:
			case PENALTY_NODE:
				r = get_node(SMALL_NODE_SIZE);
				words = SMALL_NODE_SIZE;
				break;
			
			case LIGATURE_NODE:
				r = get_node(SMALL_NODE_SIZE);
				mem[lig_char(r)] = mem[lig_char(p)];
				lig_ptr(r) = copy_node_list(lig_ptr(p));
				break;
				
			case DISC_NODE:
				r = get_node(SMALL_NODE_SIZE);
				pre_break(r) = copy_node_list(pre_break(p));
				post_break(r) = copy_node_list(post_break(p));
				break;

			case MARK_NODE:
				r = get_node(SMALL_NODE_SIZE);
				add_token_ref(mark_ptr(p));
				words = SMALL_NODE_SIZE;
				break;

			case ADJUST_NODE:
				r = get_node(SMALL_NODE_SIZE);
				adjust_ptr(r) = copy_node_list(adjust_ptr(p));
				break;
			
			default:
				confusion("copying");
				break;
			}
		}
		while (words > 0) {
			decr(words);
			mem[r + words] = mem[p + words];
		}
		link(q) = r;
		q = r;
		p = link(p);
	}
	link(q) = NULL;
	q = link(h);
	free_avail(h);
	return q;
}
