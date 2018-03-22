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
 *		mlist-hlist.c
 */

#include	"tex.h"
#include	"heap.h"
#include	"arith.h"
#include	"scan.h"
#include	"eq.h"
#include	"box.h"
#include	"math.h"
#include	"pack.h"
#include	"tfm.h"
#include	"print.h"
#include	"error.h"
#include	"str.h"
#include	"mlist-hlist.h"

#ifdef VORTEX
#include	"main.h"
#endif

extern qword	cur_c;
extern fnt	cur_f;
extern fourq	cur_i;
extern scal	cur_mu;
extern int	cur_size;
extern int	cur_style;
extern ptr	cur_mlist;
extern bool	mlist_penalties;

ptr
clean_box (p, s)
	ptr		p;
	int		s;
{
	ptr		q;
	ptr		r;
	ptr		x;
	int		save_style;

	switch (math_type(p))
	{
	case MATH_CHAR:
		cur_mlist = new_noad();
		mem[nucleus(cur_mlist)] = mem[p];
		break;

	case SUB_BOX:
		q = info(p);
		goto found; 
		break;	
	
	case SUB_MLIST:
		cur_mlist = info(p);
		break;
	
	default:
		q = new_null_box();
		goto found;
		break;
	}
	save_style = cur_style;
	cur_style = s;
	mlist_penalties = FALSE;
	mlist_to_hlist();
	q = link(temp_head);
	cur_style = save_style;
	change_size_and_mu();

found:
	if (is_char_node(q) || q == NULL)
		x = hpack(q, NATURAL);	
	else if (link(q) == NULL &&
			type(q) <= VLIST_NODE &&
			shift_amount(q) == 0)
			x = q;
	else x = hpack(q, NATURAL);
	q = list_ptr(x);
	if (is_char_node(q)) {
		r = link(q);
		if (r != NULL &&
			link(r) == NULL &&	
			!is_char_node(r) &&
			type(r) == KERN_NODE) {
			free_node(r, SMALL_NODE_SIZE);
			link(q) = NULL;
		}
	}
	return x;
}

fetch (a)
	ptr		a;
{
	cur_c = character(a);
	cur_f = fam_fnt(fam(a) + cur_size);
	if (cur_f == NULL_FONT) {
		print_err("");
		print_size(cur_size);
		print_char(' ');
		print_int(fam(a));
		print(" is undefined (character ");
		print_ASCII(qo(cur_c));
		print_char(')');
		help_undefd_mathchar();
		error();
		cur_i = null_character;
		math_type(a) = EMPTY;
	} else {
		if (qo(cur_c) >= font_bc[cur_f] &&
			qo(cur_c) <= font_ec[cur_f])
			cur_i = char_info(cur_f, cur_c);
		else cur_i = null_character;
		if (!char_exists(cur_i)) {
			char_warning(cur_f, qo(cur_c));
			math_type(a) = EMPTY;
		}
	}
}

extern char	math_spacing[];

#define	choose_mlist(L) \
	{p = L(q); L(q) = NULL;}

mlist_to_hlist ()
{
	ptr		p;
	ptr		q;
	ptr		r;
	int		s;
	int		t;
	ptr		x;
	ptr		y;
	ptr		z;
	val		pen;
	scal		delta;
	scal		max_d;
	scal		max_h;
	ptr		mlist;
	int		style;
	int		r_type;
	bool		penalties;
	int		save_style;

	mlist = cur_mlist;
	penalties = mlist_penalties;
	style = cur_style;
	q = mlist;
	r = NULL;
	r_type = OP_NOAD;
	max_h = 0;
	max_d = 0;
	change_size_and_mu();

	while (q != NULL) {
reswitch:
		delta = 0;
		switch (type(q)) {
		case BIN_NOAD:	
			switch (r_type)	{
			case BIN_NOAD:
			case OP_NOAD:
			case REL_NOAD:
			case OPEN_NOAD:
			case PUNCT_NOAD:
			case LEFT_NOAD:
				type(q) = ORD_NOAD;
				goto reswitch;
				break;

			default:
				break;
			}
			break;

		case REL_NOAD:
		case CLOSE_NOAD:
		case PUNCT_NOAD:
		case RIGHT_NOAD:
			if (r_type == BIN_NOAD)
				type(r) = ORD_NOAD;
			if (type(q) == RIGHT_NOAD)
				goto done_with_noad;
			break;

		case LEFT_NOAD:
			goto done_with_noad;
			break;

		case FRACTION_NOAD:
			make_fraction(q);
			goto check_dimensions;
			break;
		
		case OP_NOAD:
			delta = make_op(q);
			if (subtype(q) == LIMITS)
				goto check_dimensions;
			break;

		case ORD_NOAD:
			make_ord(q);
			break;

		case OPEN_NOAD:
		case INNER_NOAD:
			break;
		
		case RADICAL_NOAD:
			make_radical(q);
			break;
		
		case OVER_NOAD:
			make_over(q);
			break;

		case UNDER_NOAD:
			make_under(q);
			break;
	
		case ACCENT_NOAD:
			make_math_accent(q);
			break;
		
		case VCENTER_NOAD:
			make_vcenter(q);
			break;
		
		case STYLE_NODE:
			cur_style = subtype(q);
			change_size_and_mu();
			goto done_with_node;
			break;
		
		case CHOICE_NODE:
			switch (cur_style / 2) {
			case 0:
				choose_mlist(display_mlist);
				break;

			case 1:
				choose_mlist(text_mlist);
				break;

			case 2:
				choose_mlist(script_mlist);
				break;

			case 3:
				choose_mlist(script_script_mlist);
				break;
			}
			flush_node_list(display_mlist(q));
			flush_node_list(text_mlist(q));
			flush_node_list(script_mlist(q));
			flush_node_list(script_script_mlist(q));
			type(q) = STYLE_NODE;
			subtype(q) = cur_style;
			width(q) = 0;
			depth(q) = 0;
			if (p != NULL) {
				z = link(q);
				link(q) = p;
				while (link(p) != NULL)
					p = link(p);
				link(p) = z;
			}
			goto done_with_node;
			break;

		case INS_NODE:
		case MARK_NODE:
		case ADJUST_NODE:
		case WHATSIT_NODE:
		case PENALTY_NODE:
		case DISC_NODE:
			goto done_with_node;
			break;
		
		case RULE_NODE:
			if (height(q) > max_h)
				max_h = height(q);
			if (depth(q) > max_d)
				max_d = depth(q);
			goto done_with_node;
			break;

		case GLUE_NODE:
			if (subtype(q) == MU_GLUE) {
				x = glue_ptr(q);
				y = math_glue(x, cur_mu);
				delete_glue_ref(x);
				glue_ptr(q) = y;
				subtype(q) = NORMAL;
			} else if (cur_size != TEXT_SIZE &&
				subtype(q) == COND_MATH_GLUE) {
				p = link(q);
				if (p != NULL &&
					(type(p) == GLUE_NODE ||
					type(p) == KERN_NODE)) {
					link(q) = link(p);
					link(p) = NULL;
					flush_node_list(p);
				}
			}
			goto done_with_node;
			break;
		
		case KERN_NODE:
			math_kern(q, cur_mu);
			goto done_with_node;
			break;
		
		default:
			confusion("mlist1");
		}

		switch (math_type(nucleus(q))) {
		case MATH_CHAR:
		case MATH_TEXT_CHAR:
			fetch(nucleus(q));
			if (char_exists(cur_i)) {
				delta = char_italic(cur_f, cur_i);
#ifdef VORTEX
				/* ir_esc = math_esc(q); */
#endif
				p = new_character(cur_f, qo(cur_c));
				if (math_type(nucleus(q)) == MATH_TEXT_CHAR &&
					space(cur_f) != 0)
					delta = 0;
				if (math_type(subscr(q)) == EMPTY && delta != 0) {
					link(p) = new_kern(delta);
					delta = 0;
				}
			} else
				p = NULL;
			break;
		
		case EMPTY:
			p = NULL;
			break;

		case SUB_BOX:
			p = info(nucleus(q));
			break;

		case SUB_MLIST:
			cur_mlist = info(nucleus(q));
			save_style = cur_style;
			mlist_penalties = FALSE;
			mlist_to_hlist();
			cur_style = save_style;
			change_size_and_mu();
			p = hpack(link(temp_head), NATURAL); 
			break;

		default:
			confusion("mlist2");
		}
		new_hlist(q) = p;
		if (math_type(subscr(q)) == EMPTY && 
			math_type(supscr(q)) == EMPTY)
			goto check_dimensions;
		make_scripts(q, delta);
		
check_dimensions:
		z = hpack((ptr)new_hlist(q), NATURAL);
		if (height(z) > max_h)
			max_h = height(z);
		if (depth(z) > max_d)
			max_d = depth(z);
		free_node(z, BOX_NODE_SIZE);

done_with_noad:
		r = q;
		r_type = type(r);

done_with_node:
		q = link(q);
	} 

	if (r_type == BIN_NOAD)
		type(r) = ORD_NOAD;
	p = temp_head;
	link(p) = NULL;
	q = mlist;
	r_type = 0;
	cur_style = style;
	change_size_and_mu();

	while (q != NULL) {
		t = ORD_NOAD;
		s = NOAD_SIZE;
		pen = INF_PENALTY;
		switch (type(q)) {
		case OP_NOAD:
		case OPEN_NOAD:
		case CLOSE_NOAD:
		case PUNCT_NOAD:
		case INNER_NOAD:
			t = type(q);
			break;
		
		case BIN_NOAD:
			t = BIN_NOAD;
			pen = bin_op_penalty;
			break;
		
		case REL_NOAD:
			t = REL_NOAD;
			pen = rel_penalty;
			break; 
		
		case ORD_NOAD:
		case VCENTER_NOAD:
		case OVER_NOAD:
		case UNDER_NOAD:
			break;

		case RADICAL_NOAD:
			s = RADICAL_NOAD_SIZE;
			break;

		case ACCENT_NOAD:
			s = ACCENT_NOAD_SIZE;
			break;

		case FRACTION_NOAD:
			t = INNER_NOAD;
			s = FRACTION_NOAD_SIZE;
			break;

		case LEFT_NOAD:
		case RIGHT_NOAD:
			t = make_left_right(q, style, max_d, max_h);
			break;
		
		case STYLE_NODE:
			cur_style = subtype(q);
			s = STYLE_NODE_SIZE;
			change_size_and_mu();
			goto delete_q;
			break;

		case WHATSIT_NODE:
		case PENALTY_NODE:
		case RULE_NODE:
		case DISC_NODE:
		case ADJUST_NODE:
		case INS_NODE:
		case MARK_NODE:
		case GLUE_NODE:
		case KERN_NODE:
			link(p) = q;
			p = q;
			q = link(q);
			link(p) = NULL;
			continue;
		
		default:
			confusion("mlist3");
		}

		if (r_type > 0) {
			switch (math_spacing[r_type * 8 + t + magic_offset]) {
			case '0':
				x = 0;
				break;

			case '1':
				if (cur_style < SCRIPT_STYLE)
					x = THIN_MU_SKIP_CODE;
				else x = 0;
				break;

			case '2':
				x = THIN_MU_SKIP_CODE;
				break;

			case '3':
				if (cur_style < SCRIPT_STYLE)
					x = MED_MU_SKIP_CODE;
				else x = 0;
				break;

			case '4':
				if (cur_style < SCRIPT_STYLE)
					x = THICK_MU_SKIP_CODE;
				else x = 0;
				break;

			default:
				confusion("mlist4");
				break;
			}
			if (x != 0) {
				y = math_glue(glue_par(x), cur_mu);
				z = new_glue(y);
				glue_ref_count(y) = NULL;
				link(p) = z;
				p = z;
				subtype(z) = x + 1;
			}
		}
		if (new_hlist(q) != NULL) {
			link(p) = new_hlist(q);
			do p = link(p);
			while (link(p) != NULL);
		}
		if (penalties && link(q) != NULL && pen < INF_PENALTY) {
			r_type = type(link(q));
			if (r_type != PENALTY_NODE && r_type != REL_NOAD) {
				z = new_penalty(pen);
				link(p) = z;
				p = z;
			}
		}
		r_type = t;

delete_q:
		r = q;
		q = link(q);
		free_node(r, s);
	}
}

make_over (q)
	ptr	q;
{
	info(nucleus(q)) =
		overbar(clean_box(nucleus(q), cramped_style(cur_style)),
				3 * default_rule_thickness, default_rule_thickness);
	math_type(nucleus(q)) = SUB_BOX;
}

make_under (q)
	ptr 	q;
{
	ptr		p;
	ptr		x;
	ptr		y;
	scal	delta;

	x = clean_box(nucleus(q), cur_style);
	p = new_kern(3 * default_rule_thickness);
	link(x) = p;
	link(p) = fraction_rule(default_rule_thickness);
	y = vpack(x, NATURAL);
	delta = height(y) + depth(y) + default_rule_thickness;
	height(y) = height(x);
	depth(y) = delta - height(y);
	info(nucleus(q)) = y;
	math_type(nucleus(q)) = SUB_BOX;
}

make_vcenter (q)
	ptr		q;
{
	ptr		v;
	scal	delta;

	v = info(nucleus(q));
	if (type(v) != VLIST_NODE)
		confusion("vcenter");
	delta = height(v) + depth(v);
	height(v) = axis_height(cur_size) + half(delta);
	depth(v) = delta - height(v);
}

make_radical (q)
	ptr		q;
{
	ptr		x;
	ptr		y;
	scal	clr;
	scal	delta;

	x = clean_box(nucleus(q), cramped_style(cur_style));
	if (cur_style < TEXT_STYLE)
		clr = default_rule_thickness + (abs(math_x_height(cur_size)) / 4);
	else {
		clr = default_rule_thickness;
		clr += (abs(clr) / 4);
	}
	y = var_delimiter(left_delimiter(q), cur_size,
						height(x) + depth(x) + clr + default_rule_thickness);
	delta = depth(y) - (height(x) + depth(x) + clr);
	if (delta > 0) clr += half(delta);
	shift_amount(y) = -(height(x) + clr);
	link(y)  = overbar(x, clr, height(y));
	info(nucleus(q)) = hpack(y, NATURAL);
	math_type(nucleus(q)) = SUB_BOX;
}

make_math_accent (q)
	ptr		q;
{
	int		a;
	qword		c;
	fnt		f;
	scal		h;
	fourq		i;
	ptr		p;
	scal		s;
	scal		w;
	ptr		x;
	ptr		y;
	scal		delta;

	fetch(accent_chr(q)); 
	if (char_exists(cur_i)) {
		i = cur_i;
		c = cur_c;
		f = cur_f;
		s = 0;
		if (math_type(nucleus(q)) == MATH_CHAR) {
			fetch(nucleus(q));
			if (char_tag(cur_i) == LIG_TAG) {
				a = lig_kern_start(cur_f, cur_i);
				do	{
					cur_i = font_info[a].qqqq;
					if (qo(next_char(cur_i)) == skew_char[cur_f]) {
						if (op_bit(cur_i) >= KERN_FLAG)
							s = char_kern(cur_f, cur_i);
						break;
					}
					incr(a);
				} while (stop_bit(cur_i) < STOP_FLAG);
			}
		}
		x = clean_box(nucleus(q), cramped_style(cur_style));
		w = width(x);
		h = height(x);
		loop {
			if (char_tag(i) != LIST_TAG)
				break;
			y = rem_byte(i);
			i = char_info(f, y);
			if (char_width(f, i) > w)
				break;
			c = y;
		}
		delta = (h < x_height(f) ? h : x_height(f));
		if ((math_type(supscr(q)) != EMPTY ||
			math_type(subscr(q)) != EMPTY) &&
			math_type(nucleus(q)) == MATH_CHAR) {
				flush_node_list(x);
				x = new_noad(); 
				mem[nucleus(x)] = mem[nucleus(q)];
				mem[supscr(x)] = mem[supscr(q)];
				mem[subscr(x)] = mem[subscr(q)];
				mem[supscr(q)].hh = empty_field;
				mem[subscr(q)].hh = empty_field;
				math_type(nucleus(q)) = SUB_MLIST;
				info(nucleus(q)) = x;
				x = clean_box(nucleus(q), cur_style);
				delta = delta + height(x) - h;
				h = height(x);
		}
#ifdef VORTEX
		/* ir_esc = math_esc(q); */
#endif
		y = char_box(f, c);
		shift_amount(y) = s + half(w - width(y));
		width(y) = 0;
		p = new_kern(-delta);
		link(p) = x;
		link(y) = p;
		y = vpack(y, NATURAL);
		width(y) = width(x);
		if (height(y) < h) {
			p = new_kern(h - height(y));
			link(p) = list_ptr(y);
			list_ptr(y) = p;
			height(y) = h;
		}
		info(nucleus(q)) = y;
		math_type(nucleus(q)) = SUB_BOX;
	}
}

make_fraction (q)
	ptr		q;
{
	ptr		p;
	ptr		v;
	ptr		x;
	ptr		y;
	ptr		z;
	scal	clr;
	scal	delta;
	scal	delta1;
	scal	delta2;
	scal	shift_up;
	scal	shift_down;

	if (thickness(q) == DEFAULT_CODE)
		thickness(q) = default_rule_thickness;
	x = clean_box(numerator(q), num_style(cur_style));
	z = clean_box(denominator(q), denom_style(cur_style));
	if (width(x) < width(z))
		x = rebox(x, width(z));
	else z = rebox(z, width(x));
	if (cur_style < TEXT_STYLE) {
		shift_up = num1(cur_size);
		shift_down = denom1(cur_size);
	} else {
		shift_down = denom2(cur_size);
		if (thickness(q) != 0)
			shift_up = num2(cur_size);
		else shift_up = num3(cur_size);
	}
	if (thickness(q) == 0) {
		if (cur_style < TEXT_STYLE)
			clr = 7 * default_rule_thickness;
		else clr = 3 * default_rule_thickness;
		delta = half(clr - ((shift_up - depth(x)) - (height(z) - shift_down)));
		if (delta > 0) {
			shift_up += delta;
			shift_down += delta;
		}
	} else {
		if (cur_style < TEXT_STYLE)
			clr = 3 * thickness(q);
		else clr = thickness(q);
		delta = half(thickness(q));
		delta1 = clr -
			((shift_up - depth(x)) - (axis_height(cur_size) + delta));
		delta2 = clr - 
			((axis_height(cur_size) - delta) - (height(z) - shift_down));
		if (delta1 > 0) shift_up += delta1;
		if (delta2 > 0) shift_down += delta2;
	}
	v = new_null_box();
	type(v) = VLIST_NODE;
	height(v) = shift_up + height(x);
	depth(v) = depth(z) + shift_down;
	width(v) = width(x);
	if (thickness(q) == 0) {
		p = new_kern((shift_up - depth(x)) - (height(z) - shift_down));
		link(p) = z;
	} else {
		y = fraction_rule(thickness(q));
		p = new_kern((axis_height(cur_size)-delta) - (height(z)-shift_down));
		link(y) = p;
		link(p) = z;
		p = new_kern((shift_up - depth(x)) - (axis_height(cur_size) + delta));
		link(p)  = y;
	}
	link(x) = p;
	list_ptr(v) = x;
	if (cur_style < TEXT_STYLE)
		delta = delim1(cur_size);
	else delta = delim2(cur_size);
	x = var_delimiter(left_delimiter(q), cur_size, delta);
	link(x) = v;
	z = var_delimiter(right_delimiter(q), cur_size, delta);
	link(v) = z;
	new_hlist(q) = hpack(x, NATURAL);
}

scal
make_op (q)
	ptr		q;
{
	ptr		p;
	ptr		v;
	ptr		x;
	ptr		y;
	ptr		z;
	scal	delta;
	scal	shift_up;
	scal	shift_down;

	if (subtype(q) == NORMAL && cur_style < TEXT_STYLE)
		subtype(q) = LIMITS;
	if (math_type(nucleus(q)) == MATH_CHAR) {
		fetch(nucleus(q));
		if (cur_style < TEXT_STYLE && char_tag(cur_i) == LIST_TAG) {
			cur_c = rem_byte(cur_i);
			character(nucleus(q)) = cur_c;
			cur_i = char_info(cur_f, cur_c);
		}
		delta = char_italic(cur_f, cur_i);
		x = clean_box(nucleus(q), cur_style);
		if (math_type(subscr(q)) != EMPTY && subtype(q) != LIMITS)
			width(x) -= delta;
		shift_amount(x) = half(height(x) - depth(x)) - axis_height(cur_size);
		math_type(nucleus(q)) = SUB_BOX;
		info(nucleus(q)) = x;
	} else delta = 0;
	if (subtype(q) == LIMITS) {
		x = clean_box(supscr(q), sup_style(cur_style));
		y = clean_box(nucleus(q), cur_style);
		z = clean_box(subscr(q), sub_style(cur_style));
		v = new_null_box();
		type(v) = VLIST_NODE;
		width(v) = width(y);
		if (width(x) > width(v))
			width(v) = width(x);
		if (width(z) > width(v))
			width(v) = width(z);
		x = rebox(x, width(v));
		y = rebox(y, width(v));
		z = rebox(z, width(v));
		shift_amount(x) = half(delta);
		shift_amount(z) = -shift_amount(x);
		height(v) = height(y);
		depth(v) = depth(y);
		if (math_type(supscr(q)) == EMPTY) {
			free_node(x, BOX_NODE_SIZE);
			list_ptr(v) = y;
		} else {
			shift_up = big_op_spacing3 - depth(x);
			if (shift_up < big_op_spacing1)
				shift_up = big_op_spacing1;
			p = new_kern(shift_up);
			link(p) = y;
			link(x) = p;
			p = new_kern(big_op_spacing5);
			link(p) = x;
			list_ptr(v) = p;
			height(v) += big_op_spacing5 + height(x) + depth(x) + shift_up;
		}
		if (math_type(subscr(q)) == EMPTY)
			free_node(z, BOX_NODE_SIZE);
		else {
			shift_down = big_op_spacing4 - height(z);
			if (shift_down < big_op_spacing2)
				shift_down = big_op_spacing2;
			p = new_kern(shift_down);
			link(y) = p;
			link(p) = z;
			p = new_kern(big_op_spacing5);
			link(z) = p;
			depth(v) += big_op_spacing5 + height(z) + depth(z) + shift_down;
		}
		new_hlist(q) = v;
	}
	return delta;
}

make_ord (q)
	ptr		q;
{
	int		a;
	ptr		p;

restart:
	if (math_type(subscr(q)) == EMPTY &&
		math_type(supscr(q)) == EMPTY &&
		math_type(nucleus(q)) == MATH_CHAR) {
		p = link(q);
		if (p != NULL &&
			type(p) >= ORD_NOAD &&
			type(p) <= PUNCT_NOAD &&
			math_type(nucleus(p)) == MATH_CHAR &&
			fam(nucleus(p)) == fam(nucleus(q))) {
			math_type(nucleus(q)) = MATH_TEXT_CHAR;
			fetch(nucleus(q));
			if (char_tag(cur_i) == LIG_TAG) {
				a = lig_kern_start(cur_f, cur_i);
				cur_c = character(nucleus(p));
				do	{
					cur_i = font_info[a].qqqq;
					if (next_char(cur_i) == cur_c) {
						if (op_bit(cur_i) >= KERN_FLAG) {
							p = new_kern(char_kern(cur_f, cur_i));
							link(p) = link(q);
							link(q) = p;
							return;
						} else {
							link(q) = link(p);
							math_type(nucleus(q)) = MATH_CHAR;
							character(nucleus(q)) = rem_byte(cur_i);
							mem[subscr(q)] = mem[subscr(p)];
							mem[supscr(q)] = mem[supscr(p)];
							free_node(p, NOAD_SIZE);
							goto restart;
						}
					}
					incr(a);
				} while (stop_bit(cur_i) < STOP_FLAG);
			}
		}
	}
}

make_scripts (q, delta)
	ptr		q;
	scal	delta;
{
	ptr		p;
	int		t;
	ptr		x;
	ptr		y;
	ptr		z;
	scal	clr;
	scal	shift_up;
	scal	shift_down;

	p = new_hlist(q);
	if (is_char_node(p)) {
		shift_up = 0;
		shift_down = 0;
	} else {
		z = hpack(p, NATURAL);
		if (cur_style < SCRIPT_STYLE)	
			t = SCRIPT_SIZE;
		else t = SCRIPT_SCRIPT_SIZE;
		shift_up = height(z) - sup_drop(t);
		shift_down = depth(z) + sub_drop(t);
		free_node(z, BOX_NODE_SIZE);
	}
	if (math_type(supscr(q)) == EMPTY) {
		x = clean_box(subscr(q), sub_style(cur_style));
		width(x) += script_space;
		if (shift_down < sub1(cur_size))
			shift_down = sub1(cur_size);
		clr = height(x) - (abs(math_x_height(cur_size) * 4) / 5);
		if (shift_down < clr)
			shift_down = clr;
		shift_amount(x) = shift_down;
	} else {
		x = clean_box(supscr(q), sup_style(cur_style));
		width(x) += script_space;
		if (odd(cur_style))
			clr = sup3(cur_size);
		else if (cur_style < TEXT_STYLE)
			clr = sup1(cur_size);
		else clr = sup2(cur_size);
		if (shift_up < clr)
			shift_up = clr;
		clr = depth(x) + (abs(math_x_height(cur_size)) / 4);
		if (shift_up < clr)
			shift_up = clr;
		if (math_type(subscr(q)) == EMPTY) 
			shift_amount(x) = -shift_up;
		else {
			y = clean_box(subscr(q), sub_style(cur_style));
			width(y) += script_space;
			if (shift_down < sub2(cur_size))
				shift_down = sub2(cur_size);
			clr = 4 * default_rule_thickness -
						((shift_up - depth(x)) - (height(y) - shift_down));
			if (clr > 0) {
				shift_down += clr;
				clr = (abs(math_x_height(cur_size) * 4) / 5) -
						(shift_up - depth(x));
				if (clr > 0) {
					shift_up += clr;
					shift_down -= clr;
				}
			}
			shift_amount(x) = delta;
			p = new_kern((shift_up - depth(x)) - (height(y) - shift_down));
			link(x) = p;
			link(p) = y;
			x = vpack(x, NATURAL);
			shift_amount(x) = shift_down;
		}
	}
	if (new_hlist(q) == NULL)
		new_hlist(q) = x;
	else {
		p = new_hlist(q);
		while (link(p) != NULL)
			p = link(p);
		link(p) = x;
	}
}

make_left_right (q, style, max_d, max_h)
	ptr		q;
	int		style;
	scal	max_d;
	scal	max_h;
{
	scal	delta;
	scal	delta1;
	scal	delta2;

	if (style  < SCRIPT_STYLE)
		cur_size = TEXT_SIZE;
	else cur_size = 16 * ((style - TEXT_STYLE) / 2);
	delta2 = max_d + axis_height(cur_size);
	delta1 = max_h + max_d - delta2;
	if (delta2 > delta1)
		delta1 = delta2;
	delta = (delta1 / 500) * delimiter_factor;
	delta2 = delta1 + delta1 - delimiter_shortfall;
	if (delta < delta2)
		delta = delta2;
	new_hlist(q) = var_delimiter(delimiter(q), cur_size, delta);
	return (type(q) - (LEFT_NOAD - OPEN_NOAD));
}

extern int	magic_offset;

/*
 *	Help text
 */

help_undefd_mathchar ()
{
	help4("Somewhere in the math formula just ended, you used the",
	"stated character from an undefined font family. For example,",
	"plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,",
	"and I'll try to forget that I needed that character.");
}
