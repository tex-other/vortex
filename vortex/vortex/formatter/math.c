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
 *		math.c
 */

#include	"tex.h"
#include	"heap.h"
#include	"eq.h"
#include	"scan.h"
#include	"evalstack.h"
#include	"arith.h"
#include	"str.h"
#include	"box.h"
#include	"tfm.h"
#include	"print.h"
#include	"pack.h"
#include	"math.h"
#ifdef VORTEX
#include	"token.h"
#include	"tokenstack.h"
#include	"main.h"
#include	"allir.h"

extern struct _char	*begin_of_cs_token;
#endif

extern twoh	empty_field;

ptr
new_style (s)
	int		s;
{
	ptr		p;

	p = get_node(STYLE_NODE_SIZE);
	type(p) = STYLE_NODE;
	subtype(p) = s;
	width(p)= 0;
	depth(p) = 0;

	return p;
}

ptr
new_choice ()
{
	ptr		p;

	p = get_node(STYLE_NODE_SIZE);
	type(p) = CHOICE_NODE;
	subtype(p) = 0;
	display_mlist(p) = NULL;
	text_mlist(p) = NULL;
	script_mlist(p) = NULL;
	script_script_mlist(p) = NULL;

	return p;
}

ptr
new_noad ()
{
	ptr		p;

	p = get_node(NOAD_SIZE);
	type(p) = ORD_NOAD;
	subtype(p) = NORMAL;
#ifdef VORTEX
	if (state == TOKEN_LIST)
		math_esc(p) = ir_char(last_loc);
	else
		math_esc(p) = NIL;
#endif
	mem[nucleus(p)].hh = empty_field;
	mem[subscr(p)].hh = empty_field;
	mem[supscr(p)].hh = empty_field;

	return p;
}

print_fam_and_char (p)
	ptr		p;
{
	print_esc("fam");
	print_int(fam(p));
	print_char(' ');
	print_ASCII(qo(character(p)));
}

print_delimiter (p)
	ptr		p;
{
	val		a;

	a = small_fam(p) * 256 + qo(small_char(p));
	a = a * 0x1000 + large_fam(p) * 256 + qo(large_char(p));
	if (a < 0)
		print_val(a);
	else print_hex(a);
}

print_subsidiary_data (p, c)
	ptr		p;
	ascii	c;
{
	if (cur_length() >= depth_threshold) {
		if (math_type(p) != EMPTY)
			print(" []");
		return;
	}
	append_char(c);
	temp_ptr = p;
	switch (math_type(p))
	{
	case MATH_CHAR:
		print_ln();
		print_current_string();
		print_fam_and_char(p);
		break;
	
	case SUB_BOX:
		show_info();
		break;
	
	case SUB_MLIST:
		if (info(p) == NULL) {
			print_ln();
			print_current_string();
			print("{}");
		} else show_info();
		break;
	}
	flush_char();
}

print_style (c)
	int	  c;
{
	switch (c / 2) 
	{
	case 0:
		print_esc("displaystyle");
		break;

	case 1:
		print_esc("textstyle");
		break;

	case 2:
		print_esc("scriptstyle");
		break;

	case 3:
		print_esc("scriptscriptstyle");
		break;

	default:
		print("Unknown style!");
		break;
	}
}

print_size (s)
	int		s;
{
	if (s == 0)
		print_esc("textfont");
	else if (s == SCRIPT_SIZE)
		print_esc("scriptfont");
	else print_esc("scriptscriptfont");
}

show_normal_noad (p)
	ptr		p;
{
	switch (type(p)) 
	{
	case ORD_NOAD:
		print_esc("mathord");
		break;

	case OP_NOAD:
		print_esc("mathop");
		break;

	case BIN_NOAD:
		print_esc("mathbin");
		break;

	case REL_NOAD:
		print_esc("mathrel");
		break;

	case OPEN_NOAD:
		print_esc("mathopen");
		break;

	case CLOSE_NOAD:	
		print_esc("mathclose");
		break;

	case PUNCT_NOAD:
		print_esc("mathpunct");
		break;

	case INNER_NOAD:
		print_esc("mathinner");
		break;

	case OVER_NOAD:
		print_esc("overline");
		break;

	case UNDER_NOAD:
		print_esc("underline");
		break;

	case VCENTER_NOAD:
		print_esc("vcenter");
		break;

	case RADICAL_NOAD:
		print_esc("radical");
		print_delimiter(left_delimiter(p));
		break;

	case ACCENT_NOAD:
		print_esc("accent");
		print_fam_and_char(accent_chr(p));
		break;

	case LEFT_NOAD:
		print_esc("left");
		print_delimiter(nucleus(p));
		break;

	case RIGHT_NOAD:
		print_esc("right");
		print_delimiter(nucleus(p));
		break;

	}
	if (subtype(p) != NORMAL)
		if (subtype(p) == LIMITS)
			print_esc("limits");
		else print_esc("nolimits");
	if (type(p) < LEFT_NOAD)
		print_subsidiary_data(nucleus(p), '.');
	print_subsidiary_data(supscr(p), '^');
	print_subsidiary_data(subscr(p), '_');
}

show_fraction_noad (p)
	ptr		p;
{
	print_esc("fraction, thickness");
	if (thickness(p) == DEFAULT_CODE)
		print("= default");
	else print_scaled(thickness(p));
	if (small_fam(left_delimiter(p)) != 0 ||
		small_char(left_delimiter(p)) != MIN_QUARTERWORD ||
		large_fam(left_delimiter(p)) != 0 ||
		large_char(left_delimiter(p)) != MIN_QUARTERWORD) {
		print(", left-delimiter ");
		print_delimiter(left_delimiter(p));
	}
	if (small_fam(right_delimiter(p)) != 0 ||
		small_char(right_delimiter(p)) != MIN_QUARTERWORD ||
		large_fam(right_delimiter(p)) != 0 ||
		large_char(right_delimiter(p)) != MIN_QUARTERWORD) {
		print(", right-delimiter ");
		print_delimiter(right_delimiter(p));
	}
	print_subsidiary_data(numerator(p), '\\');
	print_subsidiary_data(denominator(p), '/');
}

show_choice_node (p)
	ptr 	p;
{
	print_esc("mathchoice");
	append_char('D');
	show_node_list(display_mlist(p));
	flush_char();
	append_char('T');
	show_node_list(text_mlist(p));
	flush_char();
	append_char('S');
	show_node_list(script_mlist(p));
	flush_char();
	append_char('s');
	show_node_list(script_script_mlist(p));
	flush_char();
}

ptr
fraction_rule (t)
	scal	t;
{
	ptr		p;

	p = new_rule();
	height(p) = t;
	depth(p) = 0;

	return p;
}

ptr
overbar (b, k, t)
	ptr		b;
	scal	k;
	scal	t;
{
	ptr		p;
	ptr		q;

	p = new_kern(k);
	link(p) = b;
	q = fraction_rule(t);
	link(q) = p;
	p = new_kern(t);
	link(p) = q;
	return (vpack(p, NATURAL));
}

ptr
var_delimiter (d, s, v)
	ptr		d;
	int		s;
	scal		v;
{
	ptr		b;
	qword		c;
	fnt		f;
	fnt		g;
	int		m;
	int		n;
	fourq		q;
	fourq		r;
	scal		u;
	scal		w;
	qword		x;
	qword		y;
	int		z;
	byte		hd;
	bool		large_attempt;

	f = NULL_FONT;
	w = 0;
	large_attempt = FALSE;
	z = small_fam(d);
	x = small_char(d);
	loop {
		if (z != 0 || x != MIN_QUARTERWORD) {
			z = z + s + 16;
			do {
				z = z - 16;
				g = fam_fnt(z);
				if (g != NULL_FONT) {
					y = x;
			contin:
					if (qo(y) >= font_bc[g] && qo(y) <= font_ec[g]) {
						q = char_info(g, y);
						if (char_exists(q)) {
							if (char_tag(q) == EXT_TAG)  {
								f = g;
								c = y;
								goto found;
							}
							hd = height_depth(q);
							u = char_height(g, hd) + char_depth(g, hd);
							if (u > w) {
								f = g;
								c = y;
								w = u;
								if (u >= v)
									goto found;
							}
							if (char_tag(q) == LIST_TAG) {
								y = rem_byte(q);
								goto contin;
							}
						}
					}
				}
			} while (z >= 16);
		}
		if (large_attempt)
			goto found;
		large_attempt = TRUE;
		z = large_fam(d);
		x = large_char(d);
	}

found:
	if (f != NULL_FONT) {
		if (char_tag(q) == EXT_TAG) {
			b = new_null_box();
			type(b) = VLIST_NODE;
			r = font_info[exten_base[f] + rem_byte(q)].qqqq;
			c = ext_rep(r);
			u = height_plus_depth(f, c);
			w = 0;
			q = char_info(f, c);
			width(b) = char_width(f, q) + char_italic(f, q);
			c = ext_bot(r); 
			if (c != MIN_QUARTERWORD)
				w += height_plus_depth(f, c);
			c = ext_mid(r);
			if (c != MIN_QUARTERWORD)
				w += height_plus_depth(f, c);
			c = ext_top(r);
			if (c != MIN_QUARTERWORD)
				w += height_plus_depth(f, c);
			n = 0;
			if (u > 0) {
				while (w < v) {
					w = w + u;
					incr(n);
					if (ext_mid(r) != MIN_QUARTERWORD)
						w = w + u;
				}
			}
			c = ext_bot(r);
			if (c != MIN_QUARTERWORD)
				stack_into_box(b, f, c);
			c = ext_rep(r);
			for (m = 1; m <= n; incr(m)) 
				stack_into_box(b, f, c);
			c = ext_mid(r);
			if (c != MIN_QUARTERWORD) {
				stack_into_box(b, f, c);
				c = ext_rep(r);
				for (m = 1; m <= n; incr(m))
					stack_into_box(b, f, c);
			}
			c = ext_top(r);
			if (c != MIN_QUARTERWORD)
				stack_into_box(b, f, c);
			depth(b) = w - height(b);
		} else
#ifdef VORTEX
		{
			/* ir_esc = math_esc(d); */
#endif
			b = char_box(f, c);
#ifdef VORTEX
		}
#endif
	} else {
		b = new_null_box();
		width(b) = null_delimiter_space;
	}
	shift_amount(b) = half(height(b) - depth(b)) - axis_height(s);
	return b;
}

ptr
char_box (f, c)
	fnt		f;
	qword		c;
{
	ptr		b;
	ptr		p;
	fourq		q;
	byte		hd;

	q = char_info(f, c);
	hd = height_depth(q);
	b = new_null_box();
	width(b) = char_width(f, q) + char_italic(f, q);
	height(b) = char_height(f, hd);
	depth(b) = char_depth(f, hd);
	p = get_avail();
	character(p) = c;
	font(p) = f;
	list_ptr(b) = p;
#ifdef VORTEX
	link_char_node(p);
#endif
	
	return b;
}

stack_into_box (b, f, c)
	ptr		b;
	fnt		f;
	qword		c;
{
	ptr		p;

#ifdef VORTEX
	/* ir_esc = math_esc(b);  */
#endif
	p = char_box(f, c);
	link(p) = list_ptr(b);
	list_ptr(b) = p;
	height(b) = height(p);
}

scal
height_plus_depth (f, c)
	fnt		f;
	qword	c;
{
	fourq	q;
	byte	hd;

	q = char_info(f, c);
	hd = height_depth(q);
	return (char_height(f, hd) + char_depth(f, hd));
}

ptr
rebox (b, w)
	ptr		b;
	scal	w;
{
	fnt		f;
	ptr		p;
	scal	v;

	if (width(b) != w && list_ptr(b) != NULL) {
		if (type(b) == VLIST_NODE)
			b = hpack(b, NATURAL);
		p = list_ptr(b);
		if (is_char_node(p) && link(p) == NULL) {
			f = font(p);
			v = char_width(f, char_info(f, character(p)));
			if (v != width(b)) 
				link(p) = new_kern(width(b) - v);
		}
		free_node(b, BOX_NODE_SIZE);
		b = new_glue(ss_glue);
		link(b) = p;
		while (link(p) != NULL)
			p = link(p);
		link(p) = new_glue(ss_glue); 
		return (hpack(b, w, EXACTLY));
	} else {
		width(b) = w;
		return b;
	}
}

#define	mu_mult(x) \
	nx_plus_y(n, x, xn_over_d(x, f, 0200000L))

ptr
math_glue (g, m)
	ptr		g;
	scal	m;
{
	scal	f;
	val		n;
	ptr		p;

	n = x_over_n(m, 0200000L);
	f = remainder;
	p = get_node(GLUE_SPEC_SIZE);
	width(p) = mu_mult(width(g));
	stretch_order(p) = stretch_order(g);
	if (stretch_order(p) == NORMAL)
		stretch(p) = mu_mult(stretch(g));
	else stretch(p) = stretch(g);
	shrink_order(p) = shrink_order(g);
	if (shrink_order(p) == NORMAL)
		shrink(p) = mu_mult(shrink(g));
	else shrink(p) = shrink(g);

	return p;
}

math_kern (p, m)
	ptr		p;
	scal	m;
{
	scal	f;
	val		n;

	if (subtype(p) == MU_GLUE) {
		n = x_over_n(m, 0200000L);
		f = remainder;
		width(p) = mu_mult(width(p));
		subtype(p) = NORMAL;
	}
}

flush_math ()
{
	flush_node_list(link(head));
	flush_node_list(incompleat_noad);
	link(head) = NULL;
	tail = head;
	incompleat_noad = NULL;
}
