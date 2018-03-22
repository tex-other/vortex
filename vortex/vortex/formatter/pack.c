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
 *		pack.c
 */

#include "tex.h"
#include "heap.h"
#include "arith.h"
#include "scan.h"
#include "tokenstack.h"
#include "eq.h"
#include "eqstack.h"
#include "evalstack.h"
#include "box.h"
#include "tfm.h"
#include "dvi.h"
#include "print.h"
#include "error.h"
#include "pack.h"

extern ptr	adjust_tail;
extern val	pack_begin_line;
extern scal	total_shrink[];
extern scal	total_stretch[];

#define	clr_dimens() \
	{d = x = 0; \
	total_stretch[FILLL] = 0; \
	total_stretch[FILL] = 0; \
	total_stretch[FIL] = 0; \
	total_stretch[NORMAL] = 0; \
	total_shrink[FILLL] = 0; \
	total_shrink[FILL] = 0; \
	total_shrink[FIL] = 0; \
	total_shrink[NORMAL] = 0;}

ptr
hpack (p, w, m)
	ptr		p;
	scal		w;
	int		m;
{
	int		b;
	scal		d;
	fnt		f;
	ptr		g;
	scal		h;
	fourq		i;
	gord		o;
	ptr		q;
	ptr		r;
	scal		s;
	scal		x;
	byte		hd;

	r = get_node(BOX_NODE_SIZE);
	type(r) = HLIST_NODE;
	subtype(r) = MIN_QUARTERWORD;
	shift_amount(r) = 0;
	q = r + LIST_OFFSET;
	link(q) = p;
	h = 0;
	clr_dimens();
	while (p != NULL) {
reswitch:
		while (is_char_node(p)) {
			f = font(p);
			i = char_info(f, character(p));
			hd = height_depth(i);
			x += char_width(f, i);
			s = char_height(f, hd);
			if (s > h)
				h = s;
			s = char_depth(f, hd);
			if (s > d)
				d = s;
			p = link(p);
		}
		if (p != NULL) {
			switch (type(p)) {
			case HLIST_NODE:
			case VLIST_NODE:
			case RULE_NODE:
			case UNSET_NODE:
				x += width(p);
				if (type(p) >= RULE_NODE)
					s = 0;
				else s = shift_amount(p);
				if (height(p) - s > h)
					h = height(p) - s;
				if (depth(p) + s > d)
					d = depth(p) + s;
				break;

			case INS_NODE:
			case MARK_NODE:
			case ADJUST_NODE:
				if (adjust_tail != NULL) {
					while (link(q) != p)
						q = link(q);
					if (type(p) == ADJUST_NODE) {
						link(adjust_tail) = adjust_ptr(p);
						while (link(adjust_tail) != NULL)
							adjust_tail = link(adjust_tail);
						p = link(p);
						free_node(link(q), SMALL_NODE_SIZE);
					} else {
						link(adjust_tail) = p;
						adjust_tail = p;
						p = link(p);
					}
					link(q) = p;
					p = q;
				}
				break;

			case WHATSIT_NODE:
				break;
			
			case GLUE_NODE:
				g = glue_ptr(p);
				x += width(g);
				o = stretch_order(g);
				total_stretch[o] += stretch(g);
				o = shrink_order(g);
				total_shrink[o] += shrink(g);
				if (subtype(p) >= A_LEADERS) {
					g = leader_ptr(p);
					if (height(g) > h)
						h = height(g);
					if (depth(g) > d)
						d = depth(g);
				}
				break;
			
			case KERN_NODE:
			case MATH_NODE:
				x += width(p);
				break;
			
			case LIGATURE_NODE:
				make_char_from_lig();
				goto reswitch;
				break;

			default:
				break;
			}
			p = link(p);
		}
	}
	if (adjust_tail != NULL)
		link(adjust_tail) = NULL;
	height(r) = h;
	depth(r) = d;
	if (m == ADDITIONAL)
		w += x;
	width(r) = w;
	x = w - x;
	if (x == 0) {
		glue_sign(r) = NORMAL;
		glue_order(r) = NORMAL;
		glue_set(r) = 0.0;
		return r;
	} else if (x > 0) {
		get_stretch_order(); 
		glue_order(r) = o;
		glue_sign(r) = STRETCHING;
		if (total_stretch[o] != 0)
			glue_set(r) = (float) x / total_stretch[o];
		else {
			glue_sign(r) = NORMAL;
			glue_set(r) = 0.0;
		}
		if (hbadness < INF_BAD && o == NORMAL && list_ptr(r) != NULL) {
			b = badness(x, total_stretch[NORMAL]);
			if (b > hbadness) {
				print_ln();
				if (b > 100)
					print_nl("Underfull");
				else print_nl("Loose");
				print(" \\hbox (badness ");
				print_int(b);
				goto common_end;
			}
		}
		return r;
	} else {
		get_shrink_order();
		glue_order(r) = o;
		glue_sign(r) = SHRINKING;
		if (total_shrink[o] != 0)
			glue_set(r) = (float) -x / total_shrink[o];
		else {
			glue_sign(r) = NORMAL;
			glue_set(r) = 0.0;
		}
		if (total_shrink[o] < -x && o == NORMAL && list_ptr(r) != NULL) {
			glue_set(r) = 1.0;
			if (-x - total_shrink[NORMAL] > hfuzz || hbadness < 100) {
				if (overfull_rule > 0 && -x - total_shrink[NORMAL] > hfuzz) {
					while (link(q) != NULL)
						q = link(q);
					link(q) = new_rule();
					width(link(q)) = overfull_rule;
				}
				print_ln();
				print_nl("Overfull \\hbox ("); 
				print_scaled(-x - total_shrink[NORMAL]);
				print("pt too wide");
				goto common_end;
			}
		} else if (hbadness < 100 && o == NORMAL && list_ptr(r) != NULL) {
			b = badness(-x, total_shrink[NORMAL]);
			if (b > hbadness) {
				print_ln();
				print_nl("Tight \\hbox (badness ");
				print_int(b);
				goto common_end;
			}
		}
		return r;
	}

common_end:
	if (output_active)
		print(") has occurred while \\output is active");
	else {
		if (pack_begin_line != 0) {
			if (pack_begin_line > 0)
				print(") in paragraph at lines ");
			else print(") in alignment at lines ");
			print_val(abs(pack_begin_line));
			print("--");
		} else print(") detected at line ");
		print_val(line);
	}
	print_ln();
	font_in_short_display = NULL_FONT;
	short_display(list_ptr(r));
	print_ln();
	begin_diagnostic();
	show_box(r);
	end_diagnostic(TRUE);
	return r;
}

ptr
vpackage (p, h, m, l)
	ptr		p;
	scal	h;
	int		m;
	scal	l;
{
	int		b;
	scal	d;
	ptr		g;
	gord	o;
	ptr		r;
	scal	s;
	scal	w;
	scal	x;

	r = get_node(BOX_NODE_SIZE);
	type(r) = VLIST_NODE;
	subtype(r) = MIN_QUARTERWORD;
	shift_amount(r) = 0;
	list_ptr(r) = p;
	w = 0;
	clr_dimens();
	while (p != NULL) {
		if (is_char_node(p))
			confusion("vpack");
		else {
			switch (type(p))
			{
			case HLIST_NODE:
			case VLIST_NODE:
			case RULE_NODE:
			case UNSET_NODE:
				x += d + height(p);
				d = depth(p);
				if (type(p) >= RULE_NODE)
					s = 0;
				else s = shift_amount(p);
				if (width(p) + s > w)
					w = width(p) + s;
				break;
			
			case WHATSIT_NODE:
				break;
			
			case GLUE_NODE:
				x += d;
				d = 0;
				g = glue_ptr(p);
				x += width(g);
				o = stretch_order(g);
				total_stretch[o] += stretch(g);
				o = shrink_order(g);
				total_shrink[o] += shrink(g);
				if (subtype(p) >= A_LEADERS) {
					g = leader_ptr(p);
					if (width(g) > w)
						w = width(g);
				}
				break;
			
			case KERN_NODE:
				x += d + width(p);
				d = 0;
				break;

			default:
				break;
			}
			p = link(p);
		}
	}
	width(r) = w;
	if (d > l) {
		x += d - l;
		depth(r) = l;
	} else depth(r) = d;
	if (m == ADDITIONAL)
		h += x;
	height(r) = h;
	x = h - x;
	if (x == 0) {
		glue_sign(r) = NORMAL;
		glue_order(r) = NORMAL;
		glue_set(r) = 0.0;
		return r;
	} else if (x > 0) {
		get_stretch_order();
		glue_order(r) = o;
		glue_sign(r) = STRETCHING;
		if (total_stretch[o] != 0)
			glue_set(r) = (float) x / total_stretch[o];
		else {
			glue_sign(r) = NORMAL;
			glue_set(r) = 0.0;
		}
		if (vbadness < INF_BAD && o == NORMAL && list_ptr(r) != NULL) {
			b = badness(x, total_stretch[NORMAL]);
			if (b > vbadness) {
				print_ln();
				if (b > 100)
					print_nl("Underfull");
				else print_nl("Loose");
				print(" \\vbox (badness ");
				print_int(b);
				goto common_end;
			}
		}
		return r;
	} else {
		get_shrink_order();
		glue_order(r) = o;
		glue_sign(r) = SHRINKING;
		if (total_shrink[o] != 0)
			glue_set(r) = (float) -x / total_shrink[o];
		else {
			glue_sign(r) = NORMAL;
			glue_set(r) = 0.0;
		}
		if (total_shrink[o] < -x && o == NORMAL && list_ptr(r) != NULL) {
			glue_set(r) = 1.0;
			if (-x - total_shrink[NORMAL] > vfuzz || vbadness < 100) {
				print_ln();
				print_nl("Overfull \\vbox (");
				print_scaled(-x - total_shrink[NORMAL]);
				print("pt too high");
				goto common_end;
			}
		} else if (vbadness < 100 && o == NORMAL && list_ptr(r) != NULL) {
			b = badness(-x, total_shrink[NORMAL]);
			if (b > vbadness) {
				print_ln();
				print_nl("Tight \\vbox (badness ");
				print_int(b);
				goto common_end;
			}
		}
		return r;
	}

common_end:
	if (output_active)
		print(") has occurred while \\output is active");
	else {
		if (pack_begin_line != 0) {
			print(") in alignment at lines ");
			print_val(abs(pack_begin_line));
			print("--");
		} else print(") detected at line ");
		print_val(line);
		print_ln();
	}
	begin_diagnostic();
	show_box(r);
	end_diagnostic(TRUE);
	return r;
}
