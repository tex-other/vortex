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
 *		dvi.c
 */

#include	"tex.h"
#include	"texext.h"
#include	"heap.h"
#include	"str.h"
#include	"io.h"
#include	"eq.h"
#include	"box.h"
#include	"scan.h"
#include	"tfm.h"
#include	"file.h"
#include	"pack.h"
#include	"print.h"
#include	"error.h"
#include	"dvi.h"

#ifdef VORTEX
#include	<sys/file.h>
#include	"allir.h"
#include	"main.h"
#include	"msg.h"
#include	"state.h"

extern str		str_vtx;
extern char		dvi_ext[];
extern int		page_shipped;
extern int		page_done;
extern int		v_count;
#endif

extern byte_file	dvi_file;

extern ptr		dvi_ptr;
extern byte		dvi_buf[];
extern ptr		dvi_limit;
extern val		dvi_offset;
extern val		dvi_gone;

extern qword		c;
extern qword		f;
extern ptr		g;
extern val		lq;
extern val		lr;
extern int		cur_s;
extern scal		cur_h;
extern scal		cur_v;
extern fnt		dvi_f;
extern scal		dvi_h;
extern scal		dvi_v;
extern scal		max_h;
extern scal		max_v;
extern scal		rule_dp;
extern scal		rule_ht;
extern scal		rule_wd;
extern int		max_push;
extern int		total_pages;
extern int		dead_cycles;
extern bool		doing_leaders;
extern val		last_bop;

extern ptr		down_ptr;
extern ptr		right_ptr;

dvi_swap ()
{
	if (dvi_limit == DVI_BUF_SIZE) {
		write_dvi(0, HALF_BUF);
		dvi_limit = HALF_BUF;
		dvi_offset += DVI_BUF_SIZE;
		dvi_ptr = 0;
	} else {
		write_dvi(HALF_BUF, DVI_BUF_SIZE);
		dvi_limit = DVI_BUF_SIZE;
	}
	dvi_gone += HALF_BUF;
}

dvi_four (x)
	val		x;
{
	if (x >= 0) {
		dvi_out(x / 0100000000);
	} else {
		x += 010000000000;
		x += 010000000000;
		dvi_out(x / 0100000000 + 128);
	}
	x %= 01000000000;
	dvi_out(x / 0200000);
	x %= 0200000;
	dvi_out(x / 0400);
	dvi_out(x % 0400);
}

dvi_pop (l)
	val		l;
{
	if (l == dvi_offset + dvi_ptr && dvi_ptr > 0)
		decr(dvi_ptr);
	else
		dvi_out(POP);
}

dvi_font_def (f)
	fnt		f;
{
	int		k;

	dvi_out(FNT_DEF1);
	dvi_out(f - FONT_BASE - 1);
	dvi_out(qo(font_check[f].b0));
	dvi_out(qo(font_check[f].b1));
	dvi_out(qo(font_check[f].b2));
	dvi_out(qo(font_check[f].b3));
	dvi_four(font_size[f]);
	dvi_four(font_dsize[f]);
	dvi_out(length(font_area[f]));
	dvi_out(length(font_name[f]));
	for (k = str_start[font_area[f]]; k < str_start[font_area[f] + 1]; incr(k))
		dvi_out(str_pool[k]);
	for (k = str_start[font_name[f]]; k < str_start[font_name[f] + 1]; incr(k))
		dvi_out(str_pool[k]);
}

#define	Y_HERE			1
#define	Z_HERE			2
#define	YZ_OK			3
#define	Y_OK			4
#define	Z_OK			5
#define	D_FIXED			6
#define	NONE_SEEN		0
#define	Y_SEEN			6
#define	Z_SEEN			12

movement (w, o)
	scal		w;
	byte		o;
{
	val		k;
	ptr		p;
	ptr		q;
	int		mstate;

	q = get_node(MOVEMENT_NODE_SIZE);
	width(q) = w;
	location(q) = dvi_offset + dvi_ptr;
	if (o == DOWN1) {
		link(q) = down_ptr;
		down_ptr = q;
	} else {
		link(q) = right_ptr;
		right_ptr = q;
	}
	mstate = NONE_SEEN;
	for (p = link(q); p != NULL; p = link(p)) {
		if (width(p) == w) {
			switch (mstate + info(p))
			{
			case NONE_SEEN + YZ_OK:
			case NONE_SEEN + Y_OK:
			case Z_SEEN + YZ_OK:
			case Z_SEEN + Y_OK:
				if (location(p) < dvi_gone)
					goto not_found;
				else {
					k = location(p) - dvi_offset;
					if (k < 0)
						k += DVI_BUF_SIZE;
					dvi_buf[(int)k] += Y1 - DOWN1;
					info(p) = Y_HERE;
					goto found;
				}
				break;
			
			case NONE_SEEN + Z_OK:
			case Y_SEEN + YZ_OK:
			case Y_SEEN + Z_OK:
				if (location(p) < dvi_gone) {
					goto not_found;
				} else {
					k = location(p) - dvi_offset;
					if (k < 0)
						k += DVI_BUF_SIZE;
					dvi_buf[(int)k] += Z1 - DOWN1;
					info(p) = Z_HERE;
					goto found;
				}
				break;
			
			case NONE_SEEN + Y_HERE:
			case NONE_SEEN + Z_HERE:
			case Y_SEEN + Z_HERE:
			case Z_SEEN + Y_HERE:
				goto found;
				break;
			}
		} else {
			switch (mstate + info(p))
			{
			case NONE_SEEN + Y_HERE:
				mstate = Y_SEEN;
				break;
			
			case NONE_SEEN + Z_HERE:
				mstate = Z_SEEN;
				break;
			
			case Y_SEEN + Z_HERE:
			case Z_SEEN + Y_HERE:
				goto not_found;
				break;
			
			default:
				break;
			}
		}
	}

not_found:
	info(q) = YZ_OK;
	if (abs(w) >= 040000000) {
		dvi_out(o + 3);
		dvi_four(w);
		return;
	}
	if (abs(w) >= 0100000) {
		dvi_out(o + 2);
		if (w < 0)
			w += 0100000000;
		dvi_out(w / 0200000);
		w %= 0200000;
		goto two;
	}
	if (abs(w) >= 0200) {
		dvi_out(o + 1);
		if (w < 0)
			w += 0200000;
		goto two;
	}
	dvi_out(o);
	if (w < 0)
		w += 0400;
	goto one;

two: dvi_out(w / 0400);
one: dvi_out(w % 0400);
	return;

found:
	info(q) = info(p);
	if (info(q) == Y_HERE) {
		dvi_out(o + Y0 - DOWN1);
		while (link(q) != p) {
			q = link(q);
			switch (info(q))
			{
			case YZ_OK:
				info(q) = Z_OK;
				break;
			
			case Y_OK:
				info(q) = D_FIXED;
				break;
			}
		}
	} else {
		dvi_out(o + Z0 - DOWN1);
		while (link(q) != p) {
			q = link(q);
			switch (info(q))
			{
			case YZ_OK:
				info(q) = Y_OK;
				break;
			
			case Z_OK:
				info(q) = D_FIXED;
				break;
			default:
				break;
			}
		}
	}
}

prune_movements (l)
	val		l;
{
	ptr		p;

	while (down_ptr != NULL) {
		if (location(down_ptr) < l)
			break;
		p = down_ptr;
		down_ptr = link(p);
		free_node(p, MOVEMENT_NODE_SIZE);
	}
	while (right_ptr != NULL) {
		if (location(right_ptr) < l)
			break;
		p = right_ptr;
		right_ptr = link(p);
		free_node(p, MOVEMENT_NODE_SIZE);
	}
}

hlist_out ()
{
	ptr		p;
	scal		lx;
	scal		edge;
	int		g_sign;
	scal		save_h;
	scal		save_v;
	gord		g_order;
	val		save_loc;
	ptr		this_box;
	scal		base_line;
	scal		leader_wd;
	scal		left_edge;
	ptr		leader_box;
	bool		outer_doing_leaders;

	this_box = temp_ptr;
	g_order = glue_order(this_box);
	g_sign = glue_sign(this_box);
	p = list_ptr(this_box);
	incr(cur_s);
	if (cur_s > 0)
		dvi_out(PUSH);
	if (cur_s > max_push)
		max_push = cur_s;
	save_loc = dvi_offset + dvi_ptr;
	base_line = cur_v;
	left_edge = cur_h;
#ifdef VORTEX
	if (v_count >= 2) {
		/* new paragraph */
		make_par_box();
		make_word_box();
	}	/* v_count == 1 means new line, not considered for now. */
	v_count= 0;
#ifdef _DVI
	fprintf(stderr,"\n[H:%d, %d]", cur_h, cur_v);
	fflush(stderr);
#endif
#endif
	while (p != NULL) {
reswitch:
		if (is_char_node(p)) {
			synch_h();
			synch_v();
			do {
				f = font(p);
				c = character(p);
				if (f != dvi_f) {
					if (!font_used[f]) {
						dvi_font_def(f);
						font_used[f] = TRUE;
#ifdef VORTEX
						pbox->_ft[pbox->_tf] = f;
						(pbox->_tf)++;
						pbox->_tn += length(font_name[f]);
#endif
					}
					if (f <= 64 + FONT_BASE) {
						dvi_out(f - FONT_BASE - 1 + FNT_NUM_0);
					} else {
						dvi_out(FNT1);
						dvi_out(f - FONT_BASE - 1);
					}
					dvi_f = f;
				}
				if (c < qi(128)) {
					dvi_out(qo(c));
				} else {
					dvi_out(SET1);
					dvi_out(qo(c));
				}
#ifdef VORTEX
				make_char_box(p, f, c);
#endif
				cur_h += char_width(f, char_info(f, c));
				p = link(p);
			} while (is_char_node(p));
			dvi_h = cur_h;
		} else {
			switch (type(p)) {
			case HLIST_NODE:
			case VLIST_NODE:
				if (list_ptr(p) == NULL)
					cur_h += width(p);
				else {
					save_h = dvi_h;
					save_v = dvi_v;
					cur_v = base_line + shift_amount(p);
					temp_ptr = p;
					edge = cur_h;
					if (type(p) == VLIST_NODE)
						vlist_out();
					else
						hlist_out();
					dvi_h = save_h;
					dvi_v = save_v;
					cur_h = edge + width(p);
					cur_v = base_line;
				}
				break;
			
			case RULE_NODE:
				rule_ht = height(p);
				rule_dp = depth(p);
				rule_wd = width(p);
#ifdef VORTEX
				make_word_box();
#ifdef _DVI
				fprintf(stderr,"\n[vR]");
				fflush(stderr);
#endif
#endif
				goto fin_rule;
				break;
			
			case WHATSIT_NODE:
				out_whatsit(p);
#ifdef _DVI
				fprintf(stderr,"\n[hW]");
				fflush(stderr);
#endif
				break;
			
			case GLUE_NODE:
				g = glue_ptr(p);
#ifdef VORTEX
				make_word_box();
#ifdef _DVI
				fprintf(stderr,"\n[hG]");
				fflush(stderr);
#endif
#endif
				rule_wd = width(g);
				if (g_sign != NORMAL) {
					if (g_sign == STRETCHING) {
						if (stretch_order(g) == g_order)
							rule_wd += round(glue_set(this_box) * stretch(g));
					} else {
						if (shrink_order(g) == g_order)
							rule_wd -= round(glue_set(this_box) * shrink(g));
					}
				}
				if (subtype(p) >= A_LEADERS) {
					leader_box = leader_ptr(p);
					if (type(leader_box) == RULE_NODE) {
						rule_ht = height(leader_box);
						rule_dp = depth(leader_box);
						goto fin_rule;
					}
					leader_wd = width(leader_box);
					if (leader_wd > 0 && rule_wd > 0) {
						edge = cur_h + rule_wd;
						lx = 0;
						if (subtype(p) == A_LEADERS) {
							save_h = cur_h;
							cur_h = left_edge +
								leader_wd * ((cur_h - left_edge) / leader_wd);
							if (cur_h < save_h)
								cur_h += leader_wd;
						} else {
							lq = rule_wd / leader_wd;
							lr = rule_wd % leader_wd;
							if (subtype(p) == C_LEADERS)
								cur_h += (lr / 2);
							else {
								lx = (2 * lr + lq + 1) / (2 * lq + 2);
								cur_h += (lr - (lq - 1) * lx) / 2;
							}
						}
						while (cur_h + leader_wd <= edge) {
							cur_v = base_line + shift_amount(leader_box);
							synch_v();
							save_v = dvi_v;
							synch_h();
							save_h = dvi_h;
							temp_ptr = leader_box;
							outer_doing_leaders = doing_leaders;
							doing_leaders = TRUE;
							if (type(leader_box) == VLIST_NODE)
								vlist_out();
							else
								hlist_out();
							doing_leaders = outer_doing_leaders;
							dvi_v = save_v;
							dvi_h = save_h;
							cur_v = save_v;
							cur_h = save_h + lx + leader_wd;
						}
					}
					cur_h = edge;
					goto next_p;
				}
				goto move_past;
				break;
			
			case KERN_NODE:
#ifdef _DVI
				fprintf(stderr,"\n[hK]");
				fflush(stderr);
#endif
			case MATH_NODE:
				cur_h += width(p);
				break;
			
			case LIGATURE_NODE:
#ifdef VORTEX
#ifdef _DVI
				fprintf(stderr,"\n[L]");
				fflush(stderr);
#endif
				make_lig_node(p);
#endif
				make_char_from_lig();
				goto reswitch;

			default: break;
			}
			goto next_p;

		fin_rule:
			if (is_running(rule_ht))
				rule_ht = height(this_box);
			if (is_running(rule_dp))
				rule_dp = depth(this_box);
			rule_ht = rule_ht + rule_dp;
			if (rule_ht > 0 && rule_wd > 0) {
				synch_h();
				cur_v = base_line + rule_dp;
				synch_v();
				dvi_out(SET_RULE);
				dvi_four(rule_ht);
				dvi_four(rule_wd);
#ifdef VORTEX
				make_rule_box(p);
#endif
				cur_v = base_line;
				dvi_h += rule_wd;
			}

		move_past:
			cur_h += rule_wd;

		next_p:
			p = link(p);
		}
	}
	prune_movements(save_loc);
	if (cur_s > 0)
		dvi_pop(save_loc);
	decr(cur_s);
}

vlist_out ()
{
	ptr		p;
	scal		lx;
	scal		edge;
	int		g_sign;
	scal		save_h;
	scal		save_v;
	gord		g_order;
	val		save_loc;
	scal		top_edge;
	ptr		this_box;
	scal		leader_ht;
	scal		left_edge;
	ptr		leader_box;
	bool		outer_doing_leaders;

	this_box = temp_ptr;
	g_order = glue_order(this_box);
	g_sign = glue_sign(this_box);
	p = list_ptr(this_box);
	incr(cur_s);
	if (cur_s > 0)
		dvi_out(PUSH);
	if (cur_s > max_push)
		max_push = cur_s;
	save_loc = dvi_offset + dvi_ptr;
	left_edge = cur_h;
	cur_v -= height(this_box);
	top_edge = cur_v;
#ifdef VORTEX
	v_count++;
#ifdef _DVI
	fprintf(stderr,"\n[V:%d, %d]", cur_h, cur_v);
	fflush(stderr);
#endif
#endif
	while (p != NULL) {
		if (is_char_node(p))
			confusion("vlistout");
		else {
			switch (type(p)) {
			case HLIST_NODE:
			case VLIST_NODE:
				if (list_ptr(p) == NULL)
					cur_v += height(p) + depth(p);
				else {
					cur_v += height(p);
					synch_v();
					save_h = dvi_h;
					save_v = dvi_v;
					cur_h = left_edge + shift_amount(p);
					temp_ptr = p;

					if (type(p) == VLIST_NODE)
						vlist_out();
					else
						hlist_out();
					dvi_h = save_h;
					dvi_v = save_v;
					cur_v = save_v + depth(p);
					cur_h = left_edge;
				}
				break;
			
			case RULE_NODE:
#ifdef _DVI
				fprintf(stderr,"\n[vR]");
				fflush(stderr);
#endif
				rule_ht = height(p);
				rule_dp = depth(p);
				rule_wd = width(p);
				goto fin_rule;
				break;
			
			case WHATSIT_NODE:
#ifdef _DVI
				fprintf(stderr,"\n[vW]");
				fflush(stderr);
#endif
				out_whatsit(p);
				break;
			
			case GLUE_NODE:
				g = glue_ptr(p);
#ifdef VORTEX
				v_count++;
#ifdef _DVI
				fprintf(stderr, "\n[vG]");
				fflush(stderr);
#endif
#endif
				rule_ht = width(g);
				if (g_sign != NORMAL) {
					if (g_sign == STRETCHING) {
						if (stretch_order(g) == g_order)
							rule_ht += round(glue_set(this_box) * stretch(g));
					} else if (shrink_order(g) == g_order)
							rule_ht -= round(glue_set(this_box) * shrink(g));
				}
				if (subtype(p) >= A_LEADERS) {
					leader_box = leader_ptr(p);
					if (type(leader_box) == RULE_NODE) {
						rule_wd = width(leader_box);
						rule_dp = 0;
						goto fin_rule;
					}
					leader_ht = height(leader_box) + depth(leader_box);
					if (leader_ht > 0 && rule_ht > 0) {
						edge = cur_v + rule_ht;
						lx = 0;
						if (subtype(p) == A_LEADERS) {
							save_v = cur_v;
							cur_v = top_edge +
								leader_ht * ((cur_v - top_edge) / leader_ht);
							if (cur_v < save_v)
								cur_v += leader_ht;
						} else {
							lq = rule_ht / leader_ht;
							lr = rule_ht % leader_ht;
							if (subtype(p) == C_LEADERS)
								cur_v += lr / 2;
							else {
								lx = (2 * lr + lq + 1) / (2 * lq + 2);
								cur_v += (lr - (lq - 1) * lx) / 2;
							}
						}
						while (cur_v + leader_ht <= edge) {
							cur_h = left_edge + shift_amount(leader_box);
							synch_h();
							save_h = dvi_h;
							cur_v += height(leader_box);
							synch_v();
							save_v = dvi_v;
							temp_ptr = leader_box;
							outer_doing_leaders = doing_leaders;
							doing_leaders = TRUE;
							if (type(leader_box) == VLIST_NODE) 
								vlist_out();
							else hlist_out();
							doing_leaders = outer_doing_leaders;
							dvi_v = save_v;
							dvi_h = save_h;
							cur_h = save_h;
							cur_v = save_v - height(leader_box)+lx+leader_ht;
						}
						cur_v = edge;
						goto next_p;
					}
				}
				goto move_past;
				break;
			
			case KERN_NODE:
#ifdef _DVI
				fprintf(stderr,"\n[vK]");
				fflush(stderr);
#endif
				cur_v += width(p);
				break;

			default:
				break;
			}
			goto next_p;

		fin_rule:
			if (is_running(rule_wd))
				rule_wd = width(this_box);
			rule_ht += rule_dp;
			cur_v += rule_ht;
			if (rule_ht > 0 && rule_wd > 0) {
				synch_h();
				synch_v();
				dvi_out(PUT_RULE);
				dvi_four(rule_ht);
				dvi_four(rule_wd);
#ifdef VORTEX
				make_par_box();
				make_word_box();
				make_rule_box(p);
#endif
			}
			goto next_p;

		move_past:
			cur_v += rule_ht;
		}

	next_p:
		p = link(p);
	}
	prune_movements(save_loc);
	if (cur_s > 0)
		dvi_pop(save_loc);
	decr(cur_s);
}

#ifdef VORTEX
#define ensure_dvi_open() { \
	if (job_name == 0) \
		job_name = str_texput; \
	sprintf(dvi_ext, ".%d.%s", total_pages+1, "dvi"); \
	str_dvi = make_string_given(dvi_ext); \
	if (access(VORTEX_DIR, F_OK) < 0) \
		mkdir(VORTEX_DIR, 0775); \
	pack_file_name(job_name, str_vtx, str_dvi); \
	while ((dvi_file = b_open_out()) == NULL) \
		prompt_file_name("file name for output", str_dvi); \
	dvi_name = b_make_name_string(dvi_file);}
#else !VORTEX
#define	ensure_dvi_open() \
	{if (dvi_name == 0) { \
		if (job_name == 0) \
			job_name = str_texput; \
		pack_job_name(str_dvi); \
		while ((dvi_file = b_open_out()) == NULL) \
			prompt_file_name("file name for output", str_dvi); \
		dvi_name = b_make_name_string(dvi_file);}}
#endif VORTEX

ship_out (p)
	ptr		p;
{
	int		j;
	int		k;
	int		s;
	val		page_loc;

#ifdef VORTEX
 	max_h = max_v = max_push = 0;
 	last_bop = -1;
 	dvi_ptr = dvi_offset = dvi_gone = 0;
 	dvi_limit = DVI_BUF_SIZE;
 	for (j = 0; j < FONT_MAX; j ++)
 		font_used[j] = FALSE;
	make_page_box();
#endif

	if (tracing_output > 0) {
		print_ln();
		print_nl("Completed box being shipped out");
	}
	if (term_offset > MAX_PRINT_LINE - 9)
		print_ln();
	else if (term_offset > 0 || file_offset > 0)
		print_char(' ');
#ifdef VORTEX
	print_nl("Processing page ");
#else
	print_char('['); 
#endif
	for (j = 9; count(j) == 0 && j > 0; decr(j)) {
	}
	for (k = 0; k <= j; incr(k)) {
		print_val(count(k));
		if (k < j)
			print_char('.');
	}
#ifdef VORTEX
	print("...");
#endif
	update_terminal();
	if (tracing_output > 0) {
		print_char(']');
		begin_diagnostic();
		show_box(p);
		end_diagnostic(TRUE);
	}
	if (height(p) > MAX_DIMEN || depth(p) > MAX_DIMEN ||
		height(p) + depth(p) + v_offset > MAX_DIMEN ||
		width(p) + h_offset > MAX_DIMEN) {
		print_err("Huge page cannot be shipped out");
		help_huge_page();
		error();
		if (tracing_output <= 0) {
			begin_diagnostic();
			print_nl("The following box has been deleted:");
			show_box(p);
			end_diagnostic(TRUE);
		}
		goto done;
	}
	if (height(p) + depth(p) + v_offset > max_v)
		max_v = height(p) + depth(p) + v_offset;
	if (width(p) + h_offset > max_h)
		max_h = width(p) + h_offset;
	dvi_h = 0;
	dvi_v = 0;
	cur_h = h_offset;
	dvi_f = NULL_FONT;
	cur_s = -1;
	ensure_dvi_open();
#ifndef VORTEX
	if (total_pages == 0) {
#endif
		dvi_out(PRE);
		dvi_out(ID_BYTE); 
		dvi_four(25400000);
		dvi_four(473628672);
		prepare_mag();
		dvi_four(mag); 
		old_setting = selector;
		selector = NEW_STRING;
		print("Common TeX output "); 
		print_int(year);
		print_char('.');
		print_two(month);
		print_char('.');
		print_two(day);
		print_char(':');
		print_two(time / 60);
		print_two(time % 60);
		selector = old_setting;
		dvi_out(cur_length());
		for (s = str_start[str_ptr]; s < pool_ptr; incr(s))
			dvi_out(str_pool[s]);
		pool_ptr = str_start[str_ptr];
#ifndef VORTEX
	}
#endif
	page_loc = dvi_offset + dvi_ptr;
	dvi_out(BOP);
	for (k = 0; k <= 9; incr(k))
		dvi_four(count(k));
	dvi_four(last_bop);
	last_bop = page_loc;
	cur_v = height(p) + v_offset;
	temp_ptr = p;
	if (type(p) == VLIST_NODE) 
		vlist_out();
	else
		hlist_out();
	dvi_out(EOP);
	incr(total_pages);

done:
	if (tracing_output <= 0)
#ifdef VORTEX
		print("done."); 
#else
		print_char(']'); 
#endif
	dead_cycles = 0;
	update_terminal();

#ifdef STAT
	if (tracing_stats > 1) {
		print_nl("Memory usage before: ");
		print_int(var_used);
		print_char('&');
		print_int(dyn_used);
		print_char(';');
	}
#endif

	flush_node_list(p);

#ifdef STAT
	if (tracing_stats > 1) {
		print(" after: ");
		print_int(var_used);
		print_char('&');
		print_int(dyn_used);
		print("; still untouched: ");
		print_int(hi_mem_min - lo_mem_max - 1);
		print_ln();
	}
#endif
#ifdef VORTEX
	ship_out_close();
	finish_page_box();
	page_shipped = TRUE;		/* page is done. */
	starting_page++;
/*	save_state(); */			/* by im */
#endif
}

/*
 *	Help text
 */

help_huge_page ()
{
	help2("The page just created is more than 18 feet tall or",
	"more than 18 feet wide, so I suspect something went wrong.");
}

#ifdef VORTEX
ship_out_close ()
{
	int		k;

	dvi_out(POST);
	dvi_four(last_bop);
	last_bop = dvi_offset + dvi_ptr - 5;
	dvi_four(25400000);
	dvi_four(473628672);
	prepare_mag();
	dvi_four(mag);
	dvi_four(max_v);
	dvi_four(max_h);
	dvi_out(max_push / 256);
	dvi_out(max_push % 256);
	dvi_out(1 / 256);
	dvi_out(1 % 256);
	for (k = 0; k < FONT_MAX; k++)
		if (font_used[k])
			dvi_font_def(k);

	dvi_out(POST_POST);
	dvi_four(last_bop);
	dvi_out(ID_BYTE);
	for (k = 4 + (DVI_BUF_SIZE - dvi_ptr) % 4; k > 0; decr(k))
		dvi_out(223);
	if (dvi_limit == HALF_BUF)
		write_dvi(HALF_BUF, DVI_BUF_SIZE);
	if (dvi_ptr > 0)
		write_dvi(0, dvi_ptr);
#ifdef VORTEX
	print_nl("Output written on ");
#else
	print(" Output written on ");
#endif
	print_str(dvi_name);
	print(" (");
/*
	print_int(total_pages);
	print(" page");
	if (total_pages != 1)
		print_char('s');
	print(", ");
*/
	print_val(dvi_offset + dvi_ptr);
	print(" bytes).");
	print_ln();
	b_close(dvi_file);
}
#endif
