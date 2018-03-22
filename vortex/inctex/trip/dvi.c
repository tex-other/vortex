/*
 * @(#)dvi.c 2.9 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 *  This file is modified for
 *
 *  IncTeX  --	Incremental TeX
 *
 *  An Editor-Independent, Adaptive, Incremental TeX Formatter
 *  Copyright (C) 1988 by Regents of the University of California
 *  (Derluen Pan)
 *
 *  All rights reserved by the copyright holders.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 * 
 */

#include	"tex.h"
#include	"texext.h"
#include	"box.h"
#include	"token.h"
#include	"scan.h"
#include	"tfm.h"
#include	"file.h"
#include	"pack.h"
#include	"dvi.h"

#ifdef INCTEX

#include	<sys/file.h>
#include	"Imain.h"

#else

int		total_pages;

#endif

byte_file	dvi_file;
str		dvi_name;
ptr		dvi_ptr;
byte		dvi_buf[DVI_BUF_SIZE];
ptr		dvi_limit;
val		dvi_offset;
val		dvi_gone;
qword		c;
qword		f;
ptr		g;
val		lq;
val		lr;
int		cur_s;
scal		cur_h;
scal		cur_v;
fnt		dvi_f;
scal		dvi_h;
scal		dvi_v;
scal		max_h;
scal		max_v;
scal		rule_dp;
scal		rule_ht;
scal		rule_wd;
int		max_push;
int		dead_cycles;
bool		doing_leaders;
val		last_bop;
ptr		down_ptr;
ptr		right_ptr;

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
    val     x;
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
    val     l;
{
    if (l == dvi_offset + dvi_ptr && dvi_ptr > 0)
        decr(dvi_ptr);
    else dvi_out(POP);
}

dvi_font_def (f)
    fnt     f;
{
    int     k;

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

#define Y_HERE          1
#define Z_HERE          2
#define YZ_OK           3
#define Y_OK            4
#define Z_OK            5
#define D_FIXED         6
#define NONE_SEEN       0
#define Y_SEEN          6
#define Z_SEEN          12

movement (w, o)
    scal    w;
    byte    o;
{
    int     k;
    ptr     p;
    ptr     q;
    int     mstate;

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
                if (location(p) < dvi_gone) {
                    goto not_found;
                } else {
                    k = location(p) - dvi_offset;
                    if (k < 0)
                        k += DVI_BUF_SIZE;
                    dvi_buf[k] += Y1 - DOWN1;
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
                    dvi_buf[k] += Z1 - DOWN1;
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
    val     l;
{
    ptr     p;

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
    ptr     p;
    scal    lx;
    scal    edge;
    int     g_sign;
    scal    save_h;
    scal    save_v;
    gord    g_order;
    val     save_loc;
    ptr     this_box;
    scal    base_line;
    scal    leader_wd;
    scal    left_edge;
    ptr     leader_box;
    bool    outer_doing_leaders;

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
                    }
                    if (f <= 64 + FONT_BASE) {
                        dvi_out(f - FONT_BASE - 1 + FNT_NUM_0);
                    } else {
                        dvi_out(FNT1);
                        dvi_out(f - FONT_BASE - 1);
                    }
                    dvi_f = f;
                }
                if (c >= qi(128))
                    dvi_out(SET1);
                dvi_out(qo(c));
                cur_h += char_width(f, char_info(f, c));
                p = link(p);
            } while (is_char_node(p));
            dvi_h = cur_h;
        } else {
            switch (type(p))
            {
            case HLIST_NODE:
            case VLIST_NODE:
                if (list_ptr(p) == NULL) {
                    cur_h += width(p);
                } else {
                    save_h = dvi_h;
                    save_v = dvi_v;
                    cur_v = base_line + shift_amount(p);
                    temp_ptr = p;
                    edge = cur_h;
                    if (type(p) == VLIST_NODE)
                        vlist_out();
                    else hlist_out();
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
                goto fin_rule;
            
            case WHATSIT_NODE:
                out_whatsit(p);
                break;
            
            case GLUE_NODE:
                g = glue_ptr(p);
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
                        rule_wd += 10;
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
                            if (subtype(p) == C_LEADERS) {
                                cur_h += lr / 2;
                            } else {
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
                            else hlist_out();
                            doing_leaders = outer_doing_leaders;
                            dvi_v = save_v;
                            dvi_h = save_h;
                            cur_v = save_v;
                            cur_h = save_h + lx + leader_wd;
                        }
                        cur_h = edge - 10;
                        goto next_p;
                    }
                }
                goto move_past;
            
            case KERN_NODE:
            case MATH_NODE:
                cur_h += width(p);
                break;
            
            case LIGATURE_NODE:
                make_char_from_lig();
                goto reswitch;

            default:
                 break;
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
    if (cur_s > 0) dvi_pop(save_loc);
    decr(cur_s);
}

vlist_out ()
{
    ptr     p;
    scal    lx;
    scal    edge;
    int     g_sign;
    scal    save_h;
    scal    save_v;
    gord    g_order;
    val     save_loc;
    scal    top_edge;
    ptr     this_box;
    scal    leader_ht;
    scal    left_edge;
    ptr     leader_box;
    bool    outer_doing_leaders;

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
    while (p != NULL) {
        if (is_char_node(p))
            confusion("vlistout");
        else {
            switch (type(p))
            {
            case HLIST_NODE:
            case VLIST_NODE:
                if (list_ptr(p) == NULL) {
                    cur_v += height(p) + depth(p);
                } else {
                    cur_v += height(p);
                    synch_v();
                    save_h = dvi_h;
                    save_v = dvi_v;
                    cur_h = left_edge + shift_amount(p);
                    temp_ptr = p;
                    if (type(p) == VLIST_NODE)
                        vlist_out();
                    else hlist_out();
                    dvi_h = save_h;
                    dvi_v = save_v;
                    cur_v = save_v + depth(p);
                    cur_h = left_edge;
                }
                break;
            
            case RULE_NODE:
                rule_ht = height(p);
                rule_dp = depth(p);
                rule_wd = width(p);
                goto fin_rule;
                break;
            
            case WHATSIT_NODE:
                out_whatsit(p);
                break;
            
            case GLUE_NODE:
                g = glue_ptr(p);
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
                        rule_ht += 10;
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
                            if (subtype(p) == C_LEADERS) {
                                cur_v += lr / 2;
                            } else {
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
                        cur_v = edge - 10;
                        goto next_p;
                    }
                }
                goto move_past;
            
            case KERN_NODE:
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

#ifdef INCTEX

#define ensure_dvi_open_inc() { \
	get_ext(total_pages+1, EXT_DVI); \
	(void) rename(name_of_file, INC_DVI); \
	while ((dvi_file = b_open_out()) == NULL) \
		prompt_file_name("file name for output", str_dvi); \
	dvi_name = b_make_name_string(dvi_file); \
}

#define ensure_dvi_open_batch() { \
	if (dvi_name == 0) { \
		if (job_name == 0) \
			job_name = str_texput; \
		pack_job_name(str_dvi); \
		while ((dvi_file = b_open_out()) == NULL) \
			prompt_file_name("file name for output", str_dvi); \
		dvi_name = b_make_name_string(dvi_file); \
	} \
}

add_pnode()
{
	P_NODE		*pnd;
	int		j;	

	if (virgin || ((p_end == NULL) && (p_bgn == NULL)) ||
	    ((p_end != NULL) && (p_end->nxt == NULL))) {
		MALLOC(pnd, P_NODE, sizeof(P_NODE));
		pnd->nxt = NULL;
		if (p_end == NULL) {
			p_bgn = p_end = pnd;
		} else {
			p_end->nxt = pnd;
			p_end = pnd;
		}
		p_all = p_max;
		for (j = 0; j < OUT_MAX; j++)
			p_end->weop[j] = NIL;
	} else if (p_end == NULL) {
		p_end = p_bgn;
		chk_eop = 0;
	} else if (p_end->nxt != NULL) {
		p_end = p_end->nxt;
		chk_eop = p_end->eop;
	}
	
	/* Reinitializing current page node */
	p_end->no = p_max;
	p_end->fp = f_cur;
	p_end->eop = f_cur->cnt;
	p_end->ftime = 0.0;
	p_end->btime = 0.0;
	p_end->stime = 0.0;
	p_end->ltime = 0.0;
	p_end->qtime = 0.0;
	
	chk_fid = p_end->fp->id;
	chk_cid = p_end->fp->cnt;

	for (j = 0; j < OUT_MAX; j++) {
		oweop[j] = p_end->weop[j];
		if (write_open[j])
			p_end->weop[j] = ftell(write_file[j]);
		else
			p_end->weop[j] = NIL;
	}
}


ship_out (p)
	ptr		p;
{
	int		j;
	int		k;
	val		page_loc;

	if (incremental) {
		max_h = max_v = max_push = 0;
		last_bop = -1;
		dvi_ptr = dvi_offset = dvi_gone = 0;
		dvi_limit = DVI_BUF_SIZE;
	 	for (j = 0; j < FONT_MAX; j ++)
 			font_used[j] = FALSE;
	}

	if (tracing_output > 0) {
		print_ln();
		print_nl("Completed box being shipped out");
	}
	if (term_offset > MAX_PRINT_LINE - 9 /*60*/)		/* originally 9 */
        	print_ln();
	else if (term_offset > 0 || file_offset > 0)
        	print_char(' ');
	print_char('['); 

	for (j = 9; count(j) == 0 && j > 0; decr(j)) {
	}
	for (k = 0; k <= j; incr(k)) {
		print_val(count(k));
		if (k < j)
			print_char('.');
	}

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
	page_shipped = page_done = TRUE;	/* page is shipped. */
/*	   Moved this here instead of after "done:" because if huge page
 *	   is detected above no dvi page is really written and total_pages
 *	   stays the same, and no dvi file
 *	   or stc file should be created in incremental mode. - DLP
 *	   set page_done to fix log discrepancy - won't read next line, then
 *	   call back_input() and reread & reprint last token in log file - DLP
 */
	if (height(p) + depth(p) + v_offset > max_v)
		max_v = height(p) + depth(p) + v_offset;
	if (width(p) + h_offset > max_h)
		max_h = width(p) + h_offset;
	dvi_h = 0;
	dvi_v = 0;
	cur_h = h_offset;
	dvi_f = null_font;

	if (incremental)  {
		ensure_dvi_open_inc();
		start_dvi();
	} else {
		ensure_dvi_open_batch();
		if (total_pages == 0)
			start_dvi();
	}

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
	cur_s = -1;

done:
/*	Saving end-of-page information moved to main_control,
 *	because occasionally some more input-reading/etc occurs
 *	after ship_out, so this part of the end-of-page state
 *	would be prematurely saved - DLP
 *
 *	if (incremental && page_shipped)
 *		add_pnode();
 */
	if (tracing_output <= 0) {
		print_char(']');
	}

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
	if (incremental && page_shipped) {	
		ship_out_close();
/* 		Moved setting of page_shipped up so it is set ONLY IF
 *		a dvi page is really written: in incremental mode a
 *		dvi file and stc file shouldn't be created yet.
 *		Especially because total_pages doesn't get incremented, so
 *		the naming scheme for each checkpt would get screwed up! - DLP
 */
	}
}

start_dvi ()
{
	int		s;

	dvi_out(PRE);
	dvi_out(ID_BYTE); 
	dvi_four(25400000);
	dvi_four(473628672);
	prepare_mag();
	dvi_four(mag); 
	old_setting = selector;
	selector = NEW_STRING;
/*
	print(" Common TeX output "); 
*/
	print(" TeX output "); 
	print_int(1957);
	print_char('.');
	print_two(11);
	print_char('.');
	print_two(20);
	print_char(':');
	print_two(7);
	print_two(0);
	selector = old_setting;
	dvi_out(cur_length());
	for (s = str_start[str_ptr]; s < pool_ptr; incr(s))
		dvi_out(str_pool[s]);
	pool_ptr = str_start[str_ptr];
}

fin_dvi ()
{
    int     f;

    while (cur_s > -1) {
        if (cur_s > 0) {
            dvi_out(POP);
        } else {
            dvi_out(EOP);
            incr(total_pages);
        }
        decr(cur_s);
    }
    if (total_pages == 0) {
        print_nl("No pages of output.");
        return;
    }
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
    dvi_out(total_pages / 256);
    dvi_out(total_pages % 256);
    while (font_ptr > FONT_BASE) {
        if (font_used[font_ptr])
            dvi_font_def(font_ptr);
        decr(font_ptr);
    }
    dvi_out(POST_POST);
    dvi_four(last_bop);
    dvi_out(ID_BYTE);
    for (f = 4 + (DVI_BUF_SIZE - dvi_ptr) % 4; f > 0; decr(f))
        dvi_out(223);
    if (dvi_limit == HALF_BUF)
        write_dvi(HALF_BUF, DVI_BUF_SIZE);
    if (dvi_ptr > 0)
        write_dvi(0, dvi_ptr);
/*  Write this msg only if in batch mode & we're at end of execution. DLP */
    if (!incremental) {
    	print_nl("Output written on ");
    	print_str(dvi_name);
    	print(" (");
    	print_int(total_pages);
    	print(" page");
    	if (total_pages != 1)
        	print_char('s');
    	print(", ");
    	print_val(dvi_offset + dvi_ptr);
    	print(" bytes).");
    };
    b_close(dvi_file);
}

#else	/* ! INCTEX */

#define ensure_dvi_open() \
    {if (dvi_name == 0) { \
        if (job_name == 0) \
            job_name = str_texput; \
        pack_job_name(str_dvi); \
        while ((dvi_file = b_open_out()) == NULL) \
            prompt_file_name("file name for output", str_dvi); \
        dvi_name = b_make_name_string(dvi_file);}}

ship_out (p)
    ptr     p;
{
    int     j;
    int     k;
    val     page_loc;

    if (tracing_output > 0) {
        print_ln();
        print_nl("Completed box being shipped out");
    }
    if (term_offset > MAX_PRINT_LINE - 9)
        print_ln();
    else if (term_offset > 0 || file_offset > 0)
        print_char(' ');
    print_char('['); 
    for (j = 9; count(j) == 0 && j > 0; decr(j))
        {}
    for (k = 0; k <= j; incr(k)) {
        print_val(count(k));
        if (k < j)
            print_char('.');
    }
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
    dvi_f = null_font;
    ensure_dvi_open();
    if (total_pages == 0)
        start_dvi();
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
    else hlist_out();
    dvi_out(EOP);
    incr(total_pages);
    cur_s = -1;

done:
    if (tracing_output <= 0)
        print_char(']'); 
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
}

start_dvi ()
{
    int     s;

    dvi_out(PRE);
    dvi_out(ID_BYTE); 
    dvi_four(25400000);
    dvi_four(473628672);
    prepare_mag();
    dvi_four(mag); 
    old_setting = selector;
    selector = NEW_STRING;
    print(" Common TeX output "); 
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
}

fin_dvi ()
{
    int     f;

    while (cur_s > -1) {
        if (cur_s > 0) {
            dvi_out(POP);
        } else {
            dvi_out(EOP);
            incr(total_pages);
        }
        decr(cur_s);
    }
    if (total_pages == 0) {
        print_nl("No pages of output.");
        return;
    }
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
    dvi_out(total_pages / 256);
    dvi_out(total_pages % 256);
    while (font_ptr > FONT_BASE) {
        if (font_used[font_ptr])
            dvi_font_def(font_ptr);
        decr(font_ptr);
    }
    dvi_out(POST_POST);
    dvi_four(last_bop);
    dvi_out(ID_BYTE);
    for (f = 4 + (DVI_BUF_SIZE - dvi_ptr) % 4; f > 0; decr(f))
        dvi_out(223);
    if (dvi_limit == HALF_BUF)
        write_dvi(HALF_BUF, DVI_BUF_SIZE);
    if (dvi_ptr > 0)
        write_dvi(0, dvi_ptr);
    print_nl("Output written on ");
    print_str(dvi_name);
    print(" (");
    print_int(total_pages);
    print(" page");
    if (total_pages != 1)
        print_char('s');
    print(", ");
    print_val(dvi_offset + dvi_ptr);
    print(" bytes).");
    b_close(dvi_file);
}

#endif  /* INCTEX */

init_dvi ()
{
    dvi_limit = DVI_BUF_SIZE;
    last_bop = -1;
    cur_s = -1;
}

/*
 *  Help text
 */

help_huge_page ()
{
    help2("The page just created is more than 18 feet tall or",
    "more than 18 feet wide, so I suspect something went wrong.");
}


#ifdef INCTEX
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
		dvi_out(DVI_PAD);
	if (dvi_limit == HALF_BUF)
		write_dvi(HALF_BUF, DVI_BUF_SIZE);
	if (dvi_ptr > 0)
		write_dvi(0, dvi_ptr);
	print(" <");
	print_str(dvi_name);
	print(" (");
	print_val(dvi_offset + dvi_ptr);
	print(" bytes)>");
	update_terminal();
	b_close(dvi_file);
}
#endif

