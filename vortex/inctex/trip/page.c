/*
 * @(#)page.c 2.5 EPA
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
#include	"token.h"
#include	"tokenstack.h"
#include	"tokenlists.h"
#include	"eqstack.h"
#include	"evalstack.h"
#include	"scan.h"
#include	"expand.h"
#include	"box.h"
#include	"pack.h"
#include	"par.h"
#include	"math.h"
#include	"dvi.h"
#include	"page.h"

#ifdef INCTEX
#include	"Imain.h"
extern	scal    best_height_plus_depth;
extern	ptr     best_page_break;
extern	val     insert_penalties;
extern	ptr     last_glue;
extern	scal    last_kern;
extern	val     last_penalty;
extern	val     least_page_cost;
extern	bool    output_active;
extern	int     page_contents;
extern	scal    page_max_depth;
extern	ptr     page_tail;
extern	scal    page_so_far[8];

#else

scal		best_height_plus_depth;
ptr		best_page_break;
val		insert_penalties;
ptr		last_glue = MAX_HALFWORD;
scal		last_kern;
val		last_penalty;
val		least_page_cost;
bool		output_active;
int		page_contents;
scal		page_max_depth;
ptr		page_tail;
scal		page_so_far[8];

#endif

scal		best_size;
scal 		cur_page_depth;

#define set_page_so_far_zero(P) (page_so_far[P] = 0)
#define set_height_zero(H)      (active_height[H] = 0)

build_page ()
{
    val     b;
    val     c;
    scal    h;
    int     n;
    ptr     p;
    ptr     q;
    ptr     r;
    scal    w;
    val     pi;
    scal    delta;
    
    if (link(contrib_head) == NULL || output_active)
        return;
    do { 
        p = link(contrib_head);
        if (last_glue != MAX_HALFWORD)
            delete_glue_ref(last_glue);
        last_penalty = 0;
        last_kern = 0;
        if (type(p) == GLUE_NODE) {
            last_glue = glue_ptr(p);
            add_glue_ref(last_glue);
        } else {
            last_glue = MAX_HALFWORD;
            if (type(p) == PENALTY_NODE)
                last_penalty = penalty(p);
            else if (type(p) == KERN_NODE)
                last_kern = width(p);
        }
        switch (type(p))
        {
        case HLIST_NODE:
        case VLIST_NODE:
        case RULE_NODE:
            if (page_contents < BOX_THERE) {
                if (page_contents == EMPTY)
                    freeze_page_specs(BOX_THERE);
                else page_contents = BOX_THERE;
                q = new_skip_param(TOP_SKIP_CODE);
                link(q) = p;
                if (width(temp_ptr) > height(p))
                    width(temp_ptr) -= height(p);
                else width(temp_ptr) = 0;
                link(q) = p;
                link(contrib_head) = q;
                continue;
            } else {
                page_total = page_total + page_depth + height(p);
                page_depth = depth(p);
                goto contribute;
            }
            break;
        
        case WHATSIT_NODE:
            goto contribute;
            break;
        
        case GLUE_NODE:
            if (page_contents < BOX_THERE)
                goto done;
            else if (precedes_break(page_tail))
                pi = 0;
            else goto update_heights;
            break;
        
        case KERN_NODE:
            if (page_contents < BOX_THERE)
                goto done;
            else if (link(p) == NULL)
                return;
            else if (type(link(p)) == GLUE_NODE)
                pi = 0;
            else goto update_heights;
            break;
        
        case PENALTY_NODE:
            if (page_contents < BOX_THERE)
                goto done;
            else pi = penalty(p);
            break;

        case MARK_NODE:
            goto contribute;
            break;

        case INS_NODE:
            if (page_contents == EMPTY)
                freeze_page_specs(INSERTS_ONLY);
            n = subtype(p);
            r = page_ins_head;
            while (n >= subtype(link(r)))
                r = link(r);
            if (subtype(r) != n) {
                q = get_node(PAGE_INS_NODE_SIZE);
                link(q) = link(r);
                link(r) = q;
                r = q;
                subtype(r) = qi(n);
                type(r) = INSERTING;
                ensure_vbox(n);
                if (box(n) == NULL)
                    height(r) = 0;
                else height(r) = height(box(n)) + depth(box(n));
                best_ins_ptr(r) = NULL;
                q = skip(n);
                if (count(n) == 1000)
                    h = height(r);
                else h = x_over_n(height(r), 1000L) * count(n);
                page_goal = page_goal - h - width(q);
                page_so_far[2 + stretch_order(q)] += stretch(q);
                page_shrink += shrink(q);
                if (shrink_order(q) != NORMAL && shrink(q) != 0) {
                    print_err("Infinite glue shrinkage inserted from ");
                    print_esc("skip");
                    print_int(n);
                    help_inf_shrink_ins();
                    error();
                }
            }
            if (type(r) == SPLIT_UP)
                insert_penalties += float_cost(p);
            else {
                last_ins_ptr(r) = p;
                delta = page_goal - page_total - page_depth + page_shrink;
                if (count(n) == 1000)
                    h = height(p);
                else h = x_over_n(height(p), 1000L) * count(n);
                if ((h <= 0 || h <= delta) &&
                    height(p) + height(r) <= dimen(n)) {
                    page_goal -= h;
                    height(r) += height(p);
                } else {
                    if (count(n) <= 0)
                        w = MAX_DIMEN;
                    else {
                        w = page_goal - page_total - page_depth;
                        if (count(n) != 1000) 
                            w = x_over_n(w, count(n)) * 1000;
                    }
                    if (w > dimen(n) - height(r))
                        w = dimen(n) - height(r);
                    q = vert_break(ins_ptr(p), w, depth(p));
                    height(r) += best_height_plus_depth;
                    if (tracing_pages > 0)
                        show_split(n, w, q);
                    if (count(n) != 1000)
                        best_height_plus_depth =
                            x_over_n(best_height_plus_depth, 1000L) * count(n);
                    page_goal -= best_height_plus_depth;
                    type(r) = SPLIT_UP;
                    broken_ptr(r) = q;
                    broken_ins(r) = p;
                    if (q == NULL)
                        insert_penalties += EJECT_PENALTY;
                    else if (type(q) == PENALTY_NODE)
                        insert_penalties += penalty(q);
                }
            }
            goto contribute;
            break;
        
        default:
            confusion("page");
            break;
        }
        if (pi < INF_PENALTY) {
            if (page_total < page_goal) {
                if (page_so_far[3] != 0 ||
                    page_so_far[4] != 0 ||
                    page_so_far[5] != 0)
                    b = 0;
                else b = badness(page_goal - page_total, page_so_far[2]);
            } else if (page_total - page_goal > page_shrink)
                b = AWFUL_BAD;
            else b = badness(page_total - page_goal, page_shrink);
            if (b < AWFUL_BAD) {
                if (pi <= EJECT_PENALTY)
                    c = pi;
                else if (b < INF_BAD)
                    c = b + pi + insert_penalties;
                else c = DEPLORABLE;
            } else c = b;
            if (insert_penalties >= 10000) c = AWFUL_BAD;
            if (tracing_pages > 0)
                show_page_stats(b, pi, c);
            if (c <= least_page_cost) {
                best_page_break = p;
                best_size = page_goal;
                least_page_cost = c;
                r = link(page_ins_head);
                while (r != page_ins_head) {
                    best_ins_ptr(r) = last_ins_ptr(r);
                    r = link(r);
                }
            }
            if (c == AWFUL_BAD || pi <= EJECT_PENALTY) {
                fire_up(p);
                if (output_active) return;
                continue;
            }
        }
        if (type(p) < GLUE_NODE || type(p) > KERN_NODE)
            goto contribute;
        
    update_heights:
        if (type(p) == KERN_NODE)
            q = p;
        else {
            q = glue_ptr(p);
            page_so_far[2 + stretch_order(q)] += stretch(q);
            page_shrink += shrink(q);
            if (shrink_order(q) != NORMAL && shrink(q) != 0) {
                print_err("Infinite glue shrinkage found on current page");
                help_inf_shrink_page();
                error();
                r = new_spec(q);
                shrink_order(r) = NORMAL;
                delete_glue_ref(q);
                glue_ptr(p) = r;
            }
        }
        page_total = page_total + page_depth + width(q);
        page_depth = 0;

    contribute:
        if (page_depth > page_max_depth) {
            page_total = page_total + page_depth - page_max_depth;
            page_depth = page_max_depth;
        }
        link(page_tail) = p;
        page_tail = p;
        link(contrib_head) = link(p);
        link(p) = NULL;
        continue;

    done:
        link(contrib_head) = link(p);
        link(p) = NULL;
        flush_node_list(p);
    } while (link(contrib_head) != NULL);
    if (nest_ptr == 0)
        tail = contrib_head;
    else contrib_tail = contrib_head;
}

ptr
prune_page_top (p)
    ptr     p;
{
    ptr     q;
    ptr     prev_p;

    prev_p = temp_head;
    link(temp_head) = p;
    while (p != NULL) {
        switch (type(p))
        {
        case HLIST_NODE:
        case VLIST_NODE:
        case RULE_NODE:
            q = new_skip_param(SPLIT_TOP_SKIP_CODE);
            link(prev_p) = q;
            link(q) = p;
            if (width(temp_ptr) > height(p))
                width(temp_ptr) -= height(p);
            else width(temp_ptr) = 0;
            p = NULL;
            break;

        case WHATSIT_NODE:
        case MARK_NODE:
        case INS_NODE:
            prev_p = p;
            p = link(prev_p);
            break;
        
        case GLUE_NODE:
        case KERN_NODE:
        case PENALTY_NODE:
            q = p;
            p = link(q);
            link(q) = NULL;
            link(prev_p) = p;
            flush_node_list(q);
            break;

        default:
            confusion("pruning");
            break;
        }
    }
    return (link(temp_head));
}

ptr
vert_break (p, h, d)
    ptr     p;
    scal    h;
    scal    d;
{
    val     b;
    ptr     q;
    ptr     r;
    int     t;
    val     pi;
    ptr     prev_p;
    scal    prev_dp;
    ptr     best_place;
    val     least_cost;

    prev_p = p;
    least_cost = AWFUL_BAD;
    do_all_six(set_height_zero);
    prev_dp = 0;
    loop {
        if (p == NULL)
            pi = EJECT_PENALTY;
        else {
            switch (type(p))
            {
            case HLIST_NODE:
            case VLIST_NODE:
            case RULE_NODE:
                cur_height = cur_height + prev_dp + height(p);
                prev_dp = depth(p);
                goto not_found;
                break;
            
            case WHATSIT_NODE:
                goto not_found;
                break;
            
            case GLUE_NODE:
                if (precedes_break(prev_p))
                    pi = 0;
                else goto update_heights;
                break;
            
            case KERN_NODE:
                if (link(p) == NULL)
                    t = PENALTY_NODE;
                else t = type(link(p));
                if (t == GLUE_NODE)
                    pi = 0;
                else goto update_heights;
                break;
            
            case PENALTY_NODE:
                pi = penalty(p);
                break;
            
            case MARK_NODE:
            case INS_NODE:
                goto not_found;
                break;
            
            default:
                confusion("vertbreak");
                break;
            }
        }
        if (pi < INF_PENALTY) {
            if (cur_height < h) {
                if (active_height[3] != 0 ||
                    active_height[4] != 0 ||
                    active_height[5] != 0)
                    b = 0;
                else b = badness(h - cur_height, active_height[2]);
            } else if (cur_height - h > active_height[6])
                b = AWFUL_BAD;
            else b = badness(cur_height - h, active_height[6]);
            if (b < AWFUL_BAD) {
                if (pi <= EJECT_PENALTY)
                    b = pi;
                else if (b < INF_BAD)
                    b += pi;
                else b = DEPLORABLE;
            }
            if (b <= least_cost) {
                best_place = p;
                least_cost = b;
                best_height_plus_depth = cur_height + prev_dp;
            }
            if (b == AWFUL_BAD || pi <= EJECT_PENALTY)
                return best_place;
        }
        if (type(p) < GLUE_NODE || type(p) > KERN_NODE)
            goto not_found;

    update_heights:
        if (type(p) == KERN_NODE)
            q = p;
        else {
            q = glue_ptr(p);
            active_height[2 + stretch_order(q)] += stretch(q);
            active_height[6] += shrink(q);
            if (shrink_order(q) != NORMAL && shrink(q) != 0) {
                print_err("Infinite glue shrinkage found in box being split");
                help_inf_shrink_box();
                error();
                r = new_spec(q);
                delete_glue_ref(q);
                shrink_order(r) = NORMAL;
                glue_ptr(p) = r;
            }
        }
        cur_height = cur_height + prev_dp + width(q);
        prev_dp = 0;

    not_found:
        if (prev_dp > d) {
            cur_height = cur_height + prev_dp - d;
            prev_dp = d;
        }
        prev_p = p;
        p = link(prev_p);
    }
}

ptr
vsplit (n, h)
    int     n;
    scal    h;
{
    ptr     p;
    ptr     q;
    ptr     v;

    v = box(n);
    if (split_first_mark != NULL) {
        delete_token_ref(split_first_mark);
        split_first_mark = NULL;
        delete_token_ref(split_bot_mark);
        split_bot_mark = NULL;
    }
    if (v == NULL)
        return NULL;
    if (type(v) != VLIST_NODE) {
        print_err("");
        print_esc("vsplit");
        print(" needs a ");
        print_esc("vbox");
        help_vsplit_vbox();
        error();
        return NULL;
    }
    q = vert_break(list_ptr(v), h, split_max_depth);
    p = list_ptr(v);
    if (p == q)
        list_ptr(v) = NULL;
    else {
        loop {
            if (type(p) == MARK_NODE) {
                if (split_first_mark == NULL) {
                    split_first_mark = mark_ptr(p);
                    split_bot_mark = split_first_mark;
                    token_ref_count(split_first_mark) += 2;
                } else {
                    delete_token_ref(split_bot_mark);
                    split_bot_mark = mark_ptr(p);
                    add_token_ref(split_bot_mark);
                }
            }
            if (link(p) == q) {
                link(p) = NULL;
                break;
            }
            p = link(p);
        }
    }
    q = prune_page_top(q);
    p = list_ptr(v);
    free_node(v, BOX_NODE_SIZE);
    if (q == NULL)
        box(n) = NULL;
    else box(n) = vpack(q, NATURAL);
    return (vpackage(p, h, EXACTLY, split_max_depth));
}

freeze_page_specs (s)
    int     s;
{
    page_contents = s;
    page_goal = vsize;
    page_max_depth = max_depth;
    page_depth = 0;
    do_all_six(set_page_so_far_zero);
    least_page_cost = AWFUL_BAD;
    if (tracing_pages > 0) {
        begin_diagnostic();
        print_nl("%% goal height=");
        print_scaled(page_goal);
        print(", max depth=");
        print_scaled(page_max_depth);
        end_diagnostic(FALSE);
    }
}

box_error (n)
    int     n;
{
    error();
    begin_diagnostic();
    print_nl("The following box has been deleted:");
    show_box(box(n));
    end_diagnostic(TRUE);
    flush_node_list(box(n));
    box(n) = NULL;
}

ensure_vbox (n)
    int     n;
{
    ptr     p;

    p = box(n);
    if (p != NULL && type(p) == HLIST_NODE) {
        print_err("Insertions can only be added to a vbox");
        help_tut();
        box_error(n);
    }
}

print_plus (s, o)
    int     s;
    chrs    o;
{
    if (page_so_far[s] != 0) {
        print(" plus ");
        print_scaled(page_so_far[s]);
        print(o);
    }
}

print_totals ()
{
    print_scaled(page_total);
    print_plus(2, "");
    print_plus(3, "fil");
    print_plus(4, "fill");
    print_plus(5, "filll");
    if (page_shrink != 0) {
        print(" minus ");
        print_scaled(page_shrink);
    }
}

show_split(n, w, q)
    int     n;
    scal    w;
    ptr     q;
{
    begin_diagnostic();
    print_nl("% split");
    print_int(n);
    print(" to ");
    print_scaled(w);
    print_char(',');
    print_scaled(best_height_plus_depth);
    print(" p=");
    if (q == NULL)
        print_int(EJECT_PENALTY);
    else if (type(q) == PENALTY_NODE)
        print_val(penalty(q));
    else print_char('0');
    end_diagnostic(FALSE);
}

show_page_stats (b, pi, c)
    val     b;
    val     pi;
    val     c;
{
    begin_diagnostic();
    print_nl("%");
    print(" t=");
    print_totals();
    print(" g=");
    print_scaled(page_goal);
    print(" b=");
    if (b == AWFUL_BAD)
        print_char('*');
    else print_val(b);
    print(" p=");
    print_val(pi);
    print(" c=");
    if (c == AWFUL_BAD)
        print_char('*');
    else print_val(c);
    if (c <= least_page_cost)
        print_char('#');
    end_diagnostic(FALSE);
}

fire_up (c)
    ptr     c;
{
    int     n;
    ptr     p;
    ptr     q;
    ptr     r;
    ptr     s;
    bool    wait;
    ptr     prev_p;
    scal    save_vfuzz;
    val     save_vbadness;
    ptr     save_split_top_skip;

    if (type(best_page_break) == PENALTY_NODE) {
        geq_word_define(INT_BASE+OUTPUT_PENALTY_CODE, penalty(best_page_break));
        penalty(best_page_break) = INF_PENALTY;
    } else geq_word_define(INT_BASE+OUTPUT_PENALTY_CODE, INF_PENALTY);
    if (bot_mark != NULL) {
        if (top_mark != NULL)
            delete_token_ref(top_mark);
        top_mark = bot_mark;
        add_token_ref(top_mark);
        delete_token_ref(first_mark);
        first_mark = NULL;
    }
    if (c == best_page_break)
        best_page_break = NULL;
    if (box(255) != NULL) {
        print_err("");
        print_esc("box");
        print("255 is not void");
        help_box_255();
        box_error(255);
    }
    insert_penalties = 0;
    save_split_top_skip = split_top_skip;
    r = link(page_ins_head);
    while (r != page_ins_head) {
        if (best_ins_ptr(r) != NULL) {
            n = qo(subtype(r));
            ensure_vbox(n);
            if (box(n) == NULL)
                box(n) = new_null_box();
            p = box(n) + LIST_OFFSET;
            while (link(p) != NULL)
                p = link(p);
            last_ins_ptr(r) = p;
        }
        r = link(r);
    }
    q = hold_head;
    link(q) = NULL;
    prev_p = page_head;
    p = link(prev_p);
    while (p != best_page_break) {
        if (type(p) == INS_NODE) {
            r = link(page_ins_head);
            while (subtype(r) != subtype(p))
                r = link(r);
            if (best_ins_ptr(r) == NULL)
                wait = TRUE;
            else {
                wait = FALSE;
                s = ins_ptr(p);
                link(last_ins_ptr(r)) = s;
                s = last_ins_ptr(r);
                if (best_ins_ptr(r) == p) {
                    if (type(r) == SPLIT_UP &&
                        broken_ins(r) == p &&
                        broken_ptr(r) != NULL) {
                        while (link(s) != broken_ptr(r))
                            s = link(s);
                        split_top_skip = split_top_ptr(p);
                        ins_ptr(p) = prune_page_top(broken_ptr(r));
                        if (ins_ptr(p) != NULL) {
                            temp_ptr = vpack(ins_ptr(p), NATURAL);
                            height(p) = height(temp_ptr) + depth(temp_ptr);
                            free_node(temp_ptr, BOX_NODE_SIZE);
                            wait = TRUE;
                        }
                        link(s) = NULL;
                    }
                    best_ins_ptr(r) = NULL;
                    n = qo(subtype(r));
                    temp_ptr = list_ptr(box(n));
                    free_node(box(n), BOX_NODE_SIZE);
                    box(n) = vpack(temp_ptr, NATURAL);
                } else {
                    while (link(s) != NULL)
                        s = link(s);
                    last_ins_ptr(r) = s;
                }
            }
            link(prev_p) = link(p);
            link(p) = NULL;
            if (wait) {
                link(q) = p;
                q = p;
                incr(insert_penalties);
            } else {
                delete_glue_ref(split_top_ptr(p));
                free_node(p, INS_NODE_SIZE);
            }
            p = prev_p;
        } else if (type(p) == MARK_NODE) {
            if (first_mark == NULL) {
                first_mark = mark_ptr(p);
                add_token_ref(first_mark);
            }
            if (bot_mark != NULL)
                delete_token_ref(bot_mark);
            bot_mark = mark_ptr(p);
            add_token_ref(bot_mark);
        }
        prev_p = p;
        p = link(prev_p);
    }
    split_top_skip = save_split_top_skip;
    if (p != NULL) {
        if (link(contrib_head) == NULL) {
            if (nest_ptr == 0)
                tail = page_tail;
            else contrib_tail = page_tail;
        }
        link(page_tail) = link(contrib_head);
        link(contrib_head) = p;
        link(prev_p) = NULL;
    }
    save_vbadness = vbadness;
    vbadness = INF_BAD;
    save_vfuzz = vfuzz;
    vfuzz = MAX_DIMEN;
    box(255) = vpackage(link(page_head), best_size, EXACTLY, page_max_depth);
    vbadness = save_vbadness;
    vfuzz = save_vfuzz;
    if (last_glue != MAX_HALFWORD)
        delete_glue_ref(last_glue);
    start_new_page();
    if (q != hold_head) {
        link(page_head) = link(hold_head);
        page_tail = q;
    }
    r = link(page_ins_head);
    while (r != page_ins_head) {
        q = link(r);
        free_node(r, PAGE_INS_NODE_SIZE);
        r = q;
    }
    link(page_ins_head) = page_ins_head;
    if (top_mark != NULL && first_mark == NULL) {
        first_mark = top_mark;
        add_token_ref(top_mark);
    }
    if (output_routine != NULL) {
        if (dead_cycles >= max_dead_cycles) {
            print_err("Output loop---");
            print_int(dead_cycles);
            print(" consecutive dead cycles");
            help_dead_cycles();
            error();
        } else {
            output_active = TRUE;
            incr(dead_cycles);
            push_nest();
            mode = -VMODE;
            prev_depth = IGNORE_DEPTH;
            mode_line = -line;
            begin_token_list(output_routine, OUTPUT_TEXT);
            new_save_level(OUTPUT_GROUP);
            normal_paragraph();
            scan_left_brace();
            return;
        }
    }
    if (link(page_head) != NULL) {
        if (link(contrib_head) == NULL) {
            if (nest_ptr == 0) tail = page_tail;
            else contrib_tail = page_tail;
        } else link(page_tail) = link(contrib_head);
        link(contrib_head) = link(page_head);
        link(page_head) = NULL;
        page_tail = page_head;
    }
    ship_out(box(255));
    box(255) = NULL;
}

/*
 *  Help text
 */

help_tut ()
{
    help3("Tut tut: You're trying to \\insert into a",
    "\\box register that now contains an \\hbox.",
    "Proceed, and I'll discard its present contents.");
}

help_vsplit_vbox ()
{
    help2("The box you are trying to split is an \\hbox.",
    "I can't split such a box, so I'll leave it alone.");
}

help_inf_shrink_ins ()
{
    help3("The correction glue for page breaking with insertions",
    "must have finite shrinkability. But you may proceed,",
    "since the offensive shrinkability has been made finite.");
}

help_inf_shrink_box ()
{
    help4("The box you are \\vsplitting contains some infinitely",
    "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
    "Such glue doesn't belong there; but you can safely proceed,",
    "since the offensive shrinkability has been made finite.");
}

help_inf_shrink_page ()
{
    help4("The page about to be output contains some infinitely",
    "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
    "Such glue doesn't belong there; but you can safely proceed,",
    "since the offensive shrinkability has been made finite.");
}

help_box_255 ()
{
    help2("You shouldn't use \\box255 except in \\output routines.",
    "Proceed, and I'll discard its present contents.");
}

help_dead_cycles ()
{
    help3("I've concluded that your \\output is awry; it never does a",
    "\\shipout, so I'm shipping \\box255 out myself. Next time",
    "increase \\maxdeadcycles if you want me to be more patient!");
}
