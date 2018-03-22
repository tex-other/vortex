
/*
 * @(#)par.c 2.9 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include	"tex.h"
#include	"tfm.h"
#include	"tokenstack.h"
#include	"evalstack.h"
#include	"box.h"
#include	"pack.h"
#include	"hyph.h"
#include	"par.h"

#ifdef INCTEX
extern	ptr     cur_p;
extern	ptr     passive;
extern	scal    background[7];
extern	scal    break_width[7];
extern	scal    active_width[7];
extern	scal    cur_active_width[7];
extern	val     threshold;
extern	bool    second_pass;
extern	scal    first_indent;
extern	scal    first_width;
extern	scal    second_indent;
extern	scal    second_width;
extern	val     fewest_demerits;
extern	val     minimum_demerits;
extern	val     minimal_demerits[4];
extern	ptr     best_bet;
extern	hword   best_line;
extern	hword   best_pl_line[4];
extern	ptr     best_place[4];
extern	hword   easy_line;
extern	hword   last_special_line;
extern	int     line_diff;
extern	scal    disc_width;
extern	hword   pass_number;
extern	ptr     printed_node;
extern	val     actual_looseness;
extern	bool    no_shrink_error_yet;
extern	ptr     just_box;
#else
ptr		cur_p;
ptr		passive;
scal		background[7];
scal		break_width[7];
scal		active_width[7];
scal		cur_active_width[7];
val		threshold;
bool		second_pass;
scal		first_indent;
scal		first_width;
scal		second_indent;
scal		second_width;
val		fewest_demerits;
val		minimum_demerits;
val		minimal_demerits[4];
ptr		best_bet;
hword		best_line;
hword		best_pl_line[4];
ptr		best_place[4];
hword		easy_line;
hword		last_special_line;
int		line_diff;
scal		disc_width;
hword		pass_number;
ptr		printed_node;
val		actual_looseness;
bool		no_shrink_error_yet;
ptr		just_box;
#endif

#define act_width   active_width[1]

#define store_background(W) \
    (active_width[W] = background[W])

#define store_break_width(W) \
    (active_width[W] = break_width[W])

#define update_active(W) \
    (active_width[W] += mem[r + W].sc)

#define copy_to_cur_active(W) \
    (cur_active_width[W] = active_width[W])

#define downdate_width(W) \
    (cur_active_width[W] -= mem[prev_r + W].sc)

#define update_width(W) \
    (cur_active_width[W] += mem[r + W].sc)

#define set_break_width_to_background(W) \
    (break_width[W] = background[W])

#define combine_two_deltas(W) \
    (mem[prev_r + W].sc += mem[r + W].sc)

#define convert_to_break_width(W) \
    (mem[prev_r + W].sc = \
         mem[prev_r + W].sc + break_width[W] - cur_active_width[W])

#define new_delta_to_break_width(W) \
    (mem[q + W].sc = break_width[W] - cur_active_width[W])

#define new_delta_from_break_width(W) \
    (mem[q + W].sc = cur_active_width[W] - break_width[W])

#define width_lig_char(C) \
    char_width(font(lig_char(C)), \
                char_info(font(lig_char(C)), character(lig_char(C))))

#define width_char(C) \
    char_width(font(C), char_info(font(C), character(C)))
        

#define kern_break() \
    {if (!is_char_node(link(cur_p)) && auto_breaking && \
        type(link(cur_p)) == GLUE_NODE) \
        try_break(0L, UNHYPHENATED); \
    act_width += width(cur_p);}

#define check_shrinkage(G) \
    {if (shrink_order(G) != NORMAL && shrink(G) != 0) \
        G = finite_shrink(G);}

line_break (final_widow_penalty)
    val     final_widow_penalty;
{
    ptr     q;
    ptr     r;
    ptr     s;
    ptr     prev_p;
    bool    auto_breaking;

    pack_begin_line = mode_line;
    link(temp_head) = link(head);
    if (is_char_node(tail)) {
        tail_append(new_penalty(INF_PENALTY));
    } else if (type(tail) != GLUE_NODE) {
        tail_append(new_penalty(INF_PENALTY));
    } else {
        type(tail) = PENALTY_NODE;
        delete_glue_ref(glue_ptr(tail));
        flush_node_list(leader_ptr(tail));
        penalty(tail) = INF_PENALTY;
    }
    link(tail) = new_param_glue(PAR_FILL_SKIP_CODE);
    pop_nest();
    no_shrink_error_yet = TRUE;
    check_shrinkage(left_skip);
    check_shrinkage(right_skip);
    q = left_skip;
    r = right_skip;
    background[1] = width(q) + width(r);
    background[2] = 0;
    background[3] = 0;
    background[4] = 0;
    background[5] = 0;
    background[2 + stretch_order(q)] = stretch(q);
    background[2 + stretch_order(r)] += stretch(r);
    background[6] = shrink(q) + shrink(r);
    minimum_demerits = AWFUL_BAD;
    minimal_demerits[VERY_LOOSE_FIT] = AWFUL_BAD;
    minimal_demerits[LOOSE_FIT] = AWFUL_BAD;
    minimal_demerits[DECENT_FIT] = AWFUL_BAD;
    minimal_demerits[TIGHT_FIT] = AWFUL_BAD;
    if (par_shape_ptr == NULL) {
        if (hang_indent == 0) {
            last_special_line = 0;
            second_width = hsize;
            second_indent = 0;
        } else {
            last_special_line = abs(hang_after);
            if (hang_after < 0) {
                first_width = hsize - abs(hang_indent);
                first_indent = (hang_indent >= 0 ? hang_indent : 0);
                second_width = hsize;
                second_indent = 0;
            } else {
                first_width = hsize;
                first_indent = 0;
                second_width = hsize - abs(hang_indent);
                second_indent = (hang_indent >= 0 ? hang_indent : 0);
            }
        }
    } else {
        last_special_line = info(par_shape_ptr) - 1;
        second_width = mem[par_shape_ptr + 2 * (last_special_line + 1)].sc;
        second_indent = mem[par_shape_ptr + 2 * last_special_line + 1].sc;
    }
    easy_line = (looseness == 0) ? last_special_line : MAX_HALFWORD;
    threshold = pretolerance;
    if (threshold >= 0) {
        if (tracing_paragraphs > 0) {
            begin_diagnostic();
            print_nl("@firstpass");
        } 
        second_pass = FALSE;
    } else {
        threshold = tolerance;
        second_pass = TRUE;
        if (tracing_paragraphs > 0)
            begin_diagnostic();
    }
    loop {
        q = get_node(ACTIVE_NODE_SIZE);
        type(q) = UNHYPHENATED;
        fitness(q) = DECENT_FIT;
        link(q) = last_active;
        break_node(q) = NULL;
        line_number(q) = prev_graf + 1;
        total_demerits(q) = 0;
        link(active) = q;
        do_all_six(store_background);
        passive = NULL;
        printed_node = temp_head;
        pass_number = 0;
        font_in_short_display = null_font;
        cur_p = link(temp_head);
        auto_breaking = TRUE;
        prev_p = cur_p;
        while (cur_p != NULL && link(active) != last_active) {
            if (is_char_node(cur_p)) {
                prev_p = cur_p;
                do {
                    act_width += width_char(cur_p);
                    cur_p = link(cur_p);
                } while (is_char_node(cur_p));
            }
            switch (type(cur_p))
            {
            case HLIST_NODE:
            case VLIST_NODE:
            case RULE_NODE:
                act_width += width(cur_p);
                break;
            
            case WHATSIT_NODE:
                break;
            
            case GLUE_NODE:
                if (auto_breaking) {
                    if (is_char_node(prev_p))
                        try_break(0L, UNHYPHENATED);
                    else if (precedes_break(prev_p)) 
                        try_break(0L, UNHYPHENATED);
                }
                check_shrinkage(glue_ptr(cur_p));
                q = glue_ptr(cur_p);
                act_width += width(q);
                active_width[2 + stretch_order(q)] += stretch(q);
                active_width[6] += shrink(q);
                if (second_pass && auto_breaking) 
                    try_hyph();
                break;
            
            case KERN_NODE:
                kern_break();
                break;
            
            case LIGATURE_NODE:
                act_width += width_lig_char(cur_p);
                break;
            
            case DISC_NODE:
                s = pre_break(cur_p);
                disc_width = 0;
                if (s == NULL) {
                    try_break(ex_hyphen_penalty, HYPHENATED);
                } else {
                    do {
                        if (is_char_node(s)) {
                            disc_width += width_char(s);
                        } else {
                            switch (type(s))
                            {
                            case LIGATURE_NODE:
                                disc_width += width_lig_char(s);
                                break;
                            
                            case HLIST_NODE:
                            case VLIST_NODE:
                            case RULE_NODE:
                            case KERN_NODE:
                                disc_width += width(s);
                                break;
                            
                            default:
                                confusion("disc3");
                                break;
                            }
                        }
                        s = link(s);
                    } while (s != NULL);
                    act_width += disc_width;
                    try_break(hyphen_penalty, HYPHENATED);
                    act_width -= disc_width;
                }
                break;
            
            case MATH_NODE:
                auto_breaking = (subtype(cur_p) == AFTER);
                kern_break();
                break;
            
            case PENALTY_NODE:
                try_break(penalty(cur_p), UNHYPHENATED);
                break;
            
            case MARK_NODE:
            case INS_NODE:
            case ADJUST_NODE:
                break;
            
            default:
                confusion("paragraph");
                break;
            }
            prev_p = cur_p;
            cur_p = link(cur_p);
        }
        if (cur_p == NULL) {
            try_break(EJECT_PENALTY, HYPHENATED);
            if (link(active) != last_active) {
                r = link(active);
                fewest_demerits = AWFUL_BAD;
                do {
                    if (type(r) != DELTA_NODE &&
                        total_demerits(r) < fewest_demerits) {
                        fewest_demerits = total_demerits(r);
                        best_bet = r;
                    }
                    r = link(r);
                } while (r != last_active);
                best_line = line_number(best_bet);
                if (looseness == 0)
                    goto done;
                r = link(active);
                actual_looseness = 0;
                do {
                    if (type(r) != DELTA_NODE) {
                        line_diff = (int) line_number(r) - (int) best_line;
                        if (line_diff < actual_looseness &&
                            looseness <= line_diff ||   
                            line_diff > actual_looseness &&
                            looseness >= line_diff) {
                            best_bet = r;
                            actual_looseness = line_diff;
                            fewest_demerits = total_demerits(r);
                        } else if (line_diff == actual_looseness && 
                            total_demerits(r) < fewest_demerits) {
                            best_bet = r;
                            fewest_demerits = total_demerits(r);
                        }
                    }
                    r = link(r);
                } while (r != last_active);
                best_line = line_number(best_bet);
                if (actual_looseness == looseness || second_pass)
                    goto done;
            }
        }
        for (q = link(active); q != last_active; q = cur_p) {
            cur_p = link(q);
            if (type(q) == DELTA_NODE)
                free_node(q, DELTA_NODE_SIZE);
            else free_node(q, ACTIVE_NODE_SIZE);
        }
        for (q = passive; q != NULL; q = cur_p) {
            cur_p = link(q);
            free_node(q, PASSIVE_NODE_SIZE);
        }
        if (tracing_paragraphs > 0)
            print_nl("@secondpass"); 
        threshold = tolerance;
        second_pass = TRUE;
    }

done:
    if (tracing_paragraphs > 0)
        end_diagnostic(TRUE);
        
    post_line_break(final_widow_penalty);
    for (q = link(active); q != last_active; q = cur_p) {
        cur_p = link(q);
        if (type(q) == DELTA_NODE)
            free_node(q, DELTA_NODE_SIZE);
        else free_node(q, ACTIVE_NODE_SIZE);
    }
    for (q = passive; q != NULL; q = cur_p) {
        cur_p = link(q);
        free_node(q, PASSIVE_NODE_SIZE);
    }
    pack_begin_line = 0;
}

try_break (pi, break_type)
    val     pi;
    int     break_type;
{
    val     b;
    val     d;
    hword   l;
    ptr     q;
    ptr     r;
    hword   old_l;
    ptr     prev_r;
    int     fit_class;
    scal    shortfall;
    scal    line_width;
    ptr     prev_prev_r;
    bool    no_break_yet;
    bool    artificial_badness;
    bool    node_r_stays_active;
    
    no_break_yet = TRUE;
    old_l = 0;
    prev_r = active;
    if (abs(pi) >= INF_PENALTY) {
        if (pi > 0) {
            update_printed_node();
            return;
        } else {
            pi = EJECT_PENALTY;
        }
    }
    do_all_six(copy_to_cur_active);
    loop {
        r = link(prev_r);
        if (type(r) == DELTA_NODE) {
            do_all_six(update_width);
            prev_prev_r = prev_r;
            prev_r = r;
            continue;
        }
        l = line_number(r);
        if (l > old_l) {
            if (minimum_demerits < AWFUL_BAD &&
                (old_l != easy_line || r == last_active)) {
                if (no_break_yet) {
                    no_break_yet = FALSE;
                    set_break_width(break_type);
                }
                if (type(prev_r) == DELTA_NODE) {
                    do_all_six(convert_to_break_width);
                } else if (prev_r == active) {
                    do_all_six(store_break_width);
                } else {
                    q = get_node(DELTA_NODE_SIZE);
                    link(q) = r;
                    type(q) = DELTA_NODE;
                    subtype(q) = 0;
                    do_all_six(new_delta_to_break_width);
                    link(prev_r) = q;
                    prev_prev_r = prev_r;
                    prev_r = q;
                }
                minimum_demerits += abs(adj_demerits);
                fit_class = VERY_LOOSE_FIT;
                while (fit_class <= TIGHT_FIT) {
                    if (minimal_demerits[fit_class] <= minimum_demerits) {
                        q = get_node(PASSIVE_NODE_SIZE);
                        link(q) = passive;
                        passive = q;
                        cur_break(q) = cur_p;
                        incr(pass_number);
                        serial(q) = pass_number;
                        prev_break(q) = best_place[fit_class];
                        q = get_node(ACTIVE_NODE_SIZE);
                        break_node(q) = passive;
                        line_number(q) = best_pl_line[fit_class] + 1;
                        fitness(q) = fit_class;
                        type(q) = break_type;
                        total_demerits(q) = minimal_demerits[fit_class];
                        link(q) = r;
                        link(prev_r) = q;
                        prev_r = q;
                        if (tracing_paragraphs > 0)
                            show_break_node(q, fit_class, break_type);
                    }
                    minimal_demerits[fit_class] = AWFUL_BAD;
                    incr(fit_class);
                }
                minimum_demerits = AWFUL_BAD;
                if (r != last_active) {
                    q = get_node(DELTA_NODE_SIZE);
                    link(q) = r;
                    type(q) = DELTA_NODE;
                    subtype(q) = 0;
                    do_all_six(new_delta_from_break_width);
                    link(prev_r) = q;
                    prev_prev_r = prev_r;
                    prev_r = q;
                }
            }
            if (r == last_active) {
                update_printed_node();
                return;
            }
            if (l > easy_line) {
                line_width = second_width;
                old_l = MAX_HALFWORD - 1;
            } else {
                old_l = l;
                if (l > last_special_line)
                    line_width = second_width;
                else if (par_shape_ptr == NULL)
                    line_width = first_width;
                else line_width = mem[par_shape_ptr + 2 * l].sc;
            }
        }
        artificial_badness = FALSE;
        shortfall = line_width - cur_active_width[1];
        if (shortfall > 0) {
            if (cur_active_width[3] != 0 ||
                cur_active_width[4] != 0 ||
                cur_active_width[5] != 0) {
                b = 0;
                fit_class = DECENT_FIT;
            } else {
                if (shortfall > 7230584 && cur_active_width[2] < 1663497) {
                    b = INF_BAD;
                    fit_class = VERY_LOOSE_FIT;
                    goto done;
                }
                b = badness(shortfall, cur_active_width[2]);
                if (b > 12)
                    if (b > 99)
                        fit_class = VERY_LOOSE_FIT;
                    else fit_class = LOOSE_FIT;
                else fit_class = DECENT_FIT;
            }
        } else {
            if (-shortfall > cur_active_width[6])
                b = INF_BAD + 1;
            else b = badness(-shortfall, cur_active_width[6]);
            if (b > 12)
                fit_class = TIGHT_FIT;
            else fit_class = DECENT_FIT;
        }
        
    done:
        if (b > INF_BAD || pi == EJECT_PENALTY) {
            if (second_pass &&
                minimum_demerits == AWFUL_BAD &&
                link(r) == last_active &&
                prev_r == active) {
                b = 0;
                artificial_badness = TRUE;
            } else if (b > threshold)
                goto deactivate;
            node_r_stays_active = FALSE;
        } else {
            prev_r = r;
            if (b > threshold)
                continue;
            node_r_stays_active = TRUE;
        }
        d = line_penalty + b;
        d = d * d;
        if (pi != 0) {
            if (pi > 0)
                d += pi * pi;
            else if (pi > EJECT_PENALTY)
                d -= pi * pi;
        }
        if (break_type == HYPHENATED && type(r) == HYPHENATED) {
            if (cur_p != NULL)
                d += double_hyphen_demerits;
            else d += final_hyphen_demerits;
        }
        if (abs(fit_class - (int) fitness(r)) > 1)
            d += adj_demerits;
        if (tracing_paragraphs > 0)
            show_break_status(r, artificial_badness, b, pi, d);
        d += total_demerits(r);
        if (d <= minimal_demerits[fit_class]) {
            minimal_demerits[fit_class] = d;
            best_place[fit_class] = break_node(r);
            best_pl_line[fit_class] = l;
            if (d < minimum_demerits)
                minimum_demerits = d;
        }
        if (node_r_stays_active) continue;
        
    deactivate:
        link(prev_r) = link(r);
        free_node(r, ACTIVE_NODE_SIZE);
        if (prev_r == active) {
            r = link(active);
            if (type(r) == DELTA_NODE) {
                do_all_six(update_active);
                do_all_six(copy_to_cur_active);
                link(active) = link(r);
                free_node(r, DELTA_NODE_SIZE);
            }
        } else if (type(prev_r) == DELTA_NODE) {
            r = link(prev_r);
            if (r == last_active) {
                do_all_six(downdate_width);
                link(prev_prev_r) = last_active;
                free_node(prev_r, DELTA_NODE_SIZE);
                prev_r = prev_prev_r;
            } else if (type(r) == DELTA_NODE) {
                do_all_six(update_width);
                do_all_six(combine_two_deltas);
                link(prev_r) = link(r);
                free_node(r, DELTA_NODE_SIZE);
            }
        }
    }
}

post_line_break (final_widow_penalty)
    val     final_widow_penalty;
{
    ptr     q;
    ptr     r;
    ptr     s;
    int     t;
    val     pen;
    hword   cur_line;
    scal    cur_width;
    scal    cur_indent;
    bool    disc_break;

    q = break_node(best_bet);
    cur_p = NULL;
    do {
        r = q;
        q = prev_break(q);
        next_break(r) = cur_p;
        cur_p = r;
    } while (q != NULL);
    cur_line = prev_graf + 1;
    do {
        q = cur_break(cur_p);
        disc_break = FALSE;
        if (q != NULL) {
            if (type(q) == GLUE_NODE) {
                delete_glue_ref(glue_ptr(q));
                glue_ptr(q) = right_skip;
                subtype(q) = RIGHT_SKIP_CODE + 1;
                add_glue_ref(right_skip);
                goto done;
            } else {
                if (type(q) == DISC_NODE) {
                    t = replace_count(q);
                    if (t == 0) {
                        r = link(q);
                    } else {
                        r = q;
                        while (t > 1) {
                            r = link(r);
                            decr(t);
                        }
                        s = link(r);
                        if (!is_char_node(s) &&
                            next_break(cur_p) != NULL &&
                            cur_break(next_break(cur_p)) == s)
                            s = r;
                        r = link(s);
                        link(s) = NULL;
                        flush_node_list(link(q));
                        replace_count(q) = 0;
                    }
                    if (post_break(q) != NULL) {
                        s = post_break(q);
                        while (link(s) != NULL)
                            s = link(s);
                        link(s) = r;
                        r = post_break(q);
                        post_break(q) = NULL;
                    }
                    if (pre_break(q) != NULL) {
                        s = pre_break(q);
                        link(q) = s;
                        while (link(s) != NULL)
                            s = link(s);
                        pre_break(q) = NULL;
                        q = s;
                    }
                    link(q) = r;
                    disc_break = TRUE;
                } else if (type(q) == MATH_NODE || type(q) == KERN_NODE) {
                    width(q) = 0;
                }
            }
        } else {
            q = temp_head; 
            while (link(q) != NULL)
                q = link(q);
        }
        r = new_param_glue(RIGHT_SKIP_CODE);
        link(r) = link(q);
        link(q) = r;
        q = r;

    done:
        r = link(q);
        link(q) = NULL;
        q = link(temp_head);
        link(temp_head) = r;
        if (left_skip != zero_glue) {
            r = new_param_glue(LEFT_SKIP_CODE);
            link(r) = q;
            q = r;
        }
        if (cur_line > last_special_line) {
            cur_width = second_width;
            cur_indent = second_indent;
        } else if (par_shape_ptr == NULL) {
            cur_width = first_width;
            cur_indent = first_indent;
        } else {
            cur_width = mem[par_shape_ptr + 2 * cur_line].sc;
            cur_indent = mem[par_shape_ptr + 2 * cur_line - 1].sc;
        }
        adjust_tail = adjust_head;
        just_box = hpack(q, cur_width, EXACTLY);
        shift_amount(just_box) = cur_indent;
        append_to_vlist(just_box);
        if (adjust_head != adjust_tail) {
            link(tail) = link(adjust_head);
            tail = adjust_tail;
        }
        adjust_tail = NULL;
        if (cur_line + 1 != best_line) {
            pen = inter_line_penalty;
            if (cur_line == prev_graf + 1)
                pen += club_penalty;
            if (cur_line + 2 == best_line)
                pen += final_widow_penalty;
            if (disc_break)
                pen += broken_penalty;
            if (pen != 0) {
                r = new_penalty(pen);
                link(tail) = r;
                tail = r;
            }
        }
        incr(cur_line);
        cur_p = next_break(cur_p);
        if (cur_p != NULL) {
            r = temp_head;
            loop {
                q = link(r);
                if (q == cur_break(cur_p))
                    break;
                if (is_char_node(q))
                    break;
                if (non_discardable(q))
                    break;
                if (subtype(q) == ACC_KERN && type(q) == KERN_NODE)
                    break;
                r = q;
            }
            if (r != temp_head) {
                link(r) = NULL;
                flush_node_list(link(temp_head));
                link(temp_head) = q;
            }
        }
    } while (cur_p != NULL);
    if (cur_line != best_line || link(temp_head) != NULL)
        confusion("line breaking");
    prev_graf = best_line - 1;
}

set_break_width (break_type)
    int     break_type;
{
    ptr     s;
    int     t;
    ptr     v;

    do_all_six(set_break_width_to_background);
    if (break_type == UNHYPHENATED || cur_p == NULL) {
        for (s = cur_p; s != NULL; s = link(s)) {
            if (is_char_node(s))
                return;
            switch (type(s))
            {
            case GLUE_NODE:
                v = glue_ptr(s);
                break_width[1] -= width(v);
                break_width[2 + stretch_order(v)] -= stretch(v);
                break_width[6] -= shrink(v);
                break;
            
            case PENALTY_NODE:
                break;
            
            case MATH_NODE:
            case KERN_NODE:
                if (subtype(s) == ACC_KERN) 
                    return;
                else break_width[1] -= width(s);
                break;

            default:
                return;
                break;
            }
        }
    } else {
        t = replace_count(cur_p);
        s = cur_p;
        while (t > 0) {
            decr(t);
            s = link(s);
            if (is_char_node(s))
                break_width[1] -= width_char(s);
            else {
                switch (type(s))
                {
                case LIGATURE_NODE:
                    break_width[1] -= width_lig_char(s);
                    break;

                case HLIST_NODE:
                case VLIST_NODE:
                case RULE_NODE:
                case KERN_NODE:
                    break_width[1] -= width(s);
                    break;

                default:
                    confusion("disc1");
                    break;
                }
            }
        }
        for (s = post_break(cur_p); s != NULL; s = link(s)) {
            if (is_char_node(s))
                break_width[1] += width_char(s);
            else {
                switch (type(s))
                {
                case LIGATURE_NODE:
                    break_width[1] += width_lig_char(s);
                    break;

                case HLIST_NODE:
                case VLIST_NODE:
                case RULE_NODE:
                case KERN_NODE:
                    break_width[1] += width(s);
                    break;

                default:
                    confusion("disc2");
                    break;
                }
            }
        }
        break_width[1] += disc_width;
    }
}

#if 0
set_break_width (break_type)
    int     break_type;
{
    ptr     s;
    int     t;
    ptr     v;

    do_all_six(set_break_width_to_background);
    s = cur_p;
    if (break_type > UNHYPHENATED && cur_p != NULL) {
        t = replace_count(cur_p);
        v = cur_p;
        while (t > 0) {
            decr(t);
            v = link(v);
            if (is_char_node(v)) {
                break_width[1] -= width_char(v);
            } else {
                switch (type(v))
                {
                case LIGATURE_NODE:
                    break_width[1] -= width_lig_char(v);
                    break;

                case HLIST_NODE:
                case VLIST_NODE:
                case RULE_NODE:
                case KERN_NODE:
                    break_width[1] -= width(v);
                    break;

                default:
                    confusion("disc1");
                    break;
                }
            }
        }
        for (s = post_break(cur_p); s != NULL; s = link(s)) {
            if (is_char_node(s)) {
                break_width[1] += width_char(s);
            } else {
                switch (type(s))
                {
                case LIGATURE_NODE:
                    break_width[1] += width_lig_char(s);
                    break;

                case HLIST_NODE:
                case VLIST_NODE:
                case RULE_NODE:
                    break_width[1] += width(s);
                    break;

                case KERN_NODE:
                    if (t == 0 && subtype(s) != ACC_KERN)
                        t = -1;
                    else break_width[1] += width(s);
                    break;

                default:
                    confusion("disc2");
                    break;
                }
            }
            incr(t);
        }
        break_width[1] += disc_width;
        if (t == 0)
            s = link(v);
    }
    for (s; s != NULL; s = link(s)) {
        if (is_char_node(s))
            return;
        switch (type(s))
        {
        case GLUE_NODE:
            v = glue_ptr(s);
            break_width[1] -= width(v);
            break_width[2 + stretch_order(v)] -= stretch(v);
            break_width[6] -= shrink(v);
            break;
        
        case PENALTY_NODE:
            break;
        
        case MATH_NODE:
        case KERN_NODE:
            if (subtype(s) == ACC_KERN) 
                return;
            else break_width[1] -= width(s);
            break;

        default:
            return;
        }
    }
}
#endif

show_break_node (q, f, h)
    ptr     q;
    int     f;
    int     h;
{
    print_nl("@@");
    print_int(serial(passive));
    print(": line ");
    print_int(line_number(q) - 1);
    print_char('.');
    print_int(f);
    if (h == HYPHENATED)
        print_char('-');
    print(" t=");
    print_val(total_demerits(q));
    print(" -> @@");
    if (prev_break(passive) == NULL)
        print("0");
    else print_int(serial(prev_break(passive)));
}

show_break_status (r, a, b, p, d)
    ptr     r;
    bool    a;
    val     b;
    val     p;
    val     d;
{
    ptr     save_link;

    if (printed_node != cur_p) {
        print_nl("");
        if (cur_p == NULL) {
            short_display(link(printed_node));
        } else {
            save_link = link(cur_p);
            link(cur_p) = NULL;
            print_nl("");
            short_display(link(printed_node));
            link(cur_p) = save_link;
        }
        printed_node = cur_p;
    }
    print_nl("@");
    if (cur_p == NULL) {
        print_esc("par");
    } else if (type(cur_p) != GLUE_NODE) {
        if (type(cur_p) == PENALTY_NODE)
            print_esc("penalty");
        else if (type(cur_p) == DISC_NODE)
            print_esc("discretionary");
        else if (type(cur_p) == KERN_NODE)
            print_esc("kern");
        else print_esc("math");
    }
    print(" via @@");
    if (break_node(r) == NULL)
        print_char('0');
    else print_int(serial(break_node(r)));
    print(" b=");
    if (a) print_char('*');
    else print_val(b);
    print(" p=");
    print_val(p);
    print(" d=");
    print_val(d);
}

update_printed_node ()
{
    int     t;

    if (cur_p == printed_node &&
        cur_p != NULL &&
        type(cur_p) == DISC_NODE)
        for (t = replace_count(cur_p); t > 0; decr(t))
            printed_node = link(printed_node);
}

ptr
finite_shrink (p)
    ptr     p;
{
    ptr     q;

    if (no_shrink_error_yet) {
        no_shrink_error_yet = FALSE;
        print_err("Infinite glue shrinkage found in a paragraph");
        help_shrink();
        error();
    }
    q = new_spec(p);
    shrink_order(q) = NORMAL;
    delete_glue_ref(p);
    return q;
}

/*
 *  Help text
 */

help_shrink()
{
    help5("The paragraph just ended includes some glue that has",
    "infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.",
    "Such glue doesn't belong there---it allows a paragraph",
    "of any length to fit on one line. But it's safe to proceed,",
    "since the offensive shrinkability has been made finite.");
}
