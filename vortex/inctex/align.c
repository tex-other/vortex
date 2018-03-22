/* 
 * Copyright (c) 1986-1992 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* This file is part of IncTeX 1.0
 *
 * Copyright (C) 1992 by Regents of the University of California
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/*
 * @(#)align.c 2.8 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"eqstack.h"
#include	"token.h"
#include	"tokenstack.h"
#include	"scan.h"
#include	"evalstack.h"
#include	"box.h"
#include	"pack.h"
#include	"math.h"
#include	"mlst-hlst.h"
#include	"align.h"

#ifdef INCTEX
extern	ptr     align_ptr;
extern	ptr     cur_align;
extern	ptr     cur_span;
extern	ptr     cur_loop;
extern	ptr     cur_head;
extern	ptr     cur_tail;
#else
ptr		align_ptr;
ptr		cur_align;
ptr		cur_span;
ptr		cur_loop;
ptr		cur_head;
ptr		cur_tail;
#endif

push_alignment ()
{
    ptr     p;

    p = get_node(ALIGN_STACK_NODE_SIZE);
    link(p) = align_ptr;
    info(p) = cur_align;
    llink(p) = preamble;
    rlink(p) = cur_span;
    mem[p + 2].i = cur_loop;
    mem[p + 3].i = align_state;
    info(p + 4) = cur_head;
    link(p + 4) = cur_tail;
    align_ptr = p;
    cur_head = get_avail();
}

pop_alignment ()
{
    ptr     p;

    free_avail(cur_head);
    p = align_ptr;
    cur_tail = link(p + 4);
    cur_head = info(p + 4);
    align_state = mem[p + 3].i;
    cur_loop = mem[p + 2].i;
    cur_span = rlink(p);
    preamble = llink(p);
    cur_align = info(p);
    align_ptr = link(p);
    free_node(p, ALIGN_STACK_NODE_SIZE);
}

init_align ()
{
    ptr     p;
    ptr     save_cs_ptr;
    
    save_cs_ptr = cur_cs;
    push_alignment();
    align_state = -1000000;
    if (mode == MMODE && (tail != head || incompleat_noad != NULL)) {
        print_err("Improper ");
        print_esc("halign");
        print(" inside $$'s");
        help_display_align();
        error();
        flush_math();
    }
    push_nest();
    if (mode == MMODE) {
        mode = -VMODE;
        prev_depth = nest[nest_ptr - 2].aux_field;
    } else if (mode > 0) {
        negate(mode);
    }
    scan_spec();
    new_save_level(ALIGN_GROUP);
    preamble = NULL;
    cur_align = align_head;
    cur_loop = NULL;
    scanner_status = ALIGNING;
    warning_index = save_cs_ptr;
    align_state = -1000000;
    loop {
        link(cur_align) = new_param_glue(TAB_SKIP_CODE);
        cur_align = link(cur_align);
        if (cur_cmd == CAR_RET)
            break;
        p = align_tokens;
        token_link(p) = NULL;
        loop {
            get_preamble_token();
            if (cur_cmd == MAC_PARAM)
                break;
            if (cur_cmd <= CAR_RET &&
                cur_cmd >= TAB_MARK &&
                align_state == -1000000) {
                if (p == align_tokens &&
                    cur_loop == NULL &&
                    cur_cmd == TAB_MARK) {
                    cur_loop = cur_align;
                } else {
                    print_err("Missing # inserted in alignment preamble");
                    help_preamble_missing();
                    back_error();
                    break;
                }
            } else if (cur_cmd != SPACER || p != align_tokens) {
                token_link(p) = new_token();
                p = token_link(p);
                token(p) = cur_tok;
            }
        }
        link(cur_align) = new_null_box();
        cur_align = link(cur_align);
        info(cur_align) = end_span;
        width(cur_align) = NULL_FLAG;
        u_part(cur_align) = token_link(align_tokens);
        p = align_tokens;
        token_link(p) = NULL;
        loop {
            get_preamble_token();
            if (cur_cmd <= CAR_RET &&
                cur_cmd >= TAB_MARK &&
                align_state == -1000000)
                break;
            if (cur_cmd == MAC_PARAM) {
                print_err("Only one # is allowed per tab");
                help_preamble_many();
                error();
                continue;
            }
            token_link(p) = new_token();
            p = token_link(p);
            token(p) = cur_tok;
        }
        token_link(p) = new_token();
        p = token_link(p);
        token(p) = END_TEMPLATE_TOKEN;
        v_part(cur_align) = token_link(align_tokens);
    }
    scanner_status = NORMAL;
    new_save_level(ALIGN_GROUP);
    if (every_cr != NULL)
        begin_token_list(every_cr, EVERY_CR_TEXT);
    align_peek();
}

get_preamble_token ()
{
restart:
    get_token();
    while (cur_chr == SPAN_CODE && cur_cmd == TAB_MARK) {
        get_token();
        if (cur_cmd > MAX_COMMAND) {
            expand();
            get_token();
        }
    }
    if (cur_cmd == ASSIGN_GLUE && cur_chr == GLUE_BASE + TAB_SKIP_CODE) {
        scan_optional_equals();
        scan_glue(GLUE_VAL);
        if (global_defs > 0)
            geq_define(GLUE_BASE + TAB_SKIP_CODE, GLUE_REF, (ptr) cur_val);
        else eq_define(GLUE_BASE + TAB_SKIP_CODE, GLUE_REF, (ptr) cur_val);
        goto restart;
    }
}

align_peek ()
{
restart:
    align_state = 1000000;
    get_nbx_token();
    if (cur_cmd == NO_ALIGN) {
        scan_left_brace();
        new_save_level(NO_ALIGN_GROUP);
        if (mode == -VMODE)
            normal_paragraph();
    } else if (cur_cmd == RIGHT_BRACE) {
        fin_align();
    } else if (cur_cmd == CAR_RET && cur_chr == CR_CR_CODE) {
        goto restart;
    } else {
        init_row();
        init_col();
    }
}

init_row()
{
    push_nest();
    mode = (-HMODE - VMODE) - mode;
    aux = 0;
    tail_append(new_glue(glue_ptr(preamble)));
    subtype(tail) = TAB_SKIP_CODE + 1;
    cur_align = link(preamble);
    cur_tail = cur_head;
    init_span(cur_align);
}

init_span (p)
    ptr     p;
{
    push_nest();
    if (mode == -HMODE) {
        space_factor = 1000;
    } else {
        prev_depth = IGNORE_DEPTH;
        normal_paragraph();
    }
    cur_span = p;
}

init_col ()
{
    extra_info(cur_align) = cur_cmd;
    if (cur_cmd == OMIT) {
        align_state = 0;
    } else {
        back_input();
        begin_token_list((ptr) u_part(cur_align), (qword) U_TEMPLATE);
    }
}

bool
fin_col ()
{
    hword   n;
    gord    o;
    ptr     p;
    ptr     q;
    ptr     r;
    ptr     s;
    ptr     u;
    scal    w;

    q = link(cur_align);
    if (cur_align == NULL || q == NULL)
        confusion("endv");
    p = link(q);
    if (p == NULL && extra_info(cur_align) < CR_CODE) {
        if (cur_loop != NULL) {
            link(q) = new_null_box();
            p = link(q);
            info(p) = end_span;
            width(p) = NULL_FLAG;
            cur_loop = link(cur_loop);
            q = align_tokens;
            r = u_part(cur_loop);
            while (r != NULL) {
                token_link(q) = new_token();
                q = token_link(q);
                token(q) = token(r);
                r = token_link(r);
            }
            token_link(q) = NULL;
            u_part(p) = token_link(align_tokens);
            q = align_tokens;
            r = v_part(cur_loop);
            while (r != NULL) {
                token_link(q) = new_token();
                q = token_link(q);
                token(q) = token(r);
                r = token_link(r);
            }
            token_link(q) = NULL;
            v_part(p) = token_link(align_tokens);
            cur_loop = link(cur_loop);
            link(p) = new_glue(glue_ptr(cur_loop));
        } else {
            print_err("Extra alignment tab has been changed to ");
            print_esc("cr");
            help_align_apply();
            extra_info(cur_align) = CR_CODE;
            error();
        }
    }
    if (extra_info(cur_align) != SPAN_CODE) {
        unsave();
        new_save_level(ALIGN_GROUP);
        if (mode == -HMODE) {
            adjust_tail = cur_tail;
            u = hpack(link(head), NATURAL);
            w = width(u);
            cur_tail = adjust_tail;
            adjust_tail = NULL;
        } else {
            u = vpackage(link(head), NATURAL, 0L);
            w = height(u);
        }
        n = MIN_QUARTERWORD;
        if (cur_span != cur_align) {
            q = cur_span;
            do  {
                incr(n);
                q = link(link(q));
            } while (q != cur_align);
            if (n > MAX_QUARTERWORD)
                confusion("256 spans");
            q = cur_span;
            while (link(info(q)) < n)
                q = info(q);
            if (link(info(q)) > n) {
                s = get_node(SPAN_NODE_SIZE);
                info(s) = info(q);
                link(s) = n;
                info(q) = s;
                width(s) = w;
            } else if (width(info(q)) < w) {
                width(info(q)) = w;
            }
        } else if (w > width(cur_align)) {
            width(cur_align) = w;
        }
        type(u) = UNSET_NODE;
        span_count(u) = n;
        get_stretch_order();
        glue_order(u) = o;
        glue_stretch(u) = total_stretch[o];
        get_shrink_order();
        glue_sign(u) = o;
        glue_shrink(u) = total_shrink[o];
        pop_nest();
        link(tail) = u;
        tail = u;
        tail_append(new_glue(glue_ptr(link(cur_align))));
        subtype(tail) = TAB_SKIP_CODE + 1;
        if (extra_info(cur_align) >= CR_CODE)
            return TRUE;
        init_span(p);
    }
    align_state = 1000000;
    get_nbx_token();
    cur_align = p;
    init_col();
    return FALSE;
}

fin_row ()
{
    ptr     p;

    if (mode == -HMODE) {
        p = hpack(link(head), NATURAL);
        pop_nest();
        append_to_vlist(p);
        if(cur_head != cur_tail) {
            link(tail) = link(cur_head);
            tail = cur_tail;
        }
    } else {
        p = vpack(link(head), NATURAL);
        pop_nest();
        link(tail) = p;
        tail = p;
        space_factor = 1000;
    }
    type(p) = UNSET_NODE;
    glue_stretch(p) = 0;
    if (every_cr != NULL)
        begin_token_list(every_cr, EVERY_CR_TEXT);
    align_peek();
}

fin_align ()
{
    int     n;
    scal    o;
    ptr     p;
    ptr     q;
    ptr     r;
    ptr     s;
    scal    t;
    ptr     u;
    ptr     v;
    scal    w;
    scal    rule_save;

    if (cur_group != ALIGN_GROUP)
        confusion("align1");
    unsave();
    if (cur_group != ALIGN_GROUP)
        confusion("align0");
    unsave();
    if (nest[nest_ptr - 1].mode_field == MMODE)
        o = display_indent;
    else o = 0;
    q = link(preamble);
    do {
        flush_list((ptr) u_part(q));
        flush_list((ptr) v_part(q));
        p = link(link(q));
        if (width(q) == NULL_FLAG) {
            width(q) = 0;
            r = link(q);
            s = glue_ptr(r);
            if (s != zero_glue) {
                add_glue_ref(zero_glue);
                delete_glue_ref(s);
                glue_ptr(r) = zero_glue;
            }
        }
        if (info(q) != end_span) {
            t = width(q) + width(glue_ptr(link(q)));
            r = info(q);
            s = end_span;
            info(s) = p;
            n = MIN_QUARTERWORD + 1;
            do {    
                width(r) -= t;
                u = info(r);
                while (link(r) > n) {
                    s = info(s);
                    n = link(info(s)) + 1;
                }
                if (link(r) < n) {
                    info(r) = info(s);
                    info(s) = r;
                    decr(link(r));
                    s = r;
                } else {
                    if (width(r) > width(info(s)))
                        width(info(s)) = width(r);
                    free_node(r, SPAN_NODE_SIZE);
                }
                r = u;
            } while (r != end_span);
        }
        type(q) = UNSET_NODE;
        span_count(q) = MIN_QUARTERWORD;
        height(q) = 0;
        depth(q) = 0;
        glue_order(q) = NORMAL;
        glue_sign(q) = NORMAL;
        glue_stretch(q) = 0;
        glue_shrink(q) = 0;
        q = p;
    } while (q != NULL);
    save_ptr -= 2;
    pack_begin_line = -mode_line;
    if (mode == -VMODE) {
        rule_save = overfull_rule;
        overfull_rule = 0;
        p = hpack(preamble, saved(1), (int) saved(0));
        overfull_rule = rule_save;
    } else {
        q = link(preamble);
        do  {
            height(q) = width(q);
            width(q) = 0;
            q = link(link(q));
        } while (q != NULL);
        p = vpackage(preamble, saved(1), (int) saved(0), MAX_DIMEN);
        q = link(preamble);
        do  {
            width(q) = height(q);
            height(q) = 0;
            q = link(link(q));
        } while (q != NULL);
    }
    pack_begin_line = 0;
    for (s = head, q = link(s); q != NULL; s = q, q = link(s)) {
        if (type(q) == UNSET_NODE) {
            if (mode == -VMODE) {
                type(q) = HLIST_NODE;
                width(q) = width(p);
            } else {
                type(q) = VLIST_NODE;
                height(q) = height(p);
            }
            glue_order(q) = glue_order(p);
            glue_sign(q) = glue_sign(p);
            glue_set(q) = glue_set(p);
            shift_amount(q) = o;
            r = link(list_ptr(q));
            s = link(list_ptr(p)); 
            do  {
                n = span_count(r); 
                t = width(s);
                w = t;
                u = hold_head;
                while (n > MIN_QUARTERWORD) {
                    decr(n);
                    s = link(s);
                    v = glue_ptr(s);
                    link(u) = new_glue(v);
                    u = link(u);
                    subtype(u) = TAB_SKIP_CODE + 1;
                    t += width(v);
                    if (glue_sign(p) == STRETCHING) {
                        if (stretch_order(v) == glue_order(p))
                            t += round(glue_set(p) * stretch(v));
                    } else if (glue_sign(p) == SHRINKING) {
                        if (shrink_order(v) == glue_order(p))
                            t -= round(glue_set(p) * shrink(v));
                    }
                    s = link(s);
                    link(u) = new_null_box();
                    u = link(u);
                    t += width(s);
                    if (mode == -VMODE) {
                        width(u) = width(s);
                    } else {
                        type(u) = VLIST_NODE;
                        height(u) = width(s);
                    }
                }
                if (mode == -VMODE) {
                    height(r) = height(q);
                    depth(r) = depth(q);
                    if (t == width(r)) {
                        glue_sign(r) = NORMAL;
                        glue_order(r) = NORMAL;
                        glue_set(r) = 0.0;
                    } else if (t > width(r)) {
                        glue_sign(r) = STRETCHING;
                        if (glue_stretch(r) == 0)
                            glue_set(r) = 0.0;
                        else glue_set(r) =
                                (float) (t - width(r)) / glue_stretch(r);
                    } else {
                        glue_order(r) = glue_sign(r);
                        glue_sign(r) = SHRINKING;
                        if (glue_shrink(r) == 0)
                            glue_set(r) = 0.0;
                        else if (glue_order(r) == NORMAL &&
                                width(r) - t > glue_shrink(r))
                            glue_set(r) = 1.0;
                        else glue_set(r) =
                            (float)(width(r) - t) / glue_shrink(r);
                    }
                    width(r) = w;
                    type(r) = HLIST_NODE;
                } else {
                    width(r) = width(q);
                    if (t == height(r)) {
                        glue_sign(r) = NORMAL;
                        glue_order(r) = NORMAL;
                        glue_set(r) = 0.0;
                    } else if (t > height(r)) {
                        glue_sign(r) = STRETCHING;
                        if (glue_stretch(r) == 0)
                            glue_set(r) = 0.0;
                        else glue_set(r) =  
                                (float) (t - height(r)) / glue_stretch(r);
                    } else {
                        glue_order(r) = glue_sign(r);
                        glue_sign(r) = SHRINKING;
                        if (glue_shrink(r) == 0)
                            glue_set(r) = 0.0;
                        else if (glue_order(r) == NORMAL &&
                                height(r) - t > glue_shrink(r))
                            glue_set(r) = 1.0;
                        else glue_set(r) = 
                                (float) (height(r) - t) / glue_shrink(r);
                    }
                    height(r) = w;
                    type(r) = VLIST_NODE;
                } 
                shift_amount(r) = 0; 
                if (u != hold_head) {
                    link(u) = link(r);
                    link(r) = link(hold_head);
                    r = u;
                }
                r = link(link(r));
                s = link(link(s));
            } while (r != NULL);
        } else if (type(q) == RULE_NODE) {
            if (is_running(width(q)))
                width(q) = width(p);
            if (is_running(height(q)))
                height(q) = height(p);
            if (is_running(depth(q)))
                depth(q) = depth(p);
            if (o != 0) {
                r = link(q);
                link(q) = NULL;
                q = hpack(q, NATURAL);
                shift_amount(q) = o;
                link(q) = r;
                link(s) = q;
            }
        }
    }
    flush_node_list(p);
    pop_alignment();
    t = aux;
    p = link(head);
    q = tail;
    pop_nest();
    if (mode == MMODE) {
        do_assignments();
        if (cur_cmd != MATH_SHIFT) {
            print_err("Missing $$ inserted");
            help_fin_display_align();
            back_error();
        } else {    
            get_x_token();
            if (cur_cmd != MATH_SHIFT) {
                print_err("Display math should end with $$");
                help_fin_display();
                back_error();
            }
        }
        pop_nest();
        tail_append(new_penalty(pre_display_penalty));
        tail_append(new_param_glue(ABOVE_DISPLAY_SKIP_CODE));
        link(tail) = p;
        if (p != NULL)
            tail = q;
        tail_append(new_penalty(post_display_penalty));
        tail_append(new_param_glue(BELOW_DISPLAY_SKIP_CODE));
        prev_depth = t;
        resume_after_display();
    } else {    
        aux = t;
        link(tail) = p;
        if (p != NULL)
            tail = q;
        if (mode == VMODE)
            build_page();
    }
}

/*
 *  Help text
 */

help_display_align ()
{
    help3("Displays can use special alignments (like \\eqalignno)",
    "only if nothing but the alignment itself is between $$'s.",
    "So I've deleted the formulas that preceded this alignment.");
}

help_fin_display_align ()
{
    help2("Displays can use special alignments (like \\eqalignno)",
    "only if nothing but the alignment itself is between $$'s.");
}

help_preamble_missing ()
{
    help3("There should be exactly one # between &'s, when an",
    "\\halign or \\valign is being set up. In this case you had",
    "none, so I've put one in; maybe that will work.");
}

help_preamble_many ()
{
    help3("There should be exactly one # between &'s, when an",
    "\\halign or \\valign is being set up. In this case you had",
    "more than one, so I'm ignoring all but the first.");
}

help_align_apply ()
{
    help3("You have given more \\span or & marks than there were",
    "in the preamble to the \\halign or \\valign now in progress.",
    "So I'll assume that you meant to type \\cr instead.");
}

help_fin_display ()
{
    help2("The `$' that I just saw supposedly matches a previous `$$'.",
    "So I shall assume that you typed `$$' both times.");
}
