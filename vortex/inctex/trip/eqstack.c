/*
 * @(#)eqstack.c 2.6 EPA
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

#ifdef INCTEX
#include	"Imain.h"
extern	ptr     save_ptr;
extern	ptr     max_save_stack;
extern	qword   cur_level;
extern	group   cur_group;
extern	ptr     cur_boundary;
extern	mword   save_stack[SAVE_SIZE];
#else
ptr		save_ptr			= 0;
ptr		max_save_stack			= 0;
qword		cur_level			= LEVEL_ONE;
group		cur_group			= BOTTOM_LEVEL;
ptr		cur_boundary			= 0;
mword 		save_stack[SAVE_SIZE];
#endif


#define check_full_save_stack() \
    {if (save_ptr > max_save_stack) { \
        max_save_stack = save_ptr; \
        if (max_save_stack > SAVE_SIZE - 6) \
            overflow("save size", SAVE_SIZE);}}

new_save_level (c)
    group   c;
{
    check_full_save_stack();
    save_type(save_ptr) = LEVEL_BOUNDARY;
    save_level(save_ptr) = cur_group;
    save_index(save_ptr) = cur_boundary;
    if (cur_level == MAX_QUARTERWORD)
        overflow("grouping levels", MAX_QUARTERWORD - MIN_QUARTERWORD);
    cur_boundary = save_ptr;
    cur_group = c;
    incr(cur_level);
    incr(save_ptr);
}

eq_destroy (w)
    mword   w;
{
    ptr     q;

    switch (eq_type_field(w))
    {
    case CALL:
    case LONG_CALL:
    case LONG_OUTER_CALL:
        delete_token_ref(equiv_field(w));
        break;

    case GLUE_REF:
        delete_glue_ref(equiv_field(w));
        break;

    case SHAPE_REF:
        q = equiv_field(w);
        if (q != NULL)
            free_node(q, link(q) + link(q) + 1);
        break;

    case BOX_REF:
        flush_node_list(equiv_field(w));
        break;
    }
}

eq_save (p, l)
    ptr     p;
    qword   l;
{
    check_full_save_stack();
    if (l == LEVEL_ZERO)
        save_type(save_ptr) = RESTORE_ZERO;
    else {
        save_stack[save_ptr] = eqtb[p];
        incr(save_ptr);
        save_type(save_ptr)= RESTORE_OLD_VALUE;
    }
    save_index(save_ptr) = p;
    save_level(save_ptr) = l;
    incr(save_ptr);
}

eq_define (p, t, e)
    ptr     p;
    qword   t;
    hword   e;
{
    if (eq_level(p) == cur_level)
        eq_destroy(eqtb[p]);
    else if (cur_level > LEVEL_ONE)
        eq_save(p, eq_level(p));
    eq_level(p) = cur_level;
    eq_type(p) = t;
    equiv(p) = e;
}

eq_word_define (p, w)
    ptr     p;
    val     w;
{
    if (xeq_level[p - INT_BASE] != cur_level) {
        eq_save(p, xeq_level[p - INT_BASE]);
        xeq_level[p - INT_BASE] = cur_level;
    }
    eqtb[p].i = w;
}

geq_define (p, t, e)
    ptr     p;
    qword   t;
    hword   e;
{
    eq_destroy(eqtb[p]);
    eq_level(p) = LEVEL_ONE;
    eq_type(p) = t;
    equiv(p) = e;
}

geq_word_define (p, w)
    ptr     p;
    val     w;
{
    eqtb[p].i = w;
    xeq_level[p - INT_BASE] = LEVEL_ONE;
}

save_for_after (t)
    hword   t;
{
    check_full_save_stack();
    save_type(save_ptr) = INSERT_TOKEN;
    save_level(save_ptr) = LEVEL_ZERO;
    save_index(save_ptr) = t;
    incr(save_ptr);
}

unsave ()
{
    qword   l;
    ptr     p;
    hword   t;

    if (cur_level > LEVEL_ONE) {
        decr(cur_level);
        loop {
            decr(save_ptr);
            if (save_type(save_ptr) == LEVEL_BOUNDARY)  
                break;
            p = save_index(save_ptr);
            if (save_type(save_ptr) == INSERT_TOKEN) {
                t = cur_tok;
                cur_tok = p;
                back_input();
                cur_tok = t;
            } else {
                if (save_type(save_ptr) == RESTORE_OLD_VALUE) {
                    l = save_level(save_ptr);
                    decr(save_ptr);
                } else 
                    save_stack[save_ptr] = eqtb[UNDEFINED_CONTROL_SEQUENCE];
#ifdef STAT
                if (p < INT_BASE) {
                    if (eq_level(p) == LEVEL_ONE) {
                        eq_destroy(save_stack[save_ptr]);
                        if (tracing_restores > 0)
                            restore_trace(p, "retaining");
                    } else {
                        eq_destroy(eqtb[p]);
                        eqtb[p] = save_stack[save_ptr];
                        if (tracing_restores > 0)
                            restore_trace(p, "restoring");
                    }
                } else if (xeq_level[p - INT_BASE] != LEVEL_ONE) {
                    eqtb[p] = save_stack[save_ptr];
                    xeq_level[p - INT_BASE] = l;
                    if (tracing_restores > 0)
                        restore_trace(p, "restoring");
                } else {
                    if (tracing_restores > 0)
                        restore_trace(p, "retaining");
                }
#else
                if (p < INT_BASE) {
                    if (eq_level(p) == LEVEL_ONE)
                        eq_destroy(save_stack[save_ptr]);
                    else {
                        eq_destroy(eqtb[p]);
                        eqtb[p] = save_stack[save_ptr];
                    }
                } else if (xeq_level[p - INT_BASE] != LEVEL_ONE) {
                    eqtb[p] = save_stack[save_ptr];
                    xeq_level[p - INT_BASE] = l;
                }
#endif
            }
        }
        cur_group = save_level(save_ptr);
        cur_boundary = save_index(save_ptr);
    } else
        confusion("curlevel");
}

off_save ()
{   
    ptr     p;

    if (cur_group == BOTTOM_LEVEL) {
        print_err("Extra ");
        print_cmd_chr(cur_cmd, cur_chr);
        help_offsave_xtra();
        error();
    } else {
        back_input();
        p = new_token();
        token_link(temp_toks) = p;
        print_err("Missing ");
        switch (cur_group) 
        {
        case SEMI_SIMPLE_GROUP:
            token(p) = CS_TOKEN_FLAG + FROZEN_END_GROUP;
            print_esc("groupend");
            break;
        
        case MATH_SHIFT_GROUP:
            token(p) = MATH_SHIFT_TOKEN + '$';
            print_char('$');
            break;
        
        case MATH_LEFT_GROUP:
            token(p) = CS_TOKEN_FLAG + FROZEN_RIGHT;
            token_link(p) = new_token();
            p = token_link(p);
            token(p) = OTHER_TOKEN + '.';
            print_esc("right.");
            break;
        
        default:
            token(p) = RIGHT_BRACE_TOKEN + '}';
            print_char('}');
            break;
        }
        print(" inserted");
        ins_list(token_link(temp_toks));
        help_offsave_missing();
        error();
    }
}

#ifdef STAT
restore_trace (p, s)
    ptr     p;
    char*   s;
{
    begin_diagnostic();
    print_char('{');
    print(s);
    print_char(' ');
    show_eqtb(p);
    print_char('}');
    end_diagnostic(FALSE);
}
#endif

/*
 *  Help text
 */

help_offsave_xtra ()
{   
    help1("Things are pretty mixed up, but I think the worst is over.");
}

help_offsave_missing ()
{
    help5("I've inserted something that you may have forgotten.",
    "(See the <inserted text> above.)",
    "With luck, this will get me unwedged. But if you",
    "really didn't forget anything, try typing `2' now; then",
    "my insertion and my current dilemma will both disappear.");
}
