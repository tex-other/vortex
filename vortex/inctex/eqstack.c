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
 * This file has been modified, with permission from Pat Monardo, for IncTeX
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/* @(#)eqstack.c 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

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
#else
ptr		save_ptr			= 0;
ptr		max_save_stack			= 0;
qword		cur_level			= LEVEL_ONE;
group		cur_group			= BOTTOM_LEVEL;
ptr		cur_boundary			= 0;
#endif

mword 		save_stack[SAVE_SIZE];


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
#ifdef INCTEX
            /* SHOULD use info(q), even says so in book, part #275. DLP */
            free_node(q, info(q) + info(q) + 1);
#else
            free_node(q, link(q) + link(q) + 1);
#endif
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
#ifdef  INCTEX
    before_eq(p);
#endif  INCTEX
    if (eq_level(p) == cur_level)
        eq_destroy(eqtb[p]);
    else if (cur_level > LEVEL_ONE)
        eq_save(p, eq_level(p));
    eq_level(p) = cur_level;
    eq_type(p) = t;
    equiv(p) = e;
#ifdef  INCTEX
    after_eq(p);
#endif  INCTEX
}

eq_word_define (p, w)
    ptr     p;
    val     w;
{
    if (xeq_level[p - INT_BASE] != cur_level) {
#ifdef  INCTEX
    	before_xeq(p - INT_BASE);
#endif  INCTEX
        eq_save(p, xeq_level[p - INT_BASE]);
        xeq_level[p - INT_BASE] = cur_level;
#ifdef  INCTEX
    	after_xeq( p - INT_BASE);
#endif  INCTEX
    }
#ifdef  INCTEX
    before_eq(p);
    eqtb[p].i = w;
    after_eq(p);
#else
    eqtb[p].i = w;
#endif  INCTEX
}

geq_define (p, t, e)
    ptr     p;
    qword   t;
    hword   e;
{
#ifdef  INCTEX
    before_eq(p);
#endif  INCTEX
    eq_destroy(eqtb[p]);
    eq_level(p) = LEVEL_ONE;
    eq_type(p) = t;
    equiv(p) = e;
#ifdef  INCTEX
    after_eq(p);
#endif  INCTEX
}

geq_word_define (p, w)
    ptr     p;
    val     w;
{
#ifdef  INCTEX
    before_eq(p);
    before_xeq(p - INT_BASE);
    eqtb[p].i = w;
    xeq_level[p - INT_BASE] = LEVEL_ONE;
    after_eq(p);
    after_xeq( p - INT_BASE);
#else
    eqtb[p].i = w;
    xeq_level[p - INT_BASE] = LEVEL_ONE;
#endif  INCTEX
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
#ifdef  INCTEX
		        before_eq(p);
#endif  INCTEX
                        eq_destroy(eqtb[p]);
                        eqtb[p] = save_stack[save_ptr];
                        if (tracing_restores > 0)
                            restore_trace(p, "restoring");
#ifdef  INCTEX
		        after_eq(p);
#endif  INCTEX
                    }
                } else if (xeq_level[p - INT_BASE] != LEVEL_ONE) {
#ifdef  INCTEX
	            before_eq(p);
    		    before_xeq(p - INT_BASE);
#endif  INCTEX
                    eqtb[p] = save_stack[save_ptr];
                    xeq_level[p - INT_BASE] = l;
#ifdef  INCTEX
		    after_eq(p);
    		    after_xeq( p - INT_BASE);
#endif  INCTEX
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
#ifdef  INCTEX
			before_eq(p);
#endif  INCTEX
                        eq_destroy(eqtb[p]);
                        eqtb[p] = save_stack[save_ptr];
#ifdef  INCTEX
			after_eq(p);
#endif  INCTEX
                    }
                } else if (xeq_level[p - INT_BASE] != LEVEL_ONE) {
#ifdef  INCTEX
		    before_eq(p);
    		    before_xeq(p - INT_BASE);
                    eqtb[p] = save_stack[save_ptr];
                    xeq_level[p - INT_BASE] = l;
		    after_eq(p);
    		    after_xeq( p - INT_BASE);
#else
                    eqtb[p] = save_stack[save_ptr];
                    xeq_level[p - INT_BASE] = l;
#endif  INCTEX
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
