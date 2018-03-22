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
/* @(#)token.c 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"scan.h"
#include	"box.h"
#include	"cond.h"
#include	"expand.h"
#include	"align.h"
#include	"tokenstack.h"
#include	"token.h"

#ifdef INCTEX
extern	tok     cur_tok;
extern	hword   cur_cmd;
extern	hword   cur_chr;
extern	ptr     cur_cs;
extern	short   end_save_globals;	/*!!-!!*/
#else
tok		cur_tok;
hword		cur_cmd;
hword		cur_chr;
ptr		cur_cs;
ptr	        tok_head;
ptr     	tok_low;
ptr     	tok_end;
int     	tok_used;
#endif INCTEX

/*
 *  This is the TeX segment
 */

tok     tok_mem[TOK_MAX-TOK_MIN+1];
ptr     tok_link[TOK_MAX-TOK_MIN+1];

ptr     par_loc;
tok     par_token;

bool    force_eof;

get_token ()
{
    no_new_control_sequence = FALSE;
    get_next();
    no_new_control_sequence = TRUE;
    if (cur_cs == 0)
        cur_tok = cur_cmd * 0400 + cur_chr;
    else cur_tok = CS_TOKEN_FLAG + cur_cs;
}

#define reduce_expanded_cc() \
    {if (buffer[k] == cur_chr && \
        cat == SUP_MARK && k < limit) { \
        cur_chr = buffer[k + 1]; \
        if (cur_chr < 0100) \
            buffer[k - 1] = cur_chr + 0100; \
        else buffer[k - 1] = cur_chr - 0100; \
        limit -= 2; \
        first -= 2; \
        while (k <= limit) { \
            buffer[k] = buffer[k + 2]; \
            incr(k);} \
        goto start_cs;}}
        
#define any_state(CAT) \
    case MID_LINE + CAT: \
    case SKIP_BLANKS + CAT: \
    case NEW_LINE + CAT
            
#define delims(CAT) \
    case MATH_SHIFT + CAT: \
    case TAB_MARK + CAT: \
    case MAC_PARAM + CAT: \
    case SUB_MARK + CAT: \
    case LETTER + CAT: \
    case OTHER_CHAR + CAT

#define mid_line(CAT) \
    case MID_LINE + CAT

#define new_line(CAT) \
    case NEW_LINE + CAT

#define skip_blanks(CAT) \
    case SKIP_BLANKS + CAT

get_next ()
{
    int     k;
    tok     t;
    int     cat;

restart:
    cur_cs = 0;
    if (state != TOKEN_LIST) {
reread:
        if (loc <= limit) {
            cur_chr = buffer[loc];
            incr(loc);
reswitch:
            cur_cmd = cat_code(cur_chr);
            switch (state + cur_cmd)
            {
            any_state(IGNORE):
            skip_blanks(SPACER):
            new_line(SPACER):
                goto reread;

            any_state(ESCAPE):
                if (loc > limit) {
                    cur_cs = NULL_CS;
                } else {
                start_cs: 
                    k = loc;
                    cur_chr = buffer[k];
                    cat = cat_code(cur_chr);
                    incr(k);
                    if (cat == LETTER)
                        state = SKIP_BLANKS;
                    else if (cat == SPACER)
                        state = SKIP_BLANKS;
                    else state = MID_LINE;
                    if (cat == LETTER && k <= limit) {
                        do {
                            cur_chr = buffer[k];
                            incr(k);
                            cat = cat_code(cur_chr);
                        } while (cat == LETTER && k <= limit);
                        reduce_expanded_cc();
                        if (cat != LETTER)
                            decr(k);
                        if (k > loc + 1) {
                            cur_cs = id_lookup(loc, k - loc);
                            loc = k;
                            goto found;
                        }
                    } else {
                        reduce_expanded_cc();
                    }
                    cur_cs = SINGLE_BASE + buffer[loc];
                    incr(loc);
                }
                found:
                cur_cmd = eq_type(cur_cs);
                cur_chr = equiv(cur_cs);
                if (cur_cmd >= OUTER_CALL)
                    check_outer_validity();
                break;
                        
            any_state(ACTIVE_CHAR):
                cur_cs = cur_chr + ACTIVE_BASE;
                cur_cmd = eq_type(cur_cs);
                cur_chr = equiv(cur_cs);
                state = MID_LINE;
                if (cur_cmd >= OUTER_CALL)
                    check_outer_validity();
                break;
            
            any_state(SUP_MARK):
                if (cur_chr == buffer[loc] && loc < limit) {
                    if (buffer[loc + 1] < 0100)
                        cur_chr = buffer[loc + 1] + 0100;
                    else cur_chr = buffer[loc + 1] - 0100;
                    loc += 2;
                    goto reswitch;
                }
                state = MID_LINE;
                break;

            any_state(INVALID_CHAR):
                print_err("Text line contains an invalid character");
                help_funny();
                deletions_allowed = FALSE;
                error();
                deletions_allowed = TRUE;
                goto restart;
            
            mid_line(SPACER):
                state = SKIP_BLANKS;
                cur_chr = ' ';
                break;
            
            mid_line(CAR_RET):
                loc = limit + 1;
                cur_cmd = SPACER;
                cur_chr = ' ';
                break;
            
            skip_blanks(CAR_RET):
            any_state(COMMENT):
                loc = limit + 1;
                goto reread;

            new_line(CAR_RET):
                loc = limit + 1;
                cur_cs = par_loc;
                cur_cmd = eq_type(cur_cs);
                cur_chr = equiv(cur_cs);
                if (cur_cmd >= OUTER_CALL)
                    check_outer_validity();
                break;
            
            mid_line(LEFT_BRACE):
                incr(align_state);
                break;

            skip_blanks(LEFT_BRACE):
            new_line(LEFT_BRACE):
                state = MID_LINE;
                incr(align_state);
                break;
            
            mid_line(RIGHT_BRACE):
                decr(align_state);
                break;
            
            skip_blanks(RIGHT_BRACE):
            new_line(RIGHT_BRACE):
                state = MID_LINE;
                decr(align_state);
                break;
            
            delims(SKIP_BLANKS):
            delims(NEW_LINE):
                state = MID_LINE;
                break;

            default:
                break;
            }
        } else {
            state = NEW_LINE; 
            if (name > 17) {
                incr(line);
                first = start;
                if (!force_eof) {
                    if (input_ln(cur_file, TRUE))
                        firm_up_the_line();
                    else force_eof = TRUE;
                }
                if (force_eof) {
                    print_char(')');
                    force_eof = FALSE;
                    update_terminal();
                    end_file_reading();
                    check_outer_validity();
                    goto restart;
                }
                if (end_line_char < 0 || end_line_char > 127)
                    decr(limit);
                else buffer[limit] = end_line_char;
                first = limit + 1;
                loc = start;
            } else {
                if (!terminal_input) {
                    cur_cmd = 0;
                    cur_chr = 0;
                    return;
                }
                if (input_ptr > 0) {
                    end_file_reading();
                    goto restart;
                }
                if (selector < LOG_ONLY)
                    open_log_file();
#ifdef TRIP
		/* for trip test */
                if (interaction >= NONSTOP_MODE) {
#else
                if (interaction > NONSTOP_MODE) {
#endif TRIP
                    if (end_line_char < 0 || end_line_char > 255)
                        incr(limit);
                    if (limit == start)
                        print_nl("(Please type a command or say `\\end')");
                    print_ln();
                    first = start;
                    prompt_input("*");
                    limit = last;
                    if (end_line_char < 0 || end_line_char > 127)
                        decr(limit);
                    else buffer[limit] = end_line_char;
                    first = limit + 1;
                    loc = start;
                } else
                    fatal_error(
                        "*** (job aborted, no legal \\end found)");
            }
            check_interrupt();
            goto reread;
        }
    } else {
        if (loc != NULL) {
            t = token(loc);
            loc = token_link(loc);
            if (t >= CS_TOKEN_FLAG) {
                cur_cs = t - CS_TOKEN_FLAG;
                cur_cmd = eq_type(cur_cs);
                cur_chr = equiv(cur_cs);
                if (cur_cmd >= OUTER_CALL) {
                    if (cur_cmd == DONT_EXPAND) {
                        cur_cs = token(loc) - CS_TOKEN_FLAG;
                        loc = NULL;
                        cur_cmd = eq_type(cur_cs);
                        cur_chr = equiv(cur_cs);
                        if (cur_cmd > MAX_COMMAND) {
                            cur_cmd = RELAX;
                            cur_chr = NO_EXPAND_FLAG;
                        }
                    } else {
                        check_outer_validity();
                    }
                }
            } else {
                cur_cmd = t / 0400;
                cur_chr = t % 0400;
                switch (cur_cmd)
                {
                case LEFT_BRACE:
                    incr(align_state);
                    break;

                case RIGHT_BRACE:
                    decr(align_state);
                    break;

                case OUT_PARAM:
                    begin_token_list(
                        param_stack[param_start + cur_chr - 1],
                        PARAMETER
                    );
                    goto restart;

                default:
                    break;
                }
            }
        } else {
            end_token_list();
            goto restart;
        }
    }
    if (cur_cmd <= CAR_RET &&
        cur_cmd >= TAB_MARK &&
        align_state == 0) {
        if (scanner_status == ALIGNING)
            fatal_error("(interwoven alignment preambles are not allowed)");
        cur_cmd = extra_info(cur_align);
        extra_info(cur_align) = cur_chr;
        if (cur_cmd == OMIT)
            begin_token_list(omit_template, (qword) V_TEMPLATE);
        else begin_token_list((ptr) v_part(cur_align), (qword) V_TEMPLATE);
        align_state = 1000000;
        goto restart;
    }
}

check_outer_validity ()
{
    ptr     p;
    ptr     q;

    if (scanner_status != NORMAL) {
        deletions_allowed = FALSE;
        if (cur_cs != 0) {
            if (state == TOKEN_LIST || name < 1 || name > 17) {
                p = new_token();
                token(p) = CS_TOKEN_FLAG + cur_cs;
                back_list(p);
            }
            cur_cmd = SPACER;
            cur_chr = ' ';
        }
        if (scanner_status > SKIPPING) {
            runaway();
            if (cur_cs == 0) {
                print_err("File ended");
            } else {
                cur_cs = 0;
                print_err("Forbidden control sequence found");
            }
            print(" while scanning ");
            p = new_token();
            switch (scanner_status)
            {
            case DEFINING:
                print("definition");
                token(p) = RIGHT_BRACE_TOKEN + '}';
                break;

            case MATCHING:
                print("use");
                token(p) = par_token;
                long_state = OUTER_CALL;
                break;

            case ALIGNING:
                print("preamble");
                token(p) = RIGHT_BRACE_TOKEN + '}';
                q = p;
                p = new_token();
                token_link(p) = q;
                token(p) = CS_TOKEN_FLAG + FROZEN_CR;
                align_state = -1000000;
                break;

            case ABSORBING:
                print("text"); 
                token(p) = RIGHT_BRACE_TOKEN + '}';
                break;
            }
            ins_list(p);
            print(" of ");
            sprint_cs(warning_index);
            help_scanner();
            error();
        } else {
            print_err("Incomplete ");
            print_cmd_chr(IF_TEST, cur_if);
            print("; all text was ignored after line ");
            print_val(skip_line);
            help_skif();
            if (cur_cs != 0)
                cur_cs = 0;
            else
                help_line[0] = 
                    "The file ended while I was skipping conditional text.";
            cur_tok = CS_TOKEN_FLAG + FROZEN_FI;
            ins_error();
        }
        deletions_allowed = TRUE;
    }
}

firm_up_the_line ()
{
    int     k;

    limit = last;
    if (pausing > 0 && interaction > NONSTOP_MODE) {
        wake_up_terminal();
        print_ln();
        if (start < limit) {
            for (k = start; k < limit; incr(k))
                print_char(buffer[k]);
        }
        first = limit;
        prompt_input("=>");
        if (last > first) {
            for (k = first; k < last; incr(k))
                buffer[k + start - first] = buffer[k];
            limit = start + last - first;
        }
    }
}

ptr
new_token ()
{
    ptr     p;

    p = tok_head;
    if (p != NULL) {
        tok_head = token_link(tok_head);
    } else if (tok_end < TOK_MAX) {
        incr(tok_end);
        p = tok_end;
    } else {
        decr(tok_low);
        p = tok_low;
        if (tok_low <= TOK_MIN) {
            runaway();
            overflow("token memory size", TOK_MAX - TOK_MIN + 1);
        }
    }
    token_link(p) = NULL;
#ifdef STAT
    incr(tok_used);
#endif
    return p;
}

init_tok_mem()
{
    int     i;

#ifdef INIT
    tok_head = NULL;
    tok_end = TOK_TOP;
    tok_low = tok_high;
    for (i = TOK_TOP; i > tok_low; decr(i)) {
        token(i) = 0;
        token_link(i) = NULL;
    }
    token(omit_template) = END_TEMPLATE_TOKEN;
    tok_used = tok_usage;
#endif

}

/*
 *  Help text
 */

help_scanner ()
{
    help4("I suspect you have forgotten a `}', causing me",
    "to read past where you wanted me to stop.",
    "I'll try to recover; but if the error is serious,",
    "you'd better type `E' or `X' now and fix your file.");
}

help_funny ()
{
    help2("A funny symbol that I can't read has just been input.",
    "Continue, and I'll forget that it ever happened.");
}

help_skif ()
{
    help3("A forbidden control sequence occurred in skipped text.",
    "This kind of error happens when you say `\\if...' and forget",
    "the matching `\\fi'. I've inserted a `\\fi'; this might work.");
}
