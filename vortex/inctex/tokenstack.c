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
/* @(#)tokenstack.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"box.h"
#include	"def.h"
#include	"token.h"
#include	"tokenlists.h"
#ifdef INCTEX
#include	"Imain.h"
#include	"tokenstack.h"
#else
#include	"tokenstack.h"
#endif

#ifdef INCTEX

extern	input   cur_input;
extern	input   input_stack[STACK_SIZE];
extern	ptr     input_ptr;
extern	ptr     max_in_stack;
extern	val     line;
extern	val     line_stack[MAX_IN_OPEN];
extern	ptr     in_open;
extern	ptr     param_stack[PARAM_SIZE];
extern	ptr     param_ptr;
extern	ptr     max_param_stack;
extern	val     align_state;
extern	ptr     base_ptr;
extern	ptr     def_ref;
extern	ptr     warning_index;
extern	int     scanner_status;

#else

input		cur_input;
input		input_stack[STACK_SIZE];
ptr		input_ptr;
ptr		max_in_stack;
val		line;
val		line_stack[MAX_IN_OPEN];
ptr		in_open;
ptr		param_stack[PARAM_SIZE];
ptr		param_ptr;
ptr		max_param_stack;
val		align_state			= 1000000;
ptr		base_ptr;
ptr		def_ref;
ptr		warning_index;
int		scanner_status;

#endif

alpha_file	 input_file[MAX_IN_OPEN];


#ifdef INCTEX

push_input()
{
	if (input_ptr > max_in_stack) {
		max_in_stack = input_ptr;
		if (input_ptr == STACK_SIZE)
			overflow("input stack size", STACK_SIZE);
	}
	cur_input.fp = f_cur;
	if (incremental) {
		if (f_cur == NULL)
			cur_input.fid = NIL;
		else
			cur_input.fid = f_cur->id;
	}
	input_stack[input_ptr] = cur_input;
	incr(input_ptr);
}

pop_input()
{
	decr(input_ptr);
	cur_input = input_stack[input_ptr];
	if (virgin)
		f_cur = cur_input.fp;
	else if (fnds != NULL)
		if (cur_input.fid<0)	/* added to catch eof error - DLP */
			f_cur = NULL;
		else
			f_cur = fnds[cur_input.fid];
	else
		f_cur == NULL;
}

#else

push_input()
{
    if (input_ptr > max_in_stack) {
        max_in_stack = input_ptr;
        if (input_ptr == STACK_SIZE)
            overflow("input stack size", STACK_SIZE);
    }
    input_stack[input_ptr] = cur_input;
    incr(input_ptr);
}

pop_input()
{
    decr(input_ptr);
    cur_input = input_stack[input_ptr];
}

#endif

begin_token_list (p, t)
    ptr     p;
    qword   t;
{
    push_input();
    state = TOKEN_LIST;
    start = p;
    token_type = t;
    if (t >= MACRO) {
        add_token_ref(p);
        if (t == MACRO) {
            param_start = param_ptr;
        } else {
            loc = token_link(p);
            if (tracing_macros > 1) {
                begin_diagnostic();
                print_nl("");
                switch (t)
                {
                case MARK_TEXT:
                    print_esc("mark");
                    break;

                case WRITE_TEXT:
                    print_esc("write");
                    break;

                default:
                    print_cmd_chr(
                        ASSIGN_TOKS,
                        t - OUTPUT_TEXT + OUTPUT_ROUTINE_LOC
                    );
                    break;
                }
                print("->");
                token_show(p);
                end_diagnostic(FALSE);
            }
        }
    } else {
        loc = p;
    }
}

end_token_list ()
{
    if (token_type >= BACKED_UP) {
        if (token_type <= INSERTED) {
            flush_list(start);
        } else {
            delete_token_ref(start);
            if (token_type == MACRO) {
                while (param_ptr > param_start) {
                    decr(param_ptr);
                    flush_list(param_stack[param_ptr]);
                }
            }
        }
    } else if (token_type == U_TEMPLATE)
        align_state = 0;
    pop_input();
    check_interrupt();
}

back_input ()
{
    ptr     p;

    while (state == TOKEN_LIST && loc == NULL)
        end_token_list();
    p = new_token();
    token(p) = cur_tok;
    if (cur_tok < RIGHT_BRACE_LIMIT)
        if (cur_tok < LEFT_BRACE_LIMIT)
            decr(align_state);
        else incr(align_state);
    push_input();
    state = TOKEN_LIST;
    start = p;
    token_type = BACKED_UP;
    loc = p;
}

back_error ()
{
    OK_to_interrupt = FALSE;
    back_input();
    OK_to_interrupt = TRUE;
    error();
}

ins_error ()
{
    OK_to_interrupt = FALSE;
    back_input();
    token_type = INSERTED; 
    OK_to_interrupt = TRUE;
    error();
}

clear_for_error_prompt ()
{
    while (state != TOKEN_LIST &&
        terminal_input &&
        input_ptr > 0 &&
        loc > limit) {
        end_file_reading();
    }
    print_ln();
    clear_terminal();
}

begin_file_reading ()
{
    if (in_open == MAX_IN_OPEN)
        overflow("text input levels",  MAX_IN_OPEN);
    if (first == BUF_SIZE)
        overflow("buffer_size", BUF_SIZE);
    incr(in_open);
    push_input();
    index = in_open;
    line_stack[index] = line;
    start = first;
    state = MID_LINE;
    name = 0;
}

end_file_reading ()
{
    first = start;
    line = line_stack[index];
    if (name > 17) a_close(cur_file);
    pop_input();
    decr(in_open);
}

runaway ()
{
    ptr     p;

    if (scanner_status > SKIPPING) {
        print_nl("Runaway ");
        switch (scanner_status) 
        {
        case DEFINING:
            print("definition");
            p = def_ref;
            break;

        case MATCHING:
            print("argument");
            p = temp_toks;
            break;

        case ALIGNING:
            print("preamble");
            p = align_tokens;
            break;

        case ABSORBING:
            print("text");
            p = def_ref;
            break;
        }
        print_char('?');
        print_ln();
        show_token_list(token_link(p), NULL, (val) ERROR_LINE - 10);
    }
}

#define begin_pseudoprint() \
    {l = tally; \
    tally = 0; \
    selector = PSEUDO; \
    trick_count = 1000000;}

show_context ()
{
    int     i;
    int     j;
    int     k;
    val     l;
    int     m;
    int     n;
    int     p;
    int     q;
    int     old_setting;

    base_ptr = input_ptr;
    input_stack[base_ptr] = cur_input;
    loop {
        cur_input = input_stack[base_ptr];
        if (base_ptr == input_ptr ||
            state != TOKEN_LIST ||
            token_type != BACKED_UP ||
            loc != NULL) {
            tally = 0;
            old_setting = selector;
            if (state != TOKEN_LIST) {
                if (name <= 17) {
                    if (terminal_input) {
                        if (base_ptr == 0)
                            print_nl("<*>");
                        else print_nl("<insert> ");
                    } else {
                        print_nl("<read ");
                        if (name == 17)
                            print_char('*');
                        else print_int(name - 1);
                        print_char('>');
                    }
                } else {
                    print_nl("l.");
                    print_val(line);
                }
                print_char(' ');
                begin_pseudoprint();
                if (buffer[limit] == end_line_char)
                    j = limit;
                else j = limit + 1;
                if (j > 0) {
                    for (i = start; i < j; incr(i)) {
                        if (i == loc)
                            set_trick_count();
                        print_str(buffer[i]);
                    }
                }
            } else {
                switch (token_type)
                {
                case PARAMETER:
                    print_nl("<argument> ");
                    break;

                case U_TEMPLATE:
                case V_TEMPLATE:
                    print_nl("<template> ");
                    break;

                case BACKED_UP:
                    if (loc == NULL)
                        print_nl("<recently read> "); 
                    else  print_nl("<to be read again> ");
                    break;

                case INSERTED:
                    print_nl("<inserted text> ");
                    break;

                case MACRO:
                    print_ln();
                    print_cs(name);
                    break;

                case OUTPUT_TEXT:
                    print_nl("<output> ");
                    break;

                case EVERY_PAR_TEXT:
                    print_nl("<everypar> ");
                    break;

                case EVERY_MATH_TEXT:
                    print_nl("<everymath> ");
                    break;

                case EVERY_DISPLAY_TEXT:
                    print_nl("<everydisplay> ");
                    break;

                case EVERY_HBOX_TEXT:
                    print_nl("<everyhbox> ");
                    break;

                case EVERY_VBOX_TEXT:
                    print_nl("<everyvbox> ");
                    break;

                case EVERY_JOB_TEXT:
                    print_nl("<everyjob >");
                    break;

                case EVERY_CR_TEXT:
                    print_nl("<everycr> ");
                    break;

                case MARK_TEXT:
                    print_nl("<mark> ");
                    break;

                case WRITE_TEXT:
                    print_nl("<write> ");
                    break;

                default:
                    print_nl("? ");
                    break;
                }
                begin_pseudoprint();
                if (token_type < MACRO)
                    show_token_list(start, loc, 100000);
                else show_token_list(token_link(start), loc, 100000);
            }
            selector = old_setting;
            if (trick_count == 1000000)
                set_trick_count();
            if (tally < trick_count)
                m = tally - first_count;
            else m = trick_count - first_count;
            if (l + first_count <= HALF_ERROR_LINE) {
                p = 0;
                n = l + first_count;
            } else {
                print("...");
                p = l + first_count - HALF_ERROR_LINE + 3;
                n = HALF_ERROR_LINE;
            }
            for (q = p; q < first_count; incr(q))
                print_char(trick_buf[q % ERROR_LINE]);
            print_ln();
            for (q = 1; q <= n; incr(q))
                print_char(' ');
            if (m + n <= ERROR_LINE)
                p = first_count + m;
            else p = first_count + ERROR_LINE - n - 3;
            for (q = first_count; q < p; q++)
                print_char(trick_buf[q % ERROR_LINE]);
            if (m + n > ERROR_LINE)
                print("...");
        }
        if (state != TOKEN_LIST && name > 17 || base_ptr == 0)
            break;
        decr(base_ptr);
    }
    cur_input = input_stack[input_ptr];
}
