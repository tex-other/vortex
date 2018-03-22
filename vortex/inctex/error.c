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
/* @(#)error.c 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */


static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"file.h"


#ifdef INCTEX

#include	"Imain.h"

extern	int     OK_to_interrupt;
extern	bool    deletions_allowed;
extern	int     error_count;
extern	chrs    help_line[6];
extern	ptr     help_ptr;
extern	int     history;
extern	int     interaction;
extern	int     interrupt;
extern	int     old_setting;
extern	bool    use_err_help;

#else

int		OK_to_interrupt;
bool		deletions_allowed;
int		error_count;
chrs		help_line[6];
ptr		help_ptr;
int		history;
int		interaction;
int		interrupt;
int		old_setting;
bool		use_err_help;

#endif


#ifdef INCTEX

jump_out ()
{
	close_files_and_terminate(FALSE);
	if (incremental)
		wrapup_inc();
	close_out(history);
}

#else

jump_out ()
{
    close_files_and_terminate(FALSE);
}

#endif

begin_diagnostic ()
{
    old_setting = selector;
    if (tracing_online <= 0 && selector == TERM_AND_LOG) {
        decr(selector);
        if (history == SPOTLESS)
            history = WARNING_ISSUED;
    }
}

end_diagnostic (blank_line)
    bool    blank_line;
{
    print_nl("");
    if (blank_line) print_ln();
    selector = old_setting;
}

print_err (msg)
    chrs    msg;
{
    if (interaction == ERROR_STOP_MODE) 
        wake_up_terminal(); 
    print_nl("! ");
    print(msg);
}

error ()
{
    ascii   c;
    int     hx;
    hword   s1;
    hword   s2;
    hword   s3;
    val     s4;

    if (history < ERROR_MESSAGE_ISSUED)
        history = ERROR_MESSAGE_ISSUED;
    print_char('.');
    show_context();
    if (interaction == ERROR_STOP_MODE) {
        loop {
            clear_for_error_prompt();
            prompt_input("? ");
            if (last == first)
                return;
            c = buffer[first];
            if (c >= 'a')
                c -= 'a' - 'A';
            switch (c)
            {
            case '1': case '2': case '3':
            case '4': case '5': case '6':
            case '7': case '8': case '9':
                if (! deletions_allowed)
                    continue;
                s1 = cur_tok;
                s2 = cur_cmd;
                s3 = cur_chr;
                s4 = align_state;
                align_state = 1000000;
                OK_to_interrupt = FALSE;
                if (last > first + 1 &&
                    buffer[first + 1] >= '0' &&
                    buffer[first + 1] <= '9')
                    c = c * 10 + buffer[first + 1] - '0' * 11;
                else c -= '0';
                while (c > 0) {
                    get_token();
                    decr(c);
                }
                cur_tok = s1;
                cur_cmd = s2;
                cur_chr = s3;
                align_state = s4;
                OK_to_interrupt = TRUE;
                help_delete_text();
                show_context();
                continue;
            
            case 'H':
                if (use_err_help)  {
                    give_err_help();
                    use_err_help = FALSE;
                } else {
                    if (help_ptr == 0)
                        help_no_help();
                    else for (hx = 0; hx < help_ptr; incr(hx)) {
                        print(help_line[hx]);
                        print_ln();
                    }
                    help_help();
                }
                continue;
                
            case 'I':
                begin_file_reading();
                if (last > first + 1) {
                    loc = first + 1;
                    buffer[first] = ' ';
                } else {
                    prompt_input("insert>");
                    loc = first;
                }
                first = last;
                limit = last - 1;
                return;
            
            case 'Q':
            case 'R':
            case 'S':
                error_count = 0;
                interaction = BATCH_MODE + c - 'Q';
                print("OK, entering ");
                switch (c)
                {
                case 'Q':
                    print_esc("batchmode");
                    decr(selector);
                    break;
                
                case 'R':
                    print_esc("nonstopmode");
                    break;
                
                case 'S':
                    print_esc("scrollmode");
                    break;
                }
                print("...");
                print_ln();
                update_terminal();
                return;
            
            case 'E':
                if (base_ptr > 0)
                    close_files_and_terminate(TRUE);
                break;

            case 'X':
                interaction = SCROLL_MODE;
                jump_out();
                break;

            default:
                print_menu();
                continue;
            }
        }
    }
    incr(error_count);
    if (error_count == 100) {
        print_nl("(That makes 100 errors; please try again.)");
        history = FATAL_ERROR_STOP;
        jump_out();
    }
    if (interaction > BATCH_MODE)
        decr(selector);
    if (use_err_help) {
        print_ln();
        give_err_help();
    } else for (hx = 0; hx < help_ptr; incr(hx)) {
        print_nl(help_line[hx]);
    }
    help_ptr = 0;
    print_ln();
    if (interaction > BATCH_MODE)
        incr(selector);
    print_ln();
}

print_menu ()
{
    print("Type <return> to proceed, S to scroll future error messages,");
    print_nl("R to run without stopping, Q to run quietly,");
    print_nl("I to insert something, ");
    if (base_ptr > 0)
        print("E to edit your file,");
    if (deletions_allowed)
        print_nl("1 or ... or 9 to ignore the next 1 to 9 tokens of input");
    print_nl("H for help, X to quit.");
}

int_error (v)
    val     v;
{
    print(" (");
    print_val(v);
    print_char(')');
    error();
}

normalize_selector ()
{
    if (job_name > 0)
        selector = TERM_AND_LOG;
    else selector = TERM_ONLY;
    if (job_name == 0)
        open_log_file();
    if (interaction == BATCH_MODE)
        decr(selector);
}

fatal_error (s)
    chrs    s;
{
    normalize_selector();
    print_err("Emergency stop");
    help1(s);
    succumb();
}

overflow (s, n)
    chrs    s;
    int     n;
{
    normalize_selector();
    print_err("TeX capacity exceeded, sorry [");
    print(s);
    print_char('=');
    print_int(n);
    print_char(']');
    help_capacity();
    succumb();
}

confusion (s)
    chrs    s;
{
    normalize_selector();
    if (history < ERROR_MESSAGE_ISSUED) {
        print_err("This can't happen (");
        print(s);
        print_char(')');
        help_broken();
    } else {
        print_err("I can't go on meeting you like this");
        help_wounded();
    }
    succumb();
}

pause_for_instructions ()
{
    if (OK_to_interrupt) {
        interaction = ERROR_STOP_MODE;
        if (selector == LOG_ONLY || selector == NO_PRINT)
            incr(selector);
        print_err("Interruption");
        help_interrupt();
        deletions_allowed = FALSE;
        error();
        deletions_allowed = TRUE;
        interrupt = 0;
    }
}

init_error ()
{
    OK_to_interrupt = TRUE;
    deletions_allowed = TRUE;
    interaction = ERROR_STOP_MODE;
    use_err_help = FALSE;
}

/*
 *  Help text
 */

help_delete_text () 
{
    help2("I have just deleted some text, as you asked.", 
    "You can now delete more, or insert, or whatever.");
}

help_no_help () 
{
    help2("Sorry, I don't know how to help in this situation.",
    "Maybe you should try asking a human?");
}

help_help () 
{
    help4("Sorry, I already gave what help I could...",
    "Maybe you should try asking a human?", 
    "An error might have occurred before I noticed any problems.",
    "``If all else fails, read the instructions.''");
}

help_capacity () 
{
    help2("If you really absolutely need more capacity,",
    "you can ask a wizard to enlarge me.");
}

help_broken () 
{
    help1("I'm broken. Please show this to someone who can fix can fix");
}

help_wounded () 
{
    help2("One of your faux pas seems to have wounded me deeply...",
    "in fact, I'm barely conscious. Please fix it and try again.");
}

help_interrupt () 
{
    help3("You rang?",
    "Try to insert some instructions for me (e.g., `I\\showlists),",
    "unless you just want to quit by typing `X'.");
}
