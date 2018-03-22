
/*
 * @(#)expand.c 2.9 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include	"tex.h"
#include	"box.h"
#include	"tokenstack.h"
#include	"scan.h"
#include	"token.h"
#include	"tokenlists.h"
#include	"cond.h"
#include	"file.h"
#include	"expand.h"

#ifdef INCTEX
extern	int     long_state;
extern	ptr     pstack[9];
extern	ptr     cur_mark[5];
#else
int		long_state;
ptr		pstack[9];
ptr		cur_mark[5];
#endif

get_x_token ()
{
restart:
    get_next();
    if (cur_cmd <= MAX_COMMAND)
        goto done;
    if (cur_cmd >= CALL) {
        if (cur_cmd < END_TEMPLATE) {
            macro_call();
        } else {
            cur_cs = FROZEN_ENDV;
            cur_cmd = ENDV;
            goto done;
        }
    } else {
        expand();
    }
    goto restart;

done:
    if (cur_cs == 0)
        cur_tok = cur_cmd * 0400 + cur_chr;
    else cur_tok = CS_TOKEN_FLAG + cur_cs;
}

expand ()
{
    int     j;
    ptr     p;
    ptr     q;
    ptr     r;
    tok     t;
    val     save_scanner_status;
    val     cv_backup = cur_val;
    int     radix_backup = radix;
    int     co_backup = cur_order;
    int     cvl_backup = cur_val_level;
    ptr     backup_backup = token_link(backup_tokens);  

    if (cur_cmd < CALL) {
        if (tracing_commands > 1)
            show_cur_cmd_chr();
        switch (cur_cmd)
        {
        case TOP_BOT_MARK:
            if (cur_mark[cur_chr] != NULL)
                begin_token_list(cur_mark[cur_chr], MARK_TEXT);
            break;

        case EXPAND_AFTER: 
            get_token();
            t = cur_tok;
            get_token();
            if (cur_cmd > MAX_COMMAND)
                expand();
            else back_input();
            cur_tok = t;
            back_input();
            break;

        case NO_EXPAND:
            save_scanner_status = scanner_status;
            scanner_status = NORMAL;
            get_token(); 
            scanner_status = save_scanner_status;
            t = cur_tok;
            back_input();
            if (t >= CS_TOKEN_FLAG) {
                p = new_token();
                token(p) = CS_TOKEN_FLAG + FROZEN_DONT_EXPAND;
                token_link(p) = loc;
                start = p;
                loc = p;
            }
            break;

        case CS_NAME:
            p = r = new_token();
            do {
                get_x_token();
                if (cur_cs == 0)
                    store_new_token(cur_tok);
            } while (cur_cs == 0);
            if (cur_cmd != END_CS_NAME) {
                print_err("Missing ");
                print_esc("endcsname");
                print(" inserted");
                help_cs();
                back_error();
            }
            j = first;
            p = token_link(r);
            while (p != NULL) {
                if (j >= max_buf_stack) {
                    max_buf_stack = j + 1;
                    if (max_buf_stack == BUF_SIZE)
                        overflow("buffer size", BUF_SIZE);
                }
                buffer[j] = token(p) % 0400;
                incr(j);
                p = token_link(p);
            }
            if (j > first + 1) {
                no_new_control_sequence = FALSE;
                cur_cs = id_lookup(first, j - first);
                no_new_control_sequence = TRUE;
            } else if (j == first) {
                cur_cs = NULL_CS;
            }
            else cur_cs = SINGLE_BASE + buffer[first];
            flush_list(r);
            if (eq_type(cur_cs) == UNDEFINED_CS)
                eq_define(cur_cs, RELAX, 256);
            cur_tok = cur_cs + CS_TOKEN_FLAG;
            back_input();
            break;
        
        case CONVERT:
            conv_toks();
            break;
        
        case THE:
            ins_the_toks();
            break;
        
        case IF_TEST:
            conditional();
            break;
        
        case FI_OR_ELSE:
            if (cur_chr > if_limit) {
                if (if_limit == IF_CODE) {
                    insert_relax();
                } else {
                    print_err("Extra ");
                    print_cmd_chr(FI_OR_ELSE, cur_chr);
                    help_extra_if();
                    error();
                }
            } else {
                while (cur_chr != FI_CODE)
                    pass_text();
                pop_cond();
            }
            break;

        case INPUT:
            if (cur_chr > 0)
                force_eof = TRUE;
            else if (name_in_progress)
                insert_relax();
            else start_input();
            break;
        
        default:
            print_err("Undefined control sequence");
            help_undefd();
            error();
            break;
        }
    } else if (cur_cmd < END_TEMPLATE) {
        macro_call();
    } else {
        cur_tok = CS_TOKEN_FLAG + FROZEN_ENDV;
        back_input();
    }
    cur_val = cv_backup;
    cur_val_level = cvl_backup;
    radix = radix_backup;
    cur_order = co_backup;
    token_link(backup_tokens) = backup_backup;
}

insert_relax ()
{
    cur_tok = CS_TOKEN_FLAG + cur_cs;
    back_input();
    cur_tok = CS_TOKEN_FLAG + FROZEN_RELAX;
    back_input();
    token_type = INSERTED;
}


macro_call ()
{
    int     m;
    int     n;
    ptr     p;
    ptr     q;
    ptr     r;
    ptr     s;
    ptr     t;
    ptr     u;
    ptr     v;
    ascii   match_chr;
    ptr     ref_count;
    ptr     rbrace_ptr;
    ptr     save_warning_index;
    val     save_scanner_status;
    int     unbalance;

    n = 0;
    ref_count = cur_chr;
    r = token_link(ref_count);
    save_scanner_status = scanner_status;
    save_warning_index = warning_index;
    warning_index = cur_cs;
    if (tracing_macros > 0) {
        begin_diagnostic();
        print_ln();
        print_cs(warning_index);
        token_show(ref_count);
        end_diagnostic(FALSE);
    }
    if (token(r) != END_MATCH_TOKEN) {
        scanner_status = MATCHING;
        unbalance = 0;
        long_state = eq_type(cur_cs);
        if (long_state >= OUTER_CALL)
            long_state -= 2;
        do {
            if (token(r) > MATCH_TOKEN + 127 ||
                token(r) < MATCH_TOKEN) {
                s = NULL;
            } else {
                match_chr = token(r) - MATCH_TOKEN;
                s = token_link(r);
                r = s;
                p = temp_toks;
                token_link(p) = NULL;
                m = 0;
            }

        contin:
            get_token();
            if (cur_tok == token(r)) {
                r = token_link(r);
                if (token(r) >= MATCH_TOKEN &&
                    token(r) <= END_MATCH_TOKEN) {
                    if (cur_tok < LEFT_BRACE_LIMIT)
                        decr(align_state);
                    goto found;
                } else {
                    goto contin;
                }
            }
            if (s != r) {
                if (s == NULL) {
                    print_err("Use of ");
                    sprint_cs(warning_index);
                    print(" doesn't match its definition");
                    help_match();
                    error();
                    goto local_exit;
                } else {
                    t = s;
                    do {
                        store_new_token(token(t));
                        incr(m);
                        u = token_link(t);
                        v = s;
                        loop {
                            if (u == r) {
                                if (cur_tok != token(v)) {
                                    break;
                                } else {
                                    r = token_link(v);
                                    goto contin;
                                }
                            }
                            if (token(u) != token(v))
                                break;
                            u = token_link(u);
                            v = token_link(v);
                        }
                        t = token_link(t);
                    } while (t != r);
                    r = s;
                }
            }
            if (cur_tok == par_token && long_state != LONG_CALL) {
                runaway_arg(n);
                align_state -= unbalance; 
                goto local_exit;
            }
            if (cur_tok < RIGHT_BRACE_LIMIT) {
                if (cur_tok < LEFT_BRACE_LIMIT) {
                    unbalance = 1;
                    loop {
                        fast_store_new_token(cur_tok);
                        get_token();
                        if (cur_tok == par_token && 
                            long_state != LONG_CALL) {
                            runaway_arg(n);
                            align_state -= unbalance; 
                            goto local_exit;
                        }
                        if (cur_tok < RIGHT_BRACE_LIMIT) {
                            if (cur_tok < LEFT_BRACE_LIMIT) {
                                incr(unbalance);
                            } else {
                                decr(unbalance);
                                if (unbalance == 0)
                                    break;
                            }
                        }
                    }
                    rbrace_ptr = p;
                    store_new_token(cur_tok);
                } else {
                    back_input();
                    print_err("Argument of ");
                    sprint_cs(warning_index);
                    print(" has an extra }");
                    help_match_xtra();
                    incr(align_state);
                    long_state = CALL;
                    cur_tok = par_token;
                    ins_error();
                }
            } else {
                if (cur_tok == SPACE_TOKEN &&
                    token(r) <= END_MATCH_TOKEN &&
                    token(r) >= MATCH_TOKEN)
                    goto contin;
                store_new_token(cur_tok);
            }
            incr(m);
            if (token(r) > END_MATCH_TOKEN || token(r) < MATCH_TOKEN)
                goto contin;

        found:
            if (s != NULL) {
                if (m == 1 &&
                    token(p) < RIGHT_BRACE_LIMIT &&
                    p != temp_toks) {
                    token_link(rbrace_ptr) = NULL;
                    free_token(p);
                    p = token_link(temp_toks);
                    pstack[n] = token_link(p);
                    free_token(p);
                } else {
                    pstack[n] = token_link(temp_toks);
                }
                incr(n);
                if (tracing_macros > 0) {
                    begin_diagnostic();
                    print_nl("");
                    print_char(match_chr);
                    print_int(n);
                    print("<-");
                    show_token_list(pstack[n - 1], NULL, 1000L);
                    end_diagnostic(FALSE);
                }
            }
        } while (token(r) != END_MATCH_TOKEN);
    }
    while (state == TOKEN_LIST && loc == NULL)
        end_token_list();
    begin_token_list(ref_count, MACRO);
    name = warning_index;
    loc = token_link(r);
    if (n > 0) {
        if (param_ptr + n > max_param_stack) {
            max_param_stack = param_ptr + n;
            if (max_param_stack >= PARAM_SIZE)
                overflow("parameter stack size", PARAM_SIZE);
        }
        for (m = 0; m < n; incr(m))
            param_stack[param_ptr + m] = pstack[m];
        param_ptr += n;
    }

local_exit:
    scanner_status = save_scanner_status; 
    warning_index = save_warning_index; 
}

runaway_arg (n) 
    int     n;
{
    int     m;

    if (long_state == CALL) { 
        runaway(); 
        print_err("Paragraph ended before "); 
        sprint_cs(warning_index); 
        print(" was complete"); 
        help_runaway(); 
        back_error();
    }
    pstack[n] = token_link(temp_toks); 
    for (m = 0; m <= n; incr(m))
        flush_list(pstack[m]);
}

x_token ()
{
    while (cur_cmd > MAX_COMMAND) {
        expand();
        get_next();
    }
    if (cur_cs == 0)
        cur_tok = cur_cmd * 0400 + cur_chr;
    else cur_tok = CS_TOKEN_FLAG + cur_cs;
}

/*
 *  Help text
 */

help_runaway ()
{
    help3("I suspect you've forgotten a `}', causing me to apply this",
    "control sequence to too much text. How can we recover?",
    "My plan is to forget the whole thing and hope for the best.");
}

help_match ()
{
    help4("If you say, e.g., `\\def\\a1{...}', then you must always",
    "put `1' after `\\a', since control sequence names are",
    "made up of letters only. The macro here has not been",
    "followed by the required stuff, so I'm ignoring it.");
}

help_match_xtra ()
{
    help6("I've run across a `}' that doesn't seem to match anything.",
    "For example, `\\def\\a#1{...}' and `\\a}' would produce",
    "this error. If you simply proceed now, the `\\par' that",
    "I've just inserted will cause me to report a runaway",
    "argument that might be the root of the problem. But if",
    "your `}' was spurious, just type `2' and it will go away.");
}

help_undefd ()
{
    help5("The control sequence at the end of the top line",
    "of your error message was never \\def'ed. If you have",
    "misspelled it (e.g., `\\hobx'), type `I' and the correct",
    "spelling (e.g., `I\\hbox'). Otherwise just continue,",
    "and I'll forget about whatever was undefined.");
}

help_cs ()
{
    help2("The control sequence marked <to be read again> should",
    "not appear between \\csname and \\endcsname.");
}

help_extra_if ()
{
    help1("I'm ignoring this; it doesn't match any \\if.");
}
