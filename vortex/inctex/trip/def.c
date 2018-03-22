/*
 *
 * @(#)def.c 2.7 EPA
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
#include	"eqstack.h"
#include	"evalstack.h"
#include	"hash.h"
#include	"token.h"
#include	"scan.h"
#include	"tokenstack.h"
#include	"expand.h"
#include	"box.h"
#include	"boxlists.h"
#include	"tokenlists.h"
#include	"file.h"
#include	"tfm.h"
#include	"dvi.h"
#include	"page.h"
#include	"def.h"

#ifdef INCTEX
extern	hword   after_token;
extern	bool    long_help_seen;
extern	val     mag_set;
#else
hword		after_token;
bool		long_help_seen;
val		mag_set;
#endif

#define glob    (a >= 4)

#define def(code, type, value) \
    {if (glob) \
        geq_define(code, type, value); \
    else eq_define(code, type, value);}

#define word_def(code, value) \
    {if (glob) \
        geq_word_define(code, value); \
    else eq_word_define(code, value);}

get_r_token ()
{
restart:
    do get_token();
    while (cur_tok == SPACE_TOKEN);
    if (cur_cs == 0 || cur_cs > FROZEN_CONTROL_SEQUENCE) {
        print_err("Missing control sequence inserted");
        help_missing_cs();
        if (cur_cs == 0) back_input();
        cur_tok = CS_TOKEN_FLAG + FROZEN_PROTECTION;
        ins_error();
        goto restart;
    }
}

prefixed_command ()
{
    int     a;
    bool    e;
    fnt     f;
    int     j;
    int     k;
    val     n;
    ptr     p;
    ptr     q;
    ptr     r;

    a = 0;
    while (cur_cmd == PREFIX) {
        if (!odd(a / cur_chr))
            a += cur_chr;
        get_nbrx_token();
        if (cur_cmd <= MAX_NON_PREFIXED_COMMAND) {
            print_err("You can't use a prefix with `");
            print_cmd_chr(cur_cmd, cur_chr);
            print("'");
            help_prefix();
            back_error();
            return;
        }
    }

    if (cur_cmd != DEF && a % 4 != 0) {
        print_err("You can't use `");
        print_esc("long");
        print("' or `");
        print_esc("outer");
        print("' with `");
        print_cmd_chr(cur_cmd, cur_chr);
        print("'");
        help_pref();
        error();
    }

    if (global_defs != 0) {
        if (global_defs < 0) {
            if (glob) a -= 4;
        } else {
            if (!glob) a += 4;
        }
    }

    switch (cur_cmd)
    {
    case SET_FONT:
        def(CUR_FONT_LOC, DATA, cur_chr);
        break;

    case DEF:
        if (odd(cur_chr) && !glob && global_defs >= 0)
            a += 4;
        e = (cur_chr >= 2);
        get_r_token();
        p = cur_cs;
        scan_toks(TRUE, e);
        def(p, CALL + (a % 4), def_ref);
        break;

    case LET:
        n = cur_chr;
        get_r_token();
        p = cur_cs;
        if (n == NORMAL) {
            do  get_token();
            while (cur_cmd == SPACER);
            if (cur_tok == OTHER_TOKEN + '=') {
                get_token();
                if (cur_cmd == SPACER)
                    get_token();
            }
        } else {
            get_token();
            q = cur_tok;
            get_token();
            back_input();
            cur_tok = q;
            back_input();
        }
        if (cur_cmd >= CALL)
            add_token_ref(cur_chr);
        def(p, cur_cmd, cur_chr);
        break;
    
    case SHORTHAND_DEF:
        n = cur_chr;
        get_r_token();
        p = cur_cs; 
        def(p, RELAX, 256);
        scan_optional_equals();
        switch ((hword) n)
        {
        case CHAR_DEF_CODE:
            scan_char_num();
            def(p, CHAR_GIVEN, (hword) cur_val);
            break;

        case MATH_CHAR_DEF_CODE:
            scan_fifteen_bit_int();
            def(p, MATH_GIVEN, (hword) cur_val);
            break;

        default:
            scan_eight_bit_int();
            switch ((hword) n)
            {
            case COUNT_DEF_CODE:
                def(p, ASSIGN_INT, (hword) (COUNT_BASE + cur_val));
                break;

            case DIMEN_DEF_CODE:
                def(p, ASSIGN_DIMEN, (hword) (SCALED_BASE + cur_val));
                break;

            case SKIP_DEF_CODE:
                def(p, ASSIGN_GLUE, (hword) (SKIP_BASE + cur_val));
                break;

            case MU_SKIP_DEF_CODE:
                def(p, ASSIGN_MU_GLUE, (hword) (MU_SKIP_BASE + cur_val));
                break;

            case TOKS_DEF_CODE:
                def(p, ASSIGN_TOKS, (hword) (TOKS_BASE + cur_val));
                break;
            }
            break;
        }
        break;

    case READ_TO_CS:
        scan_int();
        n = (int) cur_val;
        if (!scan_keyword("to")) {
            print_err("Missing `to' inserted");
            help_read_to();
            error();
        }
        get_r_token();
        p = cur_cs;
        read_toks(n, p);
        def(p, CALL, (hword) cur_val);
        break;
    
    case TOKS_REGISTER:
    case ASSIGN_TOKS:
        q = cur_cs;
        if (cur_cmd == TOKS_REGISTER) {
            scan_eight_bit_int();
            p = TOKS_BASE + cur_val;
        } else p = cur_chr;
        scan_optional_equals();
        get_nbrx_token();
        if (cur_cmd != LEFT_BRACE) {
            if (cur_cmd == TOKS_REGISTER) {
                scan_eight_bit_int();
                cur_cmd = ASSIGN_TOKS;
                cur_chr = TOKS_BASE + cur_val;
            }
            if (cur_cmd == ASSIGN_TOKS) {
                q = equiv(cur_chr);
                if (q == NULL) {
                    def(p, UNDEFINED_CS, NULL);
                } else {    
                    add_token_ref(q);
                    def(p, CALL, q);
                }
                break;
            }
        }
        back_input();
        cur_cs = q;
        q = scan_toks(FALSE, FALSE);
        if (token_link(def_ref) == NULL) {
            def(p, UNDEFINED_CS, NULL);
            free_token(def_ref);
        } else {
            if (p == OUTPUT_ROUTINE_LOC) {
                token_link(q) = new_token();
                q = token_link(q);
                token(q) = RIGHT_BRACE_TOKEN + '}';
                q = new_token();
                token(q) = LEFT_BRACE_TOKEN + '{';
                token_link(q) = token_link(def_ref);
                token_link(def_ref) = q;
            }
            def(p, CALL, def_ref);
        }
        break;

    case ASSIGN_INT:
        p = cur_chr;
        scan_optional_equals();
        scan_int();
        word_def(p, cur_val); 
        break;
    
    case ASSIGN_DIMEN:
        p = cur_chr;
        scan_optional_equals();
        scan_normal_dimen();
        word_def(p, cur_val); 
        break;
    
    case ASSIGN_GLUE:
    case ASSIGN_MU_GLUE:
        p = cur_chr;
        n = cur_cmd;
        scan_optional_equals();
        if (n == ASSIGN_MU_GLUE)
            scan_glue(MU_VAL);
        else scan_glue(GLUE_VAL);
        trap_zero_glue();
        def(p, GLUE_REF, (ptr) cur_val); 
        break;
    
    case DEF_CODE:
        if (cur_chr == CAT_CODE_BASE)
            n = MAX_CHAR_CODE;
        else if (cur_chr == MATH_CODE_BASE)
            n = 0100000;
        else if (cur_chr == SF_CODE_BASE)
            n = 077777;
        else if (cur_chr == DEL_CODE_BASE)
            n = 077777777;
        else n = 127;
        p = cur_chr;
        scan_seven_bit_int();
        p += cur_val;
        scan_optional_equals();
        scan_int(); 
        if (cur_val < 0 && p < DEL_CODE_BASE || cur_val > n) {
            print_err("Invalid code (");
            print_val(cur_val);
            if (p < DEL_CODE_BASE)
                print("), should be in the range 0..");
            else print("), should be at most ");
            print_val(n);
            help_code();
            error();
            cur_val = 0;
        }
        if (p < MATH_CODE_BASE) {
            def(p, DATA, (hword) cur_val);
        } else if (p < DEL_CODE_BASE) {
            def(p, DATA, (hword) hi(cur_val));
        } else word_def(p, cur_val);
        break;
    
    case DEF_FAMILY:
        p = cur_chr;
        scan_four_bit_int();
        p += cur_val;
        scan_optional_equals();
        scan_font_ident();
        def(p, DATA, (hword) cur_val);
        break;

    case REGISTER:
    case ADVANCE:
    case MULTIPLY:
    case DIVIDE:
        do_register_command(a);
        break;
    
    case SET_BOX:
        scan_eight_bit_int();
        if (glob)
            saved(0) = BOX_FLAG + 256 + cur_val;
        else saved(0) = BOX_FLAG + cur_val;
        scan_optional_equals();
        scan_box();
        break;
    
    case SET_AUX:
        alter_aux();
        break;

    case SET_PREV_GRAF:
        alter_prev_graf();
        break;
        
    case SET_PAGE_DIMEN:
        alter_page_so_far();
        break;

    case SET_PAGE_INT:
        alter_integer();
        break;

    case SET_BOX_DIMEN:
        alter_box_dimen();
        break;

    case SET_SHAPE:
        scan_optional_equals();
        scan_int();
        n = cur_val;
        if (n <= 0)
            p = NULL;
        else {
            p = get_node(2 * n + 1);
            info(p) = n;
            for (j = 1; j <= n; incr(j)) {
                scan_normal_dimen();
                mem[p + 2 * j - 1].sc = cur_val;
                scan_normal_dimen();
                mem[p + 2 * j].sc = cur_val;
            }
        }
        def(PAR_SHAPE_LOC, SHAPE_REF, p);
        break;
    
    case HYPH_DATA:
#ifdef  INIT
        if (cur_chr == 1) 
            new_patterns();
#else
        if (cur_chr == 1) {
            print_err("Patterns can only be loaded by INITEX");
            error();
        }
#endif
        else new_hyph_exceptions();
        break;
    
    case ASSIGN_FONT_DIMEN:
        find_font_dimen(TRUE);
        k = cur_val;
        scan_optional_equals();
        scan_normal_dimen();
        font_info[k].sc = cur_val;
        break;
    
    case ASSIGN_FONT_INT:
        n = cur_chr;
        scan_font_ident();
        f = cur_val;
        scan_optional_equals();
        scan_int(); 
#ifdef INCTEX
        if (n == 0) {
            hyphen_char[f] = cur_val;
	    if (total_pages > 1)  /* save changed entry. DLP */
		record_mem_change(&hyphen_char[f]);
	} else {
            skew_char[f] = cur_val;
	    if (total_pages > 1)  /* save changed entry. DLP */
		record_mem_change(&skew_char[f]);
	}
#else
        if (n == 0)
            hyphen_char[f] = cur_val;
        else skew_char[f] = cur_val;
#endif INCTEX
        break;

    case DEF_FONT:
        new_font(a);
        break;

    case SET_INTERACTION:
        new_interaction();
        break;

    default:
        confusion("prefix");
        break;
    }
    if (after_token != 0) {
        cur_tok = after_token;
        back_input();
        after_token = 0;
    }
} 

trap_zero_glue()
{
    if (width(cur_val) == 0 &&
        stretch(cur_val) == 0 &&
        shrink(cur_val) == 0) {
        add_glue_ref(zero_glue);
        delete_glue_ref(cur_val);
        cur_val = zero_glue;
    }
}

do_register_command (a)
    int     a;
{
    ptr     l;
    ptr     p;
    ptr     q;
    ptr     r;
    ptr     s;

    q = cur_cmd;
    if (q != REGISTER) {
        get_x_token();
        if (cur_cmd >= ASSIGN_INT && cur_cmd <= ASSIGN_MU_GLUE) {
            l = cur_chr;
            p = cur_cmd - ASSIGN_INT;
            goto found;
        }
        if (cur_cmd != REGISTER) {
            print_err("You can't use `");
            print_cmd_chr(cur_cmd, cur_chr);
            print("' after ");
            print_cmd_chr(q, 0);
            help_register();
            error();
            return;
        }
    }
    p = cur_chr;
    scan_eight_bit_int();
    switch (p) 
    {
    case INT_VAL:
        l = cur_val + COUNT_BASE;
        break;

    case DIMEN_VAL:
        l = cur_val + SCALED_BASE;
        break;
        
    case GLUE_VAL:
        l = cur_val + SKIP_BASE;
        break;
        
    case MU_VAL:
        l = cur_val + MU_SKIP_BASE;
        break;
    }

found:
    if (q == REGISTER)
        scan_optional_equals();
    else scan_keyword("by");
    arith_error = FALSE;
    if (q < MULTIPLY)  {
        if (p < GLUE_VAL) {
            if (p == INT_VAL)
                scan_int();
            else scan_normal_dimen();
            if (q == ADVANCE)
                cur_val += eqtb[l].i;
        } else {
            scan_glue(p);
            if (q == ADVANCE) {
                q = new_spec(cur_val);
                r = equiv(l);
                delete_glue_ref(cur_val);
                width(q) += width(r);
                if (stretch(q) == 0)
                    stretch_order(q) = NORMAL;
                if (stretch_order(q) == stretch_order(r)) {
                    stretch(q) += stretch(r);
                } else if (stretch_order(q) < stretch_order(r) && stretch(r)) {
                    stretch(q) = stretch(r);
                    stretch_order(q) = stretch_order(r);
                }
                if (shrink(q) == 0)
                    shrink_order(q) = NORMAL;
                if (shrink_order(q) == shrink_order(r)) {
                    shrink(q) += shrink(r);
                } else if (shrink_order(q) < shrink_order(r) && shrink(r)) {
                    shrink(q) = shrink(r); 
                    shrink_order(q) = shrink_order(r);
                }
                cur_val = q;
            }
        }
    } else {
        scan_int();
        if (p < GLUE_VAL) {
            if (q == MULTIPLY)
                cur_val = nx_plus_y(eqtb[l].i, cur_val, 0L);
            else cur_val = x_over_n(eqtb[l].i, cur_val);
        } else {
            s = equiv(l);
            r = new_spec(s);
            if (q == MULTIPLY) {
                width(r) = nx_plus_y(width(s), cur_val, 0L);
                stretch(r) = nx_plus_y(stretch(s), cur_val, 0L);
                shrink(r) = nx_plus_y(shrink(s), cur_val, 0L);
            } else {
                width(r) = x_over_n(width(s), cur_val);
                stretch(r) = x_over_n(stretch(s), cur_val);
                shrink(r) = x_over_n(shrink(s), cur_val);
            }
            cur_val = r;
        }
    }
    if (arith_error) {
        print_err("Arithmetic overflow");
        help_overflow();
        error();
        return;
    }
    if (p < GLUE_VAL) {
        word_def(l, cur_val);
    } else {
        trap_zero_glue();
        def(l, GLUE_REF, (ptr) cur_val);
    }
}

alter_aux()
{
    hword   c;

    if (cur_chr != abs(mode)) {
        report_illegal_case();
    } else {
        c = cur_chr;
        scan_optional_equals();
        if (c == VMODE)  {
            scan_normal_dimen();
            prev_depth = cur_val;
        } else {
            scan_int();
            if (cur_val <= 0 || cur_val > 32767) {
                print_err("Bad space factor");
                help_space_factor();
                int_error(cur_val);
            } else {
                space_factor = cur_val;
            }
        }
    }
}

alter_prev_graf ()
{
    ptr     p;

    nest[nest_ptr] = cur_list;
    p = nest_ptr;
    while (abs(nest[p].mode_field) != VMODE)    
        decr(p);
    scan_optional_equals();
    scan_int();
    if (cur_val < 0) {
        print_err("Bad ");
        print_esc("prevgraf");
        help_prevgraf();
        int_error(cur_val);
    } else {
        nest[p].pg_field = cur_val;
        cur_list = nest[nest_ptr];
    }
}

alter_page_so_far ()
{
    hword   c;

    c = cur_chr;
    scan_optional_equals(); 
    scan_normal_dimen();
    page_so_far[c] = cur_val;
}

alter_integer ()
{
    hword   c;
    
    c = cur_chr;
    scan_optional_equals();
    scan_int();
    if (c == 0)
        dead_cycles = cur_val;
    else insert_penalties = cur_val;
}

alter_box_dimen ()
{
    hword   b;
    hword   c;
    
    c = cur_chr;
    scan_eight_bit_int();
    b = cur_val; 
    scan_optional_equals();
    scan_normal_dimen();
    if (box(b) != NULL)
        mem[box(b) + c].sc = cur_val;
}

new_font (a)
    int     a;
{
    fnt     f;
    scal    s;
    str     t;
    ptr     u;
    int     old_setting;

    if (job_name == 0)
        open_log_file();
    get_r_token();
    u = cur_cs;
    if (u >= HASH_BASE) {
        t = text(u);
    } else if (u >= SINGLE_BASE) {
        if (u == NULL_CS)
            t = make_str_given("FONT");
        else t = u - SINGLE_BASE;
    } else {
        old_setting = selector;
        selector = NEW_STRING;
        print("FONT");
        print_str(u - ACTIVE_BASE);
        selector = old_setting;
        str_room(1);
        t = make_str();
    }
    def(u, SET_FONT, null_font);
    scan_optional_equals();
    scan_file_name();
    name_in_progress = TRUE;
    if (scan_keyword("at")) {
        scan_normal_dimen();
        s = cur_val;
        if (s <= 0 || s >= 01000000000) {
            print_err("Improper `at' size (");
            print_scaled(s);
            print("pt), replaced by 10pt");
            help_font_at();
            error();
            s = 10 * UNITY;
        }
    } else if (scan_keyword("scaled")) {
        scan_int();
        s = -cur_val;
        if (cur_val <= 0 || cur_val > 32768) {
            print_err("Illegal magnification has been changed to 1000");
            help_font_magnification();
            int_error(cur_val);
            s = -1000;
        }
    } else {
        s = -1000;
    }
    name_in_progress = FALSE;
    for (f = FONT_BASE + 1; f <= font_ptr; f++) {
        if (str_eq_str(font_name[f], cur_name) &&
            str_eq_str(font_area[f], cur_area)) {
            if (s > 0) {
                if (s == font_size[f])
                    goto common_end;
            } else if (font_size[f] == xn_over_d(font_dsize[f], -s, 1000L)) {
                goto common_end;
            }
        }
    }
    f = read_font_info(u, cur_name, cur_area, s);

common_end:
    equiv(u) = f;
    eqtb[FONT_ID_BASE + f] = eqtb[u];
    font_id_text(f) = t;
}

prepare_mag ()
{
    if (mag_set > 0 && mag != mag_set) {
        print_err("Incompatible magnification (");
        print_val(mag);
        print(");");
        print_nl(" the previous value will be retained");
        help_mag();
        int_error(mag_set);
        geq_word_define((ptr) INT_BASE + MAG_CODE, mag_set);
    }
    if (mag <= 0 || mag > 32768) {
        print_err("Illegal magnification has been changed to 1000");
        help_ill_mag();
        int_error(mag);
        geq_word_define((ptr) INT_BASE + MAG_CODE, 1000L);
    }
    mag_set = mag;
}
        
new_interaction ()
{
    print_ln();
    interaction = cur_chr;
    if (interaction == BATCH_MODE)
        selector = NO_PRINT;
    else selector = TERM_ONLY;
    if (job_name != 0)
        selector += 2;
}

do_assignments ()
{
    loop {
        get_nbrx_token();
        if (cur_cmd <= MAX_NON_PREFIXED_COMMAND)
            break;
        prefixed_command();
    }
}

#ifdef INCTEX

clopen_stream ()
{   
	int		c;
	int		n;

	c = cur_chr;
	scan_four_bit_int();
	n = cur_val; 
	if (read_open[n] != CLOSED) {
		a_close(read_file[n]);
		read_open[n] = CLOSED;
	}
	if (c != 0) {
		scan_optional_equals();
		scan_file_name();
		if (cur_ext == null_str)
			cur_ext = str_tex;
		pack_cur_name();
		if (read_file[n] = a_open_in(FALSE))
			read_open[n] = JUST_OPENED;
	}
}

#else

clopen_stream ()
{   
    int     c;
    int     n;

    c = cur_chr;
    scan_four_bit_int();
    n = cur_val; 
    if (read_open[n] != CLOSED) {
        a_close(read_file[n]);
        read_open[n] = CLOSED;
    }
    if (c != 0) {
        scan_optional_equals();
        scan_file_name();
        if (cur_ext == null_str)
            cur_ext = str_tex;
        pack_cur_name();
        if (read_file[n] = a_open_in())
            read_open[n] = JUST_OPENED;
    }
}

#endif


issue_message ()
{
    int     c;
    str     s;
    int     old_setting;

    c = cur_chr;
    scan_toks(FALSE, TRUE);
    old_setting = selector;
    selector = NEW_STRING;
    token_show(def_ref);
    selector = old_setting;
    flush_list(def_ref);
    str_room(1);
    s = make_str();
    if (c == 0) {
        if (term_offset + length(s) > MAX_PRINT_LINE - 2)
            print_ln();
        else if (term_offset > 0 || file_offset > 0)
            print_char(' ');
        print_str(s);
        update_terminal();
    } else {
        print_err("");
        print_str(s);
        if (err_help != NULL)
            use_err_help = TRUE;
        else if (long_help_seen)
            help_err_msg();
        else {
            if (interaction < ERROR_STOP_MODE)
                long_help_seen = TRUE;
            help_poirot();
        }
        error();
        use_err_help = FALSE;
    }
    flush_string();
}

give_err_help ()
{
    token_show(err_help);
}

shift_case ()
{
    ptr     b;
    byte    c;
    ptr     p;
    tok     t;

    b = cur_chr;
    scan_toks(FALSE, FALSE);
    for (p = token_link(def_ref); p != NULL; p = token_link(p)) {
        t = token(p);
        if (t < CS_TOKEN_FLAG + SINGLE_BASE) {
            if (t >= CS_TOKEN_FLAG)
                t -= ACTIVE_BASE;
            c = t % 256;
            if (c < 128 && equiv(b + c) != 0)
                t = 256 * (t / 256) + equiv(b + c);
            if (t >= CS_TOKEN_FLAG)
                token(p) = t + ACTIVE_BASE;
            else token(p) = t;
        }
    }
    back_list(token_link(def_ref));
    free_token(def_ref);
}

show_whatever ()
{
    switch (cur_chr)
    {
    case SHOW_LISTS:
        begin_diagnostic();
        show_activities();
        break;

    case SHOW_BOX_CODE:
        scan_eight_bit_int();
        begin_diagnostic();
        print_nl("> \\box");
        print_val(cur_val);
        print_char('=');
        if (box(cur_val) == NULL)
            print("void");
        else show_box(box(cur_val));
        break;
    
    case SHOW_CODE:
        get_token();
        print_nl("> ");
        if (cur_cs != 0) {
            sprint_cs(cur_cs);
            print_char('=');
        }
        print_meaning();
        goto common_end;
        break;
    
    default:
        the_toks();
        print_nl("> ");
        token_show(temp_toks);
        flush_list(token_link(temp_toks));
        goto common_end;
        break;
    }
    end_diagnostic(TRUE);
    print_err("OK");
    if (selector == TERM_AND_LOG && tracing_online <= 0) {
        selector = TERM_ONLY;
        print(" (see the transcript file)");
        selector = TERM_AND_LOG;
    }

common_end:
    if (interaction < ERROR_STOP_MODE) {
        help0();
        decr(error_count);
    } else if (tracing_online > 0)
        help_show_online();
    else help_show();
    error();
}

/*
 *  Help text
 */

help_missing_cs ()
{
    help5("Please don't say `\\def cs{...}', say `\\def\\cs{...}'.",
    "I've inserted an inaccessible control sequence so that your",
    "definition will be completed without mixing me up too badly.",
    "You can recover graciously from this error, if you're",
    "careful; see exercise 27.2 in The TeXbook.");
}

help_prefix ()
{
    help1("I'll pretend you didn't say \\long or \\outer or \\global.");
}

help_pref ()
{
    help1("I'll pretend you didn't say \\long or \\outer here.");
}

help_read_to ()
{
    help2("You should have said `\\read<number> to \\cs'.",
    "I'm going to look for the \\cs now.");
}

help_code ()
{
    help1("I'm going to use 0 instead of that illegal code value.");
}

help_register ()
{
    help1("I'm forgetting what you said and not changing anything.");
}

help_space_factor ()
{
    help1("I allow only values in the range 1..32767 here.");
}

help_prevgraf ()
{
    help1("I allow only nonnegative values here.");
}

help_overflow ()
{
    help2("I can't carry out that multiplication or division,",
    "since the result is out of range.");
}

help_font_at ()
{
    help2("I can only handle fonts at positive sizes that are",
    "less than 2048pt, so I've changed what you said to 10pt.");
}

help_font_magnification ()
{
    help1("The magnification ratio must be between 1 and 32768.");
}

help_mag()
{
    help2("I can handle only one magnification ratio per job. So I've",
    "reverted to the magnification you used earlier on this run.");
}

help_ill_mag ()
{
    help1("The magnification ratio must be between 1 and 32768.");
}

help_err_msg ()
{
    help1("(That was another \\errmessage.)");
}

help_poirot ()
{
    help4("This error message was generated by an \\errmessage",
    "command, so I can't give any explicit help.",
    "Pretend that you're Hercule Poirot: Examine all clues,",
    "and deduce the truth by order and method.");
}

help_show_online ()
{
    help3("This isn't an error message; I'm just \\showing something.",
    "Type `I\\show...' to show more (e.g., \\show\\cs,",
    "\\showthe\\count10, \\showbox255, \\showlists).");
}

help_show ()
{
    help5("This isn't an error message; I'm just \\showing something.",
    "Type `I\\show...' to show more (e.g., \\show\\cs,",
    "\\showthe\\count10, \\showbox255, \\showlists).",
    "And type `I\\tracingonline=1\\show...' to show boxes and",
    "lists on your terminal as well as in the transcript file.");
}
