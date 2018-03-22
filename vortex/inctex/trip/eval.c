/*
 * @(#)eval.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 *  This file has been modified for
 *
 *  IncTeX  --	Incremental TeX
 *
 *  An Editor-Independent, Adaptive, Incremental TeX Formatter
 *  Copyright (C) 1988 by the Regents of the University of California
 *  (Pehong Chen & Derluen Pan)
 *
 *  All rights reserved by the copyright holders.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

#include	"tex.h"
#include	"tfm.h"
#include	"eqstack.h"
#include	"token.h"
#include	"scan.h"
#include	"tokenstack.h"
#include	"evalstack.h"
#include	"box.h"
#include	"boxlists.h"
#include	"math.h"
#include	"mathlists.h"
#include	"cond.h"
#include	"def.h"
#include	"dvi.h"
#include	"pack.h"
#include	"page.h"
#include	"par.h"
#include	"eval.h"

#define vmode(CMD) \
    case VMODE + CMD

#define hmode(CMD) \
    case HMODE + CMD

#define non_math(M) \
    case VMODE + M: \
    case HMODE + M

#define mmode(CMD) \
    case MMODE + CMD

#define any_mode(CMD) \
    case VMODE + CMD: \
    case HMODE + CMD: \
    case MMODE + CMD

#ifdef INCTEX
#include	<sys/file.h>
#include	<sys/time.h>
#include	"file.h"
#include	"Imain.h"

main_control ()
{
	byte		c;
	fnt		f;
	qqqq		i;
	qqqq		j;
	int		k;
	qword		l;
	ptr		p;
	ptr		q;
	hword		r;
	int		s;
	int		t;
	bool		ligature_present;
	int		n;

	if (incremental) {
		gettimeofday(&time2, &tz);
		btime = 0.0;
	}

	if ((every_job != NULL) && (total_pages == 0))
		begin_token_list(every_job, EVERY_JOB_TEXT);

big_switch:
	if (incremental && page_done) {
/*		write file position now (part of the state) after page
 *		finished because tex sometimes reads another line after
 *		a page before coming back to big_switch, so file
 *		position has to saved at same time as state is saved -DLP
 */
		add_pnode();

		gettimeofday(&time3, &tz);
		if (state_checkpointing && ((total_pages % jump_val) == 0))
			save_state();
		else {
			get_ext(total_pages, EXT_STC);
			if (access(name_of_file, F_OK) == 0)
				unlink(name_of_file);
		}
		
		if (format_continue) {
			p_end->btime = btime;
			p_end->ftime = time_diff(time3, time2);
		}
		if (state_checkpointing && ((total_pages % jump_val) == 0)) {
			gettimeofday(&time4, &tz);
			p_end->stime = time_diff(time4, time3);
			if (p_end->ltime == 0.0)
				p_end->ltime = p_end->stime;
		}
		page_shipped = page_done = FALSE;
		return;
	} else
		get_x_token();

reswitch:
	if (interrupt) {
		if (OK_to_interrupt) {
			back_input();
			pause_for_instructions();
			goto big_switch;
		}
	}

	if (tracing_commands > 0)
		show_cur_cmd_chr();

	if (incremental && page_shipped) {
		page_done = TRUE;
		back_input();
		goto big_switch;
	}

	switch (abs(mode) + cur_cmd) {
	hmode(CHAR_GIVEN):
	hmode(LETTER):
	hmode(OTHER_CHAR):
		goto main_loop;
	
	hmode(CHAR_NUM):
		scan_char_num();
		cur_chr = cur_val;
		goto main_loop;

	hmode(SPACER):
		if (space_factor == 1000)
			goto append_normal_space;
		else
			app_space();
		break;
	
	hmode(EX_SPACE):
	mmode(EX_SPACE):
		goto append_normal_space;
	
	any_mode(RELAX):
	vmode(SPACER):
	mmode(SPACER):
		break;
	
	any_mode(IGNORE_SPACES):
		get_nbx_token();
		goto reswitch;

	vmode(STOP):
		if (its_all_over())
			return;
		break;

	any_mode(LAST_ITEM):
	any_mode(MAC_PARAM):
	non_math(EQ_NO):
	vmode(VMOVE):
	hmode(HMOVE):
	mmode(HMOVE):
	vmode(VADJUST):
	vmode(ITAL_CORR):
		report_illegal_case();
		break;

	non_math(SUP_MARK):
	non_math(SUB_MARK):
	non_math(MATH_CHAR_NUM):
	non_math(MATH_GIVEN):
	non_math(MATH_COMP):
	non_math(DELIM_NUM):
	non_math(LEFT_RIGHT):
	non_math(ABOVE):
	non_math(RADICAL):
	non_math(MATH_STYLE):
	non_math(MATH_CHOICE):
	non_math(VCENTER):
	non_math(NON_SCRIPT):
	non_math(MKERN):
	non_math(LIMIT_SWITCH):
	non_math(MSKIP):
	non_math(MATH_ACCENT):
	mmode(ENDV):
	mmode(PAR_END):
	mmode(STOP):
	mmode(VSKIP):
	mmode(UN_VBOX):
	mmode(VALIGN):
	mmode(HRULE):
		insert_dollar_sign();
		break;
	
	vmode(HRULE):
	hmode(VRULE):
	mmode(VRULE):
		tail_append(scan_rule_spec());
		if (abs(mode) == VMODE)
			prev_depth = IGNORE_DEPTH;
		else if (abs(mode) == HMODE)
			space_factor = 1000;
		break;
	
	vmode(VSKIP):
	hmode(HSKIP):
	mmode(HSKIP):
	mmode(MSKIP):
		append_glue();
		break;
	
	any_mode(KERN):
	mmode(MKERN):
		append_kern();
		break;
	
	non_math(LEFT_BRACE):
		new_save_level(SIMPLE_GROUP);
		break;

	any_mode(BEGIN_GROUP):
		new_save_level(SEMI_SIMPLE_GROUP);
		break;

	any_mode(END_GROUP):
		if (cur_group == SEMI_SIMPLE_GROUP)
			unsave();
		else
			off_save();
		break;
	
	any_mode(RIGHT_BRACE):
		if (incremental && page_shipped)
			page_done = TRUE;
		handle_right_brace();
		break;

	vmode(HMOVE):
	hmode(VMOVE):
	mmode(VMOVE):
		t = cur_chr;
		scan_normal_dimen();
		if (t == 0)
			saved(0) = cur_val;
		else
			saved(0) = -cur_val;
		scan_box();
		break;
	
	any_mode(LEADER_SHIP):
		saved(0) = LEADER_FLAG - A_LEADERS + cur_chr;
		scan_box();
		break;
	
	any_mode(MAKE_BOX):
		saved(0) = 0;
		begin_box();
		break;

	vmode(START_PAR):
		new_graf(cur_chr > 0);
		break;
	
	vmode(LETTER):
	vmode(OTHER_CHAR):
	vmode(CHAR_NUM):
	vmode(CHAR_GIVEN):
	vmode(MATH_SHIFT):
	vmode(UN_HBOX):
	vmode(VRULE):
	vmode(ACCENT):
	vmode(DISCRETIONARY):
	vmode(HSKIP):
	vmode(VALIGN):
	vmode(EX_SPACE):
		back_input();
		new_graf(TRUE);
		break;
	
	hmode(START_PAR):
	mmode(START_PAR):
		indent_in_hmode();
		break;
	
	vmode(PAR_END):
		normal_paragraph();
		if (mode > 0)
			build_page();
		break;

	hmode(PAR_END):
		if (align_state < 0)
			off_save();
		end_graf();
		if (mode == VMODE)	
			build_page();
		break;
	
	hmode(STOP):
	hmode(VSKIP):
	hmode(HRULE):
	hmode(UN_VBOX):
	hmode(HALIGN):
		head_for_vmode();
		break;
	
	any_mode(INSERT):
	hmode(VADJUST):
	mmode(VADJUST):
		begin_insert_or_adjust();
		break;
	
	any_mode(MARK):
		make_mark();
		break;
	
	any_mode(BREAK_PENALTY):
		append_penalty();
		break;
	
	any_mode(REMOVE_ITEM):
		delete_last();
		break;
	
	vmode(UN_VBOX):
	hmode(UN_HBOX):
	mmode(UN_HBOX):
		unpackage();
		break;
	
	hmode(ITAL_CORR):
		append_italic_correction();
		break;
	
	mmode(ITAL_CORR):
		tail_append(new_kern(0L));
		break;

	hmode(DISCRETIONARY):
	mmode(DISCRETIONARY):
		append_discretionary();
		break;
	
	hmode(ACCENT):
		make_accent();
		break;
	
	any_mode(CAR_RET):
	any_mode(TAB_MARK):
		align_error();
		break;

	any_mode(NO_ALIGN):
		no_align_error();
		break;
	
	any_mode(OMIT):
		omit_error();
		break;

	vmode(HALIGN):
	hmode(VALIGN):
		init_align();
		break;
	
	mmode(HALIGN):
		if (privileged())
			init_align();
		break;

	vmode(ENDV):
	hmode(ENDV):
		do_endv();
		break;
	
	any_mode(END_CS_NAME):
		cs_error();
		break;

	hmode(MATH_SHIFT):
		init_math();
		break;
	
	mmode(EQ_NO):
		if (privileged())
			start_eq_no();
		break;
	
	mmode(LEFT_BRACE):
		tail_append(new_noad()); 
		back_input();
		scan_math(nucleus(tail));
		break;

	mmode(LETTER):
	mmode(OTHER_CHAR):
	mmode(CHAR_GIVEN):
		if (cur_chr < 256)
			set_math_char(ho(math_code(cur_chr)));
		else
			set_math_char(cur_chr);
		break;
	
	mmode(CHAR_NUM):
		scan_char_num();
		cur_chr = cur_val;
		if (cur_chr < 256)
			set_math_char(ho(math_code(cur_chr)));
		else
			set_math_char(cur_chr);
		break;
	
	mmode(MATH_CHAR_NUM):
		scan_fifteen_bit_int();
		set_math_char((hword)cur_val);
		break;
	
	mmode(MATH_GIVEN):
		set_math_char(cur_chr);
		break;

	mmode(DELIM_NUM):
		scan_twenty_seven_bit_int();
		set_math_char(cur_val / 010000);
		break;
	
	mmode(MATH_COMP):
		tail_append(new_noad());
		type(tail) = cur_chr;
		scan_math(nucleus(tail));
		break;
	
	mmode(LIMIT_SWITCH):
		math_limit_switch();
		break;

	mmode(RADICAL):
		math_radical();
		break;

	mmode(ACCENT):
	mmode(MATH_ACCENT):
		math_ac();
		break;

	mmode(VCENTER):
		scan_spec();
		new_save_level(VCENTER_GROUP);
		normal_paragraph();
		push_nest();
		mode = -VMODE;
		prev_depth = IGNORE_DEPTH;
		if (every_vbox != NULL)
			begin_token_list(every_vbox, EVERY_VBOX_TEXT);
		break;
	
	mmode(MATH_STYLE):
		tail_append(new_style(cur_chr));
		break;
	
	mmode(NON_SCRIPT):
		tail_append(new_glue(zero_glue));
		subtype(tail) = COND_MATH_GLUE;
		break;
	
	mmode(MATH_CHOICE):
		append_choices();
		break;

	mmode(SUB_MARK):
	mmode(SUP_MARK):
		sub_sup();
		break;
	
	mmode(ABOVE):
		math_fraction();
		break;
	
	mmode(LEFT_RIGHT):
		math_left_right();
		break;

	mmode(MATH_SHIFT):
		if (cur_group == MATH_SHIFT_GROUP)
			after_math();
		else
			off_save();
		break;
	
	any_mode(ASSIGN_TOKS):
	any_mode(ASSIGN_INT):
	any_mode(ASSIGN_DIMEN):
	any_mode(ASSIGN_GLUE):
	any_mode(ASSIGN_MU_GLUE):
	any_mode(ASSIGN_FONT_DIMEN):
	any_mode(ASSIGN_FONT_INT):
	any_mode(SET_AUX):
	any_mode(SET_PREV_GRAF):
	any_mode(SET_PAGE_DIMEN):
	any_mode(SET_PAGE_INT):
	any_mode(SET_BOX_DIMEN):
	any_mode(SET_SHAPE):
	any_mode(DEF_CODE):
	any_mode(DEF_FAMILY):
	any_mode(SET_FONT):
	any_mode(DEF_FONT):
	any_mode(REGISTER):
	any_mode(ADVANCE):
	any_mode(MULTIPLY):
	any_mode(DIVIDE):
	any_mode(PREFIX):
	any_mode(LET):
	any_mode(SHORTHAND_DEF):
	any_mode(READ_TO_CS):
	any_mode(DEF):
	any_mode(SET_BOX):
	any_mode(TOKS_REGISTER):
	any_mode(HYPH_DATA):
	any_mode(SET_INTERACTION):
		prefixed_command();
		break;

	any_mode(AFTER_ASSIGNMENT):
		get_token();
		after_token = cur_tok;
		break;
	
	any_mode(AFTER_GROUP):
		get_token();
		save_for_after(cur_tok);
		break;

	any_mode(IN_STREAM):
		clopen_stream();
		break;

	any_mode(MESSAGE):
		issue_message();
		break;
	
	any_mode(CASE_SHIFT):
		shift_case();
		break;
	
	any_mode(XRAY):
		show_whatever();
		break;
	
	any_mode(EXTENSION):
		do_extension();
		break;
	}

	goto big_switch;

#define	make_lig_disc() \
	{if (ligature_present) { \
		p = new_ligature(f, l, link(q)); \
		link(q) = p; \
		tail = p;} \
	if (c == hyphen_char[f] && mode == HMODE) \
		tail_append(new_disc());}

#define	space_glue() \
	{p = font_glue[cur_font]; \
	if (p == NULL) { \
		f = cur_font; \
		p = new_spec(zero_glue); \
		k = param_base[f] + SPACE_CODE; \
		width(p) = font_info[k].sc; \
		stretch(p) = font_info[k + 1].sc; \
		shrink(p) = font_info[k + 2].sc; \
		font_glue[f] = p;}}

main_loop:
	f = cur_font;
	c = cur_chr;

main_loop_1:
	if (c < font_bc[f] || c > font_ec[f]) {
		char_warning(f, c);
		goto big_switch;
	}

main_loop_2:
	q = tail;
	ligature_present = FALSE;
	l = qi(c);

main_loop_3:
	s = sf_code(c);
	if (s == 1000) {
		space_factor = 1000;
	} else if (s < 1000) {
		if (s > 0) space_factor = s;
	} else if (space_factor < 1000) {
		space_factor = 1000;
	} else {
		space_factor = s;
	}
	i = char_info(f, l);
	if (char_exists(i)) {
		fast_get_avail(p);
		font(p) = f;
		character(p) = qi(c);
		link(tail) = p;
		tail = p;
	} else
		char_warning(f, qo(l));
	get_next();
	if (cur_cmd == LETTER || cur_cmd == OTHER_CHAR || cur_cmd == CHAR_GIVEN)
		r = qi(cur_chr);
	else {
		x_token();
		if (cur_cmd == LETTER || cur_cmd == OTHER_CHAR || cur_cmd == CHAR_GIVEN)
			r = qi(cur_chr);
		else if (cur_cmd == CHAR_NUM) {
			scan_char_num();
			r = qi(cur_val);
		} else
			r = qi(256);
	}
	if (char_tag(i) == LIG_TAG && r != qi(256)) {
		k = lig_kern_start(f, i);
		do {
			j = font_info[k].qqqq;
			if (next_char(j) == r) {
				if (op_bit(j) < KERN_FLAG) {
					ligature_present = TRUE;
					l = rem_byte(j);
					c = qo(r);
					goto main_loop_3;
				} else {
					make_lig_disc();
					tail_append(new_kern(char_kern(f, j)));
					c = qo(r);
					goto main_loop_2;
				}
			}
			incr(k);
		} while (stop_bit(j) < STOP_FLAG);
	}
	make_lig_disc();
	if (r == qi(256))
		goto reswitch;
	c = qo(r);
	goto main_loop_1;

append_normal_space:
	if (space_skip == zero_glue) {
		space_glue();
		q = new_glue(p);
	} else
		q = new_param_glue(SPACE_SKIP_CODE);
	link(tail) = q;
	tail = q;
	goto big_switch;
}

#else

main_control ()
{
    byte    c;
    fnt     f;
    qqqq    i;
    qqqq    j;
    int     k;
    qword   l;
    ptr     p;
    ptr     q;
    hword   r;
    int     s;
    int     t;
    bool    ligature_present;


    if (every_job != NULL)
        begin_token_list(every_job, EVERY_JOB_TEXT);

big_switch:
    get_x_token();

reswitch:
    if (interrupt) {
        if (OK_to_interrupt) {
            back_input();
            pause_for_instructions();
            goto big_switch;
        }
    }

    if (tracing_commands > 0)
        show_cur_cmd_chr();

    switch (abs(mode) + cur_cmd)
    {
    hmode(LETTER):
    hmode(OTHER_CHAR):
    hmode(CHAR_GIVEN):
        goto main_loop;
    
    hmode(CHAR_NUM):
        scan_char_num();
        cur_chr = cur_val;
        goto main_loop;

    hmode(SPACER):
        if (space_factor == 1000)
            goto append_normal_space;
        else app_space();
        break;
    
    hmode(EX_SPACE):
    mmode(EX_SPACE):
        goto append_normal_space;
    
    any_mode(RELAX):
    vmode(SPACER):
    mmode(SPACER):
        break;
    
    any_mode(IGNORE_SPACES):
        get_nbx_token();
        goto reswitch;

    vmode(STOP):
        if (its_all_over())
            return;
        break;

    any_mode(LAST_ITEM):
    any_mode(MAC_PARAM):
    non_math(EQ_NO):
    vmode(VMOVE):
    hmode(HMOVE):
    mmode(HMOVE):
    vmode(VADJUST):
    vmode(ITAL_CORR):
        report_illegal_case();
        break;

    non_math(SUP_MARK):
    non_math(SUB_MARK):
    non_math(MATH_CHAR_NUM):
    non_math(MATH_GIVEN):
    non_math(MATH_COMP):
    non_math(DELIM_NUM):
    non_math(LEFT_RIGHT):
    non_math(ABOVE):
    non_math(RADICAL):
    non_math(MATH_STYLE):
    non_math(MATH_CHOICE):
    non_math(VCENTER):
    non_math(NON_SCRIPT):
    non_math(MKERN):
    non_math(LIMIT_SWITCH):
    non_math(MSKIP):
    non_math(MATH_ACCENT):
    mmode(ENDV):
    mmode(PAR_END):
    mmode(STOP):
    mmode(VSKIP):
    mmode(UN_VBOX):
    mmode(VALIGN):
    mmode(HRULE):
        insert_dollar_sign();
        break;
    
    vmode(HRULE):
    hmode(VRULE):
    mmode(VRULE):
        tail_append(scan_rule_spec());
        if (abs(mode) == VMODE)
            prev_depth = IGNORE_DEPTH;
        else if (abs(mode) == HMODE)
            space_factor = 1000;
        break;
    
    vmode(VSKIP):
    hmode(HSKIP):
    mmode(HSKIP):
    mmode(MSKIP):
        append_glue();
        break;
    
    any_mode(KERN):
    mmode(MKERN):
        append_kern();
        break;
    
    non_math(LEFT_BRACE):
        new_save_level(SIMPLE_GROUP);
        break;

    any_mode(BEGIN_GROUP):
        new_save_level(SEMI_SIMPLE_GROUP);
        break;

    any_mode(END_GROUP):
        if (cur_group == SEMI_SIMPLE_GROUP)
            unsave();
        else off_save();
        break;
    
    any_mode(RIGHT_BRACE):
        handle_right_brace();
        break;

    vmode(HMOVE):
    hmode(VMOVE):
    mmode(VMOVE):
        t = cur_chr;
        scan_normal_dimen();
        if (t == 0)
            saved(0) = cur_val;
        else saved(0) = -cur_val;
        scan_box();
        break;
    
    any_mode(LEADER_SHIP):
        saved(0) = LEADER_FLAG - A_LEADERS + cur_chr;
        scan_box();
        break;
    
    any_mode(MAKE_BOX):
        saved(0) = 0;
        begin_box();
        break;

    vmode(START_PAR):
        new_graf(cur_chr > 0);
        break;
    
    vmode(LETTER):
    vmode(OTHER_CHAR):
    vmode(CHAR_NUM):
    vmode(CHAR_GIVEN):
    vmode(MATH_SHIFT):
    vmode(UN_HBOX):
    vmode(VRULE):
    vmode(ACCENT):
    vmode(DISCRETIONARY):
    vmode(HSKIP):
    vmode(VALIGN):
    vmode(EX_SPACE):
        back_input();
        new_graf(TRUE);
        break;
    
    hmode(START_PAR):
    mmode(START_PAR):
        indent_in_hmode();
        break;
    
    vmode(PAR_END):
        normal_paragraph();
        if (mode > 0)
            build_page();
        break;

    hmode(PAR_END):
        if (align_state < 0)
            off_save();
        end_graf();
        if (mode == VMODE)  
            build_page();
        break;
    
    hmode(STOP):
    hmode(VSKIP):
    hmode(HRULE):
    hmode(UN_VBOX):
    hmode(HALIGN):
        head_for_vmode();
        break;
    
    any_mode(INSERT):
    hmode(VADJUST):
    mmode(VADJUST):
        begin_insert_or_adjust();
        break;
    
    any_mode(MARK):
        make_mark();
        break;
    
    any_mode(BREAK_PENALTY):
        append_penalty();
        break;
    
    any_mode(REMOVE_ITEM):
        delete_last();
        break;
    
    vmode(UN_VBOX):
    hmode(UN_HBOX):
    mmode(UN_HBOX):
        unpackage();
        break;
    
    hmode(ITAL_CORR):
        append_italic_correction();
        break;
    
    mmode(ITAL_CORR):
        tail_append(new_kern(0L));
        break;

    hmode(DISCRETIONARY):
    mmode(DISCRETIONARY):
        append_discretionary();
        break;
    
    hmode(ACCENT):
        make_accent();
        break;
    
    any_mode(CAR_RET):
    any_mode(TAB_MARK):
        align_error();
        break;

    any_mode(NO_ALIGN):
        no_align_error();
        break;
    
    any_mode(OMIT):
        omit_error();
        break;

    vmode(HALIGN):
    hmode(VALIGN):
        init_align();
        break;
    
    mmode(HALIGN):
        if (privileged())
            init_align();
        break;

    vmode(ENDV):
    hmode(ENDV):
        do_endv();
        break;
    
    any_mode(END_CS_NAME):
        cs_error();
        break;

    hmode(MATH_SHIFT):
        init_math();
        break;
    
    mmode(EQ_NO):
        if (privileged())
            start_eq_no();
        break;
    
    mmode(LEFT_BRACE):
        tail_append(new_noad()); 
        back_input();
        scan_math(nucleus(tail));
        break;

    mmode(LETTER):
    mmode(OTHER_CHAR):
    mmode(CHAR_GIVEN):
        if (cur_chr < 256)
            set_math_char(ho(math_code(cur_chr)));
        else set_math_char(cur_chr);
        break;
    
    mmode(CHAR_NUM):
        scan_char_num();
        cur_chr = cur_val;
        if (cur_chr < 256)
            set_math_char(ho(math_code(cur_chr)));
        else set_math_char(cur_chr);
        break;
    
    mmode(MATH_CHAR_NUM):
        scan_fifteen_bit_int();
        set_math_char((hword) cur_val);
        break;
    
    mmode(MATH_GIVEN):
        set_math_char(cur_chr);
        break;

    mmode(DELIM_NUM):
        scan_twenty_seven_bit_int();
        set_math_char((hword) (cur_val / 010000));
        break;
    
    mmode(MATH_COMP):
        tail_append(new_noad());
        type(tail) = cur_chr;
        scan_math(nucleus(tail));
        break;
    
    mmode(LIMIT_SWITCH):
        math_limit_switch();
        break;

    mmode(RADICAL):
        math_radical();
        break;

    mmode(ACCENT):
    mmode(MATH_ACCENT):
        math_ac();
        break;

    mmode(VCENTER):
        scan_spec();
        new_save_level(VCENTER_GROUP);
        normal_paragraph();
        push_nest();
        mode = -VMODE;
        prev_depth = IGNORE_DEPTH;
        if (every_vbox != NULL)
            begin_token_list(every_vbox, EVERY_VBOX_TEXT);
        break;
    
    mmode(MATH_STYLE):
        tail_append(new_style(cur_chr));
        break;
    
    mmode(NON_SCRIPT):
        tail_append(new_glue(zero_glue));
        subtype(tail) = COND_MATH_GLUE;
        break;
    
    mmode(MATH_CHOICE):
        append_choices();
        break;

    mmode(SUB_MARK):
    mmode(SUP_MARK):
        sub_sup();
        break;
    
    mmode(ABOVE):
        math_fraction();
        break;
    
    mmode(LEFT_RIGHT):
        math_left_right();
        break;

    mmode(MATH_SHIFT):
        if (cur_group == MATH_SHIFT_GROUP)
            after_math();
        else off_save();
        break;
    
    any_mode(ASSIGN_TOKS):
    any_mode(ASSIGN_INT):
    any_mode(ASSIGN_DIMEN):
    any_mode(ASSIGN_GLUE):
    any_mode(ASSIGN_MU_GLUE):
    any_mode(ASSIGN_FONT_DIMEN):
    any_mode(ASSIGN_FONT_INT):
    any_mode(SET_AUX):
    any_mode(SET_PREV_GRAF):
    any_mode(SET_PAGE_DIMEN):
    any_mode(SET_PAGE_INT):
    any_mode(SET_BOX_DIMEN):
    any_mode(SET_SHAPE):
    any_mode(DEF_CODE):
    any_mode(DEF_FAMILY):
    any_mode(SET_FONT):
    any_mode(DEF_FONT):
    any_mode(REGISTER):
    any_mode(ADVANCE):
    any_mode(MULTIPLY):
    any_mode(DIVIDE):
    any_mode(PREFIX):
    any_mode(LET):
    any_mode(SHORTHAND_DEF):
    any_mode(READ_TO_CS):
    any_mode(DEF):
    any_mode(SET_BOX):
    any_mode(TOKS_REGISTER):
    any_mode(HYPH_DATA):
    any_mode(SET_INTERACTION):
        prefixed_command();
        break;

    any_mode(AFTER_ASSIGNMENT):
        get_token();
        after_token = cur_tok;
        break;
    
    any_mode(AFTER_GROUP):
        get_token();
        save_for_after(cur_tok);
        break;

    any_mode(IN_STREAM):
        clopen_stream();
        break;

    any_mode(MESSAGE):
        issue_message();
        break;
    
    any_mode(CASE_SHIFT):
        shift_case();
        break;
    
    any_mode(XRAY):
        show_whatever();
        break;
    
    any_mode(EXTENSION):
        do_extension();
        break;
    }
    goto big_switch;

#define make_lig_disc() \
    {if (ligature_present) { \
        p = new_ligature(f, l, link(q)); \
        link(q) = p; \
        tail = p;} \
    if (c == hyphen_char[f] && mode == HMODE) \
        tail_append(new_disc());}

#define space_glue() \
    {p = font_glue[cur_font]; \
    if (p == NULL) { \
        f = cur_font; \
        p = new_spec(zero_glue); \
        k = param_base[f] + SPACE_CODE; \
        width(p) = font_info[k].sc; \
        stretch(p) = font_info[k + 1].sc; \
        shrink(p) = font_info[k + 2].sc; \
        font_glue[f] = p;}}

main_loop:
    f = cur_font;
    c = cur_chr;

main_loop_1:
    if (c < font_bc[f] || c > font_ec[f]) {
        char_warning(f, c);
        goto big_switch;
    }

main_loop_2:
    q = tail;
    ligature_present = FALSE;
    l = qi(c);

main_loop_3:
	s = sf_code(c);
	if (s == 1000) {
		space_factor = 1000;
	} else if (s < 1000) {
		if (s > 0) space_factor = s;
	} else if (space_factor < 1000) {
		space_factor = 1000;
	} else {
		space_factor = s;
	}
    i = char_info(f, l);
    if (char_exists(i)) {
        fast_get_avail(p);
        font(p) = f;
        character(p) = qi(c);
        link(tail) = p;
        tail = p;
    } else char_warning(f, qo(l));
    get_next();
    if (cur_cmd == LETTER ||
        cur_cmd == OTHER_CHAR ||
        cur_cmd == CHAR_GIVEN)
        r = qi(cur_chr);
    else {
        x_token();
        if (cur_cmd == LETTER ||
            cur_cmd == OTHER_CHAR ||
            cur_cmd == CHAR_GIVEN)
            r = qi(cur_chr);
        else if (cur_cmd == CHAR_NUM) {
            scan_char_num();
            r = qi(cur_val);
        } else
            r = qi(256);
    }
    if (char_tag(i) == LIG_TAG && r != qi(256)) {
        k = lig_kern_start(f, i);
        do {
            j = font_info[k].qqqq;
            if (next_char(j) == r) {
                if (op_bit(j) < KERN_FLAG) {
                    ligature_present = TRUE;
                    l = rem_byte(j);
                    c = qo(r);
                    goto main_loop_3;
                } else {
                    make_lig_disc();
                    tail_append(new_kern(char_kern(f, j)));
                    c = qo(r);
                    goto main_loop_2;
                }
            }
            incr(k);
        } while (stop_bit(j) < STOP_FLAG);
    }
    make_lig_disc();
    if (r == qi(256))
        goto reswitch;
    c = qo(r);
    goto main_loop_1;

append_normal_space:
    if (space_skip == zero_glue) {
        space_glue();
        q = new_glue(p);
    } else
        q = new_param_glue(SPACE_SKIP_CODE);
    link(tail) = q;
    tail = q;
    goto big_switch;
}

#endif

app_space ()
{
    fnt     f;
    int     k;
    ptr     p;
    ptr     q;

    if (space_factor >= 2000 && xspace_skip != zero_glue)
        q = new_param_glue(XSPACE_SKIP_CODE);
    else {
        if (space_skip != zero_glue)
            p = space_skip;
        else space_glue();
        p = new_spec(p);
        if (space_factor >= 2000)
            width(p) += extra_space(cur_font);
        stretch(p) = xn_over_d(stretch(p), space_factor, 1000L);
        shrink(p) = xn_over_d(shrink(p), 1000L, space_factor);
        q = new_glue(p);
        glue_ref_count(p) = NULL;
    }
    link(tail) = q;
    tail = q;
}

insert_dollar_sign ()
{
    back_input();
    cur_tok = MATH_SHIFT_TOKEN + '$';
    print_err("Missing $ inserted");
    help_dollar();
    ins_error();
}

report_illegal_case ()
{
    you_cant();
    help_illegal_case();
    error();
}

you_cant ()
{
    print_err("You can't use `");
    print_cmd_chr(cur_cmd, cur_chr);
    print("' in ");
    print_mode(mode);
}

bool
privileged ()
{
    if (mode > 0)
        return TRUE;
    else {
        report_illegal_case();
        return FALSE;
    }
}

bool
its_all_over ()
{
    if (privileged()) {
        if (page_head == page_tail &&
            head == tail &&
            dead_cycles == 0) {
#ifdef INCTEX
		format_continue = FALSE;
#endif
            return TRUE;
        }
        back_input();
        tail_append(new_null_box());
        width(tail) = hsize;
        tail_append(new_glue(fill_glue));
        tail_append(new_penalty(-010000000000));
        build_page();
    }
    return FALSE;
}

/*
 * Help text
 */

help_dollar ()
{
    help2("I've inserted a begin-math/end-math symbol since I think",
    "you left one out. Proceed, with fingers crossed.");
}

help_illegal_case ()
{
    help4("Sorry, but I'm not programmed to handle this case;",
    "I'll just pretend that you didn't ask for it.",
    "If you're in the wrong mode, you might be able to",
    "return to the right one by typing `I}' or `I$' or `I\\par'.");
}

