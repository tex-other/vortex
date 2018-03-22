/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 * The above licensing information supersedes all licensing information
 * below.
 */

#ifndef MATH_
#define MATH_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		math.h
 */

ptr	new_style();

#define	STYLE_NODE			(UNSET_NODE + 1)
#define	STYLE_NODE_SIZE			3
#define	DISPLAY_STYLE			0
#define	TEXT_STYLE			2
#define	SCRIPT_STYLE			4
#define	SCRIPT_SCRIPT_STYLE		6
#define	CRAMPED				1

ptr	new_choice();

#define	CHOICE_NODE			(UNSET_NODE + 2)
#define	display_mlist(C)		info(C + 1)
#define	text_mlist(C)			link(C + 1)
#define	script_mlist(C)			info(C + 2)
#define	script_script_mlist(C)		link(C + 2)

ptr	new_noad();
#ifdef VORTEX
#define	NOAD_SIZE			5
#define	math_esc(T)			mem[T + 4].ir
#else
#define	NOAD_SIZE			4
#endif

#define	nucleus(N)			(N + 1)
#define	supscr(N)			(N + 2)
#define	subscr(N)			(N + 3)
#define	fam				font
#define	math_type			link

#define	ORD_NOAD			(UNSET_NODE + 3)
#define	OP_NOAD				(ORD_NOAD + 1)
#define	BIN_NOAD			(ORD_NOAD + 2)
#define	REL_NOAD			(ORD_NOAD + 3)
#define	OPEN_NOAD			(ORD_NOAD + 4)
#define	CLOSE_NOAD			(ORD_NOAD + 5)
#define	PUNCT_NOAD			(ORD_NOAD + 6)
#define	INNER_NOAD			(ORD_NOAD + 7)

#define	MATH_CHAR			1
#define	SUB_BOX				2
#define	SUB_MLIST			3
#define	MATH_TEXT_CHAR			4

#define	LIMITS				1
#define	NO_LIMITS			2

#define	left_delimiter(D)		(D + 4)
#define	right_delimiter(D)		(D + 5)
	
#define	small_fam(D)			mem[D].qqqq.b0
#define	small_char(D)			mem[D].qqqq.b1
#define	large_fam(D)			mem[D].qqqq.b2
#define	large_char(D)			mem[D].qqqq.b3
	
#define	RADICAL_NOAD			(INNER_NOAD + 1)
#define	RADICAL_NOAD_SIZE		5
#define	FRACTION_NOAD			(RADICAL_NOAD + 1)
#define	FRACTION_NOAD_SIZE		6
#define	DEFAULT_CODE			010000000000
#define	thickness			width
#define	numerator			supscr
#define	denominator			subscr

#define	null_delimiter			null_character

global	twoh	empty_field;

#define	UNDER_NOAD			(FRACTION_NOAD + 1)
#define	OVER_NOAD			(UNDER_NOAD + 1)
#define	ACCENT_NOAD			(OVER_NOAD + 1)
#define	ACCENT_NOAD_SIZE		5
#define	accent_chr(A)			(A + 4)
#define	VCENTER_NOAD			(ACCENT_NOAD + 1)
#define	LEFT_NOAD			(VCENTER_NOAD + 1)
#define	RIGHT_NOAD			(LEFT_NOAD + 1)
#define	delimiter			nucleus	

#define	scripts_allowed(N)		(type(N) >= ORD_NOAD && type(N) < LEFT_NOAD)

int		print_fam_and_char();
int		print_delimiter();
int		print_subsidiary_data();
int		print_style();
int		print_size();

#define	TEXT_SIZE			0
#define	SCRIPT_SIZE			16
#define	SCRIPT_SCRIPT_SIZE		32

int		show_normal_noad();
int		show_fraction_noad();

#define	mathsy(F, P) \
	font_info[param_base[fam_fnt(2 + F)] + P].sc

#define	math_x_height(F)		mathsy(F, 5)
#define	math_quad(F)			mathsy(F, 6)
#define	num1(F)				mathsy(F, 8)
#define	num2(F)				mathsy(F, 9)
#define	num3(F)				mathsy(F, 10)
#define	denom1(F)			mathsy(F, 11)
#define	denom2(F)			mathsy(F, 12)
#define	sup1(F)				mathsy(F, 13)
#define	sup2(F)				mathsy(F, 14)
#define	sup3(F)				mathsy(F, 15)
#define	sub1(F)				mathsy(F, 16)
#define	sub2(F)				mathsy(F, 17)
#define	sup_drop(F)			mathsy(F, 18)
#define	sub_drop(F)			mathsy(F, 19)
#define	delim1(F)			mathsy(F, 20)
#define	delim2(F)			mathsy(F, 21)
#define	axis_height(F)			mathsy(F, 22)

#define	TOTAL_MATHSY_PARAMS		22

#define	mathex(P) \
	font_info[param_base[fam_fnt(3 + cur_size)] + P].sc

#define	default_rule_thickness	mathex(8)
#define	big_op_spacing1			mathex(9)
#define	big_op_spacing2			mathex(10)
#define	big_op_spacing3			mathex(11)
#define	big_op_spacing4			mathex(12)
#define	big_op_spacing5			mathex(13)

#define	TOTAL_MATHEX_PARAMS		13

#define	cramped_style(S)		2 * (S / 2) + CRAMPED
#define	sub_style(S)			2 * (S / 4) + SCRIPT_STYLE + CRAMPED
#define	sup_style(S)			2 * (S / 4) + SCRIPT_STYLE + (S % 2)
#define	num_style(S)			S + 2 - 2 * (S / 6)
#define	denom_style(S)			2 * (S / 2) + CRAMPED + 2 - 2 * (S / 6)

#define change_size_and_mu() \
	{if (cur_style < SCRIPT_STYLE) \
		cur_size = TEXT_SIZE; \
	else cur_size = 16 * ((cur_style - TEXT_STYLE) / 2); \
	cur_mu = x_over_n(math_quad(cur_size), 18L);}

ptr		fraction_rule();
ptr		overbar();
ptr		var_delimiter();
int		stack_into_box();
scal		height_plus_depth();
ptr		char_box();
ptr		rebox();
ptr		math_glue();
int		math_kern();
int		flush_math();

#endif
