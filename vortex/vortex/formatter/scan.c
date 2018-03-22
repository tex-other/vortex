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


/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		scan.c
 */

#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"arith.h"
#include	"eq.h"
#include	"token.h"
#include	"tokenstack.h"
#include	"tokenlists.h"
#include	"evalstack.h"
#include	"char.h"
#include	"str.h"
#include	"box.h"
#include	"tfm.h"
#include	"dvi.h"
#include	"print.h"
#include	"error.h"
#include	"page.h"
#include	"scan.h"

#ifdef VORTEX
#include    "allir.h"
#include    "var.h"
#include    "macro.h"
#endif

extern gord	cur_order;
extern val	cur_val;
extern int	cur_val_level;
extern int	radix;

scan_left_brace ()
{
	get_nbrx_token();
	if (cur_cmd != LEFT_BRACE) {
		print_err("Missing { inserted");
		help_left();
		back_error();
		cur_tok = LEFT_BRACE_TOKEN;
		cur_cmd = LEFT_BRACE;
		cur_chr= '{';
		incr(align_state);
	}

#ifdef VORTEX
	else {
	  if (real_token == TRUE && group_s_node_begin != 0) {	/* if group node is to be made */
	    make_group_node(group_s_node_begin);
	    group_s_node_begin = 0;
	  }
	}
#endif
}

scan_optional_equals ()
{
	get_nbx_token();
	if (cur_tok != OTHER_TOKEN + '=')
		back_input();
}

bool
scan_keyword (s)
	char*	s;
{
	char	c;
	ptr		p;
	ptr		q;

#ifdef VORTEX

	short	save_token_count;
	
	save_token_count = token_count;
#endif

	p = backup_head;
	link(p) = NULL;
	c = *s;
	while (c != NUL) {
		get_x_token();
		if (cur_cs == 0 && (cur_chr == c || cur_chr == c - 'a' + 'A')) {
			store_new_token(cur_tok);
			incr(s);
			c = *s;
		} else if (cur_cmd != SPACER || p != backup_head) {
			back_input();
			if (p != backup_head)
				back_list(link(backup_head));

#ifdef VORTEX
			token_count = save_token_count;
#endif

			return FALSE;
		}
	}
	flush_list(link(backup_head));
	return TRUE;
}

mu_error ()
{
	print_err("Incompatible glue units");
	help_mu_error();
	error();
}

#define	scanned_result(CV, CVL) \
	{cur_val = CV; cur_val_level = CVL;}

#define	negate_glue() \
	{width(cur_val) = -width(cur_val); \
	stretch(cur_val) = -stretch(cur_val); \
	shrink(cur_val) = -shrink(cur_val);}

scan_something_internal (level, negative)
	int		level;
	bool	negative;
{
	hword	m;
	int		p;

	m = cur_chr;
	switch (cur_cmd)
	{
	case DEF_CODE:
		scan_seven_bit_int();
		if (m == MATH_CODE_BASE)
			{scanned_result(ho(math_code(cur_val)), INT_VAL);}
		else if (m < MATH_CODE_BASE)
			{scanned_result(equiv(m + cur_val), INT_VAL);}
		else {scanned_result(eqtb[m + cur_val].i, INT_VAL);}
		break;

	case TOKS_REGISTER:
	case ASSIGN_TOKS:
	case DEF_FAMILY:
	case SET_FONT:
	case DEF_FONT:
		if (level != TOK_VAL) {
			print_err("Missing number, treated as zero");
			help_missing_number();
			back_error();
			scanned_result(0, DIMEN_VAL);
		} else if (cur_cmd <= ASSIGN_TOKS) {
			if (cur_cmd < ASSIGN_TOKS)  {
				scan_eight_bit_int();
				m = TOKS_BASE + cur_val;
			}
			scanned_result(equiv(m), TOK_VAL);
		} else {
			back_input();
			scan_font_ident();
			scanned_result(FONT_ID_BASE + cur_val, IDENT_VAL);
		}
		break;

	case ASSIGN_INT:
		scanned_result(eqtb[m].i, INT_VAL);
		break;

	case ASSIGN_DIMEN:
		scanned_result(eqtb[m].sc, DIMEN_VAL);
		break;

	case ASSIGN_GLUE:
		scanned_result(equiv(m), GLUE_VAL);
		break;
	
	case ASSIGN_MU_GLUE:
		 scanned_result(equiv(m), MU_VAL);
		 break;

	case SET_AUX:
		if (abs(mode) != m) {
			print_err("Improper ");
			print_cmd_chr(SET_AUX, m);
			help_aux();
			error();
			if (level != TOK_VAL)
				{scanned_result(0, DIMEN_VAL);}
			else scanned_result(0, INT_VAL);
		} else {
			cur_val = aux; 
			if (m == VMODE)
				cur_val_level = DIMEN_VAL;
			else cur_val_level = INT_VAL;
		}
		break;

	case SET_PREV_GRAF:
		nest[nest_ptr] = cur_list;
		p = nest_ptr;
		while (abs(nest[p].mode_field) != VMODE)
			decr(p);
		scanned_result(nest[p].pg_field, INT_VAL);
		break;

	case SET_PAGE_INT:
		if (m == 0)	
			cur_val = dead_cycles;
		else cur_val = insert_penalties;
		cur_val_level = INT_VAL;
		break;

	case SET_PAGE_DIMEN:
		if (page_contents == EMPTY)
			if (m == 0)
				cur_val = MAX_DIMEN; 
			else cur_val = 0;
		else cur_val = page_so_far[m];
		cur_val_level = DIMEN_VAL;
		break;
	
	case SET_SHAPE:
		if (par_shape_ptr == NULL)
			cur_val = 0;
		else cur_val = info(par_shape_ptr);
		cur_val_level = INT_VAL;
		break;
	
	case SET_BOX_DIMEN:
		scan_eight_bit_int();
		if (box(cur_val) == NULL)
			cur_val = 0;
		else cur_val = mem[box(cur_val) + m].sc;
		cur_val_level = DIMEN_VAL;
		break;
	
	case CHAR_GIVEN:
	case MATH_GIVEN:
		scanned_result(cur_chr, INT_VAL);
		break;
	
	case ASSIGN_FONT_DIMEN:
		find_font_dimen(FALSE);
		font_info[fmem_ptr].sc = 0;
		scanned_result(font_info[cur_val].sc, DIMEN_VAL); 
		break;
	
	case ASSIGN_FONT_INT:
		scan_font_ident();
		if (m == 0)
			{scanned_result(hyphen_char[cur_val], INT_VAL);}
		else scanned_result(skew_char[cur_val], INT_VAL);
		break;

	case REGISTER:
		scan_eight_bit_int();
		switch (m)
		{
		case INT_VAL:
			cur_val = count(cur_val);
			break;

		case DIMEN_VAL:
			cur_val = dimen(cur_val);
			break;

		case GLUE_VAL:
			cur_val = skip(cur_val);
			break;
		}
		cur_val_level = m;
		break;

	case LAST_ITEM:
		if (cur_chr == GLUE_VAL)
			cur_val = zero_glue;
		else cur_val = 0;
		cur_val_level = cur_chr;
		if (!is_char_node(tail) && mode != 0) {
			switch (cur_chr)
			{
			case INT_VAL:
				if (type(tail) == PENALTY_NODE)
					cur_val = penalty(tail);
				break;

			case DIMEN_VAL:
				if (type(tail) == KERN_NODE)
					cur_val = width(tail);
				break;

			case GLUE_VAL:
				if (type(tail) == GLUE_NODE) {
					cur_val = glue_ptr(tail); 
					if (subtype(tail) == MU_GLUE)
						cur_val_level = MU_VAL;
				}
				break;

			default:
				break;
			}
		} else if (mode == VMODE && tail == head) {
			switch (cur_chr)
			{
			case INT_VAL:
				cur_val = last_penalty;
				break;

			case DIMEN_VAL:
				cur_val = last_kern;
				break;

			case GLUE_VAL:
				if (last_glue != MAX_HALFWORD)
					cur_val = last_glue;
				break;
			}
		}
		break;
			
	default:
#ifdef VORTEX
		/* if '$' was wrong char, discard math que entry */
		if (cat_code(cur_chr) == MATH_SHIFT) {
			math_que_top = math_rlink(math_que_top);
			if (math_que_top != NIL)
				math_llink(math_que_top) = NIL;
		}
#endif		
		print_err("You can't use `");
		print_cmd_chr(cur_cmd, cur_chr);
		print("' after ");
		print_esc("the");
		help_thee();
		error();
		if (level != TOK_VAL)
			{scanned_result(0, DIMEN_VAL);}
		else {scanned_result(0, INT_VAL);}
		break;
	}
	while (cur_val_level > level) {
		if (cur_val_level == GLUE_VAL)
			cur_val = width(cur_val);
		else if (cur_val_level == MU_VAL)
			mu_error();
		decr(cur_val_level);
	}
	if (negative) {
		if (cur_val_level >= GLUE_VAL) {
			cur_val = new_spec(cur_val);
			negate_glue();
		} else negate(cur_val);
	} else if (cur_val_level >= GLUE_VAL && cur_val_level <= MU_VAL)
		add_glue_ref(cur_val);
}

scan_seven_bit_int ()
{
	scan_int();
	if (cur_val < 0 || cur_val > 127) {
		print_err("Bad character code");
		help_char();
		int_error(cur_val);
		cur_val = 0;
	}
}

scan_eight_bit_int ()
{
	scan_int();
	if (cur_val < 0 || cur_val > 255) {
		print_err("Bad register code");
		help_reg();
		int_error(cur_val);
		cur_val = 0;
	}
}

scan_four_bit_int()
{
	scan_int();
	if (cur_val < 0 || cur_val > 15) {
		print_err("Bad number");
		help_number();
		int_error(cur_val);
		cur_val = 0;
	}
}

scan_char_num ()
{
	scan_int();
	if (cur_val < 0 || cur_val > 255) {
		print_err("Bad character code");
		help_char();
		int_error(cur_val);
		cur_val = 0;
	}
}

scan_fifteen_bit_int ()
{
	scan_int();
	if (cur_val < 0 || cur_val > 077777) {
		print_err("Bad math code");
		help_mathchar();
		int_error(cur_val);
		cur_val = 0;
	}
}

scan_twenty_seven_bit_int ()
{
	scan_int();
	if (cur_val < 0 || cur_val> 0777777777) {
		print_err("Bad delimiter code");
		help_del();
		int_error(cur_val);
		cur_val = 0;
	}
}

#define	get_nbsx_token() \
	{negative = FALSE; \
	do {get_nbx_token(); \
		if (cur_tok == MINUS_TOKEN) { \
			negative = !negative; \
			cur_tok = PLUS_TOKEN;} \
	} while (cur_tok == PLUS_TOKEN);}

scan_int ()
{
	int		d;
	val		m;
	bool	vacuous;
	bool	negative;
	bool	OK_so_far;

	d = 0;
	radix = 0;
	OK_so_far = TRUE;
	get_nbsx_token();
	if (cur_tok == ALPHA_TOKEN) {
		get_token();
		if (cur_tok < CS_TOKEN_FLAG) {
			cur_val = cur_chr;
			if (cur_cmd <= RIGHT_BRACE) {
				if (cur_cmd == RIGHT_BRACE)
					incr(align_state);
				else decr(align_state);
			}
		} else if (cur_tok < CS_TOKEN_FLAG + SINGLE_BASE)
			cur_val = cur_tok - CS_TOKEN_FLAG - ACTIVE_BASE;
		else cur_val = cur_tok - CS_TOKEN_FLAG - SINGLE_BASE;
		if (cur_val > 127) {
			print_err("Improper alphabetic constant");
			help_char_const();
			cur_val = 0;
			back_error();
		} else scan_optional_space();
	} else if (cur_cmd >= MIN_INTERNAL && cur_cmd <= MAX_INTERNAL)
		scan_something_internal(INT_VAL, FALSE);
	else {
		radix = 10;
		m = 214748364;
		if (cur_tok == OCTAL_TOKEN) {
			radix = 8;
			m = 02000000000;
			get_x_token();
		} else if (cur_tok == HEX_TOKEN) {
			radix = 16;
			m = 010000000000;
			get_x_token();
		}
		vacuous = TRUE;
		cur_val = 0;
		loop {
			if (cur_tok < ZERO_TOKEN + radix &&
				cur_tok >= ZERO_TOKEN &&
				cur_tok <= ZERO_TOKEN + 9)
				d = cur_tok - ZERO_TOKEN;
			else if (radix == 16) {
				if (cur_tok <= A_TOKEN + 5 && cur_tok >= A_TOKEN)
					d = cur_tok - A_TOKEN + 10;
				else if (cur_tok <= OTHER_A_TOKEN + 5 &&
					cur_tok >= OTHER_A_TOKEN)
					d = cur_tok - OTHER_A_TOKEN + 10;
				else break;
			} else break;
			vacuous = FALSE;
			if	(cur_val >= m && (cur_val > m || d > 7 || radix != 10)) {
				if (OK_so_far) {
					print_err("Number too big"); 
					help_big_num();
					error();
					cur_val = INFINITY;
					OK_so_far = FALSE;
				}
			} else cur_val = cur_val * radix + d;
			get_x_token();
		}
		if (vacuous) {
			print_err("Missing number, treated as zero");
			help_missing_number();
			back_error();
		} else if (cur_cmd != SPACER)
			back_input();
	}
	if (negative)
		negate(cur_val);
}	
	
#define	set_conversion(NUM, DENOM) \
	{num = NUM; denom = DENOM;}

scan_dimen (mu, inf, shortcut)
	bool	mu;
	bool	inf;
	bool	shortcut;
{
	val		f;
	int		k;
	scal	v;
	val		num;
	val		denom;
	bool	negative;
	val		save_cur_val;

	f = 0;
	negative = FALSE;
	arith_error = FALSE;
	cur_order = NORMAL;
	if (!shortcut) {
		get_nbsx_token();
		if (cur_cmd >= MIN_INTERNAL && cur_cmd <= MAX_INTERNAL)	{
			if (mu) {
				scan_something_internal(MU_VAL, FALSE);
				if (cur_val_level >= GLUE_VAL) {
					v = width(cur_val);
					delete_glue_ref(cur_val);
					cur_val = v;
				}
				if (cur_val_level == MU_VAL)
					goto attach_sign;
				if (cur_val_level != INT_VAL)
					mu_error();
			} else {
				scan_something_internal(DIMEN_VAL, FALSE);
				if (cur_val_level == DIMEN_VAL)
					goto attach_sign;
			}
		} else {
			back_input();
			if (cur_tok == EURO_POINT_TOKEN)
				cur_tok = POINT_TOKEN;
			if (cur_tok != POINT_TOKEN)
				scan_int();
			else {
				radix = 10;
				cur_val = 0;
			}
			if (cur_tok == EURO_POINT_TOKEN)
				cur_tok = POINT_TOKEN;
			if (radix == 10 && cur_tok == POINT_TOKEN) {
				k = 0;
				get_token();
				loop {
					get_x_token();
					if (cur_tok > ZERO_TOKEN + 9 || cur_tok < ZERO_TOKEN)
						break;
					if (k < 17) {
						dig[k] = cur_tok - ZERO_TOKEN;
						incr(k);
					}
				}
				f = round_decimals(k);
				if (cur_cmd != SPACER)
					back_input();
			}
		}
	}
	if (cur_val < 0) {
		negative = !negative;
		negate(cur_val);
	}
	if (inf) {
		if (scan_keyword("fil")) {
			cur_order = FIL;
			while (scan_keyword("l")) {
				if (cur_order == FILLL) {
					print_err("Illegal unit of measure (");
					print("replaced by filll)");
					help_filll();
					error();
				} else incr(cur_order);
			}
			goto attach_fraction;
		}
	}
	save_cur_val = cur_val;
	get_nbx_token();
	if (cur_cmd < MIN_INTERNAL || cur_cmd > MAX_INTERNAL)
		back_input();
	else {
		if (mu) {
			scan_something_internal(MU_VAL, FALSE);
			if (cur_val_level >= GLUE_VAL) {
				v = width(cur_val);
				delete_glue_ref(cur_val);
				cur_val = v;
			}
			if (cur_val_level != MU_VAL)
				mu_error();
		} else scan_something_internal(DIMEN_VAL, FALSE);
		v = cur_val;
		goto found;
	}
	if (mu)
		goto not_found;
	if (scan_keyword("em"))
		v = quad(cur_font);
	else if (scan_keyword("ex"))
		v = x_height(cur_font);
	else goto not_found;
	scan_optional_space();

found:
	cur_val = nx_plus_y(save_cur_val, v, xn_over_d(v, f, 0200000L));
	goto attach_sign;

not_found:
	if (mu)  {
		if (scan_keyword("mu"))
			goto attach_fraction;
		else {
			print_err("Illegal unit of measure (");
			print("mu inserted)");
			help_mu();
			error();
			goto attach_fraction;
		}
	}
	if (scan_keyword("true")) {
		prepare_mag();
		if (mag != 1000) {
			cur_val = xn_over_d(cur_val, 1000L, mag);
			f = (1000 * f + 0200000 * remainder) / mag;
			cur_val += f / 0200000;
			f %= 0200000;
		}
	}
	if (scan_keyword("pt"))
		goto attach_fraction;
	if (scan_keyword("in"))	{set_conversion(7227, 100);}
	else if (scan_keyword("pc")) {set_conversion(12, 1);}
	else if (scan_keyword("cm")) {set_conversion(7227, 254);}
	else if (scan_keyword("mm")) {set_conversion(7227, 2540);}
	else if (scan_keyword("bp")) {set_conversion(7227, 7200);}
	else if (scan_keyword("dd")) {set_conversion(1238, 1157);}
	else if (scan_keyword("cc")) {set_conversion(14856, 1157);}
	else if (scan_keyword("sp")) goto done;
	else {
		print_err("Illegal unit of measure (");
		print("pt inserted)");
		help_dimen();
		error();
		goto attach_fraction;
	}
	cur_val = xn_over_d(cur_val, num, denom);
	f = (num * f + 0200000 * remainder) / denom;
	cur_val += f / 0200000;
	f %= 0200000;

attach_fraction:
	if (cur_val >= 0400000)
		arith_error = TRUE;
	else cur_val = cur_val * UNITY + f;

done:
	scan_optional_space();

attach_sign:
	if (arith_error || abs(cur_val) >= 010000000000) {
		print_err("Dimension too large");
		help_big_dimen();
		error();
		cur_val = MAX_DIMEN;
		arith_error = FALSE;
	}
	if (negative)
		negate(cur_val);
}

scan_glue (level)
	int		level;
{
	ptr		q;
	bool	mu;
	bool	negative;

	mu = (level == MU_VAL);
	get_nbsx_token();
	if (cur_cmd >= MIN_INTERNAL && cur_cmd <= MAX_INTERNAL) {
		scan_something_internal(level, negative);
		if (cur_val_level >= GLUE_VAL) {
			if (cur_val_level != level)
				mu_error();
			return;
		}
		if (cur_val_level == INT_VAL)
			scan_dimen(mu, FALSE, TRUE);
		else if (level == MU_VAL)
			mu_error();
	} else {
		back_input();
		scan_dimen(mu, FALSE, FALSE);
		if (negative)
			negate(cur_val);
	}
	q = new_spec(zero_glue);
	width(q) = cur_val;
	if (scan_keyword("plus")) {
		scan_dimen(mu, TRUE, FALSE);
		stretch(q) = cur_val;
		stretch_order(q) = cur_order;
	}
	if (scan_keyword("minus")) {
		scan_dimen(mu, TRUE, FALSE);
		shrink(q) = cur_val;
		shrink_order(q) = cur_order;
	}
	cur_val = q;
}

ptr
scan_rule_spec ()
{
	ptr		q;

	q = new_rule();
	if (cur_cmd == VRULE)
		width(q) = DEFAULT_RULE;
	else {
		height(q) = DEFAULT_RULE;
		depth(q) = 0;
	}
#ifdef VORTEX
	ir_rule(q) = (_Node *) cur_cs_node;
#endif
reswitch:
	if (scan_keyword("width")) {
		scan_normal_dimen();
		width(q) = cur_val;
		goto reswitch;
	}
	if (scan_keyword("height")) {
		scan_normal_dimen();
		height(q) = cur_val;
		goto reswitch;
	}
	if (scan_keyword("depth")) {
		scan_normal_dimen();
		depth(q) = cur_val;
		goto reswitch;
	}
	return q;
}

/*
 * 	Help text
 */

help_mathchar ()
{
	help2("A numeric \\mathchar code must be between 0 and 32767.",
	"I changed this one to zero");
}

help_number ()
{
	help2("Since I expected to read a number between 0 and 15,",
	"I changed this one to zero.");
}

help_char ()
{
	help2("The numeric code following \\char must be between 0 and 255.",
	"I changed this one to zero.");
}

help_char_const ()
{
	help2("A one-character control sequence belongs after a ` mark.",
	"So I'm essentially inserting \\0 here.");
}

help_big_num ()
{
	help2("I can only go up to 2147483647 = 017777777777 = 0x7FFFFFFF.",
	"so I'm using that number instead of yours.");
}

help_missing_number ()
{
	help3("A number should have been here; I inserted `0'.",
	"(If you can't figure out why I needed a number,",
	"look up `weird error' in the TeX manual index.)");
}

help_filll ()
{
	help1("I dddon't go any higher than filll.");
}

help_mu ()
{
	help4("The unit of measurement in \\mskip glue must be mu.",
	"To recover gracefully from this error, it's best to",
	"delete the erroneous units; e.g. type `2' to delete",
	"two letters. (See Chapter 27 of the manual.)");
}

help_mu_error ()
{
	help1("I'm going to assume that 1mu=1pt when they're mixed.");
}

help_dimen ()
{
	help6("Dimensions can be in units of em, ex, in, pt, pc,",
	"cm, mm, dd, cc, bp, or sp; but yours is a new one!",
	"I'll assume that you meant to say pt, for printers' points.",
	"To recover gracefully from this error, it's best to",
	"delete the erroneous units; e.g. type `2' to delete",
	"two letters. (See Chapter 27 of The TeXbook.)");
}
	
help_big_dimen ()
{
	help2("I can't work with sizes bigger than about 19 feet.",
	"Continue and I'll use the largest value I can.");
}

help_thee ()
{
	help2("I'm forgetting what you said",
	"and using zero for this \\the.");
}

help_left ()
{
	help4("A left brace was mandatory here, so I've put one in.",
	"You might want to delete and/or insert some corrections",
	"so that I will find a matching right brace soon.",
	"If you're confused by all this, try typing `I}' now.")
}

help_aux ()
{
	help4("You can refer to \\spacefactor only in horizontal mode;",
	"you can refer to \\prevdepth only in vertical mode; and",
	"neither of these is meaningful inside \\write. So",
	"I'm forgetting what you said and using zero instead.");
}

help_del ()
{
	help2("A numeric delimiter code must be between 0 and 2^{27}-1.",
	"I changed this one to zero.");
}

help_reg ()
{
	help1("I'm forgetting what you said and not changing anything.");
}
