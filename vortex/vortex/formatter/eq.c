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
 *		eq.c
 */

#include "tex.h"
#include "cmds.h"
#include "heap.h"
#include "char.h"
#include "token.h"
#include "hash.h"
#include "box.h"
#include "print.h"
#include "error.h"
#include "eq.h"

extern mword	eqtb[];
extern qword	xeq_level[];

print_skip_param (n)
	int		n;
{
	switch (n)
	{
	case LINE_SKIP_CODE:
		print_esc("lineskip"); 
		break;

	case BASELINE_SKIP_CODE:
		print_esc("baselineskip"); 
		break;

	case PAR_SKIP_CODE:
		print_esc("parskip"); 
		break;

	case ABOVE_DISPLAY_SKIP_CODE:
		print_esc("abovedisplayskip"); 
		break;

	case BELOW_DISPLAY_SKIP_CODE:
		print_esc("belowdisplayskip"); 
		break;

	case ABOVE_DISPLAY_SHORT_SKIP_CODE:
		print_esc("abovedisplayshortskip"); 
		break;

	case BELOW_DISPLAY_SHORT_SKIP_CODE:
		print_esc("belowdisplayshortskip"); 
		break;

	case LEFT_SKIP_CODE:
		print_esc("leftskip"); 
		break;

	case RIGHT_SKIP_CODE:
		print_esc("rightskip"); 
		break;

	case TOP_SKIP_CODE:
		print_esc("topskip"); 
		break;

	case SPLIT_TOP_SKIP_CODE:
		print_esc("splittopskip"); 
		break;

	case TAB_SKIP_CODE:
		print_esc("tabskip"); 
		break;

	case SPACE_SKIP_CODE:
		print_esc("spaceskip"); 
		break;

	case XSPACE_SKIP_CODE:
		print_esc("xspaceskip"); 
		break;

	case PAR_FILL_SKIP_CODE:
		print_esc("parfillskip"); 
		break;

	case THIN_MU_SKIP_CODE:
		print_esc("thinmuskip"); 
		break;

	case MED_MU_SKIP_CODE:
		print_esc("medmuskip"); 
		break;

	case THICK_MU_SKIP_CODE:
		print_esc("thickmuskip"); 
		break;

	default:
		print("[unknown glue parameter!]"); 
		break;
	}
}

print_param (n)
	int		n;
{
	switch (n)
	{
	case PRETOLERANCE_CODE:
		print_esc("pretolerance"); 
		break;

	case TOLERANCE_CODE:
		print_esc("tolerance"); 
		break;

	case LINE_PENALTY_CODE:
		print_esc("linepenalty"); 
		break;

	case HYPHEN_PENALTY_CODE:
		print_esc("hyphenpenalty"); 
		break;

	case EX_HYPHEN_PENALTY_CODE:
		print_esc("exhyphenpenalty"); 
		break;

	case CLUB_PENALTY_CODE:
		print_esc("clubpenalty"); 
		break;

	case WIDOW_PENALTY_CODE:
		print_esc("widowpenalty"); 
		break;

	case DISPLAY_WIDOW_PENALTY_CODE:
		print_esc("displaywidowpenalty"); 
		break;

	case BROKEN_PENALTY_CODE:
		print_esc("brokenpenalty"); 
		break;

	case BIN_OP_PENALTY_CODE:
		print_esc("binoppenalty"); 
		break;

	case REL_PENALTY_CODE:
		print_esc("relpenalty"); 
		break;

	case PRE_DISPLAY_PENALTY_CODE:
		print_esc("predisplaypenalty"); 
		break;

	case POST_DISPLAY_PENALTY_CODE:
		print_esc("postdisplaypenalty"); 
		break;

	case INTER_LINE_PENALTY_CODE:
		print_esc("interlinepenalty"); 
		break;

	case DOUBLE_HYPHEN_DEMERITS_CODE:
		print_esc("doublehyphendemerits"); 
		break;

	case FINAL_HYPHEN_DEMERITS_CODE:
		print_esc("finalhyphendemerits"); 
		break;

	case ADJ_DEMERITS_CODE:
		print_esc("adjdemerits"); 
		break;

	case MAG_CODE:
		print_esc("mag"); 
		break;

	case DELIMITER_FACTOR_CODE:
		print_esc("delimiterfactor"); 
		break;

	case LOOSENESS_CODE:
		print_esc("looseness"); 
		break;

	case TIME_CODE:
		print_esc("time"); 
		break;

	case DAY_CODE:
		print_esc("day"); 
		break;

	case MONTH_CODE:
		print_esc("month"); 
		break;

	case YEAR_CODE:
		print_esc("year"); 
		break;

	case SHOW_BOX_BREADTH_CODE:
		print_esc("showboxbreadth"); 
		break;

	case SHOW_BOX_DEPTH_CODE:
		print_esc("showboxdepth"); 
		break;

	case HBADNESS_CODE:
		print_esc("hbadness"); 
		break;

	case VBADNESS_CODE:
		print_esc("vbadness"); 
		break;

	case PAUSING_CODE:
		print_esc("pause"); 
		break;

	case TRACING_ONLINE_CODE:
		print_esc("tracingonline"); 
		break;

	case TRACING_MACROS_CODE:
		print_esc("tracingmacros"); 
		break;

	case TRACING_STATS_CODE:
		print_esc("tracingstats"); 
		break;

	case TRACING_PARAGRAPHS_CODE:
		print_esc("tracingparagraphs"); 
		break;

	case TRACING_PAGES_CODE:
		print_esc("tracingpages"); 
		break;

	case TRACING_OUTPUT_CODE:
		print_esc("tracingoutput"); 
		break;

	case TRACING_LOST_CHARS_CODE:
		print_esc("tracinglostchars"); 
		break;

	case TRACING_COMMANDS_CODE:
		print_esc("tracingcommands"); 
		break;

	case TRACING_RESTORES_CODE:
		print_esc("tracingrestores"); 
		break;

	case UC_HYPH_CODE:
		print_esc("uchyph"); 
		break;

	case OUTPUT_PENALTY_CODE:
		print_esc("outputpenalty"); 
		break;

	case MAX_DEAD_CYCLES_CODE:
		print_esc("maxdeadcycles"); 
		break;

	case HANG_AFTER_CODE:
		print_esc("hangafter"); 
		break;

	case FLOATING_PENALTY_CODE:
		print_esc("floatingpenalty"); 
		break;

	case GLOBAL_DEFS_CODE:
		print_esc("globaldefs"); 
		break;

	case CUR_FAM_CODE:
		print_esc("fam"); 
		break;

	case ESCAPE_CHAR_CODE:
		print_esc("escapechar"); 
		break;

	case DEFAULT_HYPHEN_CHAR_CODE:
		print_esc("defaulthyphenchar"); 
		break;

	case DEFAULT_SKEW_CHAR_CODE:
		print_esc("defaultskewchar"); 
		break;

	case END_LINE_CHAR_CODE:
		print_esc("endlinechar"); 
		break;

	case NEW_LINE_CHAR_CODE:
		print_esc("newlinechar"); 
		break;

	default:
		print("[unknown integer parameter!]"); 
		break;
	}
}

print_length_param (n)
	int		n;
{
	switch (n)
	{
	case PAR_INDENT_CODE:
		print_esc("parindent"); 
		break;

	case MATH_SURROUND_CODE:
		print_esc("mathsurround"); 
		break;

	case LINE_SKIP_LIMIT_CODE:
		print_esc("lineskiplimit"); 
		break;

	case HSIZE_CODE:
		print_esc("hsize"); 
		break;

	case VSIZE_CODE:
		print_esc("vsize"); 
		break;

	case MAX_DEPTH_CODE:
		print_esc("maxdepth"); 
		break;

	case SPLIT_MAX_DEPTH_CODE:
		print_esc("splitmaxdepth"); 
		break;

	case BOX_MAX_DEPTH_CODE:
		print_esc("boxmaxdepth"); 
		break;

	case HFUZZ_CODE:
		print_esc("hfuzz"); 
		break;

	case VFUZZ_CODE:
		print_esc("vfuzz"); 
		break;

	case DELIMITER_SHORTFALL_CODE:
		print_esc("delimitershortfall"); 
		break;

	case NULL_DELIMITER_SPACE_CODE:
		print_esc("nulldelimiterspace"); 
		break;

	case SCRIPT_SPACE_CODE:
		print_esc("scriptspace"); 
		break;

	case PRE_DISPLAY_SIZE_CODE:
		print_esc("predisplaysize"); 
		break;

	case DISPLAY_INDENT_CODE:
		print_esc("diplayindent"); 
		break;

	case DISPLAY_WIDTH_CODE:
		print_esc("displaywidth"); 
		break;

	case OVERFULL_RULE_CODE:
		print_esc("overfullrule"); 
		break;

	case HANG_INDENT_CODE:
		print_esc("hangindent"); 
		break;

	case H_OFFSET_CODE:
		print_esc("hoffset"); 
		break;

	case V_OFFSET_CODE:
		print_esc("voffset"); 
		break;

	default:
		print("[unknown dimen parameter!]"); 
		break;
	}
}

#ifdef STAT
show_eqtb (n)
	ptr		n;
{
	if (n < ACTIVE_BASE)
		print_char('?');
	else if (n < GLUE_BASE) {
		sprint_cs(n);
		print_char('=');
		print_cmd_chr(eq_type(n), equiv(n));
		if (eq_type(n) >= CALL) {
			print_char(':');
			show_token_list(link(equiv(n)), NULL, 32L);
		}
	} else if (n < LOCAL_BASE) {
		if (n < SKIP_BASE) {
			print_skip_param(n - GLUE_BASE);
			print_char('=');
			if (n < GLUE_BASE + THIN_MU_SKIP_CODE)
				print_spec(equiv(n), "pt");
			else print_spec(equiv(n), "mu");
		} else if (n < MU_SKIP_BASE) {
			print_esc("skip");
			print_int(n - SKIP_BASE); 
			print_char('=');
			print_spec(equiv(n), "pt");
		} else {
			print_esc("muskip");
			print_int(n - MU_SKIP_BASE);
			print_char('=');
			print_spec(equiv(n), "mu");
		}
	} else if (n < INT_BASE) {
		if (n == PAR_SHAPE_LOC) {
			print_esc("parshape");
			print_char('=');
			if (par_shape_ptr == NULL)
				print_char('O');
			else print_int(info(par_shape_ptr));
		} else if (n < TOKS_BASE) {
			print_cmd_chr(ASSIGN_TOKS, n);
			print_char('=');
			if (equiv(n) != NULL)
				show_token_list(link(equiv(n)), NULL, 32L);
		} else if (n < BOX_BASE) {
			print_esc("toks");
			print_int(n - TOKS_BASE);
			print_char('=');
			if (equiv(n) != NULL)
				show_token_list(link(equiv(n)), NULL, 32L);
		} else if (n < CUR_FONT_LOC) {
			print_esc("box");
			print_int(n - BOX_BASE);
			print_char('=');
			if (equiv(n) == NULL)
				print("void");
			else {
				depth_threshold = 0;
				breadth_max = 1;
				show_node_list(equiv(n));
			}
		} else if (n < CAT_CODE_BASE) {
			if (n == CUR_FONT_LOC)
				print("current font");
			else if (n < MATH_FONT_BASE + 16) {
				print_esc("textfont");
				print_int(n - MATH_FONT_BASE);
			} else if (n < MATH_FONT_BASE + 32) {
				print_esc("scriptfont");
				print_int(n - MATH_FONT_BASE - 16);
			} else {
				print_esc("scriptscritpfont");
				print_int(n - MATH_FONT_BASE - 32);
			}
			print_char('=');
			print_esc("");
			print_str(hash[FONT_ID_BASE + equiv(n)].hh1.rh);
		} else {
			if (n < MATH_CODE_BASE) {
				if (n < LC_CODE_BASE) {
					print_esc("catcode");
					print_int(n - CAT_CODE_BASE);
				} else if (n < UC_CODE_BASE) {
					print_esc("lccode");
					print_int(n - LC_CODE_BASE);
				} else if (n < SF_CODE_BASE) {
					print_esc("uccode");
					print_int(n - UC_CODE_BASE);
				} else {
					print_esc("sfcode");
					print_int(n - SF_CODE_BASE);
				}
				print_char('=');
				print_int(equiv(n));
			} else {
				print_esc("math_code");
				print_int(n - MATH_CODE_BASE);
				print_char('=');
				print_int(ho(equiv(n)));
			}
		}
	} else if (n < DIMEN_BASE) {
		if (n < COUNT_BASE)
			print_param(n - INT_BASE);
		else if (n < DEL_CODE_BASE) {
			print_esc("count");
			print_int(n - COUNT_BASE);
		} else {
			print_esc("delcode");
			print_int(n - DEL_CODE_BASE);
		}
		print_char('=');
		print_val(eqtb[n].i);
	} else if (n <= EQTB_SIZE) {
		if (n < SCALED_BASE)
			print_length_param(n - DIMEN_BASE);
		else {
			print_esc("dimen");
			print_int(n - SCALED_BASE);
		}
		print_char('=');
		print_scaled(eqtb[n].sc);
		print("pt");
	} else print_char('?');
}
#endif

init_eq ()
{
	int		k;

	for (k = 0; k <= EQTB_SIZE - INT_BASE; incr(k))
		xeq_level[k] = LEVEL_ONE;

#ifdef INIT
	eq_type(UNDEFINED_CONTROL_SEQUENCE) = UNDEFINED_CS;
	equiv(UNDEFINED_CONTROL_SEQUENCE) = NULL;
	eq_level(UNDEFINED_CONTROL_SEQUENCE) = LEVEL_ZERO;
	for (k = ACTIVE_BASE; k < UNDEFINED_CONTROL_SEQUENCE; incr(k))
		eqtb[k]= eqtb[UNDEFINED_CONTROL_SEQUENCE];
			
	equiv(GLUE_BASE) = zero_glue;
	eq_level(GLUE_BASE) = LEVEL_ONE;
	eq_type(GLUE_BASE) = GLUE_REF;
	for (k = GLUE_BASE + 1; k < LOCAL_BASE; incr(k))
		eqtb[k] = eqtb[GLUE_BASE];
	glue_ref_count(zero_glue) += LOCAL_BASE - GLUE_BASE;

	par_shape_ptr = NULL;
	eq_type(PAR_SHAPE_LOC) = SHAPE_REF;
	eq_level(PAR_SHAPE_LOC)= LEVEL_ONE;
	for (k = OUTPUT_ROUTINE_LOC; k < TOKS_BASE + 256; incr(k))
		eqtb[k] = eqtb[UNDEFINED_CONTROL_SEQUENCE];
	box(0) = NULL;
	eq_type(BOX_BASE) = BOX_REF;
	eq_level(BOX_BASE) = LEVEL_ONE;
	for (k = BOX_BASE + 1; k < BOX_BASE + 256; incr(k))
		eqtb[k] = eqtb[BOX_BASE];
	cur_font = NULL_FONT;
	eq_type(CUR_FONT_LOC) = DATA;
	eq_level(CUR_FONT_LOC) = LEVEL_ONE;
	for (k = MATH_FONT_BASE; k < MATH_FONT_BASE + 48; incr(k))
		eqtb[k] = eqtb[CUR_FONT_LOC];
	equiv(CAT_CODE_BASE) = 0;
	eq_type(CAT_CODE_BASE) = DATA;
	eq_level(CAT_CODE_BASE) = LEVEL_ONE;
	for (k = CAT_CODE_BASE; k < INT_BASE; incr(k))
		eqtb[k] = eqtb[CAT_CODE_BASE];
	for (k = 0; k <= 127; incr(k)) {
		cat_code(k) = OTHER_CHAR;
		math_code(k) = hi(k);
		sf_code(k) = 1000;
	}
	cat_code(CARRIAGE_RETURN) = CAR_RET;
	cat_code(' ') = SPACER;
	cat_code('^') = SUP_MARK;
	cat_code('\\') = ESCAPE;
	cat_code('%') = COMMENT;
	cat_code(INVALID_CODE) = INVALID_CHAR;
	cat_code(NULL_CODE) = IGNORE;
	for (k = '0'; k <= '9'; incr(k))
		math_code(k) = hi(k + VAR_CODE);
	for (k = 'A'; k <= 'Z'; incr(k)) {
		cat_code(k) = cat_code(k + 'a' - 'A') = LETTER;
		math_code(k) = hi(k + VAR_CODE + 0x100);
		math_code(k + 'a' - 'A') = hi(k + 'a' - 'A'+ VAR_CODE + 0x100);
		lc_code(k) = lc_code(k + 'a' - 'A') = k + 'a' - 'A';
		uc_code(k) = uc_code(k + 'a' - 'A') = k;
		sf_code(k) = 999;
	}
	for (k = INT_BASE; k < DEL_CODE_BASE; incr(k))
		eqtb[k].i = 0;
	mag = 1000;
	tolerance = 10000;
	hang_after = 1;
	max_dead_cycles = 25;
	escape_char = '\\';
	end_line_char = CARRIAGE_RETURN;
	for (k = 0; k <= 127; incr(k))
		del_code(k) = -1;
	del_code('.') = 0;
	for (k = DIMEN_BASE; k <= EQTB_SIZE; incr(k))
		eqtb[k].sc = 0;
#endif
}
