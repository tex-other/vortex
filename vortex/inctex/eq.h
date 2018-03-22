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

/* Copyright (c) 1992 Regents of the University of California
 * All rights reserved.
 * 
 *  This file has been modified, with permission from Pat Monardo, for IncTeX
 */
/* @(#)eq.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

#define eq_level_field(EQ)      EQ.hh.hh2.b1
#define eq_type_field(EQ)       EQ.hh.hh2.b0
#define equiv_field(EQ)         EQ.hh.hh2.rh
#define eq_level(EQ)            eq_level_field(eqtb[EQ])
#define eq_type(EQ)             eq_type_field(eqtb[EQ])
#define equiv(EQ)               equiv_field(eqtb[EQ])

#define LEVEL_ZERO              MIN_QUARTERWORD
#define LEVEL_ONE               (LEVEL_ZERO + 1)

#define ACTIVE_BASE             1
#define SINGLE_BASE             (ACTIVE_BASE + 256)
#define NULL_CS                 (SINGLE_BASE + 256)
#define HASH_BASE               (NULL_CS + 1)
#define FROZEN_CONTROL_SEQUENCE (HASH_BASE + HASH_SIZE)
#define FROZEN_PROTECTION       (FROZEN_CONTROL_SEQUENCE)
#define FROZEN_CR               (FROZEN_CONTROL_SEQUENCE + 1)
#define FROZEN_END_GROUP        (FROZEN_CONTROL_SEQUENCE + 2)
#define FROZEN_RIGHT            (FROZEN_CONTROL_SEQUENCE + 3)
#define FROZEN_FI               (FROZEN_CONTROL_SEQUENCE + 4)
#define FROZEN_END_TEMPLATE     (FROZEN_CONTROL_SEQUENCE + 5)
#define FROZEN_ENDV             (FROZEN_CONTROL_SEQUENCE + 6)
#define FROZEN_RELAX            (FROZEN_CONTROL_SEQUENCE + 7)
#define END_WRITE               (FROZEN_CONTROL_SEQUENCE + 8)
#define FROZEN_DONT_EXPAND      (FROZEN_CONTROL_SEQUENCE + 9)
#define FROZEN_NULL_FONT        (FROZEN_CONTROL_SEQUENCE + 10)
#define FONT_ID_BASE            (FROZEN_NULL_FONT - FONT_BASE)

#define UNDEFINED_CONTROL_SEQUENCE  (FROZEN_NULL_FONT + 257)

#define GLUE_BASE               (UNDEFINED_CONTROL_SEQUENCE + 1)
#define GLUE_PARS               18
#define glue_par(G)             equiv(GLUE_BASE + G)

#define LINE_SKIP_CODE                  0
#define BASELINE_SKIP_CODE              1
#define PAR_SKIP_CODE                   2
#define ABOVE_DISPLAY_SKIP_CODE         3
#define BELOW_DISPLAY_SKIP_CODE         4
#define ABOVE_DISPLAY_SHORT_SKIP_CODE   5
#define BELOW_DISPLAY_SHORT_SKIP_CODE   6
#define LEFT_SKIP_CODE                  7
#define RIGHT_SKIP_CODE                 8
#define TOP_SKIP_CODE                   9
#define SPLIT_TOP_SKIP_CODE             10
#define TAB_SKIP_CODE                   11
#define SPACE_SKIP_CODE                 12
#define XSPACE_SKIP_CODE                13
#define PAR_FILL_SKIP_CODE              14
#define THIN_MU_SKIP_CODE               15
#define MED_MU_SKIP_CODE                16
#define THICK_MU_SKIP_CODE              17

#define line_skip                   glue_par(LINE_SKIP_CODE)
#define baseline_skip               glue_par(BASELINE_SKIP_CODE)
#define par_skip                    glue_par(PAR_SKIP_CODE)
#define above_display_skip          glue_par(ABOVE_DISPLAY_SKIP_CODE)
#define below_display_skip          glue_par(BELOW_DISPLAY_SKIP_CODE)
#define above_display_short_skip    glue_par(ABOVE_DISPLAY_SHORT_SKIP_CODE)
#define below_display_short_skip    glue_par(BELOW_DISPLAY_SHORT_SKIP_CODE)
#define left_skip                   glue_par(LEFT_SKIP_CODE)
#define right_skip                  glue_par(RIGHT_SKIP_CODE)
#define top_skip                    glue_par(TOP_SKIP_CODE)
#define split_top_skip              glue_par(SPLIT_TOP_SKIP_CODE)
#define tab_skip                    glue_par(TAB_SKIP_CODE)
#define space_skip                  glue_par(SPACE_SKIP_CODE)
#define xspace_skip                 glue_par(XSPACE_SKIP_CODE)
#define par_fill_skip               glue_par(PAR_FILL_SKIP_CODE)
#define thin_mu_skip                glue_par(THIN_MU_SKIP_CODE)
#define med_mu_skip                 glue_par(MED_MU_SKIP_CODE)
#define thick_mu_skip               glue_par(THICK_MU_USKIP_CODE)

#define SKIP_BASE               (GLUE_BASE + GLUE_PARS)
#define skip(S)                 equiv(SKIP_BASE + S)

#define MU_SKIP_BASE            (SKIP_BASE + 256)
#define mu_skip(M)              equiv(MU_SKIP_BASE + M)

#define LOCAL_BASE              (MU_SKIP_BASE + 256)

#define PAR_SHAPE_LOC           LOCAL_BASE
#define OUTPUT_ROUTINE_LOC      (LOCAL_BASE + 1)
#define EVERY_PAR_LOC           (LOCAL_BASE + 2)
#define EVERY_MATH_LOC          (LOCAL_BASE + 3)
#define EVERY_DISPLAY_LOC       (LOCAL_BASE + 4)
#define EVERY_HBOX_LOC          (LOCAL_BASE + 5)
#define EVERY_VBOX_LOC          (LOCAL_BASE + 6)
#define EVERY_JOB_LOC           (LOCAL_BASE + 7)
#define EVERY_CR_LOC            (LOCAL_BASE + 8)
#define ERR_HELP_LOC            (LOCAL_BASE + 9)

#define TOKS_BASE               (LOCAL_BASE + 10)
#define BOX_BASE                (TOKS_BASE + 256)
#define CUR_FONT_LOC            (BOX_BASE + 256)
#define MATH_FONT_BASE          (CUR_FONT_LOC + 1)
#define CAT_CODE_BASE           (MATH_FONT_BASE + 48)
#define LC_CODE_BASE            (CAT_CODE_BASE + 256)
#define UC_CODE_BASE            (LC_CODE_BASE + 256)
#define SF_CODE_BASE            (UC_CODE_BASE + 256)
#define MATH_CODE_BASE          (SF_CODE_BASE + 256)

#define par_shape_ptr           equiv(PAR_SHAPE_LOC)
#define output_routine          equiv(OUTPUT_ROUTINE_LOC)
#define every_par               equiv(EVERY_PAR_LOC)
#define every_math              equiv(EVERY_MATH_LOC)
#define every_display           equiv(EVERY_DISPLAY_LOC)
#define every_hbox              equiv(EVERY_HBOX_LOC)
#define every_vbox              equiv(EVERY_VBOX_LOC)
#define every_job               equiv(EVERY_JOB_LOC)
#define every_cr                equiv(EVERY_CR_LOC)
#define err_help                equiv(ERR_HELP_LOC)
#define toks(M)                 equiv(TOKS_BASE + M)
#define box(M)                  equiv(BOX_BASE + M)
#ifdef  INCTEX
#define note_box(M)             note_eq(BOX_BASE + M)
#define before_box(M)           before_eq(BOX_BASE + M)
#define after_box(M)            after_eq(BOX_BASE + M)
/*	save changes to eqtb via box. DLP	*/
#endif  INCTEX
#define cur_font                equiv(CUR_FONT_LOC)
#define fam_fnt(M)              equiv(MATH_FONT_BASE + M)
#define cat_code(M)             equiv(CAT_CODE_BASE + M)
#define lc_code(M)              equiv(LC_CODE_BASE + M)
#define uc_code(M)              equiv(UC_CODE_BASE + M)
#define sf_code(M)              equiv(SF_CODE_BASE + M)
#define math_code(M)            equiv(MATH_CODE_BASE + M)

#define VAR_CODE                070000

#define INT_BASE                (MATH_CODE_BASE + 256)
#define INT_PARS                50
#define int_par(M)              eqtb[INT_BASE + M].i
#ifdef  INCTEX
#define note_int_par(M)         note_eq(INT_BASE + M)
#define before_int_par(M)       before_eq(INT_BASE + M)
#define after_int_par(M)        after_eq(INT_BASE + M)
/* so far this is never used */
#endif  INCTEX

#define PRETOLERANCE_CODE           0
#define TOLERANCE_CODE              1
#define LINE_PENALTY_CODE           2
#define HYPHEN_PENALTY_CODE         3       
#define CLUB_PENALTY_CODE           4
#define EX_HYPHEN_PENALTY_CODE      5
#define WIDOW_PENALTY_CODE          6
#define DISPLAY_WIDOW_PENALTY_CODE  7
#define BROKEN_PENALTY_CODE         8
#define BIN_OP_PENALTY_CODE         9
#define REL_PENALTY_CODE            10
#define PRE_DISPLAY_PENALTY_CODE    11
#define POST_DISPLAY_PENALTY_CODE   12
#define INTER_LINE_PENALTY_CODE     13
#define DOUBLE_HYPHEN_DEMERITS_CODE 14
#define FINAL_HYPHEN_DEMERITS_CODE  15
#define ADJ_DEMERITS_CODE           16
#define MAG_CODE                    17
#define DELIMITER_FACTOR_CODE       18
#define LOOSENESS_CODE              19
#define TIME_CODE                   20
#define DAY_CODE                    21
#define MONTH_CODE                  22
#define YEAR_CODE                   23
#define SHOW_BOX_BREADTH_CODE       24
#define SHOW_BOX_DEPTH_CODE         25
#define HBADNESS_CODE               26
#define VBADNESS_CODE               27
#define PAUSING_CODE                28
#define TRACING_ONLINE_CODE         29
#define TRACING_MACROS_CODE         30
#define TRACING_STATS_CODE          31
#define TRACING_PARAGRAPHS_CODE     32
#define TRACING_PAGES_CODE          33
#define TRACING_OUTPUT_CODE         34
#define TRACING_LOST_CHARS_CODE     35
#define TRACING_COMMANDS_CODE       36
#define TRACING_RESTORES_CODE       37
#define UC_HYPH_CODE                38
#define OUTPUT_PENALTY_CODE         39
#define MAX_DEAD_CYCLES_CODE        40
#define HANG_AFTER_CODE             41
#define FLOATING_PENALTY_CODE       42
#define GLOBAL_DEFS_CODE            43
#define CUR_FAM_CODE                44
#define ESCAPE_CHAR_CODE            45
#define DEFAULT_HYPHEN_CHAR_CODE    46
#define DEFAULT_SKEW_CHAR_CODE      47
#define END_LINE_CHAR_CODE          48
#define NEW_LINE_CHAR_CODE          49

#define pretolerance            int_par(PRETOLERANCE_CODE)
#define tolerance               int_par(TOLERANCE_CODE)
#define line_penalty            int_par(LINE_PENALTY_CODE)
#define hyphen_penalty          int_par(HYPHEN_PENALTY_CODE)
#define ex_hyphen_penalty       int_par(EX_HYPHEN_PENALTY_CODE)
#define club_penalty            int_par(CLUB_PENALTY_CODE)
#define widow_penalty           int_par(WIDOW_PENALTY_CODE)
#define display_widow_penalty   int_par(DISPLAY_WIDOW_PENALTY_CODE)
#define broken_penalty          int_par(BROKEN_PENALTY_CODE)
#define bin_op_penalty          int_par(BIN_OP_PENALTY_CODE)
#define rel_penalty             int_par(REL_PENALTY_CODE)
#define pre_display_penalty     int_par(PRE_DISPLAY_PENALTY_CODE)
#define post_display_penalty    int_par(POST_DISPLAY_PENALTY_CODE)
#define inter_line_penalty      int_par(INTER_LINE_PENALTY_CODE)
#define double_hyphen_demerits  int_par(DOUBLE_HYPHEN_DEMERITS_CODE)
#define final_hyphen_demerits   int_par(FINAL_HYPHEN_DEMERITS_CODE)
#define adj_demerits            int_par(ADJ_DEMERITS_CODE)
#define mag                     int_par(MAG_CODE)
#define delimiter_factor        int_par(DELIMITER_FACTOR_CODE)
#define looseness               int_par(LOOSENESS_CODE)
#define time                    int_par(TIME_CODE)
#define day                     int_par(DAY_CODE)
#define month                   int_par(MONTH_CODE)
#define year                    int_par(YEAR_CODE)
#define show_box_breadth        int_par(SHOW_BOX_BREADTH_CODE)
#define show_box_depth          int_par(SHOW_BOX_DEPTH_CODE)
#define hbadness                int_par(HBADNESS_CODE)
#define vbadness                int_par(VBADNESS_CODE)
#define pausing                 int_par(PAUSING_CODE)
#define tracing_online          int_par(TRACING_ONLINE_CODE)
#define tracing_macros          int_par(TRACING_MACROS_CODE)
#define tracing_stats           int_par(TRACING_STATS_CODE)
#define tracing_paragraphs      int_par(TRACING_PARAGRAPHS_CODE)
#define tracing_pages           int_par(TRACING_PAGES_CODE)
#define tracing_output          int_par(TRACING_OUTPUT_CODE)
#define tracing_lost_chars      int_par(TRACING_LOST_CHARS_CODE)
#define tracing_commands        int_par(TRACING_COMMANDS_CODE)
#define tracing_restores        int_par(TRACING_RESTORES_CODE)
#define uc_hyph                 int_par(UC_HYPH_CODE)
#define max_dead_cycles         int_par(MAX_DEAD_CYCLES_CODE)
#define output_penalty          int_par(OUTPUT_PENALTY_CODE)
#define hang_after              int_par(HANG_AFTER_CODE)
#define floating_penalty        int_par(FLOATING_PENALTY_CODE)
#define global_defs             int_par(GLOBAL_DEFS_CODE)
#define cur_fam                 int_par(CUR_FAM_CODE)
#define escape_char             int_par(ESCAPE_CHAR_CODE)
#define default_hyphen_char     int_par(DEFAULT_HYPHEN_CHAR_CODE)
#define default_skew_char       int_par(DEFAULT_SKEW_CHAR_CODE)
#define end_line_char           int_par(END_LINE_CHAR_CODE)
#define new_line_char           int_par(NEW_LINE_CHAR_CODE)

#define COUNT_BASE              (INT_BASE + INT_PARS)
#define count(M)                eqtb[COUNT_BASE + M].i

#define DEL_CODE_BASE           (COUNT_BASE + 256)
#define del_code(M)             eqtb[DEL_CODE_BASE + M].i

#define DIMEN_BASE              (DEL_CODE_BASE + 256)
#define DIMEN_PARS              20
#define dimen_par(M)            eqtb[DIMEN_BASE + M].sc

#define PAR_INDENT_CODE             0
#define MATH_SURROUND_CODE          1
#define LINE_SKIP_LIMIT_CODE        2
#define HSIZE_CODE                  3
#define VSIZE_CODE                  4
#define MAX_DEPTH_CODE              5
#define SPLIT_MAX_DEPTH_CODE        6
#define BOX_MAX_DEPTH_CODE          7
#define HFUZZ_CODE                  8
#define VFUZZ_CODE                  9
#define DELIMITER_SHORTFALL_CODE    10
#define NULL_DELIMITER_SPACE_CODE   11
#define SCRIPT_SPACE_CODE           12
#define PRE_DISPLAY_SIZE_CODE       13
#define DISPLAY_WIDTH_CODE          14
#define DISPLAY_INDENT_CODE         15
#define OVERFULL_RULE_CODE          16
#define HANG_INDENT_CODE            17
#define H_OFFSET_CODE               18
#define V_OFFSET_CODE               19

#define par_indent              dimen_par(PAR_INDENT_CODE)
#define math_surround           dimen_par(MATH_SURROUND_CODE)
#define line_skip_limit         dimen_par(LINE_SKIP_LIMIT_CODE)
#define hsize                   dimen_par(HSIZE_CODE)
#define vsize                   dimen_par(VSIZE_CODE)
#define max_depth               dimen_par(MAX_DEPTH_CODE)
#define split_max_depth         dimen_par(SPLIT_MAX_DEPTH_CODE)
#define box_max_depth           dimen_par(BOX_MAX_DEPTH_CODE)
#define hfuzz                   dimen_par(HFUZZ_CODE)
#define vfuzz                   dimen_par(VFUZZ_CODE)
#define delimiter_shortfall     dimen_par(DELIMITER_SHORTFALL_CODE)
#define null_delimiter_space    dimen_par(NULL_DELIMITER_SPACE_CODE)
#define script_space            dimen_par(SCRIPT_SPACE_CODE)
#define pre_display_size        dimen_par(PRE_DISPLAY_SIZE_CODE)
#define display_width           dimen_par(DISPLAY_WIDTH_CODE)
#define display_indent          dimen_par(DISPLAY_INDENT_CODE)
#define overfull_rule           dimen_par(OVERFULL_RULE_CODE)
#define hang_indent             dimen_par(HANG_INDENT_CODE)
#define h_offset                dimen_par(H_OFFSET_CODE)
#define v_offset                dimen_par(V_OFFSET_CODE)

#define SCALED_BASE             (DIMEN_BASE + DIMEN_PARS)
#define dimen(M)                eqtb[SCALED_BASE + M].sc

#define EQTB_SIZE               (SCALED_BASE + 255)

global  mword       eqtb[];
global  qword       xeq_level[];

int     print_param();
int     print_length_param();
int     print_skip_param();
int     show_eqtb();
int     init_eq();
