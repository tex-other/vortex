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

#ifdef INCTEX
/*  This file is part of IncTeX 1.0
 *  An Incremental TeX Formatter
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"Imain.h"
#include	"texext.h"
#include	"cmds.h"
#include	"heap.h"
#include	"char.h"
#include	"str.h"
#include	"eq.h"
#include	"hash.h"
#include	"scan.h"
#include	"evalstack.h"
#include	"eqstack.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"box.h"
#include	"pack.h"
#include	"math.h"
#include	"cond.h"
#include	"io.h"
#include	"file.h"
#include	"tfm.h"
#include	"hyph.h"
#include	"dvi.h"
#include	"fmt.h"
#include	"error.h"
#include	"print.h"
#include	"page.h"

/* beginning of save area */
short		beg_save_globals		= 0x5a5a;

/* align.c */
ptr		align_ptr			= NUL;
ptr		cur_align			= NUL;
ptr		cur_span			= NUL;
ptr		cur_loop			= NUL;
ptr		cur_head			= NUL;
ptr		cur_tail			= NUL;
			
/* arith.c */			     
bool		arith_error			= FALSE;
scal		remainder			= 0;
		
/* box.c */		   
val		depth_threshold			= 0;
val		breadth_max			= 0;
fnt		font_in_short_display		= NUL;
		
/* boxlists.c */		
ptr		cur_box				= NUL;
		
/* char.c */		    
		
/* cmds.c */		    
		
/* cond.c */		    
ptr		cond_ptr			= NUL;
int		cur_if				= 0;
int		if_limit			= 0;
val		if_line				= 0;
val		skip_line			= 0;
		
/* def.c */		   
hword		after_token			= 0;
bool		long_help_seen			= FALSE;
val		mag_set				= 0;
char		defed_char			= NUL;
short		catcode				= 0;

/* dvi.c */
int		total_pages			= 0;

/* eq.c */

/* eqstack.c */
ptr		save_ptr			= 0;
ptr		max_save_stack			= 0;
qword		cur_level			= LEVEL_ONE;
group		cur_group			= BOTTOM_LEVEL;
ptr		cur_boundary			= 0;

/* error.c */
bool		OK_to_interrupt			= TRUE;
bool		deletions_allowed		= TRUE;
int		error_count			= 0;
chrs		help_line[6]			= {0};
ptr		help_ptr			= 0;
int		history				= 0;
int		interaction			= ERROR_STOP_MODE;
int		interrupt			= 0;
int		old_setting			= 0;
bool		use_err_help			= FALSE;

/* eval.c */

/* evalstack.c */
list		cur_list			= {0};
list		nest[NEST_SIZE]			= {0};
ptr		nest_ptr			= NUL;
int		max_nest_stack			= 0;
int		shown_mode			= 0;

/* expand.c */
int		long_state			= 0;
ptr		pstack[9]			= {0};
ptr		cur_mark[5]			= {0};

/* file.c */
char		name_of_file[FILE_NAME_SIZE] 	= "";
int		name_length			= 0;
int		area_delimiter			= 0;
int		ext_delimiter			= 0;
str		cur_area			= 0;
str		cur_name			= 0;
str		cur_ext				= 0;
bool		name_in_progress		= FALSE;
str		job_area			= 0;
str		job_name			= 0;
str		log_name			= 0;
str		dvi_name			= 0;
alpha_file	read_file[16]			= {0};
int		read_open[17]			= {0}; 
str		str_dvi				= 0;
str		str_log				= 0;
str		str_tex				= 0;
str		str_tfm				= 0;
str		str_fmt				= 0;
str		str_texput			= 0;
chrs		cur_path			= NULL;
char		input_path[MAX_PATH_CHARS]	= default_input_path;
char		font_path[MAX_PATH_CHARS]	= default_font_path;
char		format_path[MAX_PATH_CHARS]	= default_format_path;

/* fmt.c */

/* hash.c */
ptr		hash_used			= FROZEN_CONTROL_SEQUENCE;
bool		no_new_control_sequence		= TRUE;
int		cs_count			= 0;

/* heap.c */
ptr		mem_end				= NUL;
ptr		lo_mem_max			= NUL;
ptr		hi_mem_min			= NUL;
ptr		avail				= NUL;
ptr		rover				= NUL;
int		dyn_used			= 0;
int		var_used			= 0;
int		max_var_used			= 0;
ptr		temp_ptr			= NUL;

/* hyph.c */
ptr		ha				= NUL;
ptr		hb				= NUL;
hword		hc[66]				= {0};
fnt		hf				= 0;
int		hn				= 0;
ascii		hu[64]				= {0};
byte		hyf[65]				= {0};
int		hyf_char			= 0;
int		hyph_count			= 0;
ptr		hyph_list[HYPH_SIZE+1]		= {0};
str		hyph_word[HYPH_SIZE+1]		= {0};
int		hyphen_passed			= 0;

/* io.c */
int		last				= 0;
ascii		buffer[BUF_SIZE]		= {0};
int		first				= 0;
int		max_buf_stack			= 0;

/* math.c */

/* mathlists.c */

/* mlst-hlsts.c */
qword		cur_c				= 0;
fnt		cur_f				= 0;
qqqq		cur_i				= {0};
scal		cur_mu				= 0;
int		cur_size			= 0;
int		cur_style			= 0;
ptr		cur_mlist			= NUL;
bool		mlist_penalties			= FALSE;

/* pack.c */
ptr		adjust_tail			= NUL;
val		pack_begin_line			= 0;
scal		total_shrink[4]			= {0};
scal		total_stretch[4]		= {0};

/* page.c */
scal		best_height_plus_depth		= 0;
ptr		best_page_break			= NUL;
val		insert_penalties		= 0;
ptr		last_glue			= MAX_HALFWORD;
scal		last_kern			= 0;
val		last_penalty			= 0;
val		least_page_cost			= 0;
bool		output_active			= FALSE;
int		page_contents			= 0;
scal		page_max_depth			= 0;
ptr		page_tail			= NUL;
scal		page_so_far[8]			= {0};

/* par.c */
ptr		cur_p				= NUL;
ptr		passive				= NUL;
scal		background[7]			= {0};
scal		break_width[7]			= {0};
scal		active_width[7]			= {0};
scal		cur_active_width[7]		= {0};
val		threshold			= 0;
bool		second_pass			= 0;
scal		first_indent			= 0;
scal		first_width			= 0;
scal		second_indent			= 0;
scal		second_width			= 0;
val		fewest_demerits			= 0;
val		minimal_demerits[4]		= {0};
val		minimum_demerits		= 0;
ptr		best_bet			= NUL;
hword		best_line			= 0;
hword		best_pl_line[4]			= {0};
ptr		best_place[4]			= {NUL};
hword		easy_line			= 0;
hword		last_special_line		= 0;
int		line_diff			= 0;
scal		disc_width			= 0;
ptr		pass_number			= NUL;
ptr		printed_node			= NUL;
val		actual_looseness		= 0;
bool		no_shrink_error_yet		= 0;
ptr		just_box			= NUL;

/* print.c */
int		selector			= TERM_ONLY;

/* scan.c */
gord		cur_order			= 0;
val		cur_val				= 0;
int		cur_val_level			= INT_VAL;
int		radix				= 0;

/* str.c */
str		str_ptr				= 0;
ptr		pool_ptr			= 0;

/* tex.c */

/* texext.c */
ptr		write_loc			= 0;

/* tfm.c */
ptr		fmem_ptr			= 0;
fnt		font_ptr			= FONT_BASE;
/* new pages in 1 dvi file, don't want this to be restored any more
bool		font_used[FONT_MAX]		= {0};*//* changes randomly */

/* token.c */
hword		cur_chr				= 0;
hword		cur_cmd				= 0;
ptr		cur_cs				= 0;
hword		cur_tok				= 0;
bool		force_eof			= FALSE;
ptr		tok_head			= 0;
ptr		tok_low				= 0;
ptr		tok_end				= 0;
int		tok_used			= 0;

/* tokenlists.c */

/* tokenstack.c */
input		cur_input			= {0};
input		input_stack[STACK_SIZE]		= {0};
ptr 		input_ptr			= NUL;
ptr	 	max_in_stack			= NUL;
val		line				= 0;
val		line_stack[MAX_IN_OPEN]		= {0};
ptr		in_open				= NUL;
ptr		param_stack[PARAM_SIZE]		= {0};
ptr		param_ptr			= NUL;
ptr		max_param_stack			= NUL;
val		align_state			= 1000000;
ptr		base_ptr			= NUL;
ptr		def_ref				= NUL;
ptr		warning_index			= NUL;
int		scanner_status			= 0;

/* Imain.c */
str		str_inc				= 0;
unsigned long	chk_fid				= 0;
unsigned long	chk_cid				= 0;
unsigned long	f_max				= 0;
unsigned long	w_max				= 0;
char		inc_fn[STR_MAX]			= "";
char		base_fn[STR_MAX]		= "";
int		rfid[MAX_IN_OPEN]		= {NIL};
int		wfid[OUT_MAX]			= {NIL};

/* 
 * end of save area
 */
short		end_save_globals		= 0;
#endif
