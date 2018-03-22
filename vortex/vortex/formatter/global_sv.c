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
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Development Environment
 *
 *  This file is part of the VorTeX incremental formatter,
 *
 *  Copyright (C) 1987 by	Ikuo Minakata	(min@renoir.berkeley.edu)
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

/*
 *		global_sv.c
 */

#include <sys/types.h>
#include "tex.h"
#include "texext.h"
#include "cmds.h"
#include "heap.h"
#include "char.h"
#include "str.h"
#include "eq.h"
#include "hash.h"
#include "scan.h"
#include "evalstack.h"
#include "eqstack.h"
#include "tokenstack.h"
#include "token.h"
#include "box.h"
#include "pack.h"
#include "math.h"
#include "cond.h"
#include "io.h"
#include "file.h"
#include "tfm.h"
#include "hyph.h"
#include "dvi.h"
#include "fmt.h"
#include "error.h"
#include "print.h"
#include "page.h"
#include "main.h"
#include "macro.h"
#include "allir.h"

/*********************************************** 
 * the area necessary for context save/restore *
 ***********************************************/

short		beg_save_globals = 0xa5a5;	/* begin of global area */

/* align.c */
ptr	align_ptr = NIL;
ptr	cur_align = NIL;
ptr	cur_span = NIL;
ptr	cur_loop = NIL;
ptr	cur_head = NIL;
ptr	cur_tail = NIL;

/* arith.c */
bool	arith_error = FALSE;
scal	remainder = 0;

/* box.c */
val	depth_threshold = 0;
val	breadth_max = 0;
fnt	font_in_short_display = NIL;

/* boxlists.c */
ptr	cur_box = NIL;

/* char.c */

/* cmds.c */

/* cond.c */
ptr	cond_ptr = NIL;
int	cur_if = 0;
int	if_limit = 0;
val	if_line = 0;
val	skip_line = 0;

/* def.c */
hword	after_token = 0;
bool	long_help_seen = FALSE;
val	mag_set = 0;
char	defed_char = NIL;
short	catcode= 0;
struct  _cseq	*font_def_iri[FONT_MAX] = {0};

/* dvi.c */
int		total_pages = 0;

/* eq.c */
qword	xeq_level[EQTB_SIZE+1 - INT_BASE] = {0};

/* eqstack.c */
ptr	save_ptr = 0;
ptr	max_save_stack = 0;
qword	cur_level = LEVEL_ONE;
group	cur_group = BOTTOM_LEVEL;
ptr	cur_boundary = 0;

/* error.c */
bool	OK_to_interrupt = TRUE;
bool	deletions_allowed = TRUE;
int	error_count= 0;
char*	help_line[6] = {0};
int	help_ptr = 0;
int	history = 0;
int	interaction = ERROR_STOP_MODE;
int	interrupt = 0;
int	old_setting = 0;
bool	use_err_help = FALSE;

/* eval.c */

/* evalstack.c */
list	cur_list = 0;
list	nest[NEST_SIZE] = {0};
ptr	nest_ptr = NIL;
int	max_nest_stack = 0;
int	shown_mode = 0;

/* expand.c */
int	long_state = 0;
ptr	pstack[9] = {0};
ptr	cur_mark[5] = {0};
#ifdef VORTEX
short	macro_param_num = 0;
#endif VORTEX

/* file.c */
 _Char	*begin_of_file_name = NIL;
char	name_of_file[FILE_NAME_SIZE] = "";
int	name_length = 0;
int	area_delimiter = 0;
int	ext_delimiter = 0;
str	cur_area = 0;
str	cur_name = 0;
str	cur_ext = 0;
bool	name_in_progress = FALSE;
str	job_area = 0;
str	job_name = 0;
str	log_name = 0;
str	dvi_name = 0;
alpha_file	read_file[16] = {0};
int	read_open[17] = {0}; 
str	str_dvi = 0;
str	str_log = 0;
str	str_tex = 0;
str	str_tfm = 0;
str	str_fmt = 0;
str	str_texput = 0;
char*	cur_path = 0;
char	input_path[MAX_PATH_CHARS] = default_input_path;
char 	font_path[MAX_PATH_CHARS] = default_font_path;
char 	format_path[MAX_PATH_CHARS] = default_format_path;

/* fmt.c */

/* hash.c */
bool	no_new_control_sequence = TRUE;
ptr	hash_used = FROZEN_CONTROL_SEQUENCE;
int	cs_count = 0;

/* heap.c */
ptr	save_hi_mem_min = NIL;	/* to detect preloaded token_list */
int	max_var_used = 0;
ptr	avail = NIL;
ptr	rover = NIL;
ptr	mem_end = NIL;
ptr	lo_mem_max = NIL;
ptr	hi_mem_min = NIL;
int	dyn_used = 0;
int	var_used = 0;
ptr	temp_ptr = NIL;

/* hyph.c */
ptr	ha = NIL;
ptr	hb = NIL;
hword	hc[66] = {0};
fnt	hf = 0;
int	hn = 0;
ascii	hu[64] = {0};
byte	hyf[65] = {0};
int	hyf_char = 0;
int	hyph_count = 0;
ptr	hyph_list[HYPH_SIZE+1] = {0};
str	hyph_word[HYPH_SIZE+1] = {0};
int	hyphen_passed = 0;

/* io.c */
int	last = 0;
ascii	buffer[BUF_SIZE] = {0};
int	first = 0;
int	max_buf_stack = 0;

/* irf.c */
_Char	*irs_bol = NIL;
_Char	*irs_eol = NIL;
int	irs_read_only = FALSE;

/* iri.c */

/* irs.c */

/* irt.c */

/* irv.c */

/* main.c */
int	sig_state = FALSE;

/* math.c */

/* mathlists.c */

/* mlist-hlists.c */
qword	cur_c = 0;
fnt	cur_f = 0;
fourq	cur_i = 0;
scal	cur_mu = 0;
int	cur_size = 0;
int	cur_style = 0;
ptr	cur_mlist = NIL;
bool	mlist_penalties = FALSE;

/* msg.c */

/* pack.c */
ptr	adjust_tail = NIL;
val	pack_begin_line = 0;
scal	total_shrink[4] = {0};
scal	total_stretch[4] = {0};

/* page.c */
scal	best_height_plus_depth = 0;
ptr	best_page_break = NIL;
val	insert_penalties = 0;	
ptr	last_glue = MAX_HALFWORD;
scal	last_kern = 0;
val	last_penalty = 0;
val	least_page_cost = 0;
bool	output_active = FALSE;
int	page_contents = 0;
scal	page_max_depth = 0;
ptr	page_tail = NIL;
scal	page_so_far[8] = {0};

/* par.c */
scal	active_width[7] = 0;
ptr	cur_p = NIL;
ptr	just_box = NIL;
val	actual_looseness = 0;
scal	background[7] = 0;
ptr	best_bet = NIL;
hword	best_line = 0;
hword	best_pl_line[4] = 0;
ptr	best_place[4]= NIL;
scal	break_width[7] = 0;
scal	cur_active_width[7] = 0;
scal	disc_width = 0;
hword	easy_line = 0;
val	fewest_demerits = 0;
scal	first_indent = 0;
scal	first_width = 0;
hword	last_special_line = 0;
int	line_diff = 0;
val	minimal_demerits[4] = 0;
val	minimum_demerits= 0;
bool	no_shrink_error_yet = 0;
ptr	passive = NIL;
ptr	printed_node = NIL;
ptr	pass_number = NIL;
scal	second_indent = 0;
bool	second_pass = 0;
scal	second_width= 0;
val	threshold = 0;

/* print.c */
int	selector = TERM_ONLY;

/* scan.c */
gord	cur_order = 0;
val	cur_val = 0;
int	cur_val_level = INT_VAL;
int	radix = 0;

/* state.c */
str	str_sts = 0;
str	sts_name = 0;

/* str.c */

/* tex.c */
#ifdef VORTEX
short	input_flag = 0;
short	input_state_level = 0;
short	vmode_doll = 0;
short	backup_real_token = 0;
short	macro_first_token = 0;
short	macro_que_enable = 0;
short	real_token = 0;
short	real_input = 0;
short	warming = TRUE;
short   cond_skip = NOSKIP;
short   token_count = 0;
short	prefix_flag = 0;
short   cond_level = 0;
short   disp_watch = 0;
struct _cseq	*macro_node_name = NIL;
struct _cseq	*backup_cs_node = NIL;
short	backup_cs_level = 0;
struct _cseq	*cur_cs_node = NIL;
struct _group	*cur_group_node = NIL;
struct _node	*token_node_last = NIL;
struct _unode  	*par_node_last = NIL;
struct _char	*begin_of_space_token = NIL;
struct _char	*begin_of_word_token = NIL;
struct _char	*group_s_node_begin = NIL;
struct _char    *group_s_node_end = NIL;
struct _char	*irs_ptr = NIL;
struct _char	*irs_next = NIL;
struct _char	*begin_of_cs_token = NIL;
struct input_state_record	input_state_stack[MAX_IN_OPEN] = {0};
ptr	math_que_top = NIL;
ptr	math_que_end = NIL;
ptr	cs_que_top = NIL;
ptr	cs_que_end = NIL;
#endif


/* texext.c */

/* tfm.c */
ptr		font_glue[FONT_MAX] = {0};
fnt		font_ptr = FONT_BASE;
ptr		fmem_ptr = 0;
fourq		font_check[FONT_MAX] = {0};
scal		font_size[FONT_MAX] = {0};
scal		font_dsize[FONT_MAX] = {0};
hword		font_params[FONT_MAX] = {0};
str		font_name[FONT_MAX] = {0};
str		font_area[FONT_MAX] = {0};
byte		font_bc[FONT_MAX] = {0};
byte		font_ec[FONT_MAX] = {0};
bool		font_used[FONT_MAX] = {0};
int		hyphen_char[FONT_MAX]= {0};
int		skew_char[FONT_MAX] = {0};
int		char_base[FONT_MAX] = {0};
int		width_base[FONT_MAX] = {0};
int		height_base[FONT_MAX] = {0};
int		depth_base[FONT_MAX] = {0};
int		italic_base[FONT_MAX] = {0};
int		lig_kern_base[FONT_MAX] = {0};
int		kern_base[FONT_MAX] = {0};
int		exten_base[FONT_MAX] = {0};
int		param_base[FONT_MAX] = {0};

/* token.c */
hword	cur_chr = 0;
byte	cur_cmd = 0;
ptr	cur_cs = 0;
hword	cur_tok = 0;
bool	force_eof = FALSE;

/* tokenlists.c */
#ifdef VORTEX
int	last_loc = 0;
#endif

/* tokenstack.c */
input	cur_input= 0;
input	input_stack[STACK_SIZE] = {0};
ptr 	input_ptr = NIL;
ptr 	max_in_stack = NIL;
val		line = 0;
val		line_stack[MAX_IN_OPEN] = {0};
ptr		in_open = NIL;
ptr		param_stack[PARAM_SIZE] = {0};
ptr		param_ptr = NIL;
ptr		max_param_stack = NIL;
val		align_state = 1000000;
ptr		base_ptr = NIL;
ptr		def_ref = NIL;
ptr		warning_index = NIL;
int		scanner_status = 0;

short		end_save_globals = 0;	/* end of global area */
/*
 *	necessary to save/restore, but separately
 */
mword	eqtb[EQTB_SIZE+1];
mword	save_stack[SAVE_SIZE];
twoh	hash[UNDEFINED_CONTROL_SEQUENCE+1];
mword	mem[MEM_MAX-MEM_MIN+1];
mword	font_info[FONT_MEM_SIZE];
