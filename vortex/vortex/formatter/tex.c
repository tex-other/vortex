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
 *		tex.c
 */

#include	"tex.h"
#include	"texext.h"
#include	"cmds.h"
#include	"heap.h"
#include	"char.h"
#include	"str.h"
#include	"eq.h"
#include	"hash.h"
#include	"evalstack.h"
#include	"eqstack.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"box.h"
#include	"pack.h"
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

#ifdef VORTEX
#include	"scan.h"
#include	"allir.h"
#include	"main.h"
#include	"macro.h"
#include	"var.h"

extern		make_par_node();
extern ptr	save_hi_mem_min;

#ifdef	DEBUG
	/* for memory management debug */
extern	ptr	was_mem_end;
extern	ptr	was_lo_max;
extern	ptr	was_hi_min;
#endif	DEBUG

#endif VORTEX


extern char	banner[];
extern int	ready_already;
#ifdef VORTEX
FILE            *fopen();
#endif

#ifdef VORTEX

pre_format (argc, argv)
	int		argc;
	char 		*argv[];
{
	struct _char	*tmp;

  	init_global_variables();
	make_par_node();
	par_node_top = par_node_last;

	job_name = 0;
	history = FATAL_ERROR_STOP;
	signal(SIGINT, handle_int);
	set_paths();
	adjust_tail = NULL;
	page_tail = page_head;
	mode = VMODE;
	head = tail = contrib_head;
	prev_depth = IGNORE_DEPTH;
	mode_line = 0;
	t_open_out();
	for (first = 0; first < BUF_SIZE; incr(first))
		buffer[first] = 0;
	first = 1;
	state = NEW_LINE;
	start = 1;

	if (ready_already != 314159)
		initialize();
	else {
		save_hi_mem_min = hi_mem_min;	/* save hi_mem_min */
	}

	ready_already = 314159;
	if (starting_page == INFINITY) {
		print(banner);
		selector = TERM_ONLY;
		if (format_ident == 0)
			print(" (no format preloaded)");
		else
			print_str(format_ident);
		print_ln();
	}
	if (!decode_args(argc, argv))
		if (!init_terminal())
			exit(history);
	limit = last;
	first = last + 1;
	if (format_ident == 0 || buffer[loc] == '&') {
		if (!open_fmt_file())
			exit(history);
		if (!load_fmt_file())
			exit(history);
		w_close(fmt_file);
		while (loc < limit && buffer[loc] == ' ')
			incr(loc);
	}
	if (end_line_char < 0 || end_line_char > 127)
		decr(limit);
	else
		buffer[limit] = end_line_char;
	fix_date_and_time();
	if (interaction == BATCH_MODE)
		selector = NO_PRINT;
	else
		selector = TERM_ONLY;
	if ((loc < limit) && (cat_code(buffer[loc]) != ESCAPE))
		start_input();
	history = SPOTLESS;
}

post_format ()
{
	final_cleanup();
#ifdef DEBUG
	dump_hier(0);
#endif
	close_files_and_terminate(FALSE);
}

#else !VORTEX

main (argc, argv)
	int		argc;
	char 		*argv[];
{
	job_name = 0;
	history = FATAL_ERROR_STOP;
	signal(SIGINT, handle_int);
	set_paths();
	adjust_tail = NULL;
	page_tail = page_head;
	mode = VMODE;
	head = tail = contrib_head;
	prev_depth = IGNORE_DEPTH;
	mode_line = 0;
	t_open_out();
	for (first = 0; first < BUF_SIZE; incr(first))
		buffer[first] = 0;
	first = 1;
	state = NEW_LINE;
	start = 1;
	if (ready_already != 314159) 
		initialize();
	ready_already = 314159;
	print(banner);
	selector = TERM_ONLY;
	if (format_ident == 0)
		print(" (no format preloaded)");
	else print_str(format_ident);
	print_ln();
	if (!decode_args(argc, argv))
		if (!init_terminal())
			exit(history);
	limit = last;
	first = last + 1;
	if (format_ident == 0 || buffer[loc] == '&') {
		/*if (format_ident != 0)
			initialize();*/
		if (!open_fmt_file())
			exit(history);
		if (!load_fmt_file())
			exit(history);
		w_close(fmt_file);
		while (loc < limit && buffer[loc] == ' ')
			incr(loc);
	}
	if (end_line_char < 0 || end_line_char > 127)
		decr(limit);
	else buffer[limit] = end_line_char;
	fix_date_and_time();
	if (interaction == BATCH_MODE)
		selector = NO_PRINT;
	else selector = TERM_ONLY;
	if (loc < limit && cat_code(buffer[loc]) != ESCAPE)
		start_input();
	history = SPOTLESS;
	main_control();
	final_cleanup();
	close_files_and_terminate(FALSE);
}
#endif

#define	USAGE	"usage: %s [ -d dir ] [ file ]\n"

bool
decode_args (argc, argv)
	int		argc;
	char		**argv;
{
	int		j;
	char*		ap;
	char*		pgm;
	char		dir[MAX_PATH_CHARS];

	ap = pgm = argv[0];
	while (*pgm++ != NUL)
		if (*pgm == '/' || *pgm == '\\')
			ap = pgm, ap++;
	pgm = ap;

	decr(argc), incr(argv);
	if (argc) {
		last = first;
		while (argc) {
			if (argv[0][0] == '-') {
				for (ap = *argv + 1; *ap != NUL; incr(ap)) {
					switch (*ap) {
					case 'd':
						decr(argc), incr(argv);
						strcpy(dir, *argv);
						strcat(dir, ":");
						strcat(dir, input_path);
						strcpy(input_path, dir);
						break;

					default:
						fprintf(stderr, USAGE, pgm);
						exit(history);
					}
				}
			} else {
				j = 0;
				while (j <= FILE_NAME_SIZE && argv[0][j] != NUL) {
					buffer[last] = argv[0][j];
					incr(last), incr(j);
				}
				if (j > 0)
					buffer[last++] = ' ';
			}
			decr(argc), incr(argv);
		}
		if (last > first) {
			loc = first;
			return TRUE;
		}
	}
	return FALSE;
}

fix_date_and_time ()
{
    val        clock, begin_time();
    struct tm   *tm_ptr, *localtime();
    
    clock = begin_time();
    tm_ptr = localtime(&clock);
	time = 60 * tm_ptr->tm_hour + tm_ptr->tm_min;
    day = tm_ptr->tm_mday;
    month = tm_ptr->tm_mon + 1;
    year = tm_ptr->tm_year + 1900;
}

val
begin_time ()
{
#undef time
	val time();
	return (time((long *) 0));
}

final_cleanup ()
{
	int		c;
	
	c = cur_chr;
	if (job_name == 0)
		open_log_file();
	if (cur_level > LEVEL_ONE) {
		print_nl("(");
		print_esc("end occured ");
		print("inside a group at level ");
		print_int(cur_level - LEVEL_ONE);
		print_char(')');
	}
	while (cond_ptr != NULL) {
		print_nl("(");
		print_esc("end occurred ");
		print("when ");
		print_cmd_chr(IF_TEST, cur_if);
		if (if_line != 0) {
			print(" on line ");
			print_val(if_line);
		}
		print(" was incomplete)");
		if_line = if_line_field(cond_ptr); 
		cur_if = subtype(cond_ptr);
		cond_ptr = link(cond_ptr);
	}
	if (history != SPOTLESS && 
		(history == WARNING_ISSUED || interaction < ERROR_STOP_MODE) &&
		selector == TERM_AND_LOG) {
		selector = TERM_ONLY;
		print_nl("(see the transcript file for additional information)");
		selector = TERM_AND_LOG;
	}
	if (c == 1)
#ifdef INIT
		store_fmt_file();
#else
		print_nl("(\\dump is performed only by INITEX)");
#endif
}

close_files_and_terminate (edit)
	bool		edit;
{
	int		k;
	
	for (k = 0; k < 16; incr(k))
		if (write_open[k])
			a_close(write_file[k]);

#ifdef STAT
	if (tracing_stats > 0)  {
		print_ln();
		print(" Here is how much of TeX's memory you used:");
		print_ln();
		print_int(str_ptr);
		print(" strings out of ");
		print_int(MAX_STRINGS);
		print_ln();
		print_int(pool_ptr);
		print(" string characters out of ");
		print_int(POOL_SIZE);
		print_ln();
		print_int(lo_mem_max - MEM_MIN + mem_end - hi_mem_min);
		print(" words of memory out of ");
		print_int(mem_end + 1 - MEM_MIN);
		print_ln();
		print_int(cs_count);
		print(" multiletter control sequences out of ");
		print_int(HASH_SIZE);
		print_ln();
		print_int(fmem_ptr);
		print(" words of font info for ");
		print_int(font_ptr - FONT_BASE);
		print(" font");
		if (font_ptr != FONT_BASE + 1)
			print_char('s'); 
		print(", out of ");
		print_int(FONT_MEM_SIZE);
		print(" for ");
		print_int(FONT_MAX - FONT_BASE);
		print_ln();
		print_int(hyph_count);
		print(" hyphenation exception");
		if (hyph_count != 1) print_char('s');
		print(" out of ");
		print_int(HYPH_SIZE);
		print_ln();
		print_int(max_in_stack); print("i, ");
		print_int(max_nest_stack); print("n, ");
		print_int(max_param_stack); print("p, ");
		print_int(max_buf_stack + 1); print("b, ");
		print_int(max_save_stack + 6); print("s, ");
		print_ln();
	}
#endif
	wake_up_terminal();
	if (total_pages == 0)
		print_nl("No pages of output.");
#ifndef VORTEX
	else {
		dvi_out(POST);
		dvi_four(last_bop);
		last_bop = dvi_offset + dvi_ptr - 5;
		dvi_four(25400000);
		dvi_four(473628672);
		prepare_mag();
		dvi_four(mag);
		dvi_four(max_v);
		dvi_four(max_h);
		dvi_out(max_push / 256);
		dvi_out(max_push % 256);
		dvi_out(total_pages / 256);
		dvi_out(total_pages % 256);
		while (font_ptr > FONT_BASE) {
			if (font_used[font_ptr])
				dvi_font_def(font_ptr);
			decr(font_ptr);
		}
		dvi_out(POST_POST);
		dvi_four(last_bop);
		dvi_out(ID_BYTE);
		for (k = 4 + (DVI_BUF_SIZE - dvi_ptr) % 4; k > 0; decr(k))
			dvi_out(223);
		if (dvi_limit == HALF_BUF)
			write_dvi(HALF_BUF, DVI_BUF_SIZE);
		if (dvi_ptr > 0) write_dvi(0, dvi_ptr);
		print_nl("Output written on ");
		print_str(dvi_name);
		print(" (");
		print_int(total_pages);
		print(" page");
		if (total_pages != 1)
			print_char('s');
		print(", ");
		print_val(dvi_offset + dvi_ptr);
		print(" bytes).");
		b_close(dvi_file);
	}
#endif
	if (job_name > 0) {
		wlog_cr();
		a_close(log_file);
		selector -= 2;
		if (selector == TERM_ONLY) {
			print_nl("Transcript written on ");
			print_str(log_name);
			print_char('.');
		}
	}
	print_ln();
	if (edit)
		{}
	exit(history);
}

initialize ()
{
	init_char();
	init_strings();
	init_file();
	init_mem();
	init_eq();
	init_cmds();
	init_hyph();
	init_tfm();
#ifdef INIT
	format_ident = make_string_given(" (INITEX)");
#endif
}
 
handle_int ()
{	
	signal(SIGINT, handle_int);
	interrupt = 1;
}

/*********************************/
/*  initialize global variables  */
/*********************************/
#ifdef VORTEX
init_global_variables()
{
  input_flag = FALSE;
  input_state_level = 0;
  vmode_doll= NULL;
  backup_real_token = FALSE;
  macro_first_token = FALSE;
  macro_que_enable = FALSE;
  real_token = FALSE;
  real_input = FALSE;
  warming = TRUE;
  cond_skip = NOSKIP;
  token_count = 0;
  prefix_flag = FALSE;
  macro_node_name = NIL;
  backup_cs_node = NIL;
  backup_cs_level = 0;
  cur_cs_node = NIL;
  cur_group_node = NIL;
  token_node_last = NIL;
  par_node_last = NIL;
  par_node_top = NIL;
  begin_of_space_token = NIL;
  begin_of_word_token = NIL;
  group_s_node_begin = NIL;
  group_s_node_end = NIL;
  irs_ptr = NIL;
  irs_next = NIL;
  begin_of_cs_token = NIL;
  cond_level = 0;
  disp_watch = FALSE;
  defed_char = NIL;
  math_que_top = math_que_end = NIL;
  cs_que_top = cs_que_end = NIL;
#ifdef	DEBUG
	/* memory management debug */
  was_mem_end = MEM_MIN;
  was_lo_max = MEM_MIN;
  was_hi_min = MEM_MAX;
#endif	DEBUG
}
#endif
