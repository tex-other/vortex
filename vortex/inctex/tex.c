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

/* This file is part of IncTeX 1.0
 *
 * Copyright (C) 1992 by Regents of the University of California
 *
 * This file has been modified, with permission from Pat Monardo, for IncTeX
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/* @(#)tex.c 2.9 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"texext.h"
#include	"evalstack.h"
#include	"eqstack.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"box.h"
#include	"pack.h"
#include	"cond.h"
#include	"file.h"
#include	"tfm.h"
#include	"hyph.h"
#include	"dvi.h"
#include	"fmt.h"
#include	"page.h"

#ifdef INCTEX
#ifdef TRIP
char	banner[] = "This is Common TeX, Version 2.9";
#else
char	banner[] = "Derived from Common TeX, Version 2.9";
#endif TRIP
#else
char	banner[] = "This is Common TeX, Version 2.9";
#endif INCTEX
int     ready_already;

#ifdef INCTEX
ptr	premac_hi;
ptr	premac_lo;
#endif

#ifdef INCTEX
#include	"Imain.h"
extern char inc_banner[];
extern char state_vers[];

setup_format()
{
	job_name = 0;
	history = FATAL_ERROR_STOP;
	set_paths();
	adjust_tail = NULL;
	page_tail = page_head;
	mode = VMODE;
	head = tail = contrib_head;
	prev_depth = IGNORE_DEPTH;
	mode_line = 0;
	t_open_out();
	term_offset = file_offset = 0;	/* just for consistency - DLP */
	for (first = 0; first < BUF_SIZE; incr(first))
		buffer[first] = 0;
	last = first = 1;
	state = NEW_LINE;
	start = 1;
	if (ready_already != 314159)
		initialize();
	else {
		/* boundary of preloaded token list*/
		premac_lo = tok_low; 
		premac_hi = TOK_TOP;
		/* Treat preload macro token list area as one chunk, namely
		 * no holes in the area. This will make life simple.
		 * In the case of latex, there are 139 free entries of
		 * 21692 entries, or about 0.6%, considered negligible */
		tok_head = NULL;
		/* Kill any existing token free list which exists in the
		 * preloaded macro area. DLP */
	}
	ready_already = 314159;
}

static int
enter_name (fn)
	char		*fn;
{
	int		j;

	j = 0;
	while ((j <= FILE_NAME_SIZE) && (fn != NULL) && (fn[j] != NULL)) {
		buffer[last] = fn[j++];
		++last;
        }
	if (j > 0)
		buffer[last++] = ' ';
        if (last > first) {
            loc = first;
	    return (TRUE);
	}
	return(FALSE);
}

/* just part of pre_format() */
init_format (fn)
	char		*fn;
{
#ifndef TRIP
	print(inc_banner); 
	if (show_state) print(state_vers);
	print_ln();
#endif TRIP
	print(banner);
	selector = TERM_ONLY;
	print_str(format_ident);
	print_ln();
	if (!enter_name(fn))
		if (!init_terminal())
			exit(history);

	limit = last;
	first = last + 1;
	/* skip fmt file loading. DLP */
	if (end_line_char < 0 || end_line_char > 127)
		decr(limit);
	else
		buffer[limit] = end_line_char;
	fix_date_and_time();
	if (interaction == BATCH_MODE)
		selector = NO_PRINT;
	else
		selector = TERM_ONLY;
	if (incremental) 
		set_init_ref_state();	/* set reference for diffs. DLP */
	if ((loc < limit) && (cat_code(buffer[loc]) != ESCAPE))
		init_input();	/* instead of start_input */
	history = SPOTLESS;
}

/* expects arg (1) = fmt file, arg (2) = doc name
 */
pre_any_format (argc,argv)
    int     argc;
    char*   *argv;
{
	int	save_incremental;

#ifndef TRIP
	print(inc_banner);
	if (show_state) print(state_vers);
	print_ln();
#endif TRIP
	print(banner);
	selector = TERM_ONLY;
	if (format_ident == 0)
		print(" (no format preloaded)");
	else
		print_str(format_ident);
	print_ln();
    	if (argc) {	/* handle &fmt doc_name as parameters. DLP */
        	last = first;
        	while (argc) {
			if (incremental && (**argv!='&'))	/* !!-!! */
				save_doc_name(*argv);
			enter_name(*argv);
			argv++;
			argc--;
		}
	}

	if (first >= last)	/* no input args! read from term */
		if(!init_terminal())
			exit(history);
	
	limit = last;
	first = last + 1;
	if (format_ident == 0 || buffer[loc] == '&') {
		initial_fmt = TRUE;
		save_incremental = incremental;

		if (!open_fmt_file())
			exit(history);
		if (!load_fmt_file())
			exit(history);
		w_close(fmt_file);

		/* set range of frozen preloaded token list */
		premac_lo = tok_low;
		premac_hi = TOK_TOP;
		/* Treat preload macro token list area as one chunk, namely
		 * no holes in the area. This will make life simple.
		 * In the case of latex, there are 139 free entries of
		 * 21692 entries, or about 0.6%, considered negligible */
		tok_head = NULL;
		/* Kill any existing token free list which exists in the
		 * preloaded macro area. DLP */

		incremental = save_incremental;
		initial_fmt = FALSE;
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
	if (incremental) 
		set_init_ref_state();	/* set reference for diffs. DLP */
}

pre_incr_run ()
{
	if ((loc < limit) && (cat_code(buffer[loc]) != ESCAPE))
		init_input();	/* instead of start_input */
	history = SPOTLESS;
	if (show_state) { /*!!-!!*/
	 print_nl("tok_mem[premac_lo-1] = "); print_int(tok_mem[premac_lo-1]);
	 print("  tok_link = "); print_int(tok_link[premac_lo-1]);
	}
}

pre_virgin_run ()
{
	if ((loc < limit) && (cat_code(buffer[loc]) != ESCAPE))
		start_input();
	history = SPOTLESS;
}

post_format ()
{
	final_cleanup();
	close_files_and_terminate(FALSE);
}

#else

main (argc, argv)
    int     argc;
    char    **argv;
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
	term_offset = file_offset = 0;
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
        if (!open_fmt_file()) exit(history);
        if (!load_fmt_file()) exit(history);
        w_close(fmt_file);
        while (loc < limit && buffer[loc] == ' ')
            incr(loc);
    }
    if (end_line_char < 0 || end_line_char > 127)
        decr(limit);
    else buffer[limit] = end_line_char;
    if (interaction == BATCH_MODE)
        selector = NO_PRINT;
    else selector = TERM_ONLY;
    fix_date_and_time();
    if (loc < limit && cat_code(buffer[loc]) != ESCAPE)
        start_input();
    history = SPOTLESS;
    main_control();
    final_cleanup();
    close_files_and_terminate(FALSE);
}

#define USAGE   "usage: %s [ -d dir ] [ file ]\n"

#define	usage() { \
	print_nl("usage: "); \
	print(pgm); \
	print(" [-d D ] [ file ]"); \
	print_ln(); \
}

bool
decode_args (argc, argv)
    int     argc;
    char*   *argv;
{
    int     j;
    char*   ap;
    char*   pgm;
    char    dir[MAX_PATH_CHARS];

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
                    switch (*ap)
                    {
                    case 'd':
                        decr(argc), incr(argv);
                        strcpy(dir, *argv);
                        strcat(dir, ":");
                        strcat(dir, input_path);
                        strcpy(input_path, dir);
                        break;

                    default:
                        usage();
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

#endif

fix_date_and_time ()
{
    val        clock;
    struct tm   *tmptr, *localtime();
    
    clock = begintime();
    tmptr = localtime(&clock);
    time = 60 * tmptr->tm_hour + tmptr->tm_min;
    day = tmptr->tm_mday;
    month = tmptr->tm_mon + 1;
    year = tmptr->tm_year + 1900;
}

#undef time
begintime()
{
    return (time(0));
}

final_cleanup ()
{
    int     c;
    
    c = cur_chr;
    if (job_name == 0)
        open_log_file();
    if (cur_level > LEVEL_ONE) {
        print_nl("(");
        print_esc("end occurred ");
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


#ifdef INCTEX

close_files_and_terminate (edit)
	bool		edit;
{
	int		k;
	
	for (k = 0; k < 16; incr(k))
		if (write_open[k]) {
			a_close(write_file[k]);
			if (incremental)
				wfid[k] = NIL;
		}
#ifdef STAT
	if (tracing_stats > 0 && job_name > 0)  {
		int save_selector = selector;
		selector = LOG_ONLY;
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
		print_int(tok_end + 1 - tok_low);
		print(" words of token memory out of ");
		print_int(TOK_MAX + 1 - TOK_MIN);
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
		print_int(max_in_stack); print("i,");
		print_int(max_nest_stack); print("n,");
		print_int(max_param_stack); print("p,");
		print_int(max_buf_stack + 1); print("b,");
		print_int(max_save_stack + 6); print("s ");
		print("stack positions out of ");
		print_int(STACK_SIZE); print("i,");
		print_int(NEST_SIZE); print("n,");
		print_int(PARAM_SIZE); print("p,");
		print_int(BUF_SIZE); print("b,");
		print_int(SAVE_SIZE); print("s");
		print_ln();
		selector = save_selector;
	}
#endif
	wake_up_terminal();
	if (dvi_file != NULL)
		fin_dvi();
    	else if (total_pages == 0)
        	print_nl("No pages of output.");
/*
 * moved to close_out
 */
/*
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
*/
/*
	if (edit)
		call_edit();
	exit(history);
*/
}

#else

close_files_and_terminate (edit)
    bool    edit;
{
    int     k;
    
    for (k = 0; k < 16; incr(k))
        if (write_open[k])
            a_close(write_file[k]);
#ifdef STAT
    if (tracing_stats > 0 && job_name > 0)  {
        int save_selector = selector;
        selector = LOG_ONLY;
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
        print_int(tok_end + 1 - tok_low);
        print(" words of token memory out of ");
        print_int(TOK_MAX + 1 - TOK_MIN);
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
        print_int(max_in_stack); print("i,");
        print_int(max_nest_stack); print("n,");
        print_int(max_param_stack); print("p,");
        print_int(max_buf_stack + 1); print("b,");
        print_int(max_save_stack + 6); print("s ");
        print("stack positions out of ");
        print_int(STACK_SIZE); print("i,");
        print_int(NEST_SIZE); print("n,");
        print_int(PARAM_SIZE); print("p,");
        print_int(BUF_SIZE); print("b,");
        print_int(SAVE_SIZE); print("s");
        print_ln();
        selector = save_selector;
    }
#endif
    wake_up_terminal();
    fin_dvi();
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
    if (edit) call_edit();
    exit(history);
}
#endif

initialize ()
{
    init_char();
    init_strings();
    init_file();
    init_error();
    init_mem();
    init_tok_mem();
    init_eq();
    init_cmds();
    init_hyph();
    init_tfm();
    init_dvi();
#ifdef INIT
    format_ident = make_str_given(" (INITEX)");
#endif
}
 
handle_int ()
{   
    signal(SIGINT, handle_int);
    interrupt = 1;
}

one (s, t)
    char*   s;
    char*   t;
{   
    int     i;
    int     one;

    one = 0;
    for (i = 0; i < strlen(t); incr(i)) {
        if (strncmp(s, &t[i], 2) == 0) {
            incr(one);
            if (one > 1)
                return FALSE;
        }
    }
    if (one == 0)
        return FALSE;
    return TRUE;
}

call_edit ()
{
    char*   file;
    char*   envedit;
    int     old_setting;
    char    edit[FILE_NAME_SIZE + 17];
    char*   texedit = "/usr/ucb/vi +%d %s &";
    val getenv();

    if ((envedit = (char*) getenv("TEXEDIT")) != NULL)
        texedit = envedit;
    if (!one("%d", texedit) || !one("%s", texedit)) {
        print_err("Edit format consists of 1 filename and 1 linenumber");
        return;
    }
    file = (char*) &str_pool[str_start[str_ptr]];
    old_setting = selector;
    selector = NEW_STRING;
    print_str(input_stack[base_ptr].name_field);
    selector = old_setting;
    str_pool[pool_ptr] = NUL;
    sprintf(edit, texedit, line, file);
    if (system(edit) != 0)
        print_err("Trouble executing editor");
}
