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
 *
 *  New changes: put new dvi files in 1 single INC/<doc>.dvi file, instead
 *  of in per-page files, and put list of new pages in INC/<doc>.newpages
 *  Format of newpages = l:r for a range of new pages from l to r
 *  or l, for just 1 page, ranges are followed by a -1, then the total
 *  number of document pages.  The patchdvi utility will combine these
 *  pages with the previous complete set of dvi pages to get a new dvi file.
 *  Pages are skipped when suspend_format is called, so that is where we
 *  figure out when ranges should be written and updated.
 *
 *  font_used[] was taken out of Iglobal.c, since we no longer want it to
 *  get restored after checkpoint loads.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/file.h>
#include	<sys/stat.h>
#include	<sys/time.h>
#include	<ctype.h>
#include	<strings.h>
#include	"tex.h"
#include	"Imain.h"
#include	"file.h"
#include	"scan.h"
#include	"io.h"
#include	"dvi.h"
#include	"print.h"
#include	"tokenstack.h"
#include	"texext.h"
#include	"eqstack.h"
#include	"cond.h"
#include	"fmt.h"
#include	"Icmp.h"

int		forced_incremental;	/* TRUE = always use checkpoint - DLP*/
int		incremental;
int		divergent;
int		virgin;
int		save_zero;
int		show_state;
int		qsc_check;
int		noqsc_count;
int		reconverged;	/* TRUE if TeX back to same state as prev-DLP */
int		state_checkpointing;
int		from_beginning;
int		do_compression;
int		page_done;
int		page_shipped;
int		format_continue;
int		all_clean;
int		save_good;
int		src_changed;	/* FALSE = no change; save new file times-DLP */
int		squeaky_clean;
time_t		write_time;
int		last_max_pages;	/* to zap excess chkpts if # pages shrink-DLP */
int		lasthistory;
int		inc_dir_ok;
int		LastStartPage,
		LastEndPage;

char		*pgm_fn;			/* name of program */
char		doc_fn[FILE_NAME_SIZE];		/* name of doc root file */
unsigned long	chk_offset;	/* offset relative to old eop */
unsigned long	chk_eop;	/* previous eop in backup */
F_NODE		*f_bgn;		/* beginning of file chain */
F_NODE		*f_end;		/* end of file chain */
F_NODE		*f_cur;		/* current file pointer */
P_NODE		*p_bgn;		/* beginning of page chain */
P_NODE		*p_end;		/* end of current page chain */
P_NODE		*p_cur;		/* current dirty page */
FILE		*inc_fp;	/* pointer to inc file */
FILE		*tgt_fp;	/* pointer to the backup dvi file */
FILE		*pagelist;	/* pointer to inc file */
F_NODE		**fnds;		/* file id to read file node mapping table */
W_NODE		*w_bgn;
W_NODE		*w_end;
W_NODE		**wnds;		/* file id to write file node mapping table */
unsigned long	f_all;
unsigned long	p_all;
unsigned long	w_all;
P_NODE		*find_page();
float		format_time();
int		jump_val;	/* state checkpointing every so many pages */
float		btime;		/* time to backup source file in a page */
int		owfid[OUT_MAX];
int		oweop[OUT_MAX];

extern	short	beg_save_globals;
extern	short	end_save_globals;

/*
 *  Using history as the basis of our adaptive strategy
 */
struct timeval	time0,
		time1,
		time2,
		time3,
		time4,
		time5,
		wrapup,
		tsync;
struct timezone tz;

check_inc_dir()
{
	if (access(INC_DIR, F_OK) < 0)
		mkdir(INC_DIR, 0775);
}

get_ext (n, ext)
	int	n;
	char	ext[];
{
	char		tmp[EXT_MAX];
	str		str_ext;

	if (n >= 0)
		sprintf(tmp, "%s%d%s%s", EXT_DEL, n, EXT_DEL, ext);
	else
		sprintf(tmp, "%s%s", EXT_DEL, ext);
	str_ext = make_str_given(tmp);
	if (job_name == 0)
		job_name = str_texput;
	cur_area = str_inc;
	cur_name = job_name;
	cur_ext = str_ext;
	pack_cur_name();
	flush_string();		/* flush str_ext, don't need it anymore. DLP */
}

#define init_blocked print_nl("Option illegal in INITEX: -"); print_char(*argp); exit(-1);


main(argc, argv)
	char	*argv[];
{
	char	*argp;
	char	dir[MAX_PATH_CHARS];
	int	i;
	
	gettimeofday(&time0, &tz);

#ifdef INIT
	incremental = FALSE;		/* run as if -b option */
	state_checkpointing = FALSE;
	virgin = TRUE;
	qsc_check = trace_font = trace_eq = trace_hash = trace_xeq = FALSE;
#else
	show_state = FALSE;
	incremental = TRUE;
	state_checkpointing = qsc_check = TRUE;
	virgin = FALSE;
	trace_font = trace_eq = trace_hash = trace_xeq = TRUE;
#endif
#ifdef DEBUG
	showq = explain = FALSE;
#endif DEBUG
	signal(SIGINT, handle_int);
	forced_incremental = FALSE;	/* don't check reload overhead - DLP */
	initial_fmt = divergent = from_beginning = FALSE;
	do_compression = save_good = format_continue = all_clean = TRUE;
	save_zero = page_shipped = page_done = FALSE;
	noqsc_count = 0;
	write_time = INFINITY;
	lasthistory = SPOTLESS;
	inc_dir_ok = FALSE;	/* ./INC not checked/made yet DLP */
	str_ptr_high = 0;	/* will load hi water marks from end file */
	pool_ptr_high = 0;	/* ...if it has been created yet. */
	fmem_lowater = 0;	/* incremental font info saves. DLP */
	font_lowater = -1;	/* fmem_ptr=FIRST free,font_ptr=LAST used!! */
	src_changed = squeaky_clean = TRUE;
	chk_offset = chk_eop = 0;
	f_max = f_all = w_max = w_all = p_all = 0;
	f_bgn = f_end = f_cur = NULL;
	w_bgn = w_end = NULL;
	p_bgn = p_end = p_cur = NULL;
	btime = 0;
	fnds = NULL;
	wnds = NULL;
	jump_val = 1;
	for (i = 0; i < MAX_IN_OPEN; i++)
		rfid[i] = NIL;
	for (i = 1; i < OUT_MAX; i++) {
		wfid[i] = owfid[i] = NIL;
		oweop[i] = NIL;
	}

	setup_format();
	if ((pgm_fn = strrchr(*argv, DIR_DEL_CHR)) == NULL)
		pgm_fn = *argv;
        else
		pgm_fn++;

	/* process command line options */
 	while (--argc > 0) {
		if (**++argv == SWH_PFX) {
			for (argp = ++*argv; *argp != NULL; argp++)
				switch (*argp) {
				case 'i':
#ifdef INIT
					init_blocked;
#endif
					/* always load incremental state,
					   don't compare format to load time */
					forced_incremental = TRUE;
					incremental = TRUE;
					trace_font  = TRUE;
					trace_eq    = TRUE;
					trace_hash  = TRUE;
					trace_xeq   = TRUE;
					break;
				case 'b':
					/* batch run, overriding any other
					   options */
					incremental = FALSE;
					trace_font  = FALSE;
					trace_eq    = FALSE;
					trace_hash  = FALSE;
					trace_xeq   = FALSE;
					qsc_check   = FALSE;
					break;
				case 'v':
					/* designated virgin,
					   overrides -q below */
					virgin      = TRUE;
					trace_font  = TRUE;
					incremental = TRUE;
					trace_eq    = TRUE;
					trace_hash  = TRUE;
					trace_xeq   = TRUE;
					qsc_check   = FALSE;
					break;
				case 'c':
#ifdef INIT
					init_blocked;
#endif
					/* compression required */
					do_compression = FALSE;
					break;
				case 'q':
#ifdef INIT
					init_blocked;
#endif
					/* no quiescence checkpointing */
					qsc_check = FALSE;
					break;
#ifdef DEBUG
				case 'z':
					/* show state comparison info */
					showq = FALSE; explain = TRUE;
					break;
#endif DEBUG
				case 'n':
					/* state checkpointing turned off */
					state_checkpointing = FALSE;
					qsc_check   = FALSE;
					break;
				case 'd':
					/* directory */
					--argc;
					++argv;
					strcpy(dir, *argv);
					strcat(dir, ":");
					strcat(dir, input_path);
					strcpy(input_path, dir);
					break;
				case 'j':
				/* jump (save) every n checkpoints when saving,
				   IncTeX will delete any old checkpoints in
				   between so no booby trapped old ones will
				   be sitting around. DLP */
#ifdef INIT
					init_blocked;
#endif
					/* jumping checkpoints */
					--argc;
					++argv;
					if ((*argv == NULL) ||
					    (sscanf(*argv, "%d", &jump_val)
					     == 0) || (jump_val <= 0)) {
						print_nl("Invalid jumping checkpoints `");
						print(*argv);
						print("'.");
						usage();
						exit(-1);
					}
					break;
				case 's':
					/* show state checkpoint result */
					show_state = TRUE;
					break;
				default:
					print_nl("Unknown option ");
					print_char(*argp);
					usage();
					exit(-1);
				}
		} else
			break;
	}

	
	if (incremental)
		if (argc == 0) {
			print_nl("Missing argument.");
			usage();
			exit(-1);
		} else if (argc > 2 /*1*/) {
			print_nl("Too many arguments.");
			usage();
			exit(-1);
		} else {
			if (qsc_check)
				create_quiesc_sets();
			GETDIFFCHUNK(fdim_list);
			fdim_end = fdim_list;
			fdim_end->nxt = fdim_end->prev = NULL;
			fdim_end->size = fdim_listlen = 0;
			GETDIFFHUNK(eqtb_list);/* for eqtb changes. DLP. */
			eqtb_end = eqtb_list;
			eqtb_end->nxt = eqtb_end->prev = NULL;
			eqtb_end->size = eqtb_listlen = 0;
			note_eq( 255 );
			/* addr 255 changed by fire_up() every call */
			GETDIFFHUNK(hash_list);/* for hash changes. DLP. */
			hash_end = hash_list;
			hash_end->nxt = hash_end->prev = NULL;
			hash_end->size = hash_listlen = 0;
			GETDIFFHUNK(xeq_list);/* for xeq_level changes. DLP. */
			xeq_end = xeq_list;
			xeq_end->nxt = xeq_end->prev = NULL;
			xeq_end->size = xeq_listlen = 0;
		}

	pre_any_format(argc,argv);
#ifdef DEBUG
	if (incremental)
		save_premac();
#endif DEBUG

	if (incremental)
		inc_format();
	else {
		/* normal formatting (non-incremental) */
		virgin = TRUE;
		virgin_run();
	}
}


virgin_run ()
{

	string_okay = FALSE;
	src_changed = TRUE;
	squeaky_clean = qsc_check = FALSE;
	if (incremental) {
		check_inc_dir();
		if (!open_font_file()) {
			print_nl("Disk full? Can't create font file!");
			history = FATAL_ERROR_STOP;
			return;
		}
		if (!open_page_file()) {
			print_nl("Disk full? can't create new page file!");
			history = FATAL_ERROR_STOP;
			return;
		}
	}
	LastStartPage = 1;
	LastEndPage = -1;

	pre_virgin_run();
#ifdef DEBUG
	if (incremental && show_state) dump_inputs();
#endif DEBUG
#ifndef INIT
#ifndef TRIP
 	print_nl("Formatting ");
	print(doc_fn);
	print(" from scratch...");
#endif TRIP
#endif INIT

	while (format_continue) {
#ifdef DEBUG
		if (incremental)
			save_eqhashxeq();
#endif DEBUG
		main_control();
		LastEndPage = total_pages;

		if (qsc_check && format_continue) {
				reconverged = comparestate( total_pages );
				if (reconverged) {
				   print_nl("Reconvergence Detected!");
				   noqsc_count = 0;
				} else if (FALSE/*noqsc_count>MAX_QSC_CHECKS*/)
				   qsc_check = FALSE;
				else noqsc_count++;
			}
	}
/*
 *	gettimeofday(&time4, &tz);
 */
	if (incremental)
		wrapup_inc();
	else {
		post_format();
		close_out();	/* clean up output - DLP */
	}
}


close_out ()
{
	char		tm[STR_MAX];

	gettimeofday(&time5, &tz);
	sprintf(tm, "%.3f", time_diff(time5, time0));
	print_nl("Total processing time: "); print(tm); print(" seconds.");
	print_nl("Total document pages: "); print_int(total_pages); print(".");
	print_ln();
	if (log_file != NULL) {
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
	update_terminal();
	exit(history);
}


inc_format ()
{
	if (virgin) {
		save_zero = TRUE;
		virgin_run();
	} else if (load_inc()) {
		inc_run();
	} else {
		free_all();
		save_zero = TRUE;
		virgin = TRUE;
		virgin_run();
	}
}

/* find tex file, set up names needed for saving incremental data.
 */

save_doc_name (fn)
	char    *fn;
{
	char    *np,
		*bp;

	strcpy(name_of_file, fn);
	name_length = strlen(name_of_file);	/* test_access needs this */
	if (! test_access(READ_ACCESS, INPUT_FILE_PATH)) {
		strcpy(name_of_file, fn);
		strcat(name_of_file, ".tex");
		name_length = strlen(name_of_file);
		if (! test_access(READ_ACCESS, INPUT_FILE_PATH)) {
			print_nl("Failed to open document root file ");
			print(name_of_file);
			print_ln();
			close_out();
		}
	}
	
	strcpy(doc_fn, name_of_file);

	/* find the rightmost DIR_DEL_CHR (`/') */
	if ((np = strrchr(fn, DIR_DEL_CHR)) == NULL)
		np = fn;
	else
		np++;

	/* find the leftmost EXT_DEL (`.') */
	if ((bp = strchr(np, EXT_DEL_CHR)) != NULL)
		*bp = NULL;
	
	strcpy(inc_fn, INC_AREA);
	strcat(inc_fn, np);
	strcat(inc_fn, EXT_DEL);
	strcat(inc_fn, EXT_INC);

	/* make INC file base name for later retrieval */
	strcpy(base_fn, INC_AREA);
	if ((bp = strchr(doc_fn, DIR_DEL_CHR)) != NULL)
		bp++;
	else
		bp = doc_fn;
	strcat(base_fn, bp);
	bp = base_fn + strlen(base_fn);
	while(*bp != '.')
		bp--;
	*bp = NULL;

	if (virgin)		/* if virgin run option turned on */
		return;
	
	if (access(inc_fn, R_OK) == -1) {
		/* Access failed, virgin indeed */
		print_nl("Incremental processing info not found.");
		print_ln();
		virgin = TRUE;
	} else
		virgin = FALSE;
}


#define scan_inc(F, V) { \
	if (fscanf(inc_fp, F, V) == /*mammoth*/ (char) EOF) \
		return(FALSE); \
}



int
load_inc ()
{
	int	ident_len,
		i;
	char	*s,
		*f,
		fmtname[256];

#define FORMAT_STRING	(char *) &str_pool[str_start[format_ident]]

	if (show_state) {
		print_nl("Recovering incremental processing info from ");
		print(inc_fn);
		print("...");
		update_terminal();
	}
	if ((inc_fp = fopen(inc_fn, "r")) == NULL)
		return(FALSE);
	
	scan_inc("%d", &lasthistory);	/* tex history code */

	scan_inc("%d >", &ident_len);	/* load .fmt style id */
	for ( i=0; i<ident_len;i++ )	/* don't skip any blanks */
		scan_inc("%c", &fmtname[i]);
	fmtname[ident_len] = '\0';

	if( ident_len != str_start[format_ident+1]-str_start[format_ident])
		goto STYLE_CHANGED;
	else 			/* compare fmt id's */
		for (i=0, s=FORMAT_STRING, f=(&fmtname[0]);
		     i<ident_len;
		     i++, s++, f++) {
			if (*s != *f) 
				goto STYLE_CHANGED;
		};

	scan_inc("%x", &i);		/* read state version */
	if( i != state_version()) {	/* should use read_state_code */
		print_ln();
		print_nl("Must reformat everything: the");
		print(" state checkpoint format has changed!");
		print_ln();
		return(FALSE);		/* reformat from scratch */
	}

	scan_inc("%ld",&tsync.tv_sec);	/* read time written */

	if (! load_fnodes()) {
		fclose(inc_fp); return(FALSE);
	}
	if (! load_pnodes()) {
		fclose(inc_fp); return(FALSE);
	}
	if (! load_wnodes()) {
		fclose(inc_fp); return(FALSE);
	}

	fclose(inc_fp);	
	if (show_state) {
		print("done"); print_ln();
		update_terminal();
	}
	return(TRUE);
STYLE_CHANGED:
	print_ln();
	print_nl("Must reformat everything: this");
	print(" IncTeX was built with a different");
	print_nl("initialization format (style) than the last");
	print(" time this document was formatted.");
	print_nl("New: "); print_str(format_ident);
	print_nl("Old: "); print(fmtname); print_ln();
	print_ln();
	fclose(inc_fp);	
	return(FALSE);	/* reformat from scratch */
}
#undef FORMAT_STRING

load_fnodes ()
{
	F_NODE		*fnd;
	int		i;
	int		parent;
	struct stat	ts;

	/* Recovering file chain */
	scan_inc("%d", &f_all);
	MALLOC(fnds, F_NODE*, f_all*sizeof(F_NODE*));
	for (i = 0; i < f_all; i++) {
		MALLOC(fnd, F_NODE, sizeof(F_NODE));
		fnds[i] = fnd; 

		scan_inc("%d", &(fnd->id));
		scan_inc("%d", &(fnd->sl));
		/*MALLOC(fnd->sn, char, (fnd->sl+1)*sizeof(char));*/
		CALLOC(fnd->sn, char, fnd->sl+1, sizeof(char));
		scan_inc("%s", fnd->sn);

		scan_inc("%d", &(fnd->bmt));	/* get backup's mod time. DLP */

		scan_inc("%d", &parent);
		scan_inc("%d", &(fnd->pbl));
		scan_inc("%d", &(fnd->pel));
		if (i == 0)
			fnd->up = NULL;
		else
			fnd->up = fnds[parent];
		
		/* Initializing remaining fields */
		fnd->eof = FALSE;
		fnd->mod = SRC_NIL;
		fnd->sp = NULL;
		fnd->bl = 0;
		fnd->bn = NULL;
		fnd->bp = NULL;
		fnd->cnt = 0;
		fnd->cbl = fnd->cel = 0;
		fnd->nxt = NULL;

		if (access(fnd->sn, F_OK) == 0) {
			stat(fnd->sn, &ts);
			fnd->mt = ts.st_mtime;
		} else
			fnd->mt = INFINITY;

		/* Fix up file chain links */
		if (f_end == NULL) {
			f_bgn = f_end = f_cur = fnd;
		} else {
			f_end->nxt = fnd;
			f_end = f_cur = fnd;
		}
	}
	scan_inc("%d", &i);
	if (i != EOB)
		return(FALSE);
	return(TRUE);
}


load_pnodes ()
{
	P_NODE		*pnd;
	int		parent;
	int		i;
	int		j;

	/* Recovering page chain */
	scan_inc("%d", &p_all);
	last_max_pages = p_all;
	if( last_max_pages < 1) {
		print_nl("Restarting from scratch, last pass was apparently aborted.");
		print_ln();
		return(FALSE);
	}
	for (i = 1; i <= p_all; i++) {
		MALLOC(pnd, P_NODE, sizeof(P_NODE));

		scan_inc("%d", &(pnd->no));
		scan_inc("%d", &parent);
		pnd->fp = fnds[parent];
		scan_inc("%d", &(pnd->eop));
		scan_inc("%f", &(pnd->ftime));
		scan_inc("%f", &(pnd->btime));
		scan_inc("%f", &(pnd->stime));
		scan_inc("%f", &(pnd->ltime));
		scan_inc("%f", &(pnd->qtime));
		for (j = 1; j < OUT_MAX; j++)
			scan_inc("%d", &(pnd->weop[j]));
		
		/* Initializing remaining fields */
		if( pnd->no != i) {
			print_nl("Incremental document description damaged:");
			print_nl("Page numbers corrupted in "); print(inc_fn);
			print_nl("I expected page "); print_int((int) pnd->no);
			print(" to be "); print_int(i);
			last_max_pages = 1;
			return(FALSE);
		}
		pnd->nxt = NULL;

		if (forced_incremental) { /* fudge stats so loads chkpt - DLP */
			pnd->stime = -32765;
			pnd->ltime = -32765;
		}

		/* Fix up page chain links */
		if (p_bgn == NULL) {
			p_bgn = p_end = pnd;
		} else {
			p_end->nxt = pnd;
			p_end = pnd;
		}
	}
	scan_inc("%d", &i);
	if (i != EOB)
		return(FALSE);
	return(TRUE);
}


load_wnodes ()
{
	W_NODE		*wnd;
	int		i;
	struct stat	ts;

	scan_inc("%d", &w_all);
	if (w_all > 0) {
		MALLOC(wnds, W_NODE*, w_all*sizeof(W_NODE*));
		for (i = 0; i < w_all; i++) {
			MALLOC(wnd, W_NODE, sizeof(W_NODE));
			wnds[i] = wnd; 
	
			scan_inc("%d", &(wnd->id));
			scan_inc("%d", &(wnd->wl));
			/*MALLOC(wnd->wn, char, (wnd->wl+1)*sizeof(char));*/
			CALLOC(wnd->wn, char, wnd->wl+1, sizeof(char));
			scan_inc("%s", wnd->wn);
	
			wnd->nxt = NULL;
			if (access(wnd->wn, F_OK) == 0) {
				stat(wnd->wn, &ts);
				wnd->ct = ts.st_ctime;
			} else
				wnd->ct = INFINITY;

			/* Fix up file chain links */
			if (w_end == NULL) {
				w_bgn = w_end = wnd;
			} else {
				w_end->nxt = wnd;
				w_end = wnd;
			}
		}
	}
	scan_inc("%d", &i);
	if (i != EOB)
		return(FALSE);
	return(TRUE);
}

open_page_file()
{
	/* string pool not ready yet */
	sprintf(name_of_file, "%s%s%s", base_fn, EXT_DEL, EXT_NEWPAGES);
	if ((pagelist = fopen(name_of_file, "w")) == NULL) {
		print_nl("Failed to open new page file ");
		print(name_of_file); print_char('.');
		return(FALSE);
	} else
		return(TRUE);
}

close_page_file()
{
	if (LastEndPage>LastStartPage)
	    fprintf(pagelist,"%d:%d\n",LastStartPage,LastEndPage);
	else if (LastEndPage==LastStartPage)
	    fprintf(pagelist,"%d,\n",LastStartPage);
	fprintf(pagelist,"-1\n");
	fprintf(pagelist,"%d Total Pages\n",total_pages);
	fclose(pagelist);
}


inc_run ()
{
	int	i;

	string_okay = TRUE;
	f_cur = f_bgn;
	f_end = NULL;
	f_max = 0;
	w_max = 0;
	p_end = p_cur = NULL;
	w_end = NULL;
	w_max = 0;
	reconverged = TRUE;	/* starts from same state since at beginning! */
	LastStartPage = 1;
	LastEndPage = -1;

	check_inc_dir();
	if (!open_font_file()) {
		print_nl("Disk full? can't create font file!");
		history = FATAL_ERROR_STOP;
		return;
	}
	if (!open_page_file()) {
		print_nl("Disk full? can't create new page file!");
		history = FATAL_ERROR_STOP;
		return;
	}
	
	pre_incr_run();
#ifdef DEBUG
	if (show_state) dump_inputs();
#endif DEBUG

	while (format_continue) {
		gettimeofday(&time1, &tz);

		if (reconverged) {
			skip_pages();

			/* no formatting necessary */
			if (all_clean) {
				print_nl("Source files unchanged.");
				print_nl("No formatting necessary.");
				post_format();
				close_page_file();
				close_out();
				return;
			} else from_beginning = from_beginning ||
				((p_max==0) && (p_all==0)
				&& (lasthistory>WARNING_ISSUED));

			if (!from_beginning && (p_cur==NULL) && (p_max==0)) {
			   if (lasthistory > WARNING_ISSUED) {
				print_nl("There are no changes within the");
				print(" input that was formatted, but we'll");
				print_nl("restart from the last page because");
				print(" there was an error last time.");
				p_cur=p_bgn;
				while (p_cur->nxt!=NULL)
					p_cur=p_cur->nxt;
			   } else {
				print_nl("No source input changes - ");
				print("no formatting necessary.");
				f_max = f_all;
				p_max = p_all;
				w_max = w_all;
				src_changed = FALSE;
				goto end;
			   }
			}

			if (!from_beginning && (p_cur == NULL)) {
				for (i = 1; i < OUT_MAX; i++)
					owfid[i] = wfid[i];
				if (!append_aux()) {
                                   print_nl("!!Couldn't restore write files");
                                   print(" to previous state at page ");
                                   print_int(p_cur->no);
                                   print_nl("Delete any that are damaged and");
                                   print(" re-run in virgin mode.");
                                   history=FATAL_ERROR_STOP;
                                   goto end;
				}
				f_max = f_all;
				w_max = w_all;
				cur_level = LEVEL_ONE;
				cond_ptr = NUL;

				if (p_max < p_all) {
					print_nl("Quiescence found at page ");
					print_int(p_max);
					print(", no more formatting needed.");
					p_max = p_all;
				}
				goto end;
			}
			
			if (from_beginning)  {
				print_nl("Different input files, formatting ");
				print(doc_fn);
				print(" from scratch for safety...");
				print_ln();
				update_terminal();
				from_beginning = FALSE;
				qsc_check = FALSE;
				cont_input();
			} else if (!suspend_format())
				goto end;
		}

#ifdef DEBUG
		if (incremental)
			save_eqhashxeq();
#endif DEBUG
		/* format one dirty page */
		reconverged = FALSE;
		main_control();
		LastEndPage = total_pages;

		if (format_continue) {
			p_end->ftime = time_diff(time3, time2);
			gettimeofday(&time4, &tz);
			p_end->stime = time_diff(time4, time3);
			if (p_end->ltime == 0.0)
				p_end->ltime = p_end->stime;
	
			if (qsc_check) {
				reconverged = comparestate( total_pages );
				if (reconverged) {
				    print_nl("Reconvergence Detected!");
				    noqsc_count = 0;
				} else if (FALSE/*noqsc_count>MAX_QSC_CHECKS*/)
				    qsc_check = FALSE;
				else noqsc_count++;
				gettimeofday(&time5, &tz);
				p_end->qtime = time_diff(time5, time4);
			}
		}
	}
end:
/*
 *	gettimeofday(&time4, &tz);
 */
	wrapup_inc();
}


handle_newfile (fn, read_f)
	char		*fn;
{
	print_nl("Encountering new ");
	if (read_f)
		print("input");
	else
		print("output");
	print(" file ");
	print(fn);
	if (qsc_check) {
		qsc_check = FALSE;
		print(", quiescence checking turned off.");
	}
	print_ln();
	update_terminal();
}



/*
 * Return the time spent in skipping these pages.
 */
int
skip_pages ()
{
	F_NODE		*cfd;
	P_NODE		*cpd;
	unsigned long	cid;

	p_cur = cpd = NULL;
	for (cfd = f_cur; cfd != NULL; cfd = cfd->nxt) {
		chk_offset = 0;

		if (cfd->mod == SRC_NIL) {
			cfd->mod = check_status(cfd);
			if (cfd->mod != SRC_CLEAN)
				squeaky_clean = FALSE;
		}

		switch (cfd->mod) {
		case SRC_CLEAN:
		case SRC_SAME:
			continue;
		case SRC_MISSING:
			/* set the beginning of parent file's input line
			   as the dirty char */
			all_clean = FALSE;
			if (cfd->up == NULL)
				continue;
			else
				cpd = find_page(cfd->up, cfd->up->pbl);
				/* Let user know what's going on... DLP */
				print_nl("Input file "); print(cfd->sn);
				print(" is different & no copy was kept,");
				print(" treating it as new; it affects page ");
				print_int((int) cpd->no);print(", after line ");
				print_int((int) cfd->up->pbl);print(", file ");
				print(cfd->up->sn);
			break;
		case SRC_NOBACKUP:
			/* Assume 1st char of cfd->sn is dirty */
			all_clean = FALSE;
			if (from_beginning) {
				cpd = find_page(cfd, 1);
				break;
			} else {
				from_beginning = TRUE;
				handle_newfile(cfd->sn, TRUE);
				return;
			}
		case SRC_DIRTY:
			all_clean = FALSE;
			if ((cid = diff_source(cfd)) == -1) {
				cfd->mod = SRC_SAME;
				continue;
			}
			cpd = find_page(cfd, cid);
			if ((cpd == NULL) || (cpd->no <= p_max))
				continue;
			print(", affects page ");
			print_int((int) cpd->no);
			if (chk_offset != 0) {
				update_fnodes(cfd);
				update_pnodes(cfd, cpd);
			}
			break;
		default:
			print_nl("Unknown return code");
			print_int(cfd->mod);
			print("from check_status()...ignored");
			all_clean = FALSE;
			if (cfd->up == NULL)
				continue;
			else
				cpd = find_page(cfd->up, cfd->up->pbl);
			break;
		}
		
		if ((p_cur == NULL) || ((cpd != NULL) && (cpd->no<p_cur->no)))
			p_cur = cpd;
	}
}


int
check_status (cfd)
	F_NODE		*cfd;
{
/*	struct stat	ts;	*/

	/* complete file has been checked */
	if (cfd->eof)
		return(SRC_CLEAN);

	if (access(cfd->sn, R_OK) == -1)
		/* Source file recorded in inc but no longer exists */
		return(SRC_MISSING);

	get_src_backup(cfd);
	if (access(cfd->bn, R_OK) == -1)
		/* Backup file not found */
		if (access(cfd->sn, W_OK) == -1) {
			/* Source is read-only which is not backed up. */
			cfd->eof = TRUE;
			return(SRC_CLEAN);
		} else {
			if( its_a_sty_file( cfd->sn ) ) 
				if (cfd->mt != cfd->bmt)
					return(SRC_MISSING);  /* no copy. DLP */
				else
					return(SRC_CLEAN);
			else /* with write permission, new file, e.g. foo.aux */
				return(SRC_NOBACKUP);
		}
	else {
/*		stat(cfd->bn, &ts);	Read time from .inc fileinstead. DLP */
/*		if (cfd->mt > ts.st_mtime) */

		/* Compare timestamps */
		if (cfd->mt > cfd->bmt)
			return(SRC_DIRTY);
		else
			return(SRC_CLEAN);
	}
}

/* Returns true if style file: if name ends in ".sty". DLP
 */
its_a_sty_file (s)
	char *s;
{
	int	l;
	char	*stub;

	l = strlen(s);
	if (l > 3) {
		stub = s + l - 4;
		if (strcmp(stub,".sty") == 0)
			return(TRUE);
	}
	return(FALSE);
}

get_src_backup (fnd)
	F_NODE		*fnd;
{
	char		*np;

	/* Get corresponding backup filename
	 * check first part of pathname, kill it if it's "./" */

	if ((np = strchr(fnd->sn, DIR_DEL_CHR)) == NULL)
		np = fnd->sn;
	else {
		*np = '\0';
		if ((strcmp(fnd->sn,".") == 0) || (fnd->sn == np)) {
			*np = DIR_DEL_CHR;
			np++;		/* put np after "./" or "/" */
		} else {
			*np = DIR_DEL_CHR;
			np = fnd->sn;
		}
	}
	fnd->bl = strlen(np)+strlen(INC_AREA);
	/* MALLOC(fnd->bn, char, (fnd->bl+1)*sizeof(char));*/
	CALLOC(fnd->bn, char, fnd->bl+1, sizeof(char));
	strcpy(fnd->bn, INC_AREA);
	strcat(fnd->bn, np);
}


get_aux_backup (wnd)
	W_NODE		*wnd;
{
	char		*np;

	/* Get corresponding backup filename
	 * check first part of pathname, kill it if it's "./" */
	if ((np = strchr(wnd->wn, DIR_DEL_CHR)) == NULL)
		np = wnd->wn;
	else {
		*np = '\0';
		if ((strcmp(wnd->wn,".") == 0) || (wnd->wn == np)) {
			*np = DIR_DEL_CHR;
			np++;		/* put np after "./" or "/" */
		} else {
			*np = DIR_DEL_CHR;
			np = wnd->wn;
		}
		np++;
	}
	wnd->bl = strlen(np)+strlen(INC_AREA);
	/*MALLOC(wnd->bn, char, (wnd->bl+1)*sizeof(char));*/
	CALLOC(wnd->bn, char, wnd->bl+1, sizeof(char));
	strcpy(wnd->bn, INC_AREA);
	strcat(wnd->bn, np);
}


P_NODE *
find_page (fnd, cid)
	F_NODE		*fnd;
	unsigned long	cid;
{
	F_NODE		*old;
	F_NODE		*new;
	P_NODE		*pnd;
	unsigned long	id;
	unsigned long	eop;

	/* find the page a particular cid in file fnd belongs to */
	for (pnd = p_bgn; pnd != NULL; pnd = pnd->nxt) {
		eop = pnd->eop;
		for (old = pnd->fp; old != NULL; old = old->up) {
			id = cid;
			for (new = fnd; new != NULL; new = new->up) {
				if ((new->id == old->id) && (id < eop))
					return(pnd);
				id = new->pel;
			}
			eop = old->pel;
		}
	}
	return(NULL);
}



#define file_closed_p(FP)	(((FP) == NULL) || (ftell((FP)) == EOF))

#define close_file(FP) { \
	fclose(FP); \
	FP = NULL; \
}


/* Diff the two source files: original in cfd->sn, backup in cfd->bn.
 * Both files must exist at this point.
 * The two files are first positioned to p_end->eop and chk_eop, respectively.
 * Return the first char in which they differ.
 * Return -1 if identical.
 */
int
diff_source (cfd)
	F_NODE		*cfd;
{
	unsigned long	id;
	int		pos;
	int		c1;
	int		c2;

	/* at this point, cfd should point to an incomplete file */
	id = (p_end == NULL) ? f_bgn->id : p_end->fp->id;
	if ((id == cfd->id) && (p_end != NULL)) {
		pos = p_end->eop;
		chk_offset = pos - chk_eop;
	} else {
		pos = 0;
		chk_eop = 0;
		chk_offset = 0;
	}

	/* R_OK already verified by check_status(), so this should be safe */
	if ((cfd->sp = fopen(cfd->sn, "r")) == NULL) {
		print_nl("Failed to open file "); print(cfd->sn);
		print(" for reading."); print_ln();
		return(0);
	}
	if (fseek(cfd->sp, (long) pos, SEEK_SET) != 0) {
		print_nl("Failed to find position ");
		print_int(pos); print(" on file ");
		print(cfd->sn); print_ln();
		close_file(cfd->sp);
		return(0);
	}

	if ((cfd->bp = fopen(cfd->bn, "r")) == NULL) {
		print_nl("Failed to open file "); print(cfd->bn);
		print(" for reading."); print_ln();
		return(0);
	}
	if (fseek(cfd->bp, (long) chk_eop, SEEK_SET) != 0) {
		print_nl("Failed to find position "); print_int(chk_eop);
		print(" on file "); print(cfd->bn); print_ln();
		close_file(cfd->sp);
		close_file(cfd->bp);
		return(0);
	}

	pos = chk_eop;
	while (TRUE) {
		c1 = getc(cfd->sp);
		c2 = getc(cfd->bp);
		if ((c1 == EOF) && (c2 == EOF)) {	/* files identical */
			pos = -1;
			break;
		}
		if ((c1 != c2) || (c1 == EOF) || (c2 == EOF))
			break;
		++pos;
	}
	/* Ideally we should close the file here, but this would
	 * cause the same file pointer to be assigned to the
	 * next open file, which causes strange EOF problem
	 */
	close_file(cfd->sp);
	close_file(cfd->bp);
	/* Let user know what's going on... DLP */
	if (pos >= 0) {
		print_nl("File ");
		print(cfd->sn);
		print(" is different at character ");
		print_int(pos);
	}
	return(pos);
}


int
update_fnodes (cfd)
	F_NODE		*cfd;
{
	F_NODE		*fnd;
	P_NODE		*lpd;

	lpd = (p_end == NULL) ? p_bgn : p_end;
	for (fnd = cfd; fnd != NULL; fnd = fnd->nxt) {
		/* current input line is don't care */
		if ((fnd->up != NULL) && (fnd->up->id == cfd->id) &&
		    (fnd->pbl > lpd->eop)) {
			fnd->pbl = fnd->pbl + chk_offset;
			fnd->pel = fnd->pel + chk_offset;
		}
	}
}


int
update_pnodes (cfd, cpd)
	F_NODE		*cfd;
	P_NODE		*cpd;
{
	P_NODE		*pnd;

	/* update eop from the pnode next to *p_end until *cpd */
	for (pnd = (p_end == NULL) ? p_bgn : p_end->nxt;
	     pnd != NULL; pnd = pnd->nxt) {
		if (pnd->fp->id == cfd->id)
			pnd->eop = pnd->eop + chk_offset;
		if (pnd == cpd)
			break;
	}
}



int
suspend_format ()
{
	int		boc;
	int		eoc;
	int		i;
	int		t_tmp,
			f_tmp,		/* save log/terminal position - DLP */
			select_old,
			select_tmp;	/* save current output redirection */

	boc = (p_end == NULL) ? 1 : p_end->no + 1;
	eoc = p_cur->no - 1;

	if (check_state(boc, eoc)) {
		gettimeofday(&time2, &tz);
		for (i = 1; i < OUT_MAX; i++)
			owfid[i] = wfid[i];
		select_old = selector;	/* save curr selector setting - DLP */
		if ((time_diff(time2, time1) + p_cur->ltime < format_time()) &&
		    load_state((int) p_cur->no) && restore_file()) {
			if (LastEndPage>LastStartPage)
			  fprintf(pagelist,"%d:%d\n",LastStartPage,LastEndPage);
			else if (LastEndPage==LastStartPage)
			  fprintf(pagelist,"%d,\n",LastStartPage);
			LastStartPage = p_cur->no+1;
			select_tmp = selector;	/* save restored state's */
			selector = select_old;	/* output settings - DLP */
			t_tmp = term_offset;
			f_tmp = file_offset;
			/* not worth saving where msgs by load_state (and
			   restore_file) should have left offsets, main
			   thing is whether offsets > 0 */
			term_offset = file_offset = 50;
			if (p_cur->no <= boc)
				print_nl("Formatting skipped for page ");
			else {
				print_nl("Formatting skipped for pages ");
				print_int(boc);
				print(" through ");
			}
			print_int(p_cur->no);
			print_char('.');
			print_ln();
			update_terminal();
			if (!append_aux()) {
                        	print_nl("!!Couldn't restore write files");
                        	print(" to previous state at page ");
                        	print_int(p_cur->no);
                        	print_nl("Delete any that are damaged and");
                        	print(" re-run in virgin mode.");
                        	history=FATAL_ERROR_STOP;
                        	return(FALSE);
			}
			term_offset = t_tmp;	/* restore offsets - DLP */
			file_offset = f_tmp;
			selector = select_tmp;
			p_end = p_cur;
			boc = p_end->no + 1;
			gettimeofday(&time3, &tz);
			p_end->ltime = time_diff(time3, time2);
		}
	} else {
		if (boc < eoc) {
			print_nl("Failed to load any state ");
			print("checkpoint within current pocket [");
			print_int(boc);
			print(", ");
			print_int(eoc);
			print("].");
			update_terminal();
		}
		if (boc == 1) {
			print_nl("Reformatting ");
			print(doc_fn);
			print(" from the beginning...");
			print_ln();
			update_terminal();
			/* fix to keep cont_input from getting called twice if
			   state checkpts 1..eoc missing - DLP */
			if (p_end != NULL)
				cont_input();
		}
	}

	if (boc > eoc)
		return(TRUE);
	
	if (p_end == NULL) {
		/* fix if had to back up and reformat from scratch
		   even though file only changed from later pages.
		   Have to start page pointing at NULL (= page 0)
		   or page-end info gets off by one - DLP */
		if (boc > 1)
			p_end = p_bgn;
		cont_input();
	}

	if (boc == eoc) {
		print_nl("Formatting page ");
		print_int(boc);
		print(" as a clean page...");
		print_ln();
		update_terminal();
	} else {
		print_nl("Formatting clean document between pages ");
		print_int(boc);
		print(" and ");
		print_int(eoc);
		print("...");
		print_ln();
		update_terminal();
	}

	for (; boc <= eoc; boc++) {
#ifdef DEBUG
		if (incremental)
			save_eqhashxeq();
#endif DEBUG
		main_control();
		LastEndPage = total_pages;
	}
	return(TRUE);
}


int
check_state (boc, eoc)
	int		boc;
	int		eoc;
{
	int		i;
	
	for (i = eoc; i >= boc; i--) {
		if (open_state_file(LOAD, i))
			break;
	}

	if (i < boc)
		return(FALSE);

	for (p_cur = (p_end == NULL) ? p_bgn : p_end;
	     p_cur != NULL;
	     p_cur = p_cur->nxt) {
		if (p_cur->no == i) {
			f_cur = p_cur->fp;
			return(TRUE);
		}
	}
	return(FALSE);
}


float
format_time ()
{
	P_NODE		*pnd;
	float		tv = 0.0;
	
	pnd = (p_end == NULL) ? p_bgn : p_end->nxt;
	while (pnd != NULL) {
		tv = tv + pnd->ftime;
		if (pnd == p_cur)
			return(tv);
		pnd = pnd->nxt;
	}
	return(0.0);
}


int
restore_file ()
{
	F_NODE		*fnd;
	W_NODE		*wnd;
	int		i;

	if (f_cur == NULL)
		return(FALSE);
	if (chk_fid != f_cur->id)
		return(FALSE);

	/* Restore end of file chain */
	for (f_end = f_cur; f_end != NULL; f_end = f_end->nxt)
		if (f_end->id == (f_max - 1))
			break;
	if (f_end == NULL)
		return(FALSE);

	/* TeX starts input_file[] from 1 instead of 0 (0 used by terminal) */
	/* in_open is the number of open files */
	/* rfid[] maps index (cur_input.index_field) to read file id */
	/* fnds[] maps read file id to its fnode */
	for (i = 1; i <= in_open; i++) {
		if ((rfid[i] != NIL) && (fnds != NULL) &&
		    ((fnd = fnds[rfid[i]]) != NULL)) {
			if (file_closed_p(fnd->sp))
				if ((fnd->sp = fopen(fnd->sn, "r")) == NULL)
					return(FALSE);
			if (fnd->up != NULL) {
				if (file_closed_p(fnd->up->sp) &&
				    ((fnd->up->sp = fopen(fnd->up->sn, "r"))
				     == NULL))
					return(FALSE);
				fseek(fnd->up->sp, (long) fnd->pel, SEEK_SET);
				fnd->up->cnt = fnd->pel;
			}
			input_file[i] = fnd->sp;
		}
	}
	f_cur->cnt = chk_cid + chk_offset;
	fseek(f_cur->sp, (long) f_cur->cnt, SEEK_SET);

	/* Restore file position on write files */
	for (i = 1; i < OUT_MAX; i++) {
		if ((wfid[i] != NIL) && (wnds != NULL) &&
		    ((wnd = wnds[wfid[i]]) != NULL)) {
			if (file_closed_p(wnd->wp))
				wnd->wp = fopen(wnd->wn, "w");
				/* open without truncating file */
			write_file[i] = wnd->wp;
			write_open[i] = TRUE;
/*  any segments printed in skipped pages will by copied to aux file
 *  by append_aux()  */
		} else {
			write_file[i] = NULL;
			write_open[i] = FALSE;
		}
	}

	return(TRUE);
}	

free_all ()
{
 	if (fnds != NULL)
		free(fnds);
	free_fnodes(f_bgn);
	f_all = f_max = 0;
	f_bgn = f_end = f_cur = NULL;

	free_pnodes(p_bgn);
	p_all = 0;
	p_bgn = p_end = NULL;

	free(wnds);
	free_wnodes(w_bgn);
	w_all = w_max = 0;
	w_bgn = w_end = NULL;
}


free_fnodes (fnd)
	F_NODE		*fnd;
{
	F_NODE		*gt;

 	while (fnd != NULL) {
		gt = fnd;
		fnd = fnd->nxt;
		free(gt->sn);
		free(gt->bn);
		free(gt);
	}
}


free_pnodes (pnd)
	P_NODE		*pnd;
{
	P_NODE		*qnd;

	while (pnd != NULL) {
		qnd = pnd;
		pnd = pnd->nxt;
		free(qnd);
	}
}



free_wnodes (wnd)
	W_NODE		*wnd;
{
	W_NODE		*gt;

 	while (wnd != NULL) {
		gt = wnd;
		wnd = wnd->nxt;
		free(gt->wn);
		free(gt);
	}
}


int
wrapup_inc ()
{
	struct stat buf;
	int	i;

	post_format();

	gettimeofday(&wrapup, &tz);

	close_page_file();

	if (src_changed) {
		close_font_file();
		(void) save_strings();
		if (show_state) {
		  print_nl("Premac high = ");  print_int(premac_hi);
		  print_nl("Premac low = ");   print_int(premac_lo);
		  print_nl("fmem_lowater = "); print_int(fmem_lowater);
		  print_nl("font_lowater = "); print_int(font_lowater);
	  	  print_nl("global memory range: ");
	  	  print_int((unsigned long) &beg_save_globals);print(" - ");
	  	  print_int((unsigned long) &end_save_globals);
		}
	}
	if (! state_checkpointing) {
		print_nl("No state checkpointing in this processing cycle.");
		print_ln();
	}
	if (! squeaky_clean)	/* save new time stamps */
		save_inc();
	if (src_changed) {
		/* delete the last page chkpt (we'll never restart from there!)
	   	   and any extra from the last run. DLP */
		get_ext(total_pages, EXT_STC);
		if (qsc_check) {
			get_ext(-1, EXT_LASTSTATE);
			if (stat(name_of_file, &buf) == /*<*/ 0)
				unlink(name_of_file);
		}
		if (total_pages<last_max_pages){  /* kill xtra chkpts. DLP */
		   print_nl("Deleting checkpoints from last run for pages ");
		   print_int(total_pages+1);
		   print(" - ");
		   print_int(last_max_pages);
		}
		/* We don't need the one for total_pages, but this would
		 * take more hacking so comparestate wouldn't put out an
		 * error message when looking for it. */
		for ( i=total_pages+1;++i<=last_max_pages; ) {
		   get_ext(i, EXT_STC);
		   if (stat(name_of_file, &buf) == /*<*/0) unlink(name_of_file);
/*		   get_ext(i, EXT_DVI);
 All 1 file now	   if (stat(name_of_file, &buf) < 0) unlink(name_of_file);
 */		}
	}
	close_out();
}



int
save_inc ()
{
	int		i;

	if (show_state) {
		print_nl("Saving file & page info (in .inc)...");
		update_terminal();
	}

	save_good = TRUE;
	if ((inc_fp = fopen(inc_fn, "w")) == NULL) {
		print_nl("Failed to open inc file ");
		print(inc_fn);
		print_ln();
		return(FALSE);
	}

	fprintf(inc_fp, "%d\n", history);	/* tex history code */

						/* save .fmt style id */
	fprintf(inc_fp,"%d >",str_start[format_ident+1]-str_start[format_ident]);
	for ( i=str_start[format_ident]; i < str_start[format_ident + 1]; i++ )
		fprintf( inc_fp,"%c",str_pool[i]);
	fprintf(inc_fp,"\n");

	i = state_version();
	fprintf(inc_fp,"%x\n",i);		/* save state version */

	if (src_changed)
		fprintf(inc_fp,"%ld\n", time0.tv_sec);	/* save time started */
	else
		fprintf(inc_fp,"%ld\n", tsync.tv_sec);	/* time of last run */

	save_fnodes();
	save_pnodes();
	save_wnodes();

	if (show_state) {
		if (save_good) print("done");
		else print("!! *failed* !!");
		print_nl("History = "); print_int(history); print_ln();
	} else if (!save_good) {
		print_nl("File & page info (.inc) save !! *failed* !!");
		print_ln();
		update_terminal();
	}
	if (divergent) {
		print_nl("New auxiliary files created ");
		print("(potential divergence detected).");
		print_nl("Auxiliary processing and further ");
		print("reformatting may be necessary.");
		print_ln();
	}
	fclose(inc_fp);
	return(TRUE);
}

save_fnodes ()
{
	struct stat	ts;
	int		i;

	/* Save read file chain info.
	 * Files read AND written will be saved by save_wnodes
	 * (the mod times ts.st_mtime will be less than write_time). DLP */
	fprintf(inc_fp, "%d\n", f_max);
	for (i = 0, f_end = f_bgn; ((i < f_max) && (f_end != NULL));
	     i++, f_end = f_end->nxt) {
		/* no. & length & filename & mod time */
		fprintf(inc_fp, "%d    %d    %s    %d\n",
			f_end->id, f_end->sl, f_end->sn, f_end->mt);
	
		/* parent file id */
		if (f_end->up == NULL) {
			/* root file */
			fprintf(inc_fp, "%d    %d    %d\n", f_end->id, 0L, 0L);
		} else {
			/* non-root file */
			fprintf(inc_fp, "%d    %d    %d\n",
				f_end->up->id, f_end->pbl, f_end->pel);
		}

		/* If f_end->eof TRUE then file was already closed, a_open_out
		 * will have already checked if we should save it. DLP */
		if (src_changed && (f_end->mod != SRC_CLEAN) && (!f_end->eof)) {
			stat(f_end->sn, &ts);
			if (ts.st_mtime < write_time)
				backup_source(f_end);
		}
	}
	fprintf(inc_fp, "%d\n", EOB);
}

save_pnodes ()
{
	int		i;
	int		j;

	/* Save page chain info */
	fprintf(inc_fp, "\n%d\n", p_max);
	for (i = 0, p_end = p_bgn; ((i < p_max) && (p_end != NULL));
	     i++, p_end = p_end->nxt) {
		if (p_end->fp == NULL) {
			if (save_good) {
				save_good = FALSE; print_ln();
			}
			print_nl("Page ");
			print_int(p_end->no);
			print(" pointing to null file...abort");
			fclose(inc_fp);
			unlink(inc_fn);
			close_out();
		}
		fprintf(inc_fp,"%d   %d   %d   %.3f   %.3f   %.3f   %.3f   %.3f\n",
			p_end->no, p_end->fp->id, p_end->eop, p_end->ftime,
			p_end->btime, p_end->stime, p_end->ltime, p_end->qtime);
		for (j = 1; j < OUT_MAX; j++)
			fprintf(inc_fp, "%d  ", p_end->weop[j]);
		fprintf(inc_fp, "\n");
	}
	fprintf(inc_fp, "%d\n", EOB);
}

save_wnodes ()
{
	int		i;

	/* Save write file chain */
	fprintf(inc_fp, "\n%d\n", w_max);
	for (i = 0, w_end = w_bgn; ((i < w_max) && (w_end != NULL));
	     i++, w_end = w_end->nxt) {
		/* length & filename */
		fprintf(inc_fp, "%d    %d    %s\n",
				w_end->id, w_end->wl, w_end->wn);
		if (w_end->bn == NULL)
			get_aux_backup(w_end);
		else
			fclose(w_end->bp);
		/* change time check to <= write_time, since the first
		 * write file has mod time w_end->ct = write_time! DLP */
		if (src_changed) {	/* make copy of write file */
		  if ((access(w_end->bn, F_OK) != 0) &&
		    (w_end->ct <= write_time) &&
		    ((w_end->bp = fopen(w_end->bn, "w")) != NULL) &&
		    ((w_end->wp = fopen(w_end->wn, "r")) != NULL)) {
			fseek(w_end->wp, 0L, SEEK_END);
			copy_file(ftell(w_end->wp), 0, w_end->wn,
				  w_end->wp, w_end->bn, w_end->bp);
			fclose(w_end->wp);
			fclose(w_end->bp);
		  } else if (!divergent && (! diff_aux()))
			divergent = TRUE;
		}
	}
	fprintf(inc_fp, "%d\n", EOB);
}

int
diff_aux ()
{
	int		c1;
	int		c2;

	if ((w_end->wp = fopen(w_end->wn, "r")) == NULL)
		return(FALSE);
	if ((w_end->bp = fopen(w_end->bn, "r")) == NULL)
		return(FALSE);

	while (TRUE) {
		c1 = getc(w_end->wp);
		c2 = getc(w_end->bp);

		if (c1 != c2) {
			fclose(w_end->wp);
			fclose(w_end->bp);
			return(FALSE);
		}
		if ((c1 == EOF) && (c2 == EOF)) {
			fclose(w_end->wp);
			fclose(w_end->bp);
			return(TRUE);
		}
	}
}

#define DIRMODE 0775

int
check_dirs_made( s )
	char *s;
{	char *beg,
	     *tail;
	int  levelcount;

	levelcount = 0;
	beg = s;
	tail = strchr(beg, DIR_DEL_CHR);
	while (tail != NULL) {
	   *tail = '\0';
	   if (strcmp( beg, ".") == 0)
		; /* just skip this section */
	   else if (strcmp( beg, "..") == 0) {
		levelcount--;
		if (levelcount<0) {
			/* warning, abort backup */
			*tail = DIR_DEL_CHR;
			return(FALSE);
		}	/* otherwise skip it */
	   } else {
		levelcount++;
		if (access(s, F_OK) < 0) {
#ifdef DEBUG
		    print_nl("creating: ");print(s);
#endif DEBUG
		    if (mkdir(s,DIRMODE) < 0) {
		    	print_nl("!!Couldn't create: ");print(s);
			/* warning, couldn't create it */
			*tail = DIR_DEL_CHR;
			return(FALSE);
		    }
		}
	   }
	   *tail = DIR_DEL_CHR;
	   beg = ++tail;
	   tail = strchr(beg, DIR_DEL_CHR);
	}
	return(TRUE);
}

/* Backup source file in the ./INC directory.
 * No backup if the source is read-only
 * ...or .sty file: probably read in preamble on page 1 & we have to
 * ...restart from scratch anyways. DLP.
 */
backup_source (fnd)
	F_NODE		*fnd;
{
	struct timeval	t0;
	struct timeval	t1;

	gettimeofday(&t0, &tz);

	if ( its_a_sty_file(fnd->sn) )
			goto BACKUP_RETURN;

	if (virgin && (access(fnd->sn, W_OK) == -1))
		goto BACKUP_RETURN;	/* Read-only, no need to backup */

	if (fnd->bn == NULL)
		get_src_backup(fnd);

	if ((fnd->bp = fopen(fnd->bn, "w")) == NULL) {
	   if (!check_dirs_made(fnd->bn))
		goto BACKUP_FAIL;
	   if ((fnd->bp = fopen(fnd->bn, "w")) == NULL)
		goto BACKUP_FAIL;
	}

	if ((fnd->sp = fopen(fnd->sn, "r")) == NULL) {
		print_nl("Failed to open file ");
		print(fnd->sn);
		print(" as source of backup.");
		print_ln();
		close_file(fnd->bp);
		goto BACKUP_FAIL;
	}
	fseek(fnd->sp, 0L, SEEK_END);
	if (! copy_file(ftell(fnd->sp), 0,
			fnd->sn, fnd->sp, fnd->bn, fnd->bp)) {
		close_file(fnd->sp);
		close_file(fnd->bp);
		goto BACKUP_FAIL;
	}

	close_file(fnd->sp);
	close_file(fnd->bp);
BACKUP_RETURN:
	gettimeofday(&t1, &tz);
	btime += time_diff(t1, t0);
	return(TRUE);
BACKUP_FAIL:
	if (save_good)
		save_good = FALSE;
	print_nl("Source backup failed for file "); print(fnd->bn); print_ln();
	gettimeofday(&t1, &tz);
	btime += time_diff(t1, t0);
	return(FALSE);
}

int
copy_file (cnt, pos, sn, sp, bn, bp)
	long		cnt;
	int		pos;
	char		*sn;
	FILE		*sp;
	char		*bn;
	FILE		*bp;
{
	char		*buf;

	if (fseek(sp, pos, SEEK_SET) != 0)
		return(FALSE);

	MALLOC(buf, char, cnt*sizeof(char));
	if (fread(buf, 1, (int) cnt, sp) != cnt) {
		if (save_good) {
			save_good = FALSE; print_ln();
		}
		print_nl("Failure reading file ");
		print(sn); print_ln(); return(FALSE);
	}

	if (fwrite(buf, 1, cnt, bp) != cnt) {
		if (save_good) {
			save_good = FALSE;
			print_ln();
		}
		print_nl("Failure writing file ");
		print(bn); print_ln(); return(FALSE);
	}
	free(buf);
	return(TRUE);
}


/* Current clean pocket goes from p_end->no + 1 to p_cur->no, inclusive.
 * Write heads on all write_file[] should be current.
 * oweop[] holds the old write_file[] eops for page p_end->no.
 * owfid[] holds the old write_file[] fids on page p_end->no.
 * Current write_file[] fids are kept on wfid[].
 * if p_end != NULL
 * For each i,
 * if (oweop[i] != p_end->weop) {
 *   if (owfid[i] == wfid[i] != EOF) then concatenate (oweop[i], p_cur->weop)
 *	from file  wnds[owfid[i]]->bp to file wnds[wfid[i]]->wp.
 *	Update each pnd->weop[i] in the clean pocket with offset
 *	(p_end->weop[i] - oweop[i]).
 *   else if (owfid[i] != wfid[i] != EOF) then concatenate (oweop[i], EOF)
 *	from file wnds[owfid[i]]->bp to file wnds[owfid[i]]->wp.
 *	and open wnds[wfid[i]]->wp and copy (0, f_cur->weop[i]) to it.
 * if (p_end == NULL)
 * 	just seek to whatever f_cur points to.
 */

int
append_aux ()
{
	int		i;
	int		copy_ok;
	W_NODE		*wnd;

	copy_ok = TRUE;
	for (i = 1; i < OUT_MAX; i++) {
		if (owfid[i] == wfid[i]) {
			if ((oweop[i]!=NIL) && ((wnd=wnds[wfid[i]]) != NULL))
			  if (p_cur != NULL) { 
			    if ((p_cur->weop[i] != NIL) &&
			        (oweop[i] < p_cur->weop[i]))
				if (!copy_aux(wnd, oweop[i], p_cur->weop[i]))
					copy_ok = FALSE;
			        /* if same file, copy from eop at last
			         * formatted page to p_cur's eop */
			  } else if (!copy_aux(wnd, oweop[i], -1))
					copy_ok = FALSE;
				/* p_cur == NULL means quiescence has been
				 * detected, if we aren't at last page we
				 * have to duplicate any output that would
				 * have been done if we finished processing
				 * so we copy tail of owfid */
		} else {
			if ((oweop[i] != NIL) &&
			    ((wnd = wnds[owfid[i]]) != NULL))
				if (!copy_aux(wnd, oweop[i], -1))
					copy_ok = FALSE;
				/* copy tail of owfid to its equivalent */
			if ((p_cur != NULL) && (p_cur->weop[i] != NIL) &&
			    ((wnd = wnds[wfid[i]]) != NULL))
				if (!copy_aux(wnd, 0, p_cur->weop[i]))
					copy_ok = FALSE;
				/* copy head of wfid up to curr eop */
		}
	}
	return(copy_ok);
}

int
copy_aux (wnd, bop, eop)
	W_NODE	*wnd;
	int	bop;
	int	eop;
{
	if (wnd->bn == NULL)
		get_aux_backup(wnd);
	if (file_closed_p(wnd->bp))
		wnd->bp = fopen(wnd->bn, "r");
	if (wnd->bp == NULL) {
		print_nl("!! Error, can't duplicate ");
		print(wnd->wn);print(" because ");
		print(wnd->bn);print(" doesn't exist.");
		return(FALSE);
	}
	if (eop == -1) {
		fseek(wnd->bp, 0L, SEEK_END);
		eop = ftell(wnd->bp);
	}
	if (file_closed_p(wnd->wp))
		wnd->wp = fopen(wnd->wn, "w");
	if (!copy_file(eop - bop, bop, wnd->bn, wnd->bp, wnd->wn, wnd->wp)) {
		print_nl("!! Error, can't copy output from previous run from ");
		print(wnd->bn);print(" to "); print(wnd->wn);
		return(FALSE);
	} else
		return(TRUE);
}
#endif
