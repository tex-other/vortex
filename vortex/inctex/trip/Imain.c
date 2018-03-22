#ifdef INCTEX
/*
 *  This file is part of
 *
 *  IncTeX  --	Incremental TeX
 *
 *  An Editor-Independent, Adaptive, Incremental TeX Formatter
 *
 *  Copyright (C) 1988 by Regents of the University of California
 *  
 *  Author:
 *	Pehong Chen, currently at
 *	Olivetti Research Center, Computer Systems Research Lab
 *	Menlo Park, CA  USA
 *	(chen@orc.olivetti.com)
 *
 *	Ikuo Minakata, currently at
 *	Matsushita Electric Industrial Co., Information Systems Research Lab
 *	Osaka,  Japan
 *	(min@renoir.berkeley.edu)
 *
 *	Prof. Michael A. Harrison
 *	University of California,  Computer Science Dept.
 *	Berkeley, CA  USA
 *	(harrison@berkeley.edu)
 *
 *	Derluen Pan
 *	University of California,  Computer Science Dept.
 *	Berkeley, CA  USA
 *	(pan@ucbarpa.berkeley.edu)
 *
 *  All rights reserved by the copyright holders.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

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

int		forced_incremental;	/* TRUE = always use checkpoint - DLP*/
int		incremental;
int		divergent;
int		virgin;
int		save_zero;
int		quiescent;
int		show_state;
int		qsc_check;
int		state_checkpointing;
int		from_beginning;
int		do_compression;
int		page_done;
int		page_shipped;
int		format_continue;
int		all_clean;
int		save_good;
int		full_backup;	/* FALSE if only source backup is needed */
time_t		write_time;
int		last_max_pages; /* so we can clean up extra chkpt files if less pages */

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
int		qsc_count;
float		btime;		/* time to backup source file in a page */
int		owfid[OUT_MAX];
int		oweop[OUT_MAX];

/*
 *  Using history as the basis of our adaptive strategy
 */
struct timeval	time0;
struct timeval	time1;
struct timeval	time2;
struct timeval	time3;
struct timeval	time4;
struct timeval	time5;
struct timezone tz;

#define	QSC		2

void
get_ext (n, ext)
	char		ext[];
{
	char		tmp[EXT_MAX];
	str		str_ext;

	if (n >= 0)
		sprintf(tmp, "%s%d%s%s", EXT_DEL, n, EXT_DEL, ext);
	else
		sprintf(tmp, "%s%s", EXT_DEL, ext);
	str_ext = make_str_given(tmp);
	if (access(INC_DIR, F_OK) < 0)
		mkdir(INC_DIR, 0775);
	if (job_name == 0)
		job_name = str_texput;
	cur_area = str_inc;
	cur_name = job_name;
	cur_ext = str_ext;
	pack_cur_name();
	flush_string();		/* flush str_ext string, don't need it anymore. DLP */
}


main(argc, argv)
	char		*argv[];
{
	char		*argp;
	char		dir[MAX_PATH_CHARS];
	
	gettimeofday(&time0, &tz);

	forced_incremental = FALSE;	/* don't check reload overhead - DLP */
	signal(SIGINT, handle_int);
	show_state = FALSE;
	incremental = TRUE;
	divergent = FALSE;
	from_beginning = FALSE;
	do_compression = FALSE;
	virgin = FALSE;
	save_good = TRUE;
	save_zero = FALSE;
	quiescent = QSC;
	qsc_count = QSC - 1;
	qsc_check = FALSE;
	page_shipped = FALSE;
	page_done = FALSE;
	format_continue = TRUE;
	state_checkpointing = TRUE;
	all_clean = TRUE;
	write_time = INFINITY;
	fmem_lowater = 0;	/* incremental font info saves. DLP */
	font_lowater = -1;	/* fmem_ptr == first free, font_ptr = last used, ugh */
	GETFONTCHUNK(fdim_list);
	fdim_end = fdim_list;
	fdim_end->nxt = NULL;
	fdim_end->size = 0;
	full_backup = TRUE;
	chk_offset = 0;
	chk_eop = 0;
	f_max = 0;
	f_all = 0;
	w_max = 0;
	w_all = 0;
	p_all = 0;
	f_bgn = f_end = f_cur = NULL;
	w_bgn = w_end = NULL;
	p_bgn = p_end = p_cur = NULL;
	btime = 0;
	fnds = NULL;
	wnds = NULL;
	jump_val = 1;

	setup_format();
	if ((pgm_fn = strrchr(*argv, DIR_DEL)) == NULL)
		pgm_fn = *argv;
        else
		pgm_fn++;
	/* process command line options */
 	while (--argc > 0) {
		if (**++argv == SWH_PFX) {
			for (argp = ++*argv; *argp != NULL; argp++)
				switch (*argp) {
				case 'i':
					/* always load incremental state,
					   don't compare format to load time */
					forced_incremental = TRUE;
					break;
				case 'b':
					/* batch run, overriding any other
					   options */
					incremental = FALSE;
					break;
				case 'v':
					/* designated virgin,
					   overrides -q below */
					virgin = TRUE;
					break;
				case 'c':
					/* compression required */
					do_compression = TRUE;
					break;
				case 'q':
					/* quiescence checkpointing
					   requested */
					qsc_check = TRUE;
					break;
				case 'n':
					/* state checkpointing turned off */
					state_checkpointing = FALSE;
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

	if (incremental) {
		if (argc == 0) {
			print_nl("Missing argument.");
			usage();
			exit(-1);
		} else if (argc > 1) {
			print_nl("Too many arguments.");
			usage();
			exit(-1);
		}
		inc_format(*argv);
	} else {
		/* normal formatting (non-incremental) */
		virgin = TRUE;
		if (argc == 0)
			virgin_run("");
		else
			virgin_run(*argv);
	}
}


static
virgin_run (fn)
	char		*fn;
{
	int		i;

	string_okay = FALSE;
	if (incremental) {
		for (i = 0; i < MAX_IN_OPEN; i++)
			rfid[i] = NIL;
		for (i = 1; i < OUT_MAX; i++)
			wfid[i] = NIL;
	}
	pre_format(fn);
	if (incremental)
		save_premac();	/* save preloaded macro token_list area */
	while (format_continue) {
/*
		if (incremental)
			save_all_aux();*//* 111 save aux font arrays for checking. DLP */
		main_control();
	}
/*
 *	gettimeofday(&time4, &tz);
 */
	if (incremental)
		wrapup_inc();
	else {
		post_format();
		close_out();	/* clean up output - DLP */
		exit(history);
	}
}


close_out ()
{
	char		tm[STR_MAX];

	gettimeofday(&time5, &tz);
	sprintf(tm, "%.3f", time_diff(time5, time0));
	print_nl("Total processing time: ");
	print(tm);
	print(" seconds.");
	print_ln();
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
	update_terminal();
	exit(history);
}


static
inc_format (fn)
	char		*fn;
{
	int		code;
	int		cid;

	test_virgin(fn);
	if (virgin) {
		print_nl("Formatting ");
		print(doc_fn);
		print(" from scratch...");
		save_zero = TRUE;
		virgin_run(fn);
	} else if (load_inc()) {
		inc_run(fn);
	} else {
		fclose(inc_fp);
		free_all();
		print_nl("Failed to recover incremental processing info.");
 		print_nl("Formatting ");
		print(doc_fn);
		print(" from scratch...");
		save_zero = TRUE;
		virgin = TRUE;
		virgin_run(fn);
	}
}


static int
test_virgin (fn)
	char		*fn;
{
	char		*np;
	char		*bp;

	strcpy(name_of_file, fn);
	if (! test_access(READ_ACCESS, INPUT_FILE_PATH)) {
		strcpy(name_of_file, fn);
		strcat(name_of_file, ".tex");
		if (! test_access(READ_ACCESS, INPUT_FILE_PATH)) {
			print_nl("Failed to open document root file ");
			print(name_of_file);
			print_ln();
			close_out();
		}
	}
	strcpy(doc_fn, name_of_file);
	
	/* find the rightmost DIR_DEL (`/') */
	if ((np = strrchr(fn, DIR_DEL)) == NULL)
		np = fn;
	else
		np++;

	/* find the leftmost EXT_DEL (`.') */
	if ((bp = strchr(np, EXT_DEL)) != NULL)
		*bp = NULL;
	
	strcpy(inc_fn, INC_AREA);
	strcat(inc_fn, np);
	strcat(inc_fn, EXT_DEL);
	strcat(inc_fn, EXT_INC);

	/* make INC file base name for later retrieval */
	strcpy(base_fn, INC_AREA);
	if ((bp = strchr(name_of_file, DIR_DEL)) != NULL)
		bp++;
	else
		bp = name_of_file;
	strcat(base_fn, bp);
	bp = base_fn + strlen(base_fn);
	while(*bp != '.')
		bp--;
	*bp = NULL;
	
	if (virgin)
		return;
	
	if (access(inc_fn, R_OK) == -1) {
		/* Access failed, virgin indeed */
		print_nl("Incremental processing info not found.");
		virgin = TRUE;
	} else
		virgin = FALSE;
}


#define scan_inc(F, V) { \
	if (fscanf(inc_fp, F, V) == EOF) \
		return(FALSE); \
}



static int
load_inc ()
{
	print_nl("Recovering incremental processing info from ");
	print(inc_fn);
	print("...");
	update_terminal();
	if ((inc_fp = fopen(inc_fn, "r")) == NULL)
		return(FALSE);
	
	if (! load_fnodes())
		return(FALSE);
	if (! load_pnodes())
		return(FALSE);
	if (! load_wnodes())
		return(FALSE);

	fclose(inc_fp);	
	print("done");
	update_terminal();
	return(TRUE);
}

load_fnodes ()
{
	F_NODE		*fnd;
	int		i;
	int		parent;
	struct stat	ts;

	/* Recovering file chain */
	scan_inc("%d", &f_all);
	CALLOC(fnds, F_NODE*, f_all, sizeof(F_NODE*));
	for (i = 0; i < f_all; i++) {
		MALLOC(fnd, F_NODE, sizeof(F_NODE));
		fnds[i] = fnd; 

		scan_inc("%d", &(fnd->id));
		scan_inc("%d", &(fnd->sl));
		CALLOC(fnd->sn, char, fnd->sl, sizeof(char));
		scan_inc("%s", fnd->sn);

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
}


load_pnodes ()
{
	P_NODE		*pnd;
	int		parent;
	int		i;
	int		j;

	/* Recovering page chain */
	scan_inc("%d", &p_all);
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
			print_nl("Page numbers corrupted in ");
			print(inc_fn);
			print_nl("I expected page ");
			print_int(pnd->no);
			print(" to be ");
			print_int(i);
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
}


load_wnodes ()
{
	W_NODE		*wnd;
	int		i;
	struct stat	ts;

	scan_inc("%d", &w_all);
	if (w_all > 0) {
		CALLOC(wnds, W_NODE*, w_all, sizeof(W_NODE*));
		for (i = 0; i < w_all; i++) {
			MALLOC(wnd, W_NODE, sizeof(W_NODE));
			wnds[i] = wnd; 
	
			scan_inc("%d", &(wnd->id));
			scan_inc("%d", &(wnd->wl));
			CALLOC(wnd->wn, char, wnd->wl, sizeof(char));
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
}



/*
 * quiescent = QSC initially (QSC consecutive identical pages yield quiescence)
 */
static
inc_run (fn)
	char		*fn;
{
	int		i;

	string_okay = TRUE;
	f_cur = f_bgn;
	f_end = NULL;
	f_max = 0;
	w_max = 0;
	p_end = p_cur = NULL;
	w_end = NULL;
	w_max = 0;
	for (i = 0; i < MAX_IN_OPEN; i++)
		rfid[i] = NIL;
	for (i = 1; i < OUT_MAX; i++) {
		wfid[i] = NIL;
		owfid[i] = NIL;
		oweop[i] = NIL;
	}
	init_format(fn);
	while (format_continue) {
		gettimeofday(&time1, &tz);

		if (quiescent == qsc_count + 1) {
			skip_pages();

			/* no formatting necessary */
			if (all_clean) {
				print_nl("Source files intact (potential ");
				print("convergence detected).");
				print_nl("No formatting necessary.");
				post_format();
				close_out();
			}

			if (!from_beginning && (p_cur == NULL) && (p_max == 0)) {
				print_nl("Source files touched but no ");
				print("changes in document.");
				
				print_nl("(Potential convergence detected) ");
				print("No formatting necessary.");
				f_max = f_all;
				p_max = p_all;
				w_max = w_all;
				full_backup = FALSE;
				goto end;
			}

			if (!from_beginning && (p_cur == NULL)) {
				for (i = 1; i < OUT_MAX; i++) {
					owfid[i] = wfid[i];
					wfid[i] = NIL;
				}
				append_aux();
				f_max = f_all;
				w_max = w_all;
				cur_level = LEVEL_ONE;
				cond_ptr = NUL;

				if (p_max < p_all) {
					print_nl("Quiescence detected at page ");
					print_int(p_max);
					print(", no more formatting necessary.");
					p_max = p_all;
				}
				goto end;
			}
			
			if (from_beginning)  {
				print_nl("New source file(s) detected, formatting ");
				print(doc_fn);
				print(" from scratch...");
				print_ln();
				update_terminal();
				from_beginning = FALSE;
				cont_input();
			} else
				suspend_format();
			quiescent = 0;
		}
/*
		save_all_aux();*//* 111 save state of aux font arrays for checking. DLP */
		/* format one dirty page */
		main_control();

		if (format_continue) {
			p_end->ftime = time_diff(time3, time2);
			gettimeofday(&time4, &tz);
			p_end->stime = time_diff(time4, time3);
			if (p_end->ltime == 0.0)
				p_end->ltime = p_end->stime;
	
			if (qsc_check) {
				if (diff_target())
					++quiescent;
				else
					quiescent = 0;
				gettimeofday(&time5, &tz);
				p_end->qtime = time_diff(time5, time4);
			} else
				quiescent = 0;
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
	if (qsc_check) {
		qsc_check = FALSE;
		print_nl("Encountering new ");
		if (read_f)
			print("input");
		else
			print("output");
		print(" file ");
		print(fn);
		print(", quiescence checking turned off.");
		print_ln();
		update_terminal();
	}
}



/*
 * Return the time spent in skipping these pages.
 */
static int
skip_pages ()
{
	F_NODE		*cfd;
	P_NODE		*cpd;
	int		code;
	long		cid;

	p_cur = cpd = NULL;
	for (cfd = f_cur; cfd != NULL; cfd = cfd->nxt) {
		chk_offset = 0;

		if (cfd->mod == SRC_NIL)
			cfd->mod = check_status(cfd);

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
			if (chk_offset != 0) {
				update_fnodes(cfd);
				update_pnodes(cfd, cpd);
			}
			break;
		default:
			print_nl("Unknown return code");
			print_int(code);
			print("from check_status()...ignored");
			all_clean = FALSE;
			if (cfd->up == NULL)
				continue;
			else
				cpd = find_page(cfd->up, cfd->up->pbl);
			break;
		}
		
		if ((p_cur == NULL) ||
		    ((cpd != NULL) && (cpd->no < p_cur->no))) {
			p_cur = cpd;
			if (cpd->fp->id == cfd->id)
				qsc_count = QSC - 1;
			else
				qsc_count = QSC;
		}
	}
}


static int
check_status (cfd)
	F_NODE		*cfd;
{
	struct stat	ts;

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
			/* with write permission, new file, e.g. foo.aux */
			return(SRC_NOBACKUP);
		}
	else {
		/* Compare timestamps */
		stat(cfd->bn, &ts);
		if (cfd->mt > ts.st_mtime)
			return(SRC_DIRTY);
		else
			return(SRC_CLEAN);
	}
}


static
get_src_backup (fnd)
	F_NODE		*fnd;
{
	char		*np;

	/* Get corresponding backup filename */
	if ((np = strrchr(fnd->sn, DIR_DEL)) == NULL)
		np = fnd->sn;
	else
		np++;
	fnd->bl = strlen(np)+strlen(INC_AREA);
	CALLOC(fnd->bn, char, fnd->bl, sizeof(char));
	strcpy(fnd->bn, INC_AREA);
	strcat(fnd->bn, np);
}


static
get_aux_backup (wnd)
	W_NODE		*wnd;
{
	char		*np;

	/* Get corresponding backup filename */
	if ((np = strrchr(wnd->wn, DIR_DEL)) == NULL)
		np = wnd->wn;
	else
		np++;
	wnd->bl = strlen(np)+strlen(INC_AREA);
	CALLOC(wnd->bn, char, wnd->bl, sizeof(char));
	strcpy(wnd->bn, INC_AREA);
	strcat(wnd->bn, np);
}


static P_NODE *
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


/*
 * Diff the two source files: original in cfd->sn, backup in cfd->bn.
 * Both files must exist at this point.
 * The two files are first positioned to p_end->eop and chk_eop, respectively.
 * Return the first char in which they differ.
 * Return -1 if identical.
 */
static int
diff_source (cfd)
	F_NODE		*cfd;
{
	unsigned long	id;
	int		pos;
	char		c1;
	char		c2;

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
		print_nl("Failed to open file ");
		print(cfd->sn);
		print(" for reading.");
		print_ln();
		return(0);
	}
	if (fseek(cfd->sp, pos, SEEK_SET) != 0) {
		print_nl("Failed to find position ");
		print_int(pos);
		print(" on file ");
		print(cfd->sn);
		close_file(cfd->sp);
		return(0);
	}

	if ((cfd->bp = fopen(cfd->bn, "r")) == NULL) {
		print_nl("Failed to open file ");
		print(cfd->bn);
		print(" for reading.");
		print_ln();
		return(0);
	}
	if (fseek(cfd->bp, chk_eop, SEEK_SET) != 0) {
		print_nl("Failed to find position ");
		print_int(chk_eop);
		print(" on file ");
		print(cfd->bn);
		close_file(cfd->sp);
		close_file(cfd->bp);
		return(0);
	}

	pos = chk_eop;
	while (TRUE) {
		c1 = getc(cfd->sp);
		c2 = getc(cfd->bp);
/*
*		if ((c2 == EOF) && !cfd->eof) {
*			cfd->eof = TRUE;
*		}
*/
		if ((c1 == EOF) && (c2 == EOF)) {	/* files identical */
			pos = -1;
			break;
		}
		if ((c1 != c2) || (c1 == EOF) || (c2 == EOF))
			break;
		++pos;
	}
	/* 
	   Ideally we should close the file here, but this would
	   cause the same file pointer to be assigned to the
	   next open file, which causes strange EOF problem
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


static int
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


static int
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



suspend_format ()
{
	int		boc;
	int		eoc;
	int		i;
	int		toggled;
	int		t_tmp,
			f_tmp,		/* fix for log/terminal position - DLP */
			select_old,
			select_tmp;	/* save current output redirection */

	boc = (p_end == NULL) ? 1 : p_end->no + 1;
	eoc = p_cur->no - qsc_count;

	if (check_state(boc, eoc)) {
		gettimeofday(&time2, &tz);
		for (i = 1; i < OUT_MAX; i++)
			owfid[i] = wfid[i];
		select_old = selector;	/* save curr selector setting - DLP */
		if ((time_diff(time2, time1) + p_cur->ltime < format_time()) &&
		    load_state(p_cur->no) && restore_file()) {
			select_tmp = selector;	/* save restored state's */
			selector = select_old;	/* output settings - DLP */
			t_tmp = term_offset;
			f_tmp = file_offset;
			/* not worth saving where msgs by load_state (and
			   restore_file) should have left offsets, main
			   thing is whether offsets > 0 */
			term_offset = file_offset = 50;
			print_nl("Formatting suspended between page ");
			print_int(boc);
			print(" and page ");
			print_int(p_cur->no);
			print_char('.');
			print_ln();
			update_terminal();
			append_aux();
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
		return;
	
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

	/* format clean pages */
	if (state_checkpointing) {
		state_checkpointing = TRUE;	/* Force this TRUE for now - DLP */
		toggled = TRUE;
	} else
		toggled = FALSE;
	for (boc; boc <= eoc; boc++)
		main_control();
	if (toggled)
		state_checkpointing = TRUE;
}


static int
check_state (boc, eoc)
	int		boc;
	int		eoc;
{
	int		i;
	
	for (i = eoc; i >= boc; i--) {
		uncompress_file(i);
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


static float
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


static int
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
				fseek(fnd->up->sp, fnd->pel, SEEK_SET);
				fnd->up->cnt = fnd->pel;
			}
			input_file[i] = fnd->sp;
		}
	}
	f_cur->cnt = chk_cid + chk_offset;
	fseek(f_cur->sp, f_cur->cnt, SEEK_SET);

	/* Restore file position on write files */
	for (i = 1; i < OUT_MAX; i++) {
		if ((wfid[i] != NIL) && (wnds != NULL) &&
		    ((wnd = wnds[wfid[i]]) != NULL)) {
			if (file_closed_p(wnd->wp))
				wnd->wp = fopen(wnd->wn, "r+");
				/* open without truncating file */
			write_file[i] = wnd->wp;
			write_open[i] = TRUE;
/*
 *  aux file write head should be right in the position for the next
 *  next block of output, so no seek is necessary here.
			if ((p_end->weop[i] != NIL) && (write_file[i] != NULL))
				fseek(write_file[i], p_end->weop[i], SEEK_SET);
*/
		} else {
			write_file[i] = NULL;
			write_open[i] = FALSE;
		}
	}

	return(TRUE);
}	



/*
 * Compare the same dvi page produced by current and previous cycles.
 * The current page is pointed to by dvi_file, previous page by dvi_fnd.
 * Also, if the dvi file is newer than the corresponding backup source file,
 * it means something went wrong and therforecompare returns false.
 */
static int
diff_target()
{
	short		s1;
	short		s2;
	long		l1;
	long		l2;
	long		lm;
	int		i = 0;

	if ((tgt_fp = fopen(INC_DVI, "r")) == NULL) {
		print_nl("Failed to open backup DVI file ");
		print(INC_DVI);
		return(FALSE);
	}
	get_ext(total_pages, EXT_DVI);
	dvi_file = b_open_in();
	fseek(dvi_file, 0L, SEEK_END);
	l1 = ftell(dvi_file);
	fseek(tgt_fp, 0L, SEEK_END);
	l2 = ftell(tgt_fp);
	lm = (l1 < l2) ? l1 : l2;
	fseek(dvi_file, 0L, SEEK_SET);
	fseek(tgt_fp, 0L, SEEK_SET);
	for (i = 0; i < lm; i++) {
		s1 = DVI_MASK & getc(dvi_file);
		s2 = DVI_MASK & getc(tgt_fp);
		if (s1 != s2)
			goto false;
	}

	if (l1 == l2)
		goto true;

	if (l1 < l2) {
		for (i = lm; i < l2; i++)
			if ((s2 = DVI_MASK & getc(tgt_fp)) != DVI_PAD)
				goto false;
		goto true;
	}

	/* l1 > l2 */
	for (i = lm; i < l1; i++)
		if ((s1 = DVI_MASK & getc(dvi_file)) != DVI_PAD)
			goto false;
	goto true;

true:
	b_close(dvi_file);
	fclose(tgt_fp);
	return(TRUE);

false:
	b_close(dvi_file);
	fclose(tgt_fp);
	return(FALSE);
}

static
free_all ()
{
	cfree(fnds);
	free_fnodes(f_bgn);
	f_all = f_max = 0;
	f_bgn = f_end = f_cur = NULL;

	p_all = 0;
	p_bgn = p_end = NULL;
	free_pnodes(p_bgn);

	cfree(wnds);
	w_all = w_max = 0;
	w_bgn = w_end = NULL;
	free_wnodes(w_bgn);
}


free_fnodes (fnd)
	F_NODE		*fnd;
{
	F_NODE		*gt;

 	while (fnd != NULL) {
		gt = fnd;
		fnd = fnd->nxt;
		cfree(gt->sn);
		cfree(gt->bn);
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
		cfree(gt->wn);
		free(gt);
	}
}


int
wrapup_inc ()
{
	int i;
	struct stat *buf;

	post_format();
	if (full_backup)
		save_string();
	if (! state_checkpointing) {
		print_nl("No state checkpointed in this processing cycle.");
		print_nl("Old state files should be removed for ");
		print("consistency concerns.");
		print_ln();
	}
	save_inc();
	unlink(INC_DVI);
	if (total_pages<last_max_pages-1){	/* kill xtra chkpts. DLP */
		print_nl("Deleting checkpoints for pages ");
		print_int(total_pages+1);
		print(" - ");
		print_int(last_max_pages);
	}
	for ( ;++total_pages<=last_max_pages; ) {
		get_ext(total_pages, EXT_STC);
		if (stat(name_of_file, buf) < 0) unlink(name_of_file, buf);
	}
	close_out();
}



static int
save_inc ()
{
	int		i;
	int		t;
	char		*wb;
	FILE		*wps;
	FILE		*wpb;

	print_nl("Backing up incremental processing info...");
	update_terminal();

	save_good = TRUE;
	if (full_backup && ((inc_fp = fopen(inc_fn, "w")) == NULL) ) {
		print_nl("Failed to open inc file ");
		print(inc_fn);
		print_ln();
		return(FALSE);
	}

	save_fnodes();
	save_pnodes();
	save_wnodes();

	if (save_good) {
		print("done");
		print_ln();
	}
	if (divergent) {
		print_nl("New auxiliary files created ");
		print("(potential divergence detected).");
		print_nl("Auxiliary processing and further ");
		print("reformatting may be necessary.");
		print_ln();
	}
	if (full_backup)
		fclose(inc_fp);
	return(TRUE);
}

save_fnodes ()
{
	struct stat	ts;
	int		i;

	/* Save read file chain info */ 
	if (full_backup)
		fprintf(inc_fp, "%d\n", f_max);
	for (i = 0, f_end = f_bgn; ((i < f_max) && (f_end != NULL));
	     i++, f_end = f_end->nxt) {
		if (full_backup) {
			/* length & filename */
			fprintf(inc_fp, "%d    %d    %s\n",
				f_end->id, f_end->sl, f_end->sn);
	
			/* parent file id */
			if (f_end->up == NULL) {
				/* root file */
				fprintf(inc_fp, "%d    %d    %d\n",
					f_end->id, 0L, 0L);
			} else {
				/* non-root file */
				fprintf(inc_fp, "%d    %d    %d\n",
					f_end->up->id, f_end->pbl, f_end->pel);
			}
		}
		if ((f_end->mod != SRC_CLEAN) && (! f_end->eof)) {
			stat(f_end->sn, &ts);
			if (ts.st_mtime < write_time)
				backup_source(f_end);
		}
	}
	if (full_backup)
		fprintf(inc_fp, "%d\n", EOB);
}

save_pnodes ()
{
	int		i;
	int		j;

	if (full_backup) {
		/* Save page chain info */
		fprintf(inc_fp, "\n%d\n", p_all);
		for (i = 0, p_end = p_bgn; ((i < p_max) && (p_end != NULL));
		     i++, p_end = p_end->nxt) {
			if (p_end->fp == NULL) {
				if (save_good) {
					save_good = FALSE;
					print_ln();
				}
				print_nl("Page ");
				print_int(p_end->no);
				print(" pointing to null file...abort");
				fclose(inc_fp);
				unlink(inc_fn);
				close_out();
			}
			fprintf(inc_fp, "%d   %d   %d   %.3f   %.3f   %.3f   %.3f   %.3f\n",
				p_end->no, p_end->fp->id, p_end->eop, p_end->ftime,
				p_end->btime, p_end->stime, p_end->ltime, p_end->qtime);
			for (j = 1; j < OUT_MAX; j++)
				fprintf(inc_fp, "%d  ", p_end->weop[j]);
			fprintf(inc_fp, "\n");
		}
		fprintf(inc_fp, "%d\n", EOB);
	}
}

save_wnodes ()
{
	int		i;

	if (full_backup) {
		/* Save write file chain */
		fprintf(inc_fp, "\n%d\n", w_max);
	}
	for (i = 0, w_end = w_bgn; ((i < w_max) && (w_end != NULL));
	     i++, w_end = w_end->nxt) {
		if (full_backup) {
			/* length & filename */
			fprintf(inc_fp, "%d    %d    %s\n",
				w_end->id, w_end->wl, w_end->wn);
		}
		if (w_end->bn == NULL)
			get_aux_backup(w_end);
		else
			fclose(w_end->bp);
		if ((access(w_end->bn, F_OK) != 0) &&
		    (w_end->ct < write_time) &&
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
	if (full_backup)
		fprintf(inc_fp, "%d\n", EOB);
}


diff_aux ()
{
	char		c1;
	char		c2;

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


/*
 * Backup source file in the ./INC diectory.
 * No backup if the source is read-only.
 */
int
backup_source (fnd)
	F_NODE		*fnd;
{
	struct timeval	t0;
	struct timeval	t1;

	gettimeofday(&t0, &tz);

	if (virgin && (access(fnd->sn, W_OK) == -1)) {
		/* Read-only, no need to backup */
		return(TRUE);
	}

	if (access(INC_DIR, F_OK) < 0)
		mkdir(INC_DIR, 0775);

	if (fnd->bn == NULL)
		get_src_backup(fnd);

	if ((fnd->bp = fopen(fnd->bn, "w")) == NULL) {
		if (save_good) {
			save_good = FALSE;
			print_ln();
		}
		print_nl("Source backup failed to write on file ");
		print(fnd->bn);
		print_ln();
		return(FALSE);
	}

	if ((fnd->sp = fopen(fnd->sn, "r")) == NULL) {
		print_nl("Failed to open file ");
		print(fnd->sn);
		print(" as source of backup.");
		print_ln();
		close_file(fnd->bp);
		return(FALSE);
	}
	fseek(fnd->sp, 0L, SEEK_END);
	if (! copy_file(ftell(fnd->sp), 0,
			fnd->sn, fnd->sp, fnd->bn, fnd->bp)) {
		close_file(fnd->sp);
		close_file(fnd->bp);
		return(FALSE);
	}

	close_file(fnd->sp);
	close_file(fnd->bp);
	gettimeofday(&t1, &tz);
	btime += time_diff(t1, t0);
	return(TRUE);
}


copy_file (cnt, pos, sn, sp, bn, bp)
	char		*sn;
	FILE		*sp;
	char		*bn;
	FILE		*bp;
{
	char		*buf;
	int		ncnt;

	if (fseek(sp, pos, SEEK_SET) != 0)
		return(FALSE);

	CALLOC(buf, char, cnt, sizeof(char));
	if ((ncnt = fread(buf, 1, cnt, sp)) != cnt) {
		if (save_good) {
			save_good = FALSE;
			print_ln();
		}
		print_nl("Backup failed while reading file ");
		print(sn);
		print_ln();
		return(FALSE);
	}

	if ((ncnt = fwrite(buf, 1, cnt, bp)) != cnt) {
		if (save_good) {
			save_good = FALSE;
			print_ln();
		}
		print_nl("Backup failed while writing file ");
		print(bn);
		print_ln();
		return(FALSE);
	}
	cfree(buf);
}


/*
 * Current clean pocket goes from p_end->no + 1 to p_cur->no, inclusive.
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

append_aux ()
{
	int		i;
	W_NODE		*wnd;
	char		*buf;
	int		eop;
	struct stat	ts;

	for (i = 1; i < OUT_MAX; i++) {
		if (owfid[i] == wfid[i]) {
			if ((oweop[i] != NIL) && (p_cur != NULL) &&
			    (p_cur->weop[i] != NIL) &&
			    (oweop[i] < p_cur->weop[i]) &&
			    ((wnd = wnds[wfid[i]]) != NULL))
				copy_aux(wnd, oweop[i], p_cur->weop[i]);
		} else {
			if ((oweop[i] != NIL) &&
			    ((wnd = wnds[owfid[i]]) != NULL))
				copy_aux(wnd, oweop[i], -1);
			if ((p_cur != NULL) && (p_cur->weop[i] != NIL) &&
			    ((wnd = wnds[wfid[i]]) != NULL))
				copy_aux(wnd, 0, p_cur->weop[i]);
		}
/*
 *		if (write_time == INFINITY) {
 *			stat(wnd->wn, &ts);
 *			write_time = ts.st_mtime;
 *		}
 */
	}
}

copy_aux (wnd, bop, eop)
	W_NODE		*wnd;
{
	if (file_closed_p(wnd->wp))
		wnd->wp = fopen(wnd->wn, "r+");	/* don't truncate file - DLP */
	if (wnd->bn == NULL)
		get_aux_backup(wnd);
	if (file_closed_p(wnd->bp))
		wnd->bp = fopen(wnd->bn, "r");
	if (wnd->bp == NULL) {
		/* no backup in INC directory, must be write-only file;
		   just reposition in the file - DLP*/
		if (eop == -1)
			fseek(wnd->wp, 0L, SEEK_END);
		else 	
			fseek(wnd->wp, eop, SEEK_SET);
		return;
	}
	if (eop == -1) {
		fseek(wnd->bp, 0L, SEEK_END);
		eop = ftell(wnd->bp);
	}
	copy_file(eop - bop, bop, wnd->bn, wnd->bp, wnd->wn, wnd->wp);
}

/*
*copy_aux (wnd, bop, eop)
*	W_NODE		*wnd;
*{
*	if ((wnd->wp = fopen(wnd->wn, "w")) == NULL) {
*		print_nl("Failed to open file ");
*		print(wnd->wn);
*		print(" for writing.");
*		print_ln();
*		return(FALSE);
*	}
*		
*	if (wnd->bn == NULL)
*		get_aux_backup(wnd);
*
*	if ((wnd->bp = fopen(wnd->bn, "w")) == NULL) {
*		print_nl("Failed to open file ");
*		print(wnd->bn);
*		print(" for reading.");
*		print_ln();
*		return(FALSE);
*	}
*	if (eop == -1) {
*		fseek(wnd->bp, 0L, SEEK_END);
*		eop = ftell(wnd->bp);
*	}
*	copy_file(eop - bop, bop, wnd->bn, wnd->bp, wnd->wn, wnd->wp);
*}
*/


#endif




