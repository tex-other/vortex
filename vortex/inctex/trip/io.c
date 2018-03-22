/*
 *
 * @(#)io.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 */

/*
 *
 *  This file has been modified, with permission from Pat Monardo, for
 *
 *  IncTeX  --	The Olivetti-Berkeley-Matsushita Incremental TeX.
 *
 *  An Editor-Independent, Adaptive, Incremental TeX Formatter.
 *
 *  Copyright (C) 1988 by Olivetti Research Center
 *
 *  Author:
 *  	Pehong Chen
 *	Olivetti Research Center
 *	Menlo Park, California
 *	USA
 *	(chen@orc.olivetti.com)
 *
 *  All rights reserved by the copyright holders.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

#include	"tex.h"
#include	"tokenstack.h"
#include	"file.h"

#ifdef INCTEX

#include	<sys/types.h>
#include	<sys/file.h>
#include	<sys/stat.h>
#include	"Imain.h"
#include	"scan.h"

extern	int     last;
extern	ascii   buffer[BUF_SIZE];
extern	int     first;
extern	int     max_buf_stack;

#else

int		last;
ascii		buffer[BUF_SIZE];
int		first;
int		max_buf_stack;

#endif

#ifdef INCTEX

/* 
 *  One fid per read file.  Id never recycled.
 */

update_fnds (fnd)
	F_NODE		*fnd;
{
	F_NODE		**foo;
	int		i;

	CALLOC(foo, F_NODE*, f_max, sizeof(F_NODE*));
	for (i = 0; i < f_max - 1; i++)
		foo[i] = fnds[i];
	cfree(fnds);
	fnds = foo;
	fnds[f_max - 1] = fnd;
	f_all = f_max + 1;
}
	

add_fnode()
{
	F_NODE		*fnd;

	MALLOC(fnd, F_NODE, sizeof(F_NODE));
	fnd->sl = strlen(name_of_file);
	CALLOC(fnd->sn, char, fnd->sl, sizeof(char));
	strcpy(fnd->sn, name_of_file);
	fnd->sp = fopen(name_of_file, "r");
	fnd->bl = 0;
	fnd->bn = NULL;
	fnd->bp = NULL;
	fnd->eof = FALSE;
	fnd->mod = SRC_NIL;
	fnd->cnt = 0;
	fnd->cbl = 0;
	fnd->cel = 0;
	fnd->nxt = NULL;
	fnd->id = f_max++;
	if (f_end == NULL) {
		fnd->up = NULL;
		fnd->pbl = 0;
		fnd->pel = 0;
		f_bgn = f_end = f_cur = fnd;
	} else {
		f_end->nxt = fnd;
		f_end = f_cur = fnd;
		if ((in_open > 1) && (rfid[in_open - 1] != NIL)) {
			if (fnds == NULL) {
				for (fnd = f_bgn; fnd != NULL; fnd = fnd->nxt)
					if (fnd->id == rfid[in_open - 1])
						break;
			} else
				fnd = fnds[rfid[in_open - 1]];
			if (fnd != NULL) {
				f_end->pbl = fnd->cbl;
				f_end->pel = fnd->cel;
				f_end->up = fnd;
			}
		}
	}
  	if (fnds != NULL)
		update_fnds(f_end);
}


FILE *
a_open_in (from_input)
{
	F_NODE		*fnd;
	struct stat	ts;

	if (test_access(READ_ACCESS, INPUT_FILE_PATH))
		if (incremental && from_input) {
			if (virgin || ((f_end == NULL) && (f_bgn == NULL)) ||
			    ((f_end != NULL) && (f_end->nxt == NULL))) { 
				add_fnode();
			} else {
				if ((f_end == NULL) && (f_bgn != NULL))
					f_end = f_bgn;
				else
					f_end = f_end->nxt;
				if (strcmp(f_end->sn, name_of_file) == 0) {
					f_max++;
					f_end->sp = fopen(name_of_file, "r");
					f_end->eof = FALSE;
					if ((in_open > 1) &&
					    (rfid[in_open - 1] != NIL) &&
					    ((fnd = fnds[rfid[in_open - 1]])
					     != NULL)) {
						f_end->pbl = fnd->cbl;
						f_end->pel = fnd->cel;
						f_end->up = fnd;
					}
				} else {
					/* new file, abolish file chain */
					/* no more qsc check */
					handle_newfile(name_of_file, TRUE);
					free_fnodes(f_end->nxt);
					f_end->sl = strlen(name_of_file);
					CALLOC(f_end->sn, char, f_end->sl,
					       sizeof(char));
					strcpy(f_end->sn, name_of_file);
					f_end->sp = fopen(name_of_file, "r");
					f_end->id = f_max++;
					f_end->bl = 0;
					f_end->bn = NULL;
					f_end->bp = NULL;
					f_end->cnt = 0;
					f_end->cbl = 0;
					f_end->cel = 0;
					if ((in_open > 1) &&
					    (rfid[in_open - 1] != NIL) &&
					    ((fnd = fnds[rfid[in_open - 1]])
					     != NULL)) {
						f_end->pbl = fnd->cbl;
						f_end->pel = fnd->cel;
						f_end->up = fnd;
					}
					f_end->nxt = NULL;
					f_end->eof = FALSE;
					f_end->mod = SRC_NIL;
					/* fnds = NULL; */
					update_fnds(f_end);
				}
				f_cur = f_end;
			}

			if (access(f_end->sn, F_OK) == 0) {
				stat(f_end->sn, &ts);
				f_end->mt = ts.st_mtime;
			} else
				f_end->mt = INFINITY;

			rfid[index] = f_cur->id;
			return(f_cur->sp);
		} else
			return(fopen(name_of_file, "r"));
	return NULL;
}


/* 
 *  One wid per write file.  Id never recycled.
 */

update_wnds (wnd)
	W_NODE		*wnd;
{
	W_NODE		**foo;
	int		i;

	CALLOC(foo, W_NODE*, w_max+1, sizeof(W_NODE*));
	for (i = 0; i < w_max; i++)
		foo[i] = wnds[i];
	cfree(wnds);
	wnds = foo;
	wnds[w_max] = wnd;
	w_all = w_max + 1;
}


add_wnode()
{
	W_NODE		*wnd;
	struct stat	ts;

	MALLOC(wnd, W_NODE, sizeof(W_NODE));
	wnd->id = w_max++;
	wnd->wl = strlen(name_of_file);
	CALLOC(wnd->wn, char, wnd->wl, sizeof(char));
	strcpy(wnd->wn, name_of_file);
	wnd->wp = fopen(name_of_file, "w");
	wnd->bn = NULL;
	wnd->bp = NULL;
	wnd->nxt = NULL;
	if (w_end == NULL) {
		w_bgn = w_end = wnd;
	} else {
		w_end->nxt = wnd;
		w_end = wnd;
	}
  	if (wnds != NULL)
		update_wnds(wnd);
}

FILE *
a_open_out (from_write)
{
	struct stat	ts;
	int		i;
	W_NODE		*wnd;

	if (test_access(WRITE_ACCESS, NO_FILE_PATH)) {
		if (incremental && from_write) {
			if (virgin || ((w_end == NULL) && (w_bgn == NULL)) ||
			    ((w_end != NULL) && (w_end->nxt == NULL))){
				add_wnode();
			} else {
				wnd = NULL;
				for (i = (w_end == NULL) ? -1 : w_end->id;
				     i < (int) w_max; i++) {
					if ((w_end == NULL) && (wnd == NULL)) {
						w_end = w_bgn;
					} else if (w_end == NULL) {
						w_end = wnd;
						break;
					} else {
						wnd = w_end;
						w_end = w_end->nxt;
					}
				}
				
				if ((i != w_max) ||
				    (strcmp(w_end->wn, name_of_file) != 0)) {
					/* new file, abolish file chain */
					/* no more qsc check */
					handle_newfile(name_of_file, FALSE);
					free_wnodes(w_end->nxt);
					w_end->wl = strlen(name_of_file);
					CALLOC(w_end->wn, char,
					       w_end->wl, sizeof(char));
					strcpy(w_end->wn, name_of_file);
					w_end->id = w_max++;
					w_end->wp = fopen(name_of_file, "w");
					w_end->bn = NULL;
					w_end->bp = NULL;
					w_end->nxt = NULL;
					/* wnds = NULL; */
					update_wnds(w_end);
				} else {
					w_max++;
					w_end->wp = fopen(name_of_file, "w");
				}
			}
			if (access(w_end->wn, F_OK) == 0) {
				stat(w_end->wn, &ts);
				w_end->ct = ts.st_ctime;
			} else
				w_end->ct = INFINITY;
			if (write_time == INFINITY)
				write_time = w_end->ct;
			return(w_end->wp);
		} else			    
			return (fopen(name_of_file, "w"));
	}
	return NULL;
}

#else

FILE *
a_open_in ()
{
    if (test_access(READ_ACCESS, INPUT_FILE_PATH))
        return (fopen(name_of_file, "r"));
    return NULL;
}

FILE *
a_open_out ()
{
    if (test_access(WRITE_ACCESS, NO_FILE_PATH))
        return (fopen(name_of_file, "w"));
    return NULL;
}

#endif


FILE *
b_open_in ()
{
    if (test_access(READ_ACCESS, FONT_FILE_PATH))
        return (fopen(name_of_file, "rb"));
    return NULL;
}

FILE *
b_open_out ()
{
    if (test_access(WRITE_ACCESS, NO_FILE_PATH))
        return (fopen(name_of_file, "wb"));
    return NULL;
}

FILE *
w_open_in ()
{
    if (test_access(READ_ACCESS, FORMAT_FILE_PATH))
        return (fopen(name_of_file, "rb"));
    return NULL;
}

FILE *
w_open_out ()
{
    if (test_access(WRITE_ACCESS, NO_FILE_PATH))
        return (fopen(name_of_file, "wb"));
    return NULL;
}


#ifdef INCTEX

bool 
input_ln (f, from_file)
	alpha_file	f;
	bool		from_file;
{
	int		c;
	struct stat	ts;

	last = first;
	if (f_cur != NULL)
		f_cur->cbl = f_cur->cnt; /* beginning of current input line */
	loop {
		c = getc(f);
		if ((f_cur != NULL) && from_file)
			++(f_cur->cnt);
		if (c == EOLN)
			break;
		if (c == EOF) {
			if (f_cur != NULL) {
				f_cur->eof = TRUE;
				if (f_cur->mod != SRC_CLEAN) {
					stat(f_cur->sn, &ts);
/*
				    	if (ts.st_mtime < write_time)
				    	if (ts.st_ctime <= write_time)
*/
						backup_source(f_cur);
				}
			}
			if (last == first)
				return FALSE;
			else
				break;
		}
		if (last > max_buf_stack) {
			max_buf_stack = last + 1;
			if (max_buf_stack == BUF_SIZE - 1)
				overflow("buffer size", BUF_SIZE);
		}
		buffer[last] = c;
		incr(last);
	}

	loop {
		if (last == first)
			break;  
		else if (buffer[last - 1] != ' ')
			break;
		else
			decr(last);
	}
	if (f_cur != NULL) {
		f_cur->cel = f_cur->cnt; /* end of current input line */
	}
	return TRUE;
}

#else

bool 
input_ln (f, bypass_eoln)
    alpha_file  f;
    bool        bypass_eoln;
{
    int         c;

    last = first;
    loop {
        c = getc(f);
        if (c == EOLN)
            break;
        if (c == EOF) {
            if (last == first)
                return FALSE;
            else
                break;
        }
        if (last > max_buf_stack) {
            max_buf_stack = last + 1;
            if (max_buf_stack == BUF_SIZE - 1)
                overflow("buffer size", BUF_SIZE);
        }
        buffer[last] = c;
        incr(last);
    }
    loop {
        if (last == first)
            break;  
        else if (buffer[last - 1] != ' ')
            break;
        else decr(last);
    }
    return TRUE;
}

#endif

term_input ()
{
    int     k;

    update_terminal();
    if (!input_ln(term_in, FALSE)) 
        fatal_error("! End of file on the terminal");
    term_offset = 0;
    decr(selector);
    if (last != first)
        for (k = first; k < last; incr(k))
            print_char(buffer[k]);
    print_ln();
    incr(selector);
}

bool
init_terminal ()
{
    loop {
        fputs("**", stdout);
        update_terminal();
        if (!input_ln(term_in, FALSE)) {
            puts("\n! End of file on the terminal...why?");
            return FALSE;
        }
        loc = first;
        while (loc < last && buffer[loc] == ' ')
            incr(loc);
        if (loc < last)
            return TRUE;
        puts("Please type the name of your input file.");
    }
}

