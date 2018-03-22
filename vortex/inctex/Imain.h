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

/* Copyright (c) 1992 Regents of the University of California
 * All rights reserved.
 */

/* ANSI stdio constants */
#undef	SEEK_SET
#define	SEEK_SET	0
#undef	SEEK_CUR
#define	SEEK_CUR	1
#undef	SEEK_END
#define	SEEK_END	2

/* Global functions */

char			*strchr();
char			*strrchr();
char			*alloca();
char			*ecalloc();

/* Macros for dynamic memory management */

#define CALLOC(PTR, TYPE, N, SIZE) { \
	if ((PTR = (TYPE *) calloc(N, (SIZE))) == NULL) { \
		fprintf(stderr, "Not enough core - calloc."); \
		exit(-1); \
	} \
}

#define MALLOC(PTR, TYPE, SIZE) { \
	if ((PTR = (TYPE *) malloc(SIZE)) == NULL) { \
		fprintf(stderr, "Not enough core - malloc."); \
		exit(-1); \
	} \
}

/* definition for MIPS mammoth machine, doesn't sign extend char's. */
#define char2int(c)     ((c) & 0x80 ? 0xff - (c) : (c))

#define	LOAD		0
#define	SAVE		1

#define time_diff(A, B) \
	((A.tv_sec - B.tv_sec) + ((A.tv_usec - B.tv_usec) / 1000000.0))

typedef struct wnode {
	unsigned long	wl;		/* length of write filename */
	char		*wn;		/* write file name */
	FILE		*wp;		/* write file pointerr */
	unsigned long	bl;		/* length of backup write filename */
	char		*bn;		/* write backup file name */
	FILE		*bp;		/* write backup file pointer */
	unsigned long	id;		/* write file id */
	long		ct;		/* file creation time */
	struct wnode	*nxt;
} W_NODE;
#define DIFF_HUNK_SIZE 1012
typedef	int	*intptr;
typedef	short	shortptr;

struct	diff_hunk {		/* change list, broken into chunks */
	int		size;	/* amount used so far. DLP */
	shortptr	listchunk[DIFF_HUNK_SIZE];	/* chunk of ptr list */
	struct diff_hunk	*nxt;	/* next chunk */
	struct diff_hunk	*prev;	/* previous chunk */
};
struct	diff_chunk {		/* change list with big addr range */
	int		size;
	intptr		listchunk[DIFF_HUNK_SIZE];
	struct diff_chunk	*nxt;
	struct diff_chunk	*prev;
};
typedef struct diff_hunk  DIFF_HUNK;
typedef struct diff_chunk DIFF_CHUNK;

#define GETDIFFHUNK(XPTR)   MALLOC(XPTR,DIFF_HUNK,sizeof(DIFF_HUNK))
#define GETDIFFCHUNK(XPTR)  MALLOC(XPTR,DIFF_CHUNK,sizeof(DIFF_CHUNK))

extern	DIFF_CHUNK	*fdim_list;	/* ptr to changed font entry list */
extern	DIFF_CHUNK	*fdim_end;	/* ptr to last chunk in list */
extern	int		fdim_listlen;
extern	DIFF_HUNK	*eqtb_list;	/* ... & eqtb locs changed */
extern	DIFF_HUNK	*eqtb_end;
extern	int		eqtb_listlen;
extern	DIFF_HUNK	*hash_list;	/* ... & hash locs changed */
extern	DIFF_HUNK	*hash_end;
extern	int		hash_listlen;
extern	DIFF_HUNK	*xeq_list;	/* ... & xeq_level locs changed */
extern	DIFF_HUNK	*xeq_end;
extern	int		xeq_listlen;

		/* code flagging end of difference list */
#define END_MARKER	NULL

/* Global constants */

#define	p_max			total_pages
#define	usage() { \
	print_nl("usage: "); \
	print(pgm_fn); \
	print(" [ -bvncqs ] [-j J ] [-d D ] [ file ]"); \
	print_ln(); \
}

#define	EOB			-2
#define	NIL			-1
#define	SRC_NIL			0
#define	SRC_CLEAN		1
#define	SRC_SAME		2
#define	SRC_DIRTY		3
#define	SRC_MISSING		4
#define	SRC_NOBACKUP		5

#define	DVI_PAD			223
#define	DVI_MASK		0x00ff
#define	SWH_PFX			'-'
#define	DIR_DEL_CHR		'/'
#define	DIR_DEL			"/"
#define	EXT_DEL_CHR		'.'
#define	EXT_DEL			"."
#define	EXT_MAX			16
#define	EXT_DVI			"dvi"
#define	EXT_INC			"inc"
#define	EXT_NEWPAGES		"newpages"
#define	EXT_STC			"stc"
#define	EXT_TMPSTC		"stc_i"
#define	EXT_STR			"str"
#define	EXT_FONT		"font"
#define	EXT_FONT_TMP		"font_i"
#define	STR_MAX			128
#define INC_AREA		"./INC/"
#define INC_DIR			"./INC"
#define INC_DVI			"./INC/tmp.dvi"

/* External variables */

extern	int		incremental;
extern	int		do_compression;
extern	int		virgin;
extern	int		save_zero;
extern	int		quiescent;
extern	int		qsc_check;
extern	int		page_done;
extern	int		page_shipped;
extern	int		format_continue;
extern	int		state_checkpointing;
extern	int		show_state;
extern	str		str_inc;
extern	str     	str_ptr_high;	/* hi water mark for str_ptr */
extern	ptr     	pool_ptr_high;	/* ...& pool_ptr; from strings file */
extern	long		write_time;
extern	ptr		fmem_lowater;	/* value of fmem_ptr in last chkpt */
extern	fnt		font_lowater;	/* ..and of font_ptr */
extern	int		last_max_pages; /* lets us know if # pages shrinks */
extern	int		initial_fmt;	/* TRUE if loading 1st fmt file */
extern	int		last_max_pages;
extern	unsigned long	chk_eop;
extern	unsigned long	chk_fid;
extern	unsigned long	chk_cid;
extern	char		inc_fn[];
extern	char		base_fn[];
extern	int		rfid[];
extern	int		wfid[];
extern	int		owfid[];
extern	int		oweop[];
extern	F_NODE		*f_bgn;
extern	F_NODE		*f_end;
extern	F_NODE		*f_cur;
extern	P_NODE		*p_bgn;
extern	P_NODE		*p_end;
extern	W_NODE		*w_bgn;
extern	W_NODE		*w_end;
extern	F_NODE		**fnds;
extern	W_NODE		**wnds;
extern	unsigned long	f_max;
extern	unsigned long	f_all;
extern	unsigned long	p_all;
extern	unsigned long	w_all;
extern	unsigned long	w_max;
extern	int		jump_val;
extern	struct timeval	time0;
extern	struct timeval	time1;
extern	struct timeval	time2;
extern	struct timeval	time3;
extern	struct timeval	time4;
extern	struct timeval	time5;
extern	struct timeval	wrapup;
extern	struct timeval	tsync;
extern	struct timezone tz;
extern	float		btime;
extern	int		oucs[16];
extern	int		string_okay;
extern	ptr		premac_hi;
extern	ptr		premac_lo;

/* definitions for logging changes to TeX array structures. DLP */

extern	int	save_font;
extern	int	save_eq;
extern	int	save_eq2;	/* sometimes have to stack 2 calls! */
extern	hh	save_hash;
extern	qword	save_xeq;
extern	int	trace_font;
extern	int	trace_eq;
extern	int	trace_hash;
extern	int	trace_xeq;

#define before_font(addr) {if (incremental) save_font = *addr;}

#define before_eq(addr)   {if (incremental) save_eq = (int) eqtb[addr].i;}

#define before_eq2(addr)  {if (incremental) save_eq2 = (int) eqtb[addr].i;}

#define before_hash(addr) {if (incremental) save_hash.hh1 = hash[addr].hh1;}

#define before_xeq(addr)  {if (incremental) save_xeq = xeq_level[addr];}


#define after_font(addr)  {if (incremental && (save_font != *addr)) \
				note_font(addr);}

#define after_eq(addr)   {if (incremental && trace_eq && \
		(save_eq != (int) eqtb[addr].i)) note_eq(addr);}

#define after_eq2(addr)  {if (incremental && trace_eq && \
		(save_eq2 != (int) eqtb[addr].i)) note_eq(addr);}

#define after_hash(addr) {if (incremental && trace_hash && \
		(save_hash.all != hash[addr].all)) note_hash(addr);}

#define after_xeq(addr)  {if (incremental && trace_xeq && \
		(save_xeq != xeq_level[addr])) note_xeq(addr);}
#endif
