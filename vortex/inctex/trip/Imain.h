#ifdef INCTEX
#ifndef _INC
#define _INC
/*
 *
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
 *  All rights reserved by the copyright holder.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
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
		fprintf(stderr, "Not enough corein calloc."); \
		exit(-1); \
	} \
}

#define MALLOC(PTR, TYPE, SIZE) { \
	if ((PTR = (TYPE *) malloc(SIZE)) == NULL) { \
		fprintf(stderr, "Not enough core malloc."); \
		exit(-1); \
	} \
}

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

		/* make it small to start with. DLP. */
#define FDIM_CHUNK_SIZE 5
typedef	int	*intptr;

struct	fdim_chunk {		/* font dimension list, broken into chunks */
	int		size;	/* used size of this array chunk */
	intptr		listchunk[FDIM_CHUNK_SIZE];	/* chunk of ptr list */
	struct fdim_chunk	*nxt;	/* next chunk */
};
typedef struct fdim_chunk FDIM_CHUNK;

#define GETFONTCHUNK(XPTR)  MALLOC(XPTR,FDIM_CHUNK,sizeof(FDIM_CHUNK))

extern	FDIM_CHUNK	*fdim_list;	/* ptr to list font_info locs changed */
extern	FDIM_CHUNK	*fdim_end;	/* ptr to last chunk in list */

		/* code flagging end of font change list */
#define END_FONT_CH_LIST NULL

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

#define	SPC			' '
#define	HTB			'\t'
#define	LFD			'\n'
#define	DVI_PAD			223
#define	DVI_MASK		0x00ff
#define	ENV_SEP			':'
#define	DIR_DEL			'/'
#define	SWH_PFX			'-'
#define	EXT_DEL			"."
#define	EXT_MAX			16
#define	EXT_DVI			"dvi"
#define	EXT_INC			"inc"
#define	EXT_STC			"stc"
#define	EXT_STR			"str"
#define	EXT_STK			"stk"
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
extern	long		write_time;
extern	ptr		fmem_lowater;	/* value of fmem_ptr in last checkpoint */
extern	fnt		font_lowater;	/* ..and of font_ptr */
extern	int		last_max_pages; /* to clean up extra chkpt files if less pages */
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
extern	struct timezone tz;
extern	float		btime;
extern	int		oucs[16];
extern	int		string_okay;
extern	ptr		premac_hi;
extern	ptr		premac_lo;

#endif
#endif
