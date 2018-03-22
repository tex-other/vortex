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
 *		global_un.c
 */

#include	 <sys/types.h>
#include	 "tex.h"
#include	 "texext.h"
#include	 "cmds.h"
#include	 "heap.h"
#include	 "char.h"
#include	 "str.h"
#include	 "eq.h"
#include	 "hash.h"
#include	 "scan.h"
#include	 "evalstack.h"
#include	 "eqstack.h"
#include	 "tokenstack.h"
#include	 "token.h"
#include	 "box.h"
#include	 "pack.h"
#include	 "math.h"
#include	 "cond.h"
#include	 "io.h"
#include	 "file.h"
#include	 "tfm.h"
#include	 "hyph.h"
#include	 "dvi.h"
#include	 "fmt.h"
#include	 "error.h"
#include	 "print.h"
#include	 "page.h"
#include	 "main.h"
#include	 "macro.h"
#include	 "allir.h"
#include	 "state.h"

/*************************************************** 
 * the area not necessary for context save/restore *
 ***************************************************/

/* align.c */

/* arith.c */

/* box.c */

/* boxlists.c */

/* char.c */
ascii	xord[128];
char	xchr[128];

/* cmds.c */

/* cond.c */

/* def.c */

/* dvi.c */
byte_file	dvi_file;
ptr		dvi_ptr;
byte		dvi_buf[DVI_BUF_SIZE];
ptr		dvi_limit = DVI_BUF_SIZE;
val		dvi_offset;
val		dvi_gone;
qword		c;
qword		f;
ptr		g;
val		lq;
val		lr;
int		cur_s;
scal		cur_h;
scal		cur_v;
fnt		dvi_f;
scal		dvi_h;
scal		dvi_v;
scal		max_h;
scal		max_v;
scal		rule_dp;
scal		rule_ht;
scal		rule_wd;
int		max_push;
int		dead_cycles;
bool		doing_leaders;
val		last_bop = -1;
ptr		down_ptr;
ptr		right_ptr;
#ifdef VORTEX
int		page_shipped = FALSE;
int		page_done = FALSE;
char		dvi_ext[EXT_MAX];
int		v_count = 0;
#endif

/* eq.c */

/* eqstack.c */

/* error.c */

/* eval.c */

/* evalstack.c */

/* expand.c */

/* file.c */

/* fmt.c */
word_file	fmt_file;
str		format_ident;


/* hash.c */

/* heap.c */
/*

/* hyph.c */
int	hyf_distance[256];
qword	hyf_next[256];
int	hyf_num[256];
int	trie_max;
qword	trie_op_ptr;
twoh	trie[TRIE_SIZE+1];
struct _node	*hir[64];

/* inc.c */
char	docname[64];			/* document name */
int	starting_page = INFINITY;
int	viewing_page  = 1;
int	format_continue = TRUE;

/* io.c */

/* irf.c */

#ifdef	VORTEX
_File	*file_root = NIL;	/* root of file tree IRf */
_File	*file_curr = NIL;	/* current file for reading */
_File	*file_update = NIL;	/* last file updated */
#endif

/* iri.c */

/* irs.c */

/* irt.c */
long			qid;
long			wid;
long			cid;
short			empty_qbox = FALSE;	/* true if new qbox is empty */
short			empty_wbox = FALSE;	/* true if new wbox is empty */
short			page_curr = 0;		/* current page number */
struct _node		*ir_cur = NIL;		/* for use in back input */
struct _cbox		*cbox = NIL;		/* current char box */
struct _wbox		*wbox = NIL;		/* current word box */
struct _qbox		*qbox = NIL;		/* current par box */
struct _pbox		*pbox = NIL;		/* current new page box */
struct _pbox		*pbox_curr = NIL;	/* current touched page box */
struct _pbox		*pbox_head = NIL;	/* head of page box list */
struct _cseq		*last_cs = NIL;		/* last cs node */

/* irv.c */

/* main.c */
char	program[40],
	log_fname[200],
	USAGE[] = "Usage: %s [ -dlt ] [ hostname portnumber ].\n";
FILE	*log_fp;
int	show_debug = FALSE;
int	tex_only = FALSE;	/* for creating undumped version */
int	local_only = FALSE;	/* retrieve data via local file access */
 int	write_out;		/* write out files */
u_long	*tex_pool;
int	tex_max;
int	Fid = 0;

/* math.c */
twoh	empty_field;

/* mathlists.c */
#ifdef VORTEX
int		in_math = FALSE;
#endif

/* mlist-hlists.c */
char	math_spacing[] = 
		"0234000122*4000133**3**344*0400400*000000234000111*1111112341011";
int		magic_offset = - 9 * ORD_NOAD;

/* msg.c */
char	colon[] = ": ";

/* pack.c */

/* page.c */
scal	best_size;
scal	cur_page_depth;

/* par.c */

/* print.c */
alpha_file	log_file;
char	dig[23];
int	file_offset;
val	first_count;
val	tally;
int	term_offset;
ascii	trick_buf[ERROR_LINE];
val	trick_count;

/* scan.c */

/* state.c */
#ifdef VORTEX
byte_file	sts_file;
ptr		xrover;
#endif

/* str.c */
ascii 	str_pool[POOL_SIZE];
ptr	pool_ptr;
ptr	str_start[MAX_STRINGS];
str	str_ptr;
str	null_str;

/* tex.c */
char	banner[] = "This is VorTeX Formatter, Version -1.0";
int	ready_already;
#ifdef VORTEX
struct _unode	*par_node_top = NIL;
#endif

/* texext.c */
alpha_file	write_file[16];
bool	write_open[18];
ptr	write_loc;

/* tfm.c */
byte_file	tfm_file;
fourq		null_character;

/* token.c */
ptr	par_loc;
hword	par_token;

/* tokenlists.c */

/* tokenstack.c */
#ifndef VORTEX
alpha_file	input_file[MAX_IN_OPEN];
#else
_File		*input_file[MAX_IN_OPEN];
#endif


















