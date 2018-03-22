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

#ifndef MAIN_
#define MAIN_

/*
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Preparation Environment
 *
 *  This file is part of the VorTeX incremental formatter
 *
 *  Copyright (C) 1987 by   Pehong Chen		(phc@renoir.berkeley.edu)
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

#undef	TRUE
#define	TRUE			1
#undef	FALSE
#define	FALSE			0
#undef	NIL
#define	NIL			'\0'

#define CALLOC(PTR, TYPE, N, SIZE) { \
	if ((PTR = (TYPE *) calloc(N, (SIZE))) == NIL) { \
		msg(STDERR, "Not enough corein calloc."); \
	} \
}

#define MALLOC(PTR, TYPE, SIZE) { \
	if ((PTR = (TYPE *) malloc(SIZE)) == NIL) { \
		msg(STDERR, "Not enough core malloc."); \
	} \
}

#define ALLOCA(PTR, TYPE, SIZE) { \
	if ((PTR = (TYPE *) alloca(SIZE)) == NIL) { \
		msg(STDERR, "Not enough core alloca."); \
	} \
}

/*
 * a box id has four fields
 *	page_no		10 bits	== 1K pages per document
 *	par_no		 6 bits == 64 pars  per page
 *	word_no		10 bits == 1K words per paragraph
 *	char_no		 6 bits == 64 letters per word
 */
#define P_BUMP			0x00400000
#define Q_BUMP			0x00010000
#define W_BUMP			0x00000040
#define P_MASK			0xffc00000
#define Q_MASK			0x003f0000
#define W_MASK			0x0000ffc0
#define C_MASK			0x0000003f
#define MASK10			0x000003ff
#define MASK6			0x0000003f
#define P_SHIFT			22
#define Q_SHIFT			16
#define W_SHIFT			6
#define PID(I)			((I) >> P_SHIFT)
#define QID(I)			(((I) >> Q_SHIFT) & MASK6)
#define WID(I)			(((I) >> W_SHIFT) & MASK10)
#define CID(I)			((I) & MASK6)

#define	A_MASK			0x0000007f
#define	F_MASK			0x00003f80
#define	A_SHIFT			7
#define	F_SHIFT			14
#define FID(I)			(((I) & F_MASK) >> A_SHIFT)

#define	U_LONG			sizeof(u_long)
#define	U_SHORT			sizeof(u_short)
#define MAX2(A, B)		(((A) > (B)) ? (A) : (B))
#define MIN2(A, B)		(((A) < (B)) ? (A) : (B))

extern			tex_only;
extern			local_only;
extern			show_debug;
extern			in_math;
extern			last_loc;
extern			page_shipped;
extern			page_done;
extern			starting_page;
extern			viewing_page;
extern			format_continue;

extern struct _file	*ir_find_file();
extern struct _file	*ir_insert_file();
extern struct _file	*file_root;	/* root of file tree IRf */
extern struct _file	*file_curr;	/* current file for reading */
extern struct _file	*file_prev;	/* last file updated */
extern struct _char	*irs_bol;
extern struct _char	*irs_eol;

extern struct _pbox	*pbox;
extern struct _pbox	*pbox_curr;
extern struct _pbox	*pbox_head;
extern struct _cseq	*last_cs;
extern struct _char	*irs_next;
extern struct _char	*irs_ptr;
extern struct _node	*ir_cur;
extern struct _node	*ir_esc;
extern struct _node	*ir_math;

char			*ts_reqname();
char			*tp_reqname();
char			*alloca();
char			*ecalloc();
struct _node		*find_target();
struct _char		*find_source();
struct _file		*ir_find_file();
struct _file		*ir_insert_file();

/* Code to do timing tests */
#ifdef TIME
#include <sys/time.h>

extern struct timeval time0, time1, time2, time3;
extern struct timezone tz;
extern float timePer;
#endif


#endif
