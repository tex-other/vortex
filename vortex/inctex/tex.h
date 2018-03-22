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

/* Copyright (c) 1992 Regents of the University of California
 * All rights reserved.
 * 
 *  This file has been modified, with permission from Pat Monardo, for IncTeX
 */
/*
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

#include	<stdio.h>
#include	<signal.h>
#include	<sys/time.h>

/*
 * constants in the outer block
 */

#define NUL                 '\0'
#define EOLN                '\n'
#define FALSE               0
#define TRUE                1
#define EMPTY               0


#define BUF_SIZE            500
#define DVI_BUF_SIZE        4096
#define ERROR_LINE          64
#define FILE_NAME_SIZE      104
#define FONT_BASE           0
#define FONT_MAX            150
#define FONT_MEM_SIZE       35000
#define HALF_BUF            2048
#define HALF_ERROR_LINE     32
#define HASH_SIZE           3000
#define HASH_PRIME          2551
#define HYPH_SIZE           307
#define MAX_IN_OPEN         15
#define MAX_PRINT_LINE      72
#define MAX_STRINGS         4400
#define NEST_SIZE           40
#define PARAM_SIZE          30
#define POOL_SIZE           40000
#define SAVE_SIZE           1000
#define STACK_SIZE          200
#define STRING_VACANCIES    1000
#define TRIE_OP_HASH_SIZE   512
#define TRIE_SIZE           8000

#ifdef BIG
#define MEM_BOT             0
#define MEM_TOP             50000
#define TOK_BOT             0
#define TOK_TOP             30000
#ifdef INIT
#define MEM_MIN             MEM_BOT
#define MEM_MAX             MEM_TOP
#define TOK_MIN             TOK_BOT
#define TOK_MAX             TOK_TOP
#else
#define MEM_MIN             0
#define MEM_MAX             55000
#define TOK_MIN             0
#define TOK_MAX             55000
#endif
#define MIN_QUARTERWORD     0
#define MAX_QUARTERWORD     255
#define MIN_HALFWORD        0
#define MAX_HALFWORD        65535
#endif

#ifdef BIGG
#define MEM_BOT             0
#define MEM_TOP             250000
#define TOK_BOT             0
#define TOK_TOP             30000
#ifdef INIT
#define MEM_MIN             MEM_BOT
#define MEM_MAX             MEM_TOP
#define TOK_MIN             TOK_BOT
#define TOK_MAX             TOK_TOP
#else
#define MEM_MIN             0
#define MEM_MAX             524200
#define TOK_MIN             0
#define TOK_MAX             55000
#endif
#define MIN_QUARTERWORD     0
#define MAX_QUARTERWORD     255
#define MIN_HALFWORD        0
#define MAX_HALFWORD        655350
#endif

#if !defined(BIGG) && !defined(BIG)
#define MEM_BOT             0
#ifdef TRIP
/* for trip test */
#define MEM_TOP             3000
#else
#define MEM_TOP             45000
#endif TRIP
#define TOK_BOT             0
#define TOK_TOP             30000
#ifdef INIT
#define MEM_MIN             MEM_BOT
#define MEM_MAX             MEM_TOP
#define TOK_MIN             TOK_BOT
#define TOK_MAX             TOK_TOP
#else
#define MEM_MIN             0
#define MEM_MAX             50000
#define TOK_MIN             0
#define TOK_MAX             50000
#endif
#define MIN_QUARTERWORD     0
#define MAX_QUARTERWORD     255
#define MIN_HALFWORD        0
#define MAX_HALFWORD        65535
#endif

/*
 *  types in the outer block
 */

#define global      extern
#if defined(BIG) || defined(BIGG)
typedef unsigned char   qword;
typedef unsigned long   hword;
#else
typedef unsigned char   qword;
typedef unsigned short  hword;
#endif
typedef unsigned char   ascii;
typedef int             bool;
typedef unsigned char   byte;
typedef char *          chrs;
typedef int             fnt;
typedef unsigned char   gord;
typedef float           gratio;
typedef int             group;
typedef hword           ptr;
typedef long            scal;
typedef hword           str;
typedef hword           tok;
typedef long            val;
typedef FILE            *word_file;
typedef FILE            *alpha_file;            
typedef FILE            *byte_file;
typedef union { 
    struct { 
        hword   rh; 
        hword   lh; 
    } hh1; 
    struct { 
        hword   rh; 
        qword   b0; 
        qword   b1; 
    } hh2; 
#ifdef INCTEX
    int all;		/* something that's the right size, works with == */
#endif INCTEX
} hh;
typedef struct { 
    qword   b0;
    qword   b1;
    qword   b2;
    qword   b3;
} qqqq;
typedef union { 
    hh      hh;  
    qqqq    qqqq;
    long    i;   
    gratio  gr;  
} mword;

/*
 *  variables in the outer block
 */

global  char        banner[];
global  int         ready_already;

/*
 *  functions in the outer block
 */

int     final_cleanup();
int     close_files_and_terminate();
int     initialize();
bool    decode_args();
int     handle_int();
int     call_edit();

/*
 * some common programming idioms
 */

#define sc                  i
#define incr(i)             ++(i)
#define decr(i)             --(i)
#define odd(i)              ((i) & 1)
#define abs(i)              ((i) >= 0 ? (i) : -(i))
#define round(x)            (long) ((x) > 0.0 ? ((x) + 0.5) : ((x) - 0.5))
#define negate(x)           (x) = -(x)
#define loop                while (1)
#include "cmds.h"
#include "char.h"
#include "str.h"
#include "io.h"
#include "print.h"
#include "error.h"
#include "heap.h"
#include "eq.h"
#include "hash.h"
#include "arith.h"

#ifdef INCTEX

#define	OUT_MAX		16

/* IncTeX-specific data structure */

/*
 * Each F_NODE corresponds to a file in the document
 */
typedef struct fnode {
	unsigned long	sl;		/* length of source filename */
	char		*sn;		/* source file name */
	FILE		*sp;		/* source file pointer */
	unsigned long	bl;		/* length of backup filename */
	char		*bn;		/* backup file name */
	FILE		*bp;		/* backup file pointer */
	unsigned long	id;		/* file id */
	long		mt;		/* file modify time */
	long		bmt;		/* backup copy's modify time. DLP */
	short		mod;		/* file mode */
	short		eof;		/* EOF scanned? */
	unsigned long	cnt;		/* current file size */
	unsigned long	cbl;		/* beginning of current input line */
	unsigned long	cel;		/* end of current input line */
	unsigned long	pbl;		/* beginning of parent input line */
	unsigned long	pel;		/* end of parent input line */
	struct fnode	*up;		/* parent fnode link */
	struct fnode	*nxt;		/* next fnode link */
} F_NODE;

/*
 * Each P_NODE corresponds to a page in the document
 */
typedef struct p_node {
	unsigned long	no;		/* page number */
	struct fnode	*fp;		/* file pointer */
	unsigned long	eop;		/* char id (offset) within the file */
	int		weop[OUT_MAX];	/* array of write_file eops */
	float		ftime;		/* time to format this page */
	float		btime;		/* time to backup file in this page */
	float		stime;		/* time to save state of this page */
	float		ltime;		/* time to load this page */
	float		qtime;		/* time to check quiescence */
	struct p_node	*nxt;		/* link */
} P_NODE;

#endif
