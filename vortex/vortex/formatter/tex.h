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

#ifndef TEX_
#define TEX_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		tex.h
 */

#include	<stdio.h>
#include	<signal.h>
#include	<sys/time.h>

/*
 * constants in the outer block
 */

#define	NUL				'\0'
#define	EOLN				'\n'
#define	FALSE				0
#define	TRUE				1
#define	EMPTY				0

#define	BUF_SIZE			5000
#define	DVI_BUF_SIZE			1024
#define	ERROR_LINE			78
#define	FILE_NAME_SIZE			104
#define	FONT_BASE			0
#define	FONT_MAX			200
#define	FONT_MEM_SIZE			25000
#define	HALF_BUF			512
#define	HALF_ERROR_LINE			39
#define	HASH_SIZE			3000
#define	HASH_PRIME			2551
#define	HYPH_SIZE			307
#define	MAX_IN_OPEN			15
#define	MAX_PRINT_LINE			78
#define	MAX_STRINGS			5400
#define	NEST_SIZE			40
#define	PARAM_SIZE			30
#define	POOL_SIZE			25000
#define	SAVE_SIZE			600
#define	STACK_SIZE			200
#define	STRING_VACANCIES		1000
#define	TRIE_OP_HASH_SIZE		512
#define	TRIE_SIZE			8000

#ifdef INIT
#if !defined(BIGG) && !defined(BIG)
#define	MEM_BOT				0
#define	MEM_TOP			 	54000
#define	MEM_MIN				MEM_BOT
#define	MEM_MAX 			MEM_TOP
#define	MIN_QUARTERWORD			0
#define	MAX_QUARTERWORD			255
#define	MIN_HALFWORD			0
#define	MAX_HALFWORD			65535
#endif

#ifdef BIG
#define	MEM_BOT				0
#define	MEM_TOP			 	64000
#define	MEM_MIN				MEM_BOT
#define	MEM_MAX 			MEM_TOP
#define	MIN_QUARTERWORD			0
#define	MAX_QUARTERWORD			255
#define	MIN_HALFWORD			0
#define	MAX_HALFWORD			655350
#endif

#ifdef BIGG
#define	MEM_BOT				0
#define	MEM_TOP			 	84000
#define	MEM_MIN				MEM_BOT
#define	MEM_MAX 			MEM_TOP
#define	MIN_QUARTERWORD			0
#define	MAX_QUARTERWORD			255
#define	MIN_HALFWORD			0
#define	MAX_HALFWORD			6553500
#endif

#else 

#if !defined(BIGG) && !defined(BIG)
#define	MEM_BOT				0
#define	MEM_TOP			 	54000
#define	MEM_MIN				0
#define	MEM_MAX 			60000
#define	MIN_QUARTERWORD			0
#define	MAX_QUARTERWORD			255
#define	MIN_HALFWORD			0
#define	MAX_HALFWORD			65535
#endif

#ifdef BIG
#define	MEM_BOT				0
#define	MEM_TOP			 	64000
#define	MEM_MIN				0
#define	MEM_MAX 			90000
#define	MIN_QUARTERWORD			0
#define	MAX_QUARTERWORD			255
#define	MIN_HALFWORD			0
#define	MAX_HALFWORD			655350
#endif

#ifdef BIGG
#define	MEM_BOT				0
#define	MEM_TOP			 	84000
#define	MEM_MIN				0
#define	MEM_MAX 			128000
#define	MIN_QUARTERWORD			0
#define	MAX_QUARTERWORD			255
#define	MIN_HALFWORD			0
#define	MAX_HALFWORD			6553500
#endif

#endif

/*
 *	types in the outer block
 */

#define	global		extern

#define	ascii		unsigned char
#define	bool		short
#define	byte		unsigned char
#define	fnt		short
#define	gord		unsigned char
#define	gratio		float
#define	group		short
#define	ptr		hword
#define	sc		i
#define	scal		long 
#define	str		hword
#define	val		long
#define	qword		unsigned char

#if defined(BIG) || defined(BIGG)
#define	hword		unsigned long
#else
#define	hword		unsigned short
#endif

typedef	FILE	*word_file;
typedef	FILE 	*alpha_file;			
typedef	FILE	*byte_file;

typedef union { 
	struct { 
		hword	rh; 
		hword	lh; 
	} hh1; 
	struct { 
		hword	rh; 
		qword	b0; 
		qword	b1; 
	} hh2; 
} twoh;

typedef struct { 
	qword	b0;
	qword	b1;
	qword	b2;
	qword	b3;
} fourq;

typedef union {
	long		i;   
	gratio		gr;  
	twoh		hh;  
	fourq		qqqq;
#ifdef VORTEX
	struct _node	*ir;
#endif
} mword;

/*
 *	variables in the outer block
 */

global	char		banner[];
global	int		ready_already;

/*
 *	functions in the outer block
 */

int		final_cleanup();
int		close_files_and_terminate();
int		initialize();
bool		decode_args();
int		handle_int();

/*
 * some common programming idioms
 */

#define	incr(i)				++(i)
#define	decr(i)			 	--(i)
#define	odd(i)				((i) & 1)
#define	abs(i)				((i) >= 0 ? (i) : -(i))
#define	round(x)			(long) ((x) > 0.0 ? ((x) + 0.5) : ((x) - 0.5))
#define	negate(x)			(x) = -(x)
#define	loop				while (1)

#endif
