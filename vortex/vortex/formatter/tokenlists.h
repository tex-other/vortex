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

#ifndef TOKENLISTS_
#define TOKENLISTS_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		tokenlists.h
 */

ptr		str_toks();
ptr		the_toks();
int		conv_toks();

#define	NUMBER_CODE			0
#define	ROMAN_NUMERAL_CODE		1
#define	STRING_CODE			2
#define	MEANING_CODE			3
#define	FONT_NAME_CODE			4
#define	JOB_NAME_CODE			5

#define	token_ref_count(T)		info(T)

ptr		scan_toks();
int		read_toks();
int		ins_the_toks();
int		print_meaning();

int		flush_list();

#define	add_token_ref(T) incr(token_ref_count(T))

#define	delete_token_ref(T) { \
	if (token_ref_count(T) == NULL) \
		flush_list(T); \
	else \
		decr(token_ref_count(T)); \
}

#ifdef VORTEX

#define	store_new_token(T) { \
	q = get_avail(); \
	link(p) = q; \
	info(q) = T; \
	ir_char(q) = (_Node *) irs_ptr; \
/*	fprintf(stderr, "\nT=%d, q=%d, irs=%x, c=%c", T, q, irs_ptr, irs_ptr->_ch); */\
/*	fflush(stderr); */\
	p = q; \
}

#define	fast_store_new_token(T) { \
	fast_get_avail(q); \
	link(p) = q; \
	info(q) = T; \
	ir_char(q) = (_Node *) irs_ptr; \
/*	fprintf(stderr, "\nT=%d, q=%d, irs=%x, c=%c", T, q, irs_ptr, irs_ptr->_ch); */\
/*	fflush(stderr); */\
	p = q; \
}

#else

#define	store_new_token(T) \
	{q = get_avail(); link(p) = q; info(q) = T; p = q;}

#define	fast_store_new_token(T) \
	{fast_get_avail(q); link(p) = q; info(q) = T; p = q;}

#endif
int		show_token_list();
int		token_show();

#endif
