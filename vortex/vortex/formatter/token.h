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

#ifndef TOKEN_
#define TOKEN_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		token.h
 */

global	hword		cur_tok;
global	byte		cur_cmd;
global	hword		cur_chr;
global	ptr		cur_cs;

#define	LEFT_BRACE_TOKEN		0400
#define	LEFT_BRACE_LIMIT		01000
#define	RIGHT_BRACE_TOKEN		01000
#define	RIGHT_BRACE_LIMIT		01400
#define	MATH_SHIFT_TOKEN		01400
#define	TAB_TOKEN			02000
#define	OUT_PARAM_TOKEN			02400
#define	SPACE_TOKEN			05040
#define	LETTER_TOKEN			05400
#define	OTHER_TOKEN			06000
#define	MATCH_TOKEN			06400
#define	END_MATCH_TOKEN			07000
#define	CS_TOKEN_FLAG			010000
#define	END_TEMPLATE_TOKEN		CS_TOKEN_FLAG + FROZEN_END_TEMPLATE

#define	NO_EXPAND_FLAG			257

global	ptr		par_loc;
global	hword		par_token;
global	bool		force_eof;

int	get_token();
int	get_next();

int	check_outer_validity();
int	firm_up_the_line();

#endif
