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

#ifndef TOKENSTASCK_
#define TOKENSTASCK_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		tokensta.h
 */

typedef struct
{
	qword	state_field;
	qword	index_field;
	hword	start_field;
	hword	loc_field;
	hword	limit_field;
	hword	name_field;
#ifdef VORTEX
	struct _file	*fp;
	struct _char	*bol;
	struct _char	*eol;
	struct _char	*nxt;
#endif
} input;

global	input		cur_input;
global	input		input_stack[];
global	ptr		input_ptr;
global	ptr		max_in_stack;

#define	state				cur_input.state_field
#define	index				cur_input.index_field
#define	start				cur_input.start_field
#define	loc				cur_input.loc_field
#define	limit				cur_input.limit_field
#define	name				cur_input.name_field
	
#define	MID_LINE			1
#define	SKIP_BLANKS			(2 + MAX_CHAR_CODE)
#define	NEW_LINE			(3 + MAX_CHAR_CODE + MAX_CHAR_CODE)

#define	terminal_input	(name == 0)

#ifndef VORTEX                                                                                        
global	alpha_file	input_file[];
#else
global	struct _file	*input_file[];
#endif

global	ptr		in_open;
global	val		line;
global	val		line_stack[];

#define	cur_file			input_file[index]

#define	TOKEN_LIST			0
#define	param_start			limit
#define	token_type			index

#define	PARAMETER			0
#define	U_TEMPLATE			1
#define	V_TEMPLATE			2
#define	BACKED_UP			3
#define	INSERTED			4
#define	MACRO				5
#define	OUTPUT_TEXT			6
#define	EVERY_PAR_TEXT			7
#define	EVERY_MATH_TEXT			8
#define	EVERY_DISPLAY_TEXT		9
#define	EVERY_HBOX_TEXT			10
#define	EVERY_VBOX_TEXT			11
#define	EVERY_JOB_TEXT			12
#define	EVERY_CR_TEXT			13
#define	MARK_TEXT			14
#define	WRITE_TEXT			15

global	ptr		param_ptr;
global	ptr		param_stack[];
global	ptr		max_param_stack;

int	runaway();
int	show_context();

global	val		align_state;
global	ptr		base_ptr;
global	ptr		def_ref;
global	ptr		warning_index;
global	int		scanner_status;

#define	SKIPPING		1
#define	DEFINING		2
#define	MATCHING		3
#define	ALIGNING		4
#define	ABSORBING		5

#define	set_trick_count() \
	{first_count = tally; \
	trick_count = tally + 1 + ERROR_LINE - HALF_ERROR_LINE; \
	if (trick_count < ERROR_LINE) \
		trick_count = ERROR_LINE;}

#define	magic_c()			set_trick_count()

int	push_input();
int	pop_input();

int	begin_token_list();
int	end_token_list();

#define	back_list(L)			begin_token_list(L, BACKED_UP)
#define	ins_list(L)			begin_token_list(L, INSERTED)

int	back_input();
int	back_error();

int	begin_file_reading();
int	end_file_reading();

int	clear_for_error_prompt();

#endif
