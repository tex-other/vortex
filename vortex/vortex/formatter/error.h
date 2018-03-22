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

#ifndef ERROR_
#define ERROR_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		error.h
 */

#define	BATCH_MODE				0
#define	NONSTOP_MODE				1
#define	SCROLL_MODE				2
#define	ERROR_STOP_MODE				3

#define	wake_up_terminal()

global	int		old_setting;

int	begin_diagnostic();
int	end_diagnostic();

int	print_err();

global	int		interaction;
global	bool		deletions_allowed;
global	int		history;
global	int		error_count;

#ifndef NOHELP 
#define	help0() \
	{help_ptr = 0;}

#define	help1(h0) \
	{help_ptr = 1; help_line[0] = h0;}

#define	help2(h0, h1) \
	{help_ptr = 2; \
	help_line[0] = h0; help_line[1] = h1;}

#define	help3(h0, h1, h2) \
	{help_ptr = 3; help_line[0] = h0; \
	help_line[1] = h1; help_line[2] = h2;}

#define	help4(h0, h1, h2, h3) \
	{help_ptr = 4; \
	help_line[0] = h0; help_line[1] = h1; \
	help_line[2] = h2; help_line[3] = h3;}

#define	help5(h0, h1, h2, h3, h4) \
	{help_ptr = 5; help_line[0] = h0; \
	help_line[1] = h1; help_line[2] = h2; \
	help_line[3] = h3; help_line[4] = h4;}

#define	help6(h0, h1, h2, h3, h4, h5) \
	{help_ptr = 6; \
	help_line[0] = h0; help_line[1] = h1; \
	help_line[2] = h2; help_line[3] = h3; \
	help_line[4] = h4; help_line[5] = h5;}
#else
#define	help0()
#define	help1(h0)
#define	help2(h0, h1)
#define	help3(h0, h1, h2)
#define	help4(h0, h1, h2, h3)
#define	help5(h0, h1, h2, h3, h4) 
#define	help6(h0, h1, h2, h3, h4, h5)
#endif

global	char*			help_line[];
global	int			help_ptr;
global	bool			use_err_help;

int	jump_out();
int	error();
int	int_error();
int	normalize_selector();

#define	succumb() \
	{if (interaction == ERROR_STOP_MODE) \
		interaction = SCROLL_MODE; \
	error(); history = FATAL_ERROR_STOP; jump_out();}

#define	SPOTLESS				0
#define	WARNING_ISSUED				1
#define	ERROR_MESSAGE_ISSUED			2
#define	FATAL_ERROR_STOP			3

int	fatal_error();
int	overflow();
int	confusion();

global	int		interrupt;
global	bool		OK_to_interrupt;

#define	check_interrupt() \
	{if (interrupt != 0) pause_for_instructions();}

int	pause_for_instructions();

#endif
