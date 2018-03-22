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
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		io.c
 */

#include	"tex.h"
#include	"char.h"
#include	"str.h"
#include	"tokenstack.h"
#include	"print.h"
#include	"file.h"
#include	"io.h"

#ifdef VORTEX
#include	"allir.h"
#include	"main.h"
#endif VORTEX

extern int		last;
extern ascii		buffer[];
extern int		first;
extern int		max_buf_stack;

FILE *
a_open_in ()
{
	if (test_access(READ_ACCESS, INPUT_FILE_PATH))
		return (fopen(name_of_file, "r"));
	return NULL;
}

FILE *
a_open_out ()
{
	if (test_access(WRITE_ACCESS, NO_FILE_PATH))
		return (fopen(name_of_file, "w"));
	return NULL;
}

FILE *
b_open_in ()
{
	if (test_access(READ_ACCESS, FONT_FILE_PATH))
		return (fopen(name_of_file, "r"));
	return NULL;
}

FILE *
b_open_out ()
{
	if (test_access(WRITE_ACCESS, NO_FILE_PATH))
		return (fopen(name_of_file, "w"));
	return NULL;
}

FILE *
w_open_in ()
{
	if (test_access(READ_ACCESS, FORMAT_FILE_PATH))
		return (fopen(name_of_file, "r"));
	return NULL;
}

FILE *
w_open_out ()
{
	if (test_access(WRITE_ACCESS, NO_FILE_PATH))
		return (fopen(name_of_file, "w"));
	return NULL;
}

#ifdef VORTEX

bool 
input_ln_vir (f, bypass_eoln)
	alpha_file	f;
	bool		bypass_eoln;
{
	int		c;

	last = first;
	loop {
		c = getc(f);
		if (c == EOLN)
			break;
		if (c == EOF) {
			if (last == first)
				return FALSE;
			else
				break;
		}
		if (last > max_buf_stack) {
			max_buf_stack = last + 1;
			if (max_buf_stack == BUF_SIZE - 1)
				overflow("buffer size", BUF_SIZE);
		}
		buffer[last] = xord[c];
		incr(last);
	}
	loop {
		if (last == first)
			break;	
		else if (buffer[last - 1] != ' ')
			break;
		else decr(last);
	}
	return TRUE;
}

bool 
input_ln ()
{
	int		c;

	irs_bol = irs_eol;
	last = first;
	loop {
		c = ir_getc();
		if (c == EOLN)
			break;
		if (c == EOF) {
			if (last == first)
				return FALSE;
			else
				break;
		}
		if (last > max_buf_stack) {
			max_buf_stack = last + 1;
			if (max_buf_stack == BUF_SIZE - 1)
				overflow("buffer size", BUF_SIZE);
		}
		buffer[last] = xord[c];
		incr(last);
	}
	loop {
		if (last == first)
			break;	
		else if (buffer[last - 1] != ' ')
			break;
		else
			decr(last);
	}
	return TRUE;
}

#else !VORTEX

bool 
input_ln (f, bypass_eoln)
	alpha_file	f;
	bool		bypass_eoln;
{
	int			c;

	last = first;
	loop {
		c = getc(f);
		if (c == EOLN)
			break;
		if (c == EOF) {
			if (last == first)
				return FALSE;
			else
				break;
		}
		if (last > max_buf_stack) {
			max_buf_stack = last + 1;
			if (max_buf_stack == BUF_SIZE - 1)
				overflow("buffer size", BUF_SIZE);
		}
		buffer[last] = xord[c];
		incr(last);
	}
	loop {
		if (last == first)
			break;	
		else if (buffer[last - 1] != ' ')
			break;
		else decr(last);
	}
	return TRUE;
}

#endif VORTEX

term_input ()
{
	int		k;

	update_terminal();
#ifdef VORTEX
	if (!input_ln_vir(term_in, FALSE)) 
#else !VORTEX
	if (!input_ln(term_in, FALSE)) 
#endif VORTEX
		fatal_error("! End of file on the terminal");
	term_offset = 0;
	decr(selector);
	if (last != first)
		for (k = first; k < last; incr(k))
			print_char(buffer[k]);
	print_ln();
	incr(selector);
}

bool
init_terminal ()
{
	loop {
		fputs("**", stdout);
		update_terminal();
#ifdef VORTEX
		if (!input_ln_vir(term_in, FALSE)) {
#else !VORTEX
		if (!input_ln(term_in, FALSE)) {
#endif VORTEX
			puts("\n! End of file on the terminal...why?");
			return FALSE;
		}
		loc = first;
		while (loc < last && buffer[loc] == ' ')
			incr(loc);
		if (loc < last)
			return TRUE;
		puts("Please type the name of your input file.");
	}
}



