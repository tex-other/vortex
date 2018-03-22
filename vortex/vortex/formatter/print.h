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

#ifndef PRINT_
#define PRINT_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		print.h
 */

#define	NO_PRINT			16 
#define	TERM_ONLY			17
#define	LOG_ONLY			18
#define	TERM_AND_LOG			19
#define	PSEUDO				20
#define	NEW_STRING			21
#define	MAX_SELECTOR			21

global	alpha_file			log_file;

global	int				selector;

global	int				term_offset;
global	int				file_offset;

global	char				dig[];
global	val				tally;
global	ascii				trick_buf[];
global	val				trick_count;
global	val				first_count;

int		print();
int		print_char();
int		print_sym();
int		print_ln();
int		print_esc();
int		print_nl();
int		print_the_digs();
int		print_int();
int		print_val();
int		print_hex();
int		print_ASCII();
int		print_roman_int();
int		print_current_string();

#ifdef VORTEX
#include		"gl_comm.h"
#include		"ts_comm.h"
#include		"allir.h"
#include		"main.h"

#define MSG_MAX		128
extern char		msg_buf[];
extern short		msg_ptr;

#define	wterm(c) { \
	if (tex_only || local_only) \
		putchar(c); \
	else  { \
		putc(c, stderr); \
/*		msg_buf[msg_ptr++] = c; \
		if ((msg_ptr == MSG_MAX) || (c == '\n')) { \
			ts_send(TSC_TEXMESSAGE, file_curr->id, msg_ptr, msg_buf); \
			msg_ptr = 0; \
		} \
*/	} \
}
#define	wterm_ln(c)		{ wterm(c);  wterm('\n'); }
#define	wterm_cr()		wterm('\n');
#else
#define	wterm(c)		putchar(c);
#define	wterm_ln(c)		{putchar(c); putchar('\n');}
#define	wterm_cr()		putchar('\n');
#endif

#define	wlog(c)			putc(c, log_file)
#define	wlog_ln(c)		{putc(c, log_file); putc('\n', log_file);}
#define	wlog_cr()		putc('\n', log_file);

#define	wfile(c)		putc(c, write_file[selector])
#define	wfile_ln(c)		{putc(c, write_file[selector]); \
				 putc('\n', write_file[selector]);}
#define	wfile_cr()		putc('\n', write_file[selector]);

#endif
