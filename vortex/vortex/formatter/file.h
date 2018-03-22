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

#ifndef FILE_
#define FILE_

/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		file.h
 */

global	char		name_of_file[];
global	int		name_length;
global	str		cur_name;
global	str		cur_area;
global	str		cur_ext;
global	int		area_delimiter;
global	int		ext_delimiter;
global	char		input_path[];
global	char		format_path[];
global	char		font_path[];

#define	MAX_PATH_CHARS			1024
#define	default_font_path	 	".:/usr/local/lib/tex/fonts"
#define	default_format_path 		".:/usr/local/lib/tex/macros"
#define	default_input_path 		".:/usr/local/lib/tex/macros"
#define	TeX_format_default		"plain.fmt"
#define	a_make_name_string(f)		make_name_string()
#define	b_make_name_string(f)		make_name_string()
#define	w_make_name_string(f)		make_name_string()

int		set_paths();
int		print_file_name();
int		pack_file_name();
bool		open_fmt_file();
str		make_name_string();
int		write_name_string();

global	bool	name_in_progress;
int		scan_file_name();

#define	pack_cur_name() \
	{pack_file_name(cur_name, cur_area, cur_ext);}

int		pack_job_name();
int		prompt_job_name();

global	str		dvi_name;

global	str		job_name;
global	str		job_area;
global	str		log_name;

int		open_log_file();
int		start_input();

global	alpha_file	read_file[];
global	int		read_open[];

#define	JUST_OPENED			1
#define	CLOSED				2

bool	test_access();

#define READ_ACCESS			4
#define WRITE_ACCESS 			2

#define NO_FILE_PATH 			0
#define INPUT_FILE_PATH 		1
#define FONT_FILE_PATH 			2
#define FORMAT_FILE_PATH 		3

global	str		str_dvi;
global	str		str_tex;
global	str		str_log;
global	str		str_tfm;
global	str		str_fmt;
global	str		str_texput;

#ifdef INIT
int		init_file();
#endif

#endif




