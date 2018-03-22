/*
 *
 * @(#)file.c 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 */

/*
 *
 *  This file has been modified, with permission from Pat Monardo, for
 *
 *  IncTeX  --	The Olivetti-Berkeley-Matsushita Incremental TeX.
 *
 *  An Editor-Independent, Adaptive, Incremental TeX Formatter.
 *
 *  Copyright (C) 1988 by Olivetti Research Center
 *
 *  Author:
 *  	Pehong Chen
 *	Olivetti Research Center
 *	Menlo Park, California
 *	USA
 *	(chen@orc.olivetti.com)
 *
 *  All rights reserved by the copyright holders.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

#include	"tex.h"
#include	"token.h"
#include	"scan.h"
#include	"tokenstack.h"
#include	"fmt.h"
#include	"file.h"

#ifdef INCTEX

#include	"Imain.h"

extern	char		name_of_file[FILE_NAME_SIZE];
extern	int		name_length;
extern	int		area_delimiter;
extern	int		ext_delimiter;
extern	str		cur_area;
extern	str		cur_name;
extern	str		cur_ext;
extern	bool		name_in_progress;
extern	str		job_area;
extern	str		job_name;
extern	str		log_name;
extern	alpha_file      read_file[16];
extern	int		read_open[17]; 
extern	str		str_dvi;
extern	str		str_log;
extern	str		str_tex;
extern	str		str_tfm;
extern	str		str_fmt;
extern	str		str_texput;

#else

char		name_of_file[FILE_NAME_SIZE];
int		name_length;
int		area_delimiter;
int		ext_delimiter;
str		cur_area;
str		cur_name;
str		cur_ext;
bool		name_in_progress;
str		job_area;
str		job_name;
str		log_name;
alpha_file      read_file[16];
int		read_open[17]; 
str		str_dvi;
str		str_log;
str		str_tex;
str		str_tfm;
str		str_fmt;
str		str_texput;
#endif

bool
begin_name ()
{
    area_delimiter = 0;
    ext_delimiter = 0;
}

bool
more_name (c)
    ascii   c;
{
    if (c == ' ') {
        return FALSE;
    } else {
        if (c == '/') {
            area_delimiter = pool_ptr;
            ext_delimiter = 0;
        } else if (c == '.' && ext_delimiter == 0) {
            ext_delimiter = pool_ptr;
        }
        str_room(1);
        append_char(c);
        return TRUE;
    }
}

end_name ()
{
    if (str_ptr + 3 > MAX_STRINGS)
        overflow("number of strings", MAX_STRINGS);
    if (area_delimiter == 0) {
        cur_area = null_str;
    } else {
        cur_area = str_ptr;
        incr(str_ptr);
        str_start[str_ptr] = area_delimiter + 1;
    }
    if (ext_delimiter == 0) {
        cur_ext = null_str;
        cur_name = make_str();
    } else {
        cur_name = str_ptr;
        incr(str_ptr);
        str_start[str_ptr] = ext_delimiter;
        cur_ext = make_str();
    }
}

#define append_to_name(F) \
    {c = F; name_of_file[k] = xchr[c]; incr(k);}

pack_file_name (n, a, e)
    str     n;
    str     a;
    str     e;
{
    ascii   c;
    int     j;
    int     k;
        
    if (length(a) + length(n) + length(e) >= FILE_NAME_SIZE)
        overflow("file name size", FILE_NAME_SIZE);
    k = 0;
    for (j = str_start[a]; j < str_start[a+1]; incr(j))
        append_to_name(str_pool[j]);
    for (j = str_start[n]; j < str_start[n+1]; incr(j))
        append_to_name(str_pool[j]);
    for (j = str_start[e]; j < str_start[e+1]; incr(j))
        append_to_name(str_pool[j]);
    name_length = k;
    name_of_file[k] = NUL;
}

print_file_name (n, a, e)
    str     n;
    str     a;
    str     e;
{
    print_str(a);
    print_str(n);
    print_str(e);
}

pack_job_name (s)
{
    cur_area = job_area;
    cur_name = job_name;
    cur_ext = s;
    pack_cur_name();
}

str
make_name_string ()
{
    int     k;

    str_room(name_length);
    for (k = 0; k < name_length; incr(k))
        append_char(xord[name_of_file[k]]);
    return (make_str());
}

scan_file_name ()
{
    name_in_progress = TRUE;
    get_nbx_token();
    begin_name();
    loop {
        if (cur_cmd > OTHER_CHAR || cur_chr > 127) {
            back_input();
            break;
        }
        if (!more_name(cur_chr))
            break;
        get_x_token();
    }
    end_name();
    name_in_progress = FALSE;
}

prompt_file_name (s, e)
    chrs    s;
    str     e;
{
    int     k;

    if (s[0] == 'i' && s[1] == 'n')
        print_nl("! I can't find file `");
    else print_nl("! I can't write on file `");
    print_file_name(cur_name, cur_area, cur_ext);
    print("'.");
    if (e == str_tex)
        show_context();
    print_nl("Please type another ");
    print(s);
    if (interaction < SCROLL_MODE)
        fatal_error("*** (job aborted, file error in nonstop mode)");
    clear_terminal();
    prompt_input(": ");
    begin_name();
    k = first;
    while (buffer[k] == ' ' && k < last)
        incr(k);
    loop {
        if (k == last)
            break;
        if (! more_name(buffer[k]))
            break;
        incr(k);
    }
    end_name();
    if (cur_ext == null_str)
        cur_ext = e;
    pack_cur_name();
}


#ifdef INCTEX

init_input () 
{
	scan_file_name();
	if (cur_ext == null_str)
		cur_ext = str_tex;
	pack_cur_name();
	loop {
		begin_file_reading();
		if (cur_file = a_open_in(TRUE))
			break;
		end_file_reading();
		if (cur_ext == str_tex) {
			cur_ext = null_str;
			pack_cur_name();
			begin_file_reading();
			if (cur_file = a_open_in(TRUE))
				break;
			end_file_reading();
		}
		prompt_file_name("input file name", str_tex);
	}
	name = a_make_name_string(cur_file);
	if (job_name == 0) {
		job_area = cur_area;
		job_name = cur_name;
		open_log_file();
		if (job_area != null_str)
			set_def_area();
	} 
}


cont_input ()
{
	if (term_offset + length(name) > MAX_PRINT_LINE - 2)
		print_ln();
	else if (term_offset > 0 || file_offset > 0)
		print_char(' ');
	print_char('(');
	print_str(name);
	update_terminal(); 
	state = NEW_LINE;
	
	fseek(cur_file, 0L, SEEK_SET);
	input_ln(cur_file, TRUE);
	firm_up_the_line();
	if (end_line_char < 0 || end_line_char > 127)
		decr(limit);
	else
		buffer[limit] = end_line_char;
	first = limit + 1;
	loc = start;
	line = 1;
	
	load_str_file();
}

start_input ()
{
	scan_file_name();
	if (cur_ext == null_str)
		cur_ext = str_tex;
	pack_cur_name();
	loop {
		begin_file_reading();
		if (cur_file = a_open_in(TRUE))
			break;
		end_file_reading();
		if (cur_ext == str_tex) {
			cur_ext = null_str;
			pack_cur_name();
			begin_file_reading();
			if (cur_file = a_open_in(TRUE))
				break;
			end_file_reading();
		}
		prompt_file_name("input file name", str_tex);
	}
	name = a_make_name_string(cur_file);
	if (job_name == 0) {
		job_area = cur_area;
		job_name = cur_name;
		open_log_file();
		if (job_area != null_str)
			set_def_area();
	}
	if (term_offset + length(name) > MAX_PRINT_LINE - 2)
		print_ln();
	else if (term_offset > 0 || file_offset > 0)
		print_char(' ');
	print_char('(');
	print_str(name);
	update_terminal(); 
	state = NEW_LINE;
	
	input_ln(cur_file, TRUE);
	firm_up_the_line();
	if (end_line_char < 0 || end_line_char > 127)
		decr(limit);
	else
		buffer[limit] = end_line_char;
	first = limit + 1;
	loc = start;
	line = 1;
}

open_log_file ()
{
    int     k;
    int     l;
    char    months[37]; 
    int     old_setting;

    old_setting = selector;
    if (job_name == 0) {
        job_area = null_str;
        job_name = str_texput;
    }
    pack_job_name(str_log);
    while ((log_file = a_open_out(FALSE)) == NULL)
        prompt_file_name("transcript file name", str_log);
    log_name = a_make_name_string(log_file);
    selector = LOG_ONLY;
    fputs(banner, log_file);
    if (format_ident == 0)
        print(" (no format preloaded)");
    else print_str(format_ident);
    print_char(' ');
    print_int(day);
    print_char(' ');
    strcpy(months, "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC");
    for (k = 3 * month - 3; k < 3 * month; incr(k))
        putc(months[k], log_file);
    print_char(' ');
    print_int(year);
    print_char(' ');
    print_two(time / 60);
    print_char(':');
    print_two(time % 60);
    input_stack[input_ptr] = cur_input;
    print_nl("**");
    l = input_stack[0].limit_field;
    if (buffer[l] == end_line_char) decr(l);
    for (k = 1; k <= l; incr(k))
        print_char(buffer[k]);
    print_ln();
    selector = old_setting + 2; 
}

#else

start_input ()
{
    scan_file_name();
    if (cur_ext == null_str)
        cur_ext = str_tex;
    pack_cur_name();
    loop {
        begin_file_reading();
        if (cur_file = a_open_in())
            break;
        end_file_reading();
        if (cur_ext == str_tex) {
            cur_ext = null_str;
            pack_cur_name();
            begin_file_reading();
            if (cur_file = a_open_in())
                break;
            end_file_reading();
        }
        prompt_file_name("input file name", str_tex);
    }
    name = a_make_name_string(cur_file);
    if (job_name == 0) {
        job_area = cur_area;
        job_name = cur_name;
        open_log_file();
        if (job_area != null_str)
            set_def_area();
    }
    if (term_offset + length(name) > MAX_PRINT_LINE - 2)
        print_ln();
    else if (term_offset > 0 || file_offset > 0)
        print_char(' ');
    print_char('(');
    print_str(name);
    update_terminal(); 
    state = NEW_LINE;
    input_ln(cur_file, FALSE);
    firm_up_the_line();
    if (end_line_char < 0 || end_line_char > 127)
        decr(limit);
    else buffer[limit] = end_line_char;
    first = limit + 1;
    loc = start;
    line = 1;
}

open_log_file ()
{
    int     k;
    int     l;
    char    months[37]; 
    int     old_setting;

    old_setting = selector;
    if (job_name == 0) {
        job_area = null_str;
        job_name = str_texput;
    }
    pack_job_name(str_log);
    while ((log_file = a_open_out()) == NULL)
        prompt_file_name("transcript file name", str_log);
    log_name = a_make_name_string(log_file);
    selector = LOG_ONLY;
    fputs(banner, log_file);
    if (format_ident == 0)
        print(" (no format preloaded)");
    else print_str(format_ident);
    print_char(' ');
    print_int(day);
    print_char(' ');
    strcpy(months, "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC");
    for (k = 3 * month - 3; k < 3 * month; incr(k))
        putc(months[k], log_file);
    print_char(' ');
    print_int(year);
    print_char(' ');
    print_two(time / 60);
    print_char(':');
    print_two(time % 60);
    input_stack[input_ptr] = cur_input;
    print_nl("**");
    l = input_stack[0].limit_field;
    if (buffer[l] == end_line_char) decr(l);
    for (k = 1; k <= l; incr(k))
        print_char(buffer[k]);
    print_ln();
    selector = old_setting + 2; 
}

#endif


bool 
open_fmt_file ()
{
    int     j;
    
    j = loc;
    if (buffer[loc] == '&') {
        incr(loc);
        j = loc;
        buffer[last] = ' ';
        while (buffer[j] != ' ') incr(j);
        pack_buffered_name(loc, j);
        if (fmt_file = w_open_in()) goto found;
        wake_up_terminal();
        puts("Sorry, I can't find that format, will try PLAIN.");
        update_terminal();
    }
    strcpy(name_of_file, TeX_format_default);
    name_length = 9;
    if ((fmt_file = w_open_in()) == NULL) {
        puts("I can't find the PLAIN format file!");
        return FALSE;
    }
found: 
    loc = j;
    return TRUE;
}

pack_buffered_name (a, b)
    int     a;
    int     b;
{
    ascii   c;
    int     j;
    int     k;

    k = 0;
    for (j = a; j < b; incr(j))
        append_to_name(buffer[j]);
    append_to_name(xchr['.']);
    append_to_name(xchr['f']);
    append_to_name(xchr['m']);
    append_to_name(xchr['t']);
    append_to_name(NUL);
    name_length = b - a + 4;
}

/* 
 * fixed arrays are used to hold the paths, to avoid any possible problems
 * involving interaction of malloc and undump
 */ 

#ifdef INCTEX

extern	chrs    cur_path;
extern	char    input_path[MAX_PATH_CHARS];
extern	char    font_path[MAX_PATH_CHARS];
extern	char    format_path[MAX_PATH_CHARS];

#else

chrs		cur_path;
char		input_path[MAX_PATH_CHARS]	= default_input_path;
char		font_path[MAX_PATH_CHARS]	= default_font_path;
char		format_path[MAX_PATH_CHARS]	= default_format_path;

#endif

set_paths ()
{
    chrs    env_path;
    chrs    getenv();
    
    if (env_path = getenv("TEXINPUTS"))
        copy_path(input_path, env_path, MAX_PATH_CHARS);
    if (env_path = getenv("TEXFONTS"))
        copy_path(font_path, env_path, MAX_PATH_CHARS);
    if (env_path = getenv("TEXFORMATS"))
        copy_path(format_path, env_path, MAX_PATH_CHARS);
}

/*
 * copy_path(s1,s2,n) copies at most n characters (including the null)
 * from string s2 to string s1, giving an error message for paths
 * that are too long.
 */

copy_path (s1, s2, n)
    chrs        s1;
    chrs        s2;
    int         n;
{
    int         i;

    i = 0;
    while (s2[i] != NUL) {
        s1[i] = s2[i];
        incr(i);
        if (i == n) {
            fprintf(stderr, "! Environment search path is too big\n");
            s1[i - 1] = '\0';
            return;
        }
    }
    s1[i] = NUL;
}

#define append_to_def_area(C) \
    {if (i == MAX_PATH_CHARS)  \
        overflow("def_area", MAX_PATH_CHARS); \
    def_area[i] = C; \
    incr(i), incr(j);}

set_def_area()
{   
    char    c;
    int     i;
    int     j;
    char    def_area[MAX_PATH_CHARS];

    i = 0;
    j = str_start[job_area];
    while (j < str_start[job_area + 1])
        append_to_def_area(str_pool[j]);
    append_to_def_area(':');
    j = 0;
    while ((c = input_path[j]) != NUL)
        append_to_def_area(c);
    append_to_def_area(NUL);
    strcpy(input_path, def_area);
}

/*
 *  test_access(amode, file_path)
 *
 *  Test whether or not the file whose name is in the global name_of_file
 *  can be opened for reading according to access mode.
 *
 *  If the filename given in name_of_file does not begin with '/', we try 
 *  prepending all the ':'-separated areanames in the appropriate path to the
 *  filename until access can be made.
 */

bool
test_access (amode, file_path)
    int     amode;
    int     file_path;
{
    int     nl;
    bool    ok;
    char    original_name[FILE_NAME_SIZE];

    strcpy(original_name, name_of_file);
    nl = name_length;
    switch (file_path)
    {
    case NO_FILE_PATH:
        cur_path = NULL;
        break;

    case INPUT_FILE_PATH: 
        cur_path = input_path;
        break;

    case FONT_FILE_PATH: 
        cur_path = font_path;
        break;

    case FORMAT_FILE_PATH:
        cur_path = format_path;
        break;
    }
    if (name_of_file[0] == '/' ||
        name_of_file[0] == '.' && name_of_file[1] == '/')
        cur_path = NULL;
    do {
        strcpy(name_of_file, original_name);
        name_length = nl;
        get_real_name();
        switch (amode)
        {
        case READ_ACCESS:
            ok = access(name_of_file, amode) == 0 ? TRUE : FALSE;
            break;

        case WRITE_ACCESS:
            ok = fclose(fopen(name_of_file, "w")) == 0;
            break;
        }
    } while (!ok && cur_path != NULL);
    return ok;
}

#define append_to_real_name(C) \
    {if (i == FILE_NAME_SIZE) \
        overflow("real_name", FILE_NAME_SIZE); \
    real_name[i] = C; \
    incr(i), incr(j);}

get_real_name ()
{
    int     i;
    int     j;
    char    real_name[FILE_NAME_SIZE];
    
    i = j = 0;
    if (cur_path) {
        while (cur_path[j] != ':' && cur_path[j] != NUL)
            append_to_real_name(cur_path[j]);
        if (j == 0) {
            append_to_real_name('.');
            append_to_real_name('/');
            --j;
        } else if (real_name[j - 1] != '/') {
            append_to_real_name('/');
            --j;
        }
        if (cur_path[j] == NUL)
            cur_path = NULL;
        else cur_path += j;
    }
    j = 0;
    while (j < name_length)
        append_to_real_name(name_of_file[j]);
    append_to_real_name(NUL);
    strcpy(name_of_file, real_name);
    name_length = i - 1;
}


#ifdef INCTEX

init_file ()
{
	int     i;

	name_in_progress = FALSE;
	str_tex = make_str_given(".tex");
	str_dvi = make_str_given(".dvi");
	str_log = make_str_given(".log");
	str_tfm = make_str_given(".tfm");
	str_fmt = make_str_given(".fmt");
	str_texput = make_str_given("texput");
	str_inc = make_str_given(INC_AREA);
	for (i = 0; i <= 16; incr(i)) 
        	read_open[i] = CLOSED;
}

#else

init_file ()
{
    int     i;

    name_in_progress = FALSE;
    str_tex = make_str_given(".tex");
    str_dvi = make_str_given(".dvi");
    str_log = make_str_given(".log");
    str_tfm = make_str_given(".tfm");
    str_fmt = make_str_given(".fmt");
    str_texput = make_str_given("texput");
    for (i = 0; i <= 16; incr(i)) 
	read_open[i] = CLOSED;
}

#endif

