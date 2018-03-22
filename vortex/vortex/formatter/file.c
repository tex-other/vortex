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

§ÅﬁWWπ$Å"Rñ˝$6ò6‡6p6Ë6¯8,¥ÅYWG1â‘ﬂ"%ˆÿâõùh5Ä5ò5f9§ÅﬁW.„π$∂‘Á!Sñ˝$77¥ÅYW]®ıª ;r|9Sˆ6
#include		<sys/file.h>
#include		"state.h"
str			str_vtx;
#endif

#ifdef VORTEX
#include		"allir.h"
#include		"macro.h"
#include		"allir.h"
#include		"main.h"
#include		"msg.h"
#include		"var.h"

extern int		make_word_node();
extern int		total_pages;
extern _Char		*begin_of_file_name;
#endif

extern char		name_of_file[];
extern int		name_length;
	
extern int		area_delimiter;
extern int		ext_delimiter;
	
extern str		cur_area;
extern str		cur_name;
extern str		cur_ext;
	
extern bool		name_in_progress;
extern str		job_area;
extern str		job_name;
extern str		log_name;
extern str		dvi_name;

extern alpha_file	read_file[];

extern int		read_open[]; 
	
extern str		str_dvi;
extern str		str_log;
extern str		str_tex;
extern str		str_tfm;
extern str		str_fmt;
extern str		str_texput;

bool
begin_name ()
{
	area_delimiter = 0;
	ext_delimiter = 0;
}

bool
more_name (c)
	ascii	c;
{
	if (c == ' ')
		return FALSE;
	else {
		if (c == '/') {
			area_delimiter = pool_ptr;
			ext_delimiter = 0;
		} else if (c == '.' && ext_delimiter == 0)
			ext_delimiter = pool_ptr;
		str_room(1);
		append_char(c);
		return TRUE;
	}
}

end_name ()
{
	if (str_ptr + 3 > MAX_STRINGS)
		overflow("number of strings", MAX_STRINGS);
	if (area_delimiter == 0)
		cur_area = null_str;
	else {
		cur_area = str_ptr;
		incr(str_ptr);
		str_start[str_ptr] = area_delimiter + 1;
	}
	if (ext_delimiter == 0) {
		cur_ext = null_str;
		cur_name = make_string();
	} else {
		cur_name = str_ptr;
		incr(str_ptr);
		str_start[str_ptr] = ext_delimiter;
		cur_ext = make_string();
	}
}

#define	append_to_name(F) \
	{c = F; name_of_file[k] = xchr[c]; incr(k); \
}

pack_file_name (n, a, e)
	str		n;
	str		a;
	str		e;
{
	ascii		c;
	int		j;
	int		k;
		
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
	str		n;
	str		a;
	str		e;
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
	int		k;

	str_room(name_length);
	for (k = 0; k < name_length; incr(k))
		append_char(xord[name_of_file[k]]);
	return (make_string());
}

scan_file_name ()
{
	name_in_progress = TRUE;
	get_nbx_token();

#ifdef VORTEX
	begin_of_file_name = irs_ptr;
#endif

	begin_name();
	loop {
		if (cur_cmd > OTHER_CHAR || cur_chr > 127) {
			back_input();
			break;
		}
		if (!more_name(cur_chr)) break;
		get_x_token();
	}
	end_name();
	name_in_progress = FALSE;
}

prompt_file_name (s, e)
	char*	s;
	str		e;
{
	int		k;

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

#ifdef VORTEX

find_input (root)
	_File		*root;
{
	if (root == NIL)
		return(FALSE);
	else if (strcmp(name_of_file, root->fn) == 0) {
		file_curr = root;
		irs_eol = file_curr->hd;
		return(TRUE);
	} else if ((root->lt != NIL) && find_input(root->lt))
		return(TRUE);
	else if ((root->rt != NIL) && find_input(root->rt))
		return(TRUE);
	else
		return(FALSE);
}

start_input ()
{
  	real_input = real_token; /* \input was real token? */
	scan_file_name();
	if (cur_ext == null_str)
		cur_ext = str_tex;
	pack_cur_name();
	begin_file_reading();
	ts_texinput();
}

read_input ()
{
	struct _cseq	*tmp;
	struct _input	*new_input_node;
	ptr		px;

	cur_file = file_curr;
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

	if (!tex_only && (starting_page == INFINITY)) {
		 /* make sure to read from 1st char next pass */
		loc = limit + 1;
		save_state();
		starting_page = 1;
		viewing_page = 1;
	}

	input_ln();
	firm_up_the_line();
	if (end_line_char < 0 || end_line_char > 127)
		decr(limit);
	else
		buffer[limit] = end_line_char;
	first = limit + 1;
	loc = start;
	line = 1;

	irs_next = irs_bol;
 
	if (warming != TRUE) {
		if (input_state_level >= MAX_IN_OPEN) {
			fprintf(stderr, ">>> input stack overflow! <<<\n");
			return;
		}
		if (begin_of_space_token != 0) {
			make_space_node(begin_of_space_token, irs_ptr);
			begin_of_space_token = 0;
		}
		if (begin_of_word_token != 0) {
			make_word_node(begin_of_word_token, irs_ptr);
			begin_of_word_token = 0;
		}
		/* save current \input state */
		input_state_stack[input_state_level].cur_group_node = cur_group_node;
		input_state_stack[input_state_level].token_node_last = token_node_last;
		input_state_stack[input_state_level].par_node_last = par_node_last;
		input_state_level++;
		/* set new \input state */
		cur_group_node = NIL;
		token_node_last = NIL;
		par_node_last = NIL;
		/* destroy the last cseq queue(\input) entry */
		make_par_node(NIL);	/* make 1st par node for new file */
		if (real_input) {	/* if real `\input` */
			if (cs_que_top != NIL) {
				px = cs_que_top;
				tmp = (struct _cseq *)cs_node_field(px);
				cs_que_top = cs_rlink(px);
				if (cs_que_top != NIL)
					cs_llink(cs_que_top) = NIL;
				free_node(px, CS_QUE_SIZE);
				new_input_node = (struct _input *)realloc(tmp, sizeof(struct _input));
				new_input_node->_ty = NODE_INPUT;
				new_input_node->_bon = (struct _node *)begin_of_file_name;
				new_input_node->_dn = (struct _node *)par_node_last;
				new_input_node->_fp = file_curr;
			}
		}
		input_flag = TRUE;
	}
}

#else !VORTEX

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

#endif VORTEX

open_log_file ()
{
	int		k;
	int		l;
	char		months[37]; 
	int		old_setting;

	old_setting = selector;
	if (job_name == 0) {
		job_area = null_str;
		job_name = str_texput;
	}
#ifdef VORTEX
	if (tex_only)
		pack_job_name(str_log);
	else {
		if (access(VORTEX_DIR, F_OK) < 0)
			mkdir(VORTEX_DIR, 0775);
		pack_file_name(job_name, str_vtx, str_log);
	}
#else
	pack_job_name(str_log);
#endif
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

bool 
open_fmt_file ()
{
	int		j;
	
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
	int		a;
	int		b;
{
	ascii	c;
	int		j;
	int		k;

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

extern char	*cur_path;

extern char	input_path[];
extern char 	font_path[];
extern char 	format_path[];

set_paths ()
{
	char* 	env_path;
	char*	getenv();
	    
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
	char* 		s1;
	char*		s2;
	int		 	n;
{
	int			i;

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
	incr(i), incr(j); \
}

set_def_area()
{	
	char		c;
	int		i;
	int		j;
	char		def_area[MAX_PATH_CHARS];

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
 *	test_access(amode, file_path)
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
	int		amode;
	int		file_path;
{
	int		nl;
	bool		ok;
	char		original_name[FILE_NAME_SIZE];
    
	strcpy(original_name, name_of_file);
	nl = name_length;
	switch (file_path) {
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
		switch (amode) {
		case READ_ACCESS:
			ok = access(name_of_file, amode) == 0 ? TRUE : FALSE;
			break;

		case WRITE_ACCESS:
			{FILE *fp = fopen(name_of_file, "w");
			ok = fp != (FILE *) 0;
			if (ok) fclose(fp);}
			break;
		}
	} while (!ok && cur_path != NULL);
	return ok;
}

#define	append_to_real_name(C) \
	{if (i == FILE_NAME_SIZE) \
		overflow("real_name", FILE_NAME_SIZE); \
	real_name[i] = C; \
	incr(i), incr(j); \
}

get_real_name ()
{
	int		i;
	int		j;
	char		real_name[FILE_NAME_SIZE];
    
	i = j = 0;
	if (cur_path) {
		while (cur_path[j] != ':' && cur_path[j] != NUL)
			append_to_real_name(cur_path[j]);
		if (real_name[i - 1] != '/')
			append_to_real_name('/');
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

init_file ()
{
	int		i;

	name_in_progress = FALSE;
	str_tex = make_string_given(".tex");
	str_dvi = make_string_given(".dvi");
	str_log = make_string_given(".log");
	str_tfm = make_string_given(".tfm");
	str_fmt = make_string_given(".fmt");
	str_texput = make_string_given("texput");
#ifdef TRUE
	str_vtx = make_string_given(VORTEX_AREA);
#endif
	for (i = 0; i <= 16; incr(i)) 
		read_open[i] = CLOSED;
}
