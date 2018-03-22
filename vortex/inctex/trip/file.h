
/*
 * @(#)file.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  char    name_of_file[];
global  int     name_length;

global  str     cur_name;
global  str     cur_area;
global  str     cur_ext;

global  int     area_delimiter;
global  int     ext_delimiter;

int     print_file_name();
int     pack_file_name();

#define TeX_format_default  "plain.fmt"

bool    open_fmt_file();

str     make_name_string();

#define a_make_name_string(f)       make_name_string()
#define b_make_name_string(f)       make_name_string()
#define w_make_name_string(f)       make_name_string()

global  bool    name_in_progress;
int     scan_file_name();

#define pack_cur_name() \
    {pack_file_name(cur_name, cur_area, cur_ext);}

int     pack_job_name();
int     prompt_job_name();

global  str     job_name;
global  str     job_area;
global  str     log_name;

int     open_log_file();
int     start_input();

global  alpha_file  read_file[];
global  int     read_open[];

#define JUST_OPENED         1
#define CLOSED              2

bool    test_access();

#define READ_ACCESS         4
#define WRITE_ACCESS        2

#define MAX_PATH_CHARS  1024

global  char    input_path[];
global  char    format_path[];
global  char    font_path[];

int     set_paths();

#define default_font_path       "."
#define default_format_path     "."
#define default_input_path      "."

#define NO_FILE_PATH        0
#define INPUT_FILE_PATH     1
#define FONT_FILE_PATH      2
#define FORMAT_FILE_PATH    3

global  str     str_dvi;
global  str     str_tex;
global  str     str_log;
global  str     str_tfm;
global  str     str_fmt;
global  str     str_texput;

#ifdef INIT
int     init_file();
#endif
