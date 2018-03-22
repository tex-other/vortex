
/*
 * @(#)def.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  hword   after_token;
global  bool    long_help_seen;
global  val     mag_set;

int     get_r_token();
int     prefixed_command();

#define CHAR_DEF_CODE       0
#define MATH_CHAR_DEF_CODE  1
#define COUNT_DEF_CODE      2
#define DIMEN_DEF_CODE      3
#define SKIP_DEF_CODE       4
#define MU_SKIP_DEF_CODE    5
#define TOKS_DEF_CODE       6

int     do_register_command();
int     trap_zero_glue();
int     alter_aux();
int     alter_prev_graf();
int     alter_page_so_far();
int     alter_integer();
int     alter_box_dimen();
int     new_font();
int     prepare_mag();
int     new_interaction();
int     do_assignments();
int     clopen_stream();
int     issue_message();
int     give_err_help();
int     shift_case();
int     show_whatever();

#define SHOW_CODE           0
#define SHOW_BOX_CODE       1
#define SHOW_THE_CODE       2
#define SHOW_LISTS          3
