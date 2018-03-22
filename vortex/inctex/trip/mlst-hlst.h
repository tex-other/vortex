
/*
 * @(#)mlst-hlst.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

global  qword   cur_c;
global  fnt     cur_f;
global  qqqq    cur_i;
global  ptr     cur_mlist;
global  scal    cur_mu;
global  int     cur_size;
global  int     cur_style;
global  bool    mlist_penalties;

ptr     clean_box();
int     fetch();

#define new_hlist(N)            mem[nucleus(N)].i

int     mlist_to_hlist();

int     make_over();
int     make_under();
int     make_vcenter();
int     make_radical();
int     make_math_accent();
int     make_fraction();
scal    make_op();
int     make_ord();
int     make_scripts();
int     make_left_right();

global  int magic_offset;
