
/*
 * @(#)par.h 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define last_active         active

#define VERY_LOOSE_FIT      0
#define LOOSE_FIT           1
#define DECENT_FIT          2
#define TIGHT_FIT           3

#define PASSIVE_NODE_SIZE   2
#define cur_break           rlink
#define prev_break          llink
#define next_break          prev_break
#define serial              info

#define ACTIVE_NODE_SIZE    3
#define UNHYPHENATED        0
#define HYPHENATED          1
#define fitness             subtype
#define break_node          rlink
#define line_number         llink
#define total_demerits(D)   mem[(D) + 2].i

#define DELTA_NODE          2
#define DELTA_NODE_SIZE     7

#define do_all_six(F) \
    {F(1); F(2); F(3); F(4); F(5); F(6);}

global  ptr     just_box;

int     line_break();

global  ptr     passive;
global  ptr     printed_node;
global  hword   pass_number;

global  scal    active_width[];
global  scal    cur_active_width[];
global  scal    background[];
global  scal    break_width[];
global  bool    no_shrink_error_yet;

ptr     finite_shrink();

global  ptr     cur_p;
global  bool    second_pass;
global  val     threshold;

int     try_break();

#define AWFUL_BAD           07777777777

global  val     minimal_demerits[];
global  val     minimum_demerits;
global  ptr     best_place[];
global  hword   best_pl_line[];

global  hword   easy_line;
global  hword   last_special_line;
global  scal    first_width;
global  scal    second_width;
global  scal    first_indent;
global  scal    second_indent;
global  scal    disc_width;
global  ptr     best_bet;
global  val     fewest_demerits;
global  hword   best_line;
global  val     actual_looseness;
global  int     line_diff;

int     post_line_break();
