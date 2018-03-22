
/*
 * @(#)evalstack.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define VMODE       1
#define HMODE       (VMODE + MAX_COMMAND + 1)
#define MMODE       (HMODE + MAX_COMMAND + 1)

int     push_nest();
int     pop_nest();
int     print_mode();

#define IGNORE_DEPTH    -65536000

typedef struct
{
    int     mode_field;
    ptr     head_field;
    ptr     tail_field;
    int     pg_field;
    val     aux_field;
    val     ml_field;
}
    list;

global  list    cur_list;
global  ptr     nest_ptr;
global  list    nest[];
global  int     max_nest_stack;

#define mode                cur_list.mode_field
#define head                cur_list.head_field
#define tail                cur_list.tail_field
#define prev_graf           cur_list.pg_field
#define aux                 cur_list.aux_field
#define prev_depth          aux
#define space_factor        aux
#define incompleat_noad     aux
#define mode_line           cur_list.ml_field

global  int     shown_mode;

int     show_activities();

#define tail_append(N) \
    {link(tail) = N; tail = link(tail);}
