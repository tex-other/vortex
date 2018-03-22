
/*
 * @(#)heap.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define qi(M)           (M + MIN_QUARTERWORD)
#define qo(M)           (M - MIN_QUARTERWORD)
#define hi(M)           (M + MIN_HALFWORD)
#define ho(M)           (M - MIN_HALFWORD)

#ifdef NULL
#undef NULL
#endif
#define NULL            MIN_HALFWORD

global  mword   mem[];
global  ptr     lo_mem_max;
global  ptr     hi_mem_min;

global  int     var_used;
global  int     max_var_used;
global  int     dyn_used;

#define link(M)         mem[M].hh.hh1.rh
#define info(M)         mem[M].hh.hh1.lh

global  ptr     avail;
global  ptr     mem_end;

ptr get_avail();


#ifdef  STAT
#define fast_get_avail(M) \
    {M = avail; \
    if (M == NULL) M = get_avail(); \
    else {avail = link(M); link(M) = NULL; incr(dyn_used);}}
#else
#define fast_get_avail(M) \
    {M = avail; \
    if (M == NULL) M = get_avail(); \
    else {avail = link(M); link(M) = NULL;}}
#endif


#ifdef STAT
#define free_avail(M) \
    {link(M) = avail; avail = M; decr(dyn_used);}
#else
#define free_avail(M) \
    {link(M) = avail; avail = M;}
#endif

int     flush_list();

#define is_empty(M)         (link(M) == EMPTY_FLAG)
#define EMPTY_FLAG          MAX_HALFWORD
#define node_size           info
#define llink(M)            info(M + 1)
#define rlink(M)            link(M + 1)

global  ptr rover;

ptr     get_node();
int     free_node();

global  ptr temp_ptr;

#define zero_glue       MEM_BOT
#define fil_glue        (zero_glue + GLUE_SPEC_SIZE)
#define fill_glue       (fil_glue + GLUE_SPEC_SIZE)
#define ss_glue         (fill_glue + GLUE_SPEC_SIZE)
#define fil_neg_glue    (ss_glue + GLUE_SPEC_SIZE)

#define LO_MEM_STAT_MAX (fil_neg_glue + GLUE_SPEC_SIZE - 1)

#define page_ins_head   (MEM_TOP)
#define contrib_head    (MEM_TOP - 1)
#define page_head       (MEM_TOP - 2)
#define temp_head       (MEM_TOP - 3)
#define hold_head       (MEM_TOP - 4)
#define adjust_head     (MEM_TOP - 5)
#define active          (MEM_TOP - 7)
#define align_head      (MEM_TOP - 8)
#define end_span        (MEM_TOP - 9)
#define lig_trick       (MEM_TOP - 10)
#define garbage         (MEM_TOP - 10)

#define HI_MEM_STAT_MIN     (MEM_TOP - 10)
#define HI_MEM_STAT_USAGE   11

bool    init_mem();

#ifdef INIT
int     sort_avail();
#endif
