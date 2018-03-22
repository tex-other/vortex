
/*
 * @(#)heap.c 2.6 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#include	"tex.h"
#include	"box.h"
#include	"evalstack.h"
#include	"par.h"
#include	"page.h"

#ifdef INCTEX
extern	mword   mem[MEM_MAX-MEM_MIN+1];
extern	ptr     mem_end;
extern	ptr     lo_mem_max;
extern	ptr     hi_mem_min;
extern	ptr     avail;
extern	int     dyn_used;
extern	ptr     rover;
extern	int     var_used;
extern	int     max_var_used;
extern	ptr     temp_ptr;
#else
mword		mem[MEM_MAX-MEM_MIN+1];
ptr		mem_end;
ptr		lo_mem_max;
ptr		hi_mem_min;
ptr		avail;
int		dyn_used;
ptr		rover;
int		var_used;
int		max_var_used;
ptr		temp_ptr;
#endif

ptr
get_avail ()
{
    ptr     p;

    p = avail;
    if (p != NULL) {
        avail = link(avail);
    } else if (mem_end < MEM_MAX) {
        incr(mem_end);
        p = mem_end;
    } else {
        decr(hi_mem_min);
        p = hi_mem_min;
        if (hi_mem_min <= lo_mem_max) {
            runaway();
            overflow("main memory size", MEM_MAX - MEM_MIN);
        }
    }
    link(p) = NULL;
#ifdef STAT
    incr(dyn_used);
#endif
    return p;
}

ptr
get_node (s)
    int     s;
{
    ptr     p;
    ptr     q;
    int     r;
    int     t;

restart:
    p = rover;
    do {
        q = p + node_size(p);
        while (is_empty(q)) {
            t = rlink(q);
            if (q == rover)
                rover = t;
            llink(t) = llink(q);
            rlink(llink(q)) = t;
            q += node_size(q);
        }
        r = q - s;
        if (r > (int) p + 1)  {
            node_size(p) = r - p;
            rover = p;
            goto found;
        }
        if (r == p && rlink(p) != p) {
            rover = rlink(p);
            t = llink(p);
            llink(rover) = t;
            rlink(t) = rover;
            goto found;
        }
        node_size(p) = q - p;
        p = rlink(p);
    } while (p != rover);
    if (s == 010000000000)
        return MAX_HALFWORD;
    if (lo_mem_max + 2 < hi_mem_min && 
        lo_mem_max + 2 <= MEM_BOT + MAX_HALFWORD) {
        if (lo_mem_max + 1000 < hi_mem_min)
            t = lo_mem_max + 1000;
        else t = (lo_mem_max + hi_mem_min + 2) / 2;
        p = llink(rover);
        q = lo_mem_max;
        rlink(p) = q;
        llink(rover) = q;
        if (t > MEM_BOT + MAX_HALFWORD)
            t = MEM_BOT + MAX_HALFWORD;
        rlink(q) = rover;
        llink(q) = p;
        link(q) = EMPTY_FLAG;
        node_size(q) = t - lo_mem_max;
        lo_mem_max = t;
        link(lo_mem_max) = NULL;
        info(lo_mem_max) = NULL; 
        rover = q;
        goto restart;
    }

    overflow("main memory size", MEM_MAX + 1 - MEM_MIN);

found:
    link(r) = NULL;
#ifdef STAT
    var_used += s;
#endif
    return r;
}

free_node (p, s)
    ptr     p;
    hword   s;
{
    ptr     q;

    node_size(p) = s;
    link(p) = EMPTY_FLAG;
    q = llink(rover);
    llink(p) = q;
    rlink(p) = rover;
    llink(rover) = p;
    rlink(q) = p;
#ifdef STAT
    var_used -= s;
#endif
}

init_mem ()
{
    int     k;
    
#ifdef INIT
    for (k = MEM_BOT + 1; k <= LO_MEM_STAT_MAX; k++)
        mem[k].sc = 0;
    for (k = MEM_BOT; k <= LO_MEM_STAT_MAX; k += GLUE_SPEC_SIZE) {
        glue_ref_count(k) = NULL + 1;
        stretch_order(k) = NORMAL;
        shrink_order(k) = NORMAL;
    }
    stretch(fil_glue) = UNITY;
    stretch_order(fil_glue) = FIL;
    stretch(fill_glue) = UNITY;
    stretch_order(fill_glue) = FILL;
    stretch(ss_glue) = UNITY;
    stretch_order(ss_glue) = FIL;
    shrink(ss_glue) = UNITY;
    shrink_order(ss_glue) = FIL;
    stretch(fil_neg_glue) = -UNITY;
    stretch_order(fil_neg_glue) = FIL;
    
    rover = LO_MEM_STAT_MAX + 1;
    link(rover) = EMPTY_FLAG;
    node_size(rover) = 1000;
    llink(rover) = rover;
    rlink(rover) = rover;

    lo_mem_max = rover + 1000;
    link(lo_mem_max) = NULL;
    info(lo_mem_max) = NULL;
    for (k = HI_MEM_STAT_MIN; k <= MEM_TOP; incr(k))
        mem[k] = mem[lo_mem_max];

    link(end_span) = MAX_QUARTERWORD + 1;
    info(end_span) = NULL;
    type(last_active) = HYPHENATED;
    subtype(last_active) = 0;
    line_number(last_active) = MAX_HALFWORD;
    type(page_ins_head) = SPLIT_UP;
    subtype(page_ins_head) = qi(255);
    link(page_ins_head) = page_ins_head;
    type(page_head) = GLUE_NODE;
    subtype(page_head) = NORMAL;

    avail = NULL;
    mem_end = MEM_TOP;
    hi_mem_min = HI_MEM_STAT_MIN;
    var_used = LO_MEM_STAT_MAX + 1 - MEM_BOT;
    dyn_used = HI_MEM_STAT_USAGE;
#endif
}

sort_avail()
{
    ptr     p;
    ptr     q;
    ptr     r;
    ptr     old_rover;

#ifdef INIT
    get_node(010000000000);
    p = rlink(rover);
    rlink(rover) = MAX_HALFWORD;
    old_rover = rover;
    while (p != old_rover) {
        if (p < rover) {
            q = p;
            p = rlink(q);
            rlink(q) = rover;
            rover = q;
        } else {
            q = rover;
            while (rlink(q) < p)
                q = rlink(q);
            r = rlink(p);
            rlink(p) = rlink(q);
            rlink(q) = p;
            p = r;
        }
    }
    p = rover;
    while (rlink(p) != MAX_HALFWORD) {
        llink(rlink(p)) = p;
        p = rlink(p);
    }
    rlink(p) = rover;
    llink(rover) = p;
#endif
}
