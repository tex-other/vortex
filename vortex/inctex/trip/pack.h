
/*
 * @(#)pack.h 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 *
 * 
 */

#define EXACTLY             0
#define ADDITIONAL          1
#define NATURAL             0L, ADDITIONAL

global  ptr     adjust_tail;
global  scal    total_stretch[];
global  scal    total_shrink[];

global  long    pack_begin_line;

#define make_char_from_lig() \
    {mem[lig_trick] = mem[lig_char(p)]; \
    link(lig_trick) = link(p); \
    p = lig_trick;}

#define get_stretch_order() \
    {if (total_stretch[FILLL] != 0) o = FILLL; \
    else if (total_stretch[FILL] != 0) o = FILL; \
    else if (total_stretch[FIL] != 0) o = FIL; \
    else o = NORMAL;}
            
#define get_shrink_order() \
    {if (total_shrink[FILLL] != 0) o = FILLL; \
    else if (total_shrink[FILL] != 0) o = FILL; \
    else if (total_shrink[FIL] != 0) o = FIL; \
    else o = NORMAL;}

#define vpack(P, H)         vpackage(P, H, MAX_DIMEN)
ptr     vpackage();
ptr     hpack();
