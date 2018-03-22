/* 
 * Copyright (c) 1987 The Regents of the University of California.
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

#include <math.h>
#include <stdio.h>    
#include <math.h>    
#include <X/Xlib.h>
#include <sys/types.h>    
#include <tp_comm.h> 
#include <gl_comm.h>     
#include "document.h"    
#include "font.h"    
    
/*other includes like for doc page and communications up here*/
    
extern int ox,oy;       /*stored since we cannot get "mouse moved" events*/
extern int sel_lev;     /*current level of selection that main must know*/

#ifndef DST
#define DST(x,y) (x*x + y*y)
#endif
#ifndef Max
#define Max(a,b) (a > b ? a : b)
#endif
#ifndef Min
#define Min(a,b) (a > b ? b : a)
#endif
#ifndef RND
#define RND(x) ((x > 0) ? ((x - (int) x) > .5 ? ((int) x + 1): (int) x) : \
		(((int) x - x ) > .5 ?((int) x - 1):(int) x))
#endif

#define NOOP   0    
#define SEL_CH 1    
#define SEL_WD 2
#define SEL_PR 3
#define SEL_PG 4

#define DEL   2 /*max change for resetting of events*/
#define EPS   3 /*tunable episilon for searches*/

/*this is for returning stuff at given levels and searches*/

typedef struct _Ubox { Nbox *ub_nb;
		       Tbox *ub_tb;
		   } Ubox;

typedef struct _Abox { int a_lev;         
		       Nbox *a_pp;
		       Nbox *a_pb;
		       Nbox *a_wb;    
		       Tbox *a_ch;
		   } Abox;

typedef struct _Gbox { int *g_bx;
		       int *g_by;
		       int *g_bw;
		       int *g_bh;
		       int g_nb;
		       short *g_bits;
		   } Gbox;

