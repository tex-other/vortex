/* 
 * Copyright (c) 1986-1992 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*  This file is part of IncTeX 1.0
 *
 *  Copyright (C) 1992 by Regents of the University of California
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/*
 * @(#)evalstack.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"box.h"
#include	"page.h"
#include	"tokenstack.h"
#include	"evalstack.h"

#ifdef INCTEX
extern	list    cur_list;
extern	list    nest[NEST_SIZE];
extern	ptr     nest_ptr;
extern	int     max_nest_stack;
extern	int     shown_mode;
#else
list		cur_list;
list		nest[NEST_SIZE];
ptr		nest_ptr;
int		max_nest_stack;
int		shown_mode;
#endif

push_nest ()
{
    if (nest_ptr > max_nest_stack) {
        max_nest_stack = nest_ptr;
        if (nest_ptr == NEST_SIZE)
            overflow("semantic nest size", NEST_SIZE);
    }
    nest[nest_ptr] = cur_list;
    incr(nest_ptr);
    head = get_avail();
    tail = head;
    prev_graf = 0;
    mode_line = line;
}

pop_nest ()
{
    free_avail(head);
    decr(nest_ptr);
    cur_list = nest[nest_ptr];
}

print_mode (m)
    int     m;
{
    if (m > 0) {
        switch (m / (MAX_COMMAND + 1))
        {
        case 0:
            print("vertical");
            break;

        case 1:
            print("horizontal");
            break;

        case 2:
            print("display math");
            break;
        }
    } else if (m == 0)
        print("no");
     else { 
        switch (-m / (MAX_COMMAND + 1)) 
        {
        case 0:
            print("internal vertical");
            break;

        case 1:
            print("restricted horizontal");
            break;

        case 2: 
            print("math");
            break;
        }
    }
    print(" mode");
}

show_activities ()
{
    val     a;
    int     p;
    int     m;
    ptr     q;
    ptr     r;
    val     t;

    nest[nest_ptr] = cur_list;
    print_nl("");
    print_ln();
    for (p = nest_ptr; p >= 0; decr(p)) {
        m = nest[p].mode_field;
        a = nest[p].aux_field;
        print_nl("### ");
        print_mode(m);
        print(" entered at line ");
        print_val(abs(nest[p].ml_field));
        if (nest[p].ml_field < 0)
            print(" (\\output routine)");
        if (p == 0) {
            if (page_head != page_tail) {
                print_nl("### current page:");
                if (output_active)
                    print(" (held over for next output)");
                show_box(link(page_head));
                if (page_contents > EMPTY) {
                    print_nl("total height ");
                    print_totals();
                    print_nl(" goal height ");
                    print_scaled(page_goal);
                    r = link(page_ins_head);
                    while (r != page_ins_head) {
                        print_ln();
                        print_esc("insert");
                        t = qo(subtype(r));
                        print_int((int) t);
                        print(" adds ");
                        t = x_over_n(height(r), 1000L) * count(t);
                        print_scaled((scal) t);
                        if (type(r) == SPLIT_UP) {
                            q = page_head;
                            t = 0;
                            do  {
                                q = link(q);
                                if (type(q) == INS_NODE && 
                                    subtype(q) == subtype(r))
                                    incr(t);
                            } while (q != broken_ins(r));
                            print(", #");
                            print_int((int) t);
                            print(" might split");
                        }
                        r = link(r);
                    }
                }
            }
            if (link(contrib_head) != NULL)
                print_nl("### recent contributions:");
        }
        show_box(link(nest[p].head_field)); 
        switch (abs(m) / (MAX_COMMAND + 1))
        {
        case 0:
            print_nl("prevdepth ");
            if (a <= IGNORE_DEPTH)
                print("ignored");
            else print_scaled((scal) a);
            if (nest[p].pg_field != 0) {
                print(", prevgraf ");
                print_int(nest[p].pg_field);
                print(" line");
                if (nest[p].pg_field != 1)
                    print_char('s');
            }
            break;

        case 1:
            print_nl("spacefactor ");
            print_int((int) a);
            break;

        case 2:
            if (a != NULL) {
                print_nl("this will be denominator of:");
                show_box((ptr) a);
            }
            break;
        }
    }
}
