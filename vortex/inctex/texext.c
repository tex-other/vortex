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

/* This file is part of IncTeX 1.0
 *
 * Copyright (C) 1992 by Regents of the University of California
 *
 * This file has been modified, with permission from Pat Monardo, for IncTeX
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */
/* @(#)texext.c 2.5 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"token.h"
#include	"box.h"
#include	"scan.h"
#include	"def.h"
#include	"tokenstack.h"
#include	"tokenlists.h"
#include	"evalstack.h"
#include	"file.h"
#include	"dvi.h"
#include	"texext.h"


#ifdef INCTEX

#include	"Imain.h"

extern	ptr     write_loc;

#else

ptr		write_loc;

#endif

alpha_file	write_file[16];
bool		write_open[18];


do_extension ()
{
    int     i;
    int     j;
    int     k;
    ptr     p;
    ptr     q;
    ptr     r;

    switch (cur_chr)
    {
    case OPEN_NODE:
        new_write(OPEN_NODE_SIZE);
        scan_optional_equals();
        scan_file_name();
        open_name(tail) = cur_name;
        open_area(tail) = cur_area;
        open_ext(tail) = cur_ext;
        break;
    
    case WRITE_NODE:
        k = cur_cs;
        new_write(WRITE_NODE_SIZE);
        cur_cs = k;
        scan_toks(FALSE, FALSE);
        write_tokens(tail) = def_ref;
        break;
    
    case CLOSE_NODE:
        new_write(WRITE_NODE_SIZE);
        write_tokens(tail) = NULL;
        break;
    
    case SPECIAL_NODE:
        new_whatsit(SPECIAL_NODE, WRITE_NODE_SIZE);
        cur_cs = k;
        scan_toks(FALSE, TRUE);
        write_tokens(tail) = def_ref;
        break;

    case IMMEDIATE_CODE:
        get_x_token();
        if (cur_cmd == EXTENSION && cur_chr <= CLOSE_NODE) {
            p = tail;
            do_extension();
            out_whatsit(tail);
            flush_node_list(tail);
            tail = p;
            link(p) = NULL;
        } else back_input();
        break;

    default:
        confusion("ext1");
        break;
    }
}

new_whatsit (s, w)
    int     s;
    int     w;
{
    ptr     p;

    p = get_node(w);
    type(p) = WHATSIT_NODE;
    subtype(p) = s;
    link(tail) = p;
    tail = p;
}

show_whatsit (p)
    ptr     p;
{ 
    switch (subtype(p)) 
    { 
    case OPEN_NODE: 
        print_write("openout", p);
        print_char('='); 
        print_file_name(open_name(p), open_area(p), open_ext(p)); 
        break; 
     
    case WRITE_NODE: 
        print_write("write", p); 
        print_mark(write_tokens(p)); 
        break; 
     
    case CLOSE_NODE: 
        print_write("closeout", p); 
        break; 
    
    case SPECIAL_NODE: 
        print_esc("special"); 
        print_mark(write_tokens(p)); 
        break; 
     
    default: 
        print("whatsit?"); 
        break;
    } 
} 

ptr
copy_whatsit (p)
    ptr     p;
{ 
    ptr     q;

    switch (subtype(p)) 
    { 
    case OPEN_NODE: 
        q = get_node(OPEN_NODE_SIZE); 
        mem[q + 2] = mem[p + 2];
        mem[q + 1] = mem[p + 1];
        break; 
     
    case WRITE_NODE: 
    case SPECIAL_NODE:
        q = get_node(WRITE_NODE_SIZE); 
        add_token_ref(write_tokens(p)); 
        mem[q + 1] = mem[p + 1];
        break; 
     
    case CLOSE_NODE: 
        q = get_node(WRITE_NODE_SIZE); 
        mem[q + 1] = mem[p + 1];
        break; 

    default:
        confusion("ext2");
        break;
    }
    return q;
}
            
free_whatsit (p)
    ptr     p;
{ 
    switch (subtype(p)) 
    { 
    case OPEN_NODE: 
        free_node(p, OPEN_NODE_SIZE); 
        break; 
     
    case WRITE_NODE: 
    case SPECIAL_NODE:
        delete_token_ref(write_tokens(p)); 
        free_node(p, WRITE_NODE_SIZE); 
        break; 
     
    case CLOSE_NODE: 
        free_node(p, WRITE_NODE_SIZE); 
        break; 
    
    default:
        confusion("ext3");
        break;
    } 
}


#ifdef INCTEX

out_whatsit (p)
	ptr		p;
{
	short		j;
    
	switch (subtype(p)) {
	case OPEN_NODE:
	case WRITE_NODE:
	case CLOSE_NODE:
		if (!doing_leaders) {
			j = write_stream(p);
			if (subtype(p) == WRITE_NODE)
				out_write(p);
			else {
				if (write_open[j]) {
					a_close(write_file[j]);
					if (incremental)
						wfid[j] = NIL;
				}
				if (subtype(p) == CLOSE_NODE)
					write_open[j] = FALSE;
				else {
					cur_name = open_name(p);
					cur_area = open_area(p);
					cur_ext = open_ext(p);
					if (cur_ext == null_str)
						cur_ext = str_tex;
					if (cur_area == null_str)
						cur_area = job_area;
					pack_cur_name();
					if ((write_file[j] = a_open_out(TRUE)) == NULL) {
						cur_area = null_str;
						pack_cur_name();
						while ((write_file[j] = a_open_out(TRUE)) == NULL)
							prompt_file_name("output file name", str_tex);
					}
					if (incremental)
						wfid[j] = w_end->id;
					write_open[j] = TRUE;
				}
			}
		}
		break;
    
	case SPECIAL_NODE:
		out_special(p);
		break;
	
	default:
		confusion("ext4");
		break;
	}
}

#else

out_whatsit (p)
    ptr     p;
{
    short   j;

    switch (subtype(p))
    {
    case OPEN_NODE:
    case WRITE_NODE:
    case CLOSE_NODE:
        if (!doing_leaders) {
            j = write_stream(p);
            if (subtype(p) == WRITE_NODE)
                out_write(p);
            else {
                if (write_open[j])
                    a_close(write_file[j]);
                if (subtype(p) == CLOSE_NODE)
                    write_open[j] = FALSE;
                else {
                    cur_name = open_name(p);
                    cur_area = open_area(p);
                    cur_ext = open_ext(p);
                    if (cur_ext == null_str)
                        cur_ext = str_tex;
                    if (cur_area == null_str)
                        cur_area = job_area;
                    pack_cur_name();
                    if ((write_file[j] = a_open_out()) == NULL) {
                        cur_area = null_str;
                        pack_cur_name();
                        while ((write_file[j] = a_open_out()) == NULL)
                            prompt_file_name("output file name", str_tex);
                    }
                    write_open[j] = TRUE;
                }
            }
        }
        break;

    case SPECIAL_NODE:
        out_special(p);
        break;
    
    default:
        confusion("ext4");
        break;
    }
}

#endif


new_write (w)
    int     w;
{
    ptr     p;

    new_whatsit(cur_chr, w);
    if (w != WRITE_NODE_SIZE)
        scan_four_bit_int();
    else {
        scan_int(); 
        if (cur_val < 0)
            cur_val = 17;
        else if (cur_val > 15)
            cur_val = 16;
    }
    write_stream(tail) = cur_val;
}

print_write (s, p)
    chrs    s;
    ptr     p;
{
    print_esc(s);
    if (write_stream(p) < 16)
        print_int(write_stream(p));
    else if (write_stream(p) == 16)
        print_char('*');
    else print_char('-');
}

out_write (p)
    ptr     p;
{
    int     j;
    ptr     q;
    ptr     r;
    int     old_mode;
    int     old_setting;

    q = new_token();
    token(q) = RIGHT_BRACE_TOKEN + '}';
    r = new_token();
    token_link(q) = r;
    token(r) = END_WRITE_TOKEN;
    ins_list(q);
    begin_token_list(write_tokens(p), WRITE_TEXT);
    q = new_token();
    token(q) = LEFT_BRACE_TOKEN + '{';
    ins_list(q);
    old_mode = mode;
    mode = 0;
    cur_cs = write_loc;
    scan_toks(FALSE, TRUE);
    get_token();
    if (cur_tok != END_WRITE_TOKEN) {
        print_err("Unbalanced write command");
        help_unbal_write();
        error();
        do get_token();
        while (cur_tok != END_WRITE_TOKEN);
    }
    mode = old_mode;
    end_token_list();
    old_setting = selector;
    j = write_stream(p);
    if (write_open[j])
        selector = j;
    else {
        if (j == 17 && selector == TERM_AND_LOG)
            selector = LOG_ONLY;
        print_nl("");
    }
    show_token_list(token_link(def_ref), NULL, 10000000);
    print_ln();
    flush_list(def_ref);
    selector = old_setting;
}

out_special (p)
    ptr     p;
{
    int     old_setting;
    int     k;

    synch_h();
    synch_v();
    old_setting = selector;
    selector = NEW_STRING;
    show_token_list(token_link(write_tokens(p)),NULL,(val)POOL_SIZE-pool_ptr);
    selector = old_setting;
    str_room(1);
    if (cur_length() < 256) {
        dvi_out(XXX1);
        dvi_out(cur_length());
    } else {
        dvi_out(XXX4);
        dvi_four(cur_length());
    }
    for (k = str_start[str_ptr]; k < pool_ptr; incr(k))
        dvi_out(str_pool[k]);
    pool_ptr = str_start[str_ptr];
}

/*
 *  Help text
 */

help_unbal_write ()
{
    help2("On this page there's a \\write with fewer real {'s than }'s.",
    "I can't handle that very well; good luck.");
}

