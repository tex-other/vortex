/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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


/*
 *    Copyright 1986, 1987 Pat Joseph Monardo. All rights reserved.
 *    Copying of this file is granted according to the provisions 
 *    specified in the file COPYING which must accompany this file.
 */


/*
 *		texext.c
 */

#include		"tex.h"
#include		"cmds.h"
#include		"heap.h"
#include		"eq.h"
#include		"hash.h"
#include		"token.h"
#include		"box.h"
#include		"scan.h"
#include		"def.h"
#include		"tokenstack.h"
#include		"tokenlists.h"
#include		"evalstack.h"
#include		"io.h"
#include		"str.h"
#include		"file.h"
#include		"dvi.h"
#include		"print.h"
#include		"error.h"
#include		"texext.h"

#ifdef VORTEX
#include		"allir.h"
#include		"main.h"

extern struct _cseq	*save_cs_node;
#endif

extern alpha_file	write_file[];

extern bool		write_open[];
extern ptr		write_loc;

do_extension ()
{
	int		i;
	int		j;
	int		k;
	ptr		p;
	ptr		q;
	ptr		r;

	switch (cur_chr) {
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
#ifdef VORTEX		
		new_whatsit(SPECIAL_NODE, SPECIAL_NODE_SIZE, TRUE);
#else
		new_whatsit(SPECIAL_NODE, WRITE_NODE_SIZE);
#endif
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

#ifdef VORTEX
new_whatsit (s, w, special)
	int		s;
	int		w;
	int		special;
{
	ptr		p;

	p = get_node(w);
	type(p) = WHATSIT_NODE;
	subtype(p) = s;
	link(tail) = p;
	tail = p;
	if (special)
		ir_special(p) = (_Node *) save_cs_node;
}
#else
new_whatsit (s, w)
	int		s;
	int		w;
{
	ptr		p;

	p = get_node(w);
	type(p) = WHATSIT_NODE;
	subtype(p) = s;
	link(tail) = p;
	tail = p;
}
#endif

show_whatsit (p)
	ptr		p;
{ 
	switch (subtype(p)) { 
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
		print_int(write_stream(p)); 
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
	ptr		p;
{ 
	ptr		q;

	switch (subtype(p)) { 
	case OPEN_NODE: 
		q = get_node(OPEN_NODE_SIZE); 
		mem[q + 2] = mem[p + 2];
		mem[q + 1] = mem[p + 1];
		break; 
#ifdef VORTEX
	case WRITE_NODE: 
		q = get_node(WRITE_NODE_SIZE); 
		add_token_ref(write_tokens(p)); 
		mem[q + 1] = mem[p + 1];
		break;

	case SPECIAL_NODE:
		q = get_node(SPECIAL_NODE_SIZE); 
		add_token_ref(write_tokens(p)); 
		mem[q + 1] = mem[p + 1];
		ir_special(q) = ir_special(p);
		break;
	
#else
	case WRITE_NODE: 
	case SPECIAL_NODE:
		q = get_node(WRITE_NODE_SIZE); 
		add_token_ref(write_tokens(p)); 
		mem[q + 1] = mem[p + 1];
		break; 
#endif
	 
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
	ptr		p;
{ 
	switch (subtype(p)) { 
	case OPEN_NODE: 
		free_node(p, OPEN_NODE_SIZE); 
		break; 

#ifdef VORTEX	 
	case WRITE_NODE: 
		delete_token_ref(write_tokens(p)); 
		free_node(p, WRITE_NODE_SIZE); 
		break; 

	case SPECIAL_NODE:
		delete_token_ref(write_tokens(p)); 
		free_node(p, SPECIAL_NODE_SIZE); 
		break;

#else
	case WRITE_NODE: 
	case SPECIAL_NODE:
		delete_token_ref(write_tokens(p)); 
		free_node(p, WRITE_NODE_SIZE); 
		break; 
#endif
	 
	case CLOSE_NODE: 
		free_node(p, WRITE_NODE_SIZE); 
		break; 
	
	default:
		confusion("ext3");
		break;
	} 
}

out_whatsit (p)
	ptr		p;
{
	int		j;

	switch (subtype(p)) {
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

new_write (w)
	int		w;
{
#ifdef VORTEX
	new_whatsit(cur_chr, w, FALSE);
#else
	new_whatsit(cur_chr, w);
#endif
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
	char*	s;
	ptr		p;
{
	print_esc(s);
	if (write_stream(p) < 16)
		print_int(write_stream(p));
	else if (write_stream(p) == 16)
		print_char('*');
	else print_char('-');
}

out_write (p)
	ptr		p;
{
	int		j;
	ptr		q;
	ptr		r;
	int		old_mode;
	int		old_setting;

	q = get_avail();
	info(q) = RIGHT_BRACE_TOKEN + '}';
	r = get_avail();
	link(q) = r;
	info(r) = END_WRITE_TOKEN;
	ins_list(q);
	begin_token_list(write_tokens(p), WRITE_TEXT);
	q = get_avail();
	info(q) = LEFT_BRACE_TOKEN + '{';
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
	show_token_list(link(def_ref), NULL, 10000000);
	print_ln();
	flush_list(def_ref);
	selector = old_setting;
}

out_special (p)
	ptr		p;
{
	int		old_setting;
	int		k;

	synch_h();
	synch_v();
	old_setting = selector;
	selector = NEW_STRING;
	show_token_list(link(write_tokens(p)),NULL,(val)POOL_SIZE-pool_ptr);
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
#ifdef VORTEX
	make_special_box(p);
#endif
	pool_ptr = str_start[str_ptr];
}

/*
 *	Help text
 */

help_unbal_write ()
{
	help2("On this page there's a \\write with fewer real {'s than }'s",
	"I can't handle that very well; good luck.");
}
