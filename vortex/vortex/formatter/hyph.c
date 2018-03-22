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
 *		hyph.c
 */

#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"token.h"
#include	"eq.h"
#include	"str.h"
#include	"tfm.h"
#include	"box.h"
#include	"scan.h"
#include	"tokenstack.h"
#include	"par.h"
#include	"print.h"
#include	"error.h"
#include	"hyph.h"

#ifdef VORTEX
#include	"allir.h"
#include	"main.h"

extern struct _node	*hir[];
#endif

extern ptr	ha;
extern ptr	hb;
extern hword	hc[];
extern fnt	hf;
extern int	hn;
extern ascii	hu[];
extern byte	hyf[];
extern int	hyf_char;
extern int	hyf_distance[];
extern qword	hyf_next[];
extern int	hyf_num[];
extern int	hyph_count;
extern ptr	hyph_list[];
extern str	hyph_word[];
extern int	hyphen_passed;
extern int	trie_max;
extern qword	trie_op_ptr;
extern twoh	trie[];

hyphenate ()
{
	ascii		c;
	int		h;
	int		i;
	int		j;
	str		k;
	int		l;
	ptr		q;
	ptr		r;
	ptr		s;
	int		u;
	qword		v;
	int		z;
	ptr		hyf_node;
	ptr		major_tail;
	ptr		minor_tail;

	for (j = 0; j <= hn; incr(j))
		hyf[j] = 0;
	h = hc[1];
	for (j = 2; j <= hn; incr(j))
		h = (h + h + hc[j])		 % HYPH_SIZE;
	loop {
		k = hyph_word[h];
		if (k == 0)
			goto not_found;
		if (length(k) < hn)
			goto not_found;
		if (length(k) == hn) {
			j = 1;
			u = str_start[k];
			do {
				if (str_pool[u] < hc[j]) goto not_found;
				if (str_pool[u] > hc[j]) goto done;
				incr(u); incr(j);
			} while (j <= hn);
			for (s = hyph_list[h]; s != NULL; s = link(s))
				hyf[info(s)] = 1;
			goto found;
		}

	done:
		if (h > 0)
			decr(h);
		else h = HYPH_SIZE;
	}

not_found:
	hc[0] = 127;
	hc[hn + 1] = 127;
	hc[hn + 2] = 256;
	for (j = 0; j <= hn - 2; incr(j)) {
		z = hc[j];
		l = j;
		while (hc[l] == trie_char(z)) {
			if (trie_op(z) != MIN_QUARTERWORD) {
				v = trie_op(z);
				do {
					i = l - hyf_distance[v];
					if (hyf_num[v] > hyf[i])
						hyf[i] = hyf_num[v];
					v = hyf_next[v];
				} while (v != MIN_QUARTERWORD);
			}
			incr(l);
			z = trie_link(z) + hc[l];
		}
	}

found:
	hyf[1] = 0;
	hyf[hn - 2] = 0;
	hyf[hn - 1] = 0;
	hyf[hn] = 0;

	for (j = 2; j <= hn - 3; incr(j))
		if (odd(hyf[j])) goto found1;
	return;

found1:
	q = link(hb);
	link(hb) = NULL;
	s = cur_p;
	while (link(s) != ha)
		s = link(s);
	link(s) = NULL;
	flush_node_list(ha);
	j = 0;
	do {
		l = j;
		j = reconstitute(j + 1, hn);
		if (hyphen_passed != 0) {
			r = get_node(SMALL_NODE_SIZE);
			link(s) = r;
			link(r) = link(hold_head);
			type(r) = DISC_NODE;
			major_tail = link(hold_head);
			if (link(major_tail) != NULL)
				major_tail = link(major_tail);
			i = hyphen_passed;
			minor_tail = NULL;
			hyf_node = new_character(hf, (ascii) hyf_char);
			if (hyf_node != NULL) {
				incr(i);
				c = hu[i];
				hu[i] = hyf_char;
			}
			do {
				l = reconstitute(l + 1, i);
				if (minor_tail == NULL)
					pre_break(r) = link(hold_head);
				else link(minor_tail) = link(hold_head);
				minor_tail = link(hold_head);
				if (link(minor_tail) != NULL)
					minor_tail = link(minor_tail);
			} while (l != i);
			if (hyf_node != NULL) {
				hu[i] = c;
				free_avail(hyf_node);
				decr(i);
				l = i;
			}
			hyf[i] = 0;
			minor_tail = NULL;
			post_break(r) = NULL;
			while (l < j) {
				do {
					l = reconstitute(l + 1, hn);
					if (minor_tail == NULL)
						post_break(r) = link(hold_head);
					else link(minor_tail) = link(hold_head);
					minor_tail = link(hold_head);
					if (link(minor_tail) != NULL) {
						hyf[l] = 0;
						minor_tail = link(minor_tail);
					}
				} while (l < j);
				while (l > j) {
					j = reconstitute(j + 1, hn);
					link(major_tail) = link(hold_head);
					major_tail = link(hold_head);
					if (link(major_tail) != NULL) {
						hyf[j] = 0;
						major_tail = link(major_tail);
					}
				}
			}
			i = 0; 
			s = r;
			while(link(s) != NULL) {
				incr(i);
				s =  link(s);
			}
			replace_count(r) = i;
		} else {
			link(s) = link(hold_head);
			s = link(s);
			if (link(s) != NULL)
				s = link(s);
		}
		if (odd(hyf[j])) {
			r = new_disc();
			pre_break(r) = new_character(hf, (ascii) hyf_char);
			link(s) = r;
			s = r;
		}
	} while (j != hn);
	link(s) = q;
}
	
reconstitute (j, n)
	int		j;
	int		n;
{
	qword		c;
	qword		d;
	ptr		p;
	fourq		q;
	int		r;
	ptr		s;
	scal		w;

	hyphen_passed = 0;
	s = hold_head;
	w = 0;
	c = d = qi(hu[j]);
	loop {
contin: 	p = get_avail();
		font(p) = hf;
		character(p) = c;
#ifdef VORTEX
		ir_char(p) = hir[j];
#endif
		link(s) = p;
		if (j == n)
			break;
		q = char_info(hf, d);
		if (char_tag(q) != LIG_TAG)
			break;
		r = lig_kern_start(hf, q);
		c = qi(hu[j + 1]);
		loop {
			q = font_info[r].qqqq;
			if (next_char(q) == c) {
				if (odd(hyf[j]) && hyphen_passed == 0)
					hyphen_passed = j;
				if (op_bit(q) < KERN_FLAG) {
					d = rem_byte(q);
					incr(j);
					s = p;
					goto contin;
				} else {
					w = char_kern(hf, q);
					goto done;
				}
			} else if (stop_bit(q) < STOP_FLAG)
				incr(r);
			else goto done;
		}
	}

done:
	if (s != hold_head) {
		p = new_ligature(hf, d, link(hold_head));
		link(hold_head) = p;
	}
	if (w != 0)
		link(link(hold_head)) = new_kern(w);
	return j;
}

new_hyph_exceptions ()
{
	int		h;
	int		j;
	str		k;
	int		n;
	ptr		p;
	ptr		q;
	str		s;
	str		t;
	int		u;
	int		v;

	n = 0;
	p = NULL;
	scan_left_brace();
	loop {
		get_x_token();

	reswitch:
		switch (cur_cmd) {
		case LETTER:
		case OTHER_CHAR:
		case CHAR_GIVEN:
			if (cur_chr == '-') {
				if (n > 1) {
					q = get_avail();
#ifdef VORTEX
					ir_char(q) = ir_char(p);
#endif
					link(q) = p;
					info(q) = n;
					p = q;
				}
			} else {
				if (cur_chr > 127 || lc_code(cur_chr) == 0) {
					print_err("Not a letter");
					help_hyph_lccode();
					error();
				} else if (n < 63) {
					incr(n);
					hc[n] = lc_code(cur_chr) - 1;
				}
			}
			break;
		
		case CHAR_NUM:
			scan_char_num();
			cur_chr = cur_val;
			cur_cmd = CHAR_GIVEN;
			goto reswitch;
			break;

		case SPACER:
		case RIGHT_BRACE:
			if (n > 4) {
				str_room(n);
				h = 0;
				for (j = 1; j <= n; incr(j)) {
					h = (h + h + hc[j]) % HYPH_SIZE;
					append_char(hc[j]);
				}
				s = make_string();
				loop {
					if (p == NULL)
						break;
					if (info(p) < n - 2)
						break;
					q = link(p);
					free_avail(p);
					p = q;
				}
				if (hyph_count == HYPH_SIZE)
					overflow("exception dictionary", HYPH_SIZE);
				incr(hyph_count);
				while (hyph_word[h] != 0) {
					k = hyph_word[h];
					if (length(k) < length(s))
						goto found;
					if (length(k) > length(s))
						goto not_found;
					u = str_start[k];
					v = str_start[s];
					do {
						if (str_pool[u] < str_pool[v])
							goto found;
						if (str_pool[u] > str_pool[v])
							goto not_found;
						incr(u); incr(v);
					} while (u != str_start[k + 1]);

				found:
					q = hyph_list[h];
					hyph_list[h] = p;
					p = q;
					t = hyph_word[h];
					hyph_word[h] = s;
					s = t;
				
			not_found:
					if (h > 0)
						decr(h);
					else h = HYPH_SIZE;
				}
				hyph_word[h] = s;
				hyph_list[h] = p;
			}
			if (cur_cmd == RIGHT_BRACE)
				return;
			n = 0;
			p = NULL;
			break;

		default:
			print_err("Improper ");
			print_esc("hyphenation");
			print(" will be flushed");
			help_hyph();
			error();
			break;
		}
	}
}

/*
 *	Help text
 */

help_hyph_lccode ()
{
	help2("Letters in \\hyphenation words must have \\lccode > 0",
	"Proceed; I'll ignore the character I just read.");
}

help_hyph ()
{
	help2("Hyphenation exceptions must contain only letters",
	"and hyphens. But continue; I'll forgive and forget.");
}

#ifdef INIT

qword	trie_op_hash[TRIE_OP_HASH_SIZE+1];	
qword	trie_op_ptr;

int		trie_hash[TRIE_SIZE+1];
ascii	trie_c[TRIE_SIZE+1];
qword	trie_o[TRIE_SIZE+1];
int		trie_l[TRIE_SIZE+1];
int		trie_r[TRIE_SIZE+1];
int		trie_min;
int		trie_ptr;
bool	trie_taken[TRIE_SIZE+1];

qword
new_trie_op (d, n, v)
	int		d;
	int		n;
	qword	v;
{
	int		h;
	qword	u;

	h = abs(n + 313 * d + 361 * v) % TRIE_OP_HASH_SIZE;
	loop {
		u = trie_op_hash[h];
		if (u == MIN_QUARTERWORD) {
			if (trie_op_ptr == MAX_QUARTERWORD)
				return MIN_QUARTERWORD;
			incr(trie_op_ptr);
			hyf_distance[trie_op_ptr] = d;
			hyf_num[trie_op_ptr] = n;
			hyf_next[trie_op_ptr] = v;
			trie_op_hash[h] = trie_op_ptr;
			return trie_op_ptr;
		}
		if (hyf_distance[u] == d && 
			hyf_num[u] == n &&
			hyf_next[u] == v)
				return u;
		if (h > 0)
			decr(h);
		else h = TRIE_OP_HASH_SIZE;
	}
}
		
trie_node (p)
	int		p;
{
	int		h;
	int		q;

	h = abs(trie_c[p] + 
			1009 * trie_o[p] +
			2718 * trie_l[p] +
			3142 * trie_r[p])
			% TRIE_SIZE;
	loop {
		q = trie_hash[h];
		if (q == 0) {
			trie_hash[h] = p; 
			return p;
		}
		if (trie_c[q] == trie_c[p] &&
			trie_o[q] == trie_o[p] &&
			trie_l[q] == trie_l[p] && 
			trie_r[q] == trie_r[p])
			return q;
		if (h > 0) 
			decr(h);
		else h = TRIE_SIZE;
	}
}

compress_trie (p)
	int		p;
{
	if (p == 0)
		return 0;
	else {
		trie_l[p] = compress_trie(trie_l[p]);
		trie_r[p] = compress_trie(trie_r[p]);
		return (trie_node(p));
	}
}

init_pattern_memory ()
{
	int		h;
	int		p;

	for (h = 0; h <= TRIE_OP_HASH_SIZE; incr(h))
		trie_op_hash[h] = MIN_QUARTERWORD;
	trie_op_ptr = MIN_QUARTERWORD;
	trie_root = 0;
	trie_c[0] = 0;
	trie_ptr = 0;
	for (p = 0; p <= TRIE_SIZE; incr(p))
		trie_hash[p] = 0;
}

init_trie_memory ()
{
	int		p;

	for (p = 0; p <= trie_ptr; incr(p))
		trie_ref[p] = 0;
	trie_max = trie_min = 128;
	trie_link(0) = 1;
	trie_taken[0] = FALSE;
	for (p = 1; p <= 128; p++) {
		trie_back(p) = p - 1;
		trie_link(p) = p + 1;
		trie_taken[p] = FALSE;
	}
}

first_fit (p)
	int		p;
{
	ascii	c;
	int		h;
	int		q;
	int		z;

	c = trie_c[p];
	if (c < trie_min)
		trie_min = c;
	z = trie_link(trie_min - 1);
	loop {
		if (z < c)
			goto not_found;
		h = z - c;
		if (trie_max < h + 128) {
			if (TRIE_SIZE <= h + 128)
				overflow("pattern memory", TRIE_SIZE);
			do {
				incr(trie_max); 
				trie_taken[trie_max] = FALSE;
				trie_link(trie_max) = trie_max + 1;
				trie_back(trie_max) = trie_max - 1;
			} while (trie_max != h + 128);
		}
		if (trie_taken[h])
			goto not_found;
		for (q = trie_r[p]; q > 0; q = trie_r[q])
			if (trie_link(h + trie_c[q]) == 0)
				goto not_found;
		goto found;

	not_found:
		z = trie_link(z);
	}

found:
	trie_taken[h] = TRUE;
	trie_ref[p] = h;
	q = p;
	do {
		z = h + trie_c[q];
		trie_back(trie_link(z)) = trie_back(z);
		trie_link(trie_back(z)) = trie_link(z);
		trie_link(z) = 0;
		q = trie_r[q];
	} while (q != 0);
}

trie_pack (p)
	int		p;
{
	int		q;

	do {	
		q = trie_l[p];
		if (q > 0 && trie_ref[q] == 0) {
			first_fit(q);
			trie_pack(q);
		}
		p = trie_r[p];
	} while (p != 0);
}

trie_fix (p)
	int		p;
{
	ascii	c;
	int		q;
	int		z;

	z = trie_ref[p];
	while (p != 0) {
		q = trie_l[p];
		c = trie_c[p];
		trie_link(z + c) = trie_ref[q];
		trie_char(z + c) = c;
		trie_op(z + c) = trie_o[p];
		if (q > 0)
			trie_fix(q);
		p = trie_r[p];
	}
}

new_patterns ()
{
	ascii	c;
	twoh	h;
	int		k;
	int		l;
	int		p;
	int		q;
	int		r;
	int		s;
	qword	v;
	bool	digit_sensed;
	bool	first_child;

	scan_left_brace();
	init_pattern_memory();
	k = 0;
	hyf[0] = 0;
	digit_sensed = FALSE;
	loop {
		get_x_token();
		switch (cur_cmd)
		{
		case LETTER:
		case OTHER_CHAR:
			if (digit_sensed || cur_chr < '0' || cur_chr > '9') {
				if (cur_chr == '.')
					cur_chr = 128;
				else {
					cur_chr = lc_code(cur_chr);
					if (cur_chr == 0) {
						print_err("Nonletter");
						help1("(See Appendix H.)");
						error();
						cur_chr = 128;
					}
				}
				if (k < 63) {
					incr(k);
					hc[k] = cur_chr - 1;
					hyf[k] = 0;
					digit_sensed = FALSE;
				}
			} else {
				hyf[k] = cur_chr - '0';
				if (k < 63)
					digit_sensed = TRUE;
			}
			break;

		case SPACER:
		case RIGHT_BRACE:
			if (k > 0) {
				if (hc[1] == 127)
					hyf[0] = 0;
				if (hc[k] == 127)
					hyf[k] = 0;
				l = k;
				v = MIN_QUARTERWORD;
				loop {
					if (hyf[l] != 0)
						v = new_trie_op(k - l, hyf[l], v);
					if (l > 0)
						decr(l);
					else break;
				}
				q = 0; 
				while (l < k) {
					incr(l);
					c = hc[l];
					p = trie_l[q];
					first_child = TRUE;
					while (p > 0 && c > trie_c[p]) {
						q = p;
						p = trie_r[q];
						first_child = FALSE;
					}
					if (p == 0 || c < trie_c[p]) {
						if (trie_ptr == TRIE_SIZE)
							overflow("pattern memory", TRIE_SIZE);
						incr(trie_ptr);
						trie_r[trie_ptr] = p;
						p = trie_ptr;
						trie_l[p] = 0;
						if (first_child)
							trie_l[q] = p;
						else trie_r[q] = p;
						trie_c[p] = c;
						trie_o[p] = MIN_QUARTERWORD;
					}
					q = p;
				}
				if (trie_o[q] != MIN_QUARTERWORD) {
					print_err("Duplicate pattern");
					help1("(See Appendix H)");
					error();
				}
				trie_o[q] = v;
			}
			if (cur_cmd == RIGHT_BRACE)
				goto done;
			k = 0;
			hyf[0] = 0;
			digit_sensed = FALSE;
			break;

		default:
			print_err("Bad ");
			print_esc("patterns");
			help1("(See Appendix H)");
			error();
			break;
		}
	}

done:
	trie_root = compress_trie(trie_root);
	init_trie_memory();
	if (trie_root != 0) {
		first_fit(trie_root);
		trie_pack(trie_root);
	}
	r = 0;
	while (trie_taken[r])
		incr(r);
	trie_ref[0] = r;
	trie_fix(trie_root);
	r = 0;
	h.hh2.rh = 0;
	h.hh2.b0 = 0;
	h.hh2.b1 = 0;
	do {
		s = trie_link(r);
		trie[r] = h;
		r = s;
	} while (r <= trie_max);
}
#endif

init_hyph ()
{
#ifdef INIT
	int		k;
	int		z;

	trie_op_ptr = MIN_QUARTERWORD;
	trie_link(0) = 0;
	trie_char(0) = 0;
	trie_op(0) = 0;
	for (k = 1; k <= 127; k++)
		trie[k] = trie[0];
	trie_max = 127;
	for (z = 0; z <= HYPH_SIZE; z++) {
		hyph_word[z] = 0;
		hyph_list[z] = NULL;
	}
	hyph_count = 0;
#endif
}
