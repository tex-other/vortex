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
 *		fmt.c
 */

#include "tex.h"
#include "texext.h"
#include "heap.h"
#include "token.h"
#include "eq.h"
#include "box.h"
#include "eqstack.h"
#include "hash.h"
#include "file.h"
#include "tfm.h"
#include "str.h"
#include "hyph.h"
#include "token.h"
#include "io.h"
#include "print.h"
#include "error.h"
#include "fmt.h"

extern word_file	fmt_file;

extern str	format_ident;

#define	undump_ascii() 		(str_pool[k] = getc(fmt_file))
#define	undump_int(M)		M = getw(fmt_file)
#define	undump_wd(M)		fread(&M, sizeof(M), 1, fmt_file);
#define	undump_hh(M)		fread(&M, sizeof(M), 1, fmt_file);
#define	undump_qqqq(M)		fread(&M, sizeof(M), 1, fmt_file);

#define undump(MIN, MAX, X) \
	{undump_int(x); \
	if (x < MIN || x > MAX) \
		goto bad_fmt; \
	else X = x;}

#define	too_small(S) \
	printf("---! Must increase the %s\n", S)

#define	undump_size(MIN, MAX, TABLE, X) \
	{undump_int(x); \
	if (x < MIN) goto bad_fmt; \
	if (x > MAX) too_small(TABLE); \
	else X = x;}

bool
load_fmt_file ()
{
	int		j;
	int		k;
	ptr		p;
	ptr		q;
	int		w;
	int		x;

	undump_int(x);
	if (x != MEM_BOT) goto bad_fmt;
	undump_int(x);
	if (x != MEM_TOP) goto bad_fmt;
	undump_int(x);
	if (x != EQTB_SIZE) goto bad_fmt;
	undump_int(x);
	if (x != HASH_PRIME) goto bad_fmt;
	undump_int(x);
	if (x != HYPH_SIZE) goto bad_fmt;

	undump_int(pool_ptr);
	undump_int(str_ptr);
	undump_int(null_str);
	for (k = 0; k <= str_ptr; incr(k))
		undump(0, pool_ptr, str_start[k]);
	for (k = 0; k < pool_ptr; incr(k))
		undump_ascii();

	undump(LO_MEM_STAT_MAX + 1000, HI_MEM_STAT_MIN - 1, lo_mem_max);
	undump(LO_MEM_STAT_MAX  + 1, lo_mem_max, rover);
	p = MEM_BOT;
	q = rover;
	x = 0;
	do {
		for (k = p; k <= q + 1; incr(k))
			undump_wd(mem[k]);
		p = q + node_size(q);
		if (p > lo_mem_max || q >= rlink(q) && rlink(q) != rover)
				goto bad_fmt;
		q = rlink(q);
	} while (q != rover);
	for (k = p; k <= lo_mem_max; incr(k))
		undump_wd(mem[k]);

	undump(lo_mem_max + 1, HI_MEM_STAT_MIN, hi_mem_min);
	undump(NULL, MEM_TOP, avail);
	mem_end = MEM_TOP;
	for (k = hi_mem_min; k <= mem_end; incr(k))
		undump_wd(mem[k]);

	undump_int(var_used);
	undump_int(dyn_used);

	k = ACTIVE_BASE;
	do {
		undump_int(x);
		if (x < 1 || k + x > EQTB_SIZE + 1)
			goto bad_fmt;
		for (j = k; j < k + x; incr(j))
			undump_wd(eqtb[j]);
		k += x;
		undump_int(x);
		if (x < 0 || k + x > EQTB_SIZE + 1)
			goto bad_fmt;
		for (j = k; j < k + x; incr(j))	
			eqtb[j] = eqtb[k - 1];
		k += x;
	} while (k <= EQTB_SIZE);
	undump(HASH_BASE, FROZEN_CONTROL_SEQUENCE, par_loc);
	par_token = CS_TOKEN_FLAG + par_loc;
	undump(HASH_BASE, FROZEN_CONTROL_SEQUENCE, write_loc);

	undump(HASH_BASE, FROZEN_CONTROL_SEQUENCE, hash_used);
	p = HASH_BASE - 1;
	do {
		undump(p + 1, hash_used, p);
		undump_hh(hash[p]);
	} while (p != hash_used);
	for (p = hash_used + 1; p < UNDEFINED_CONTROL_SEQUENCE; incr(p))
		undump_hh(hash[p]);
	undump_int(cs_count);

	undump_size(7, FONT_MEM_SIZE, "font mem size", fmem_ptr);
	for (k = 0; k < fmem_ptr; incr(k))
		undump_wd(font_info[k]);
	undump_size(FONT_BASE, FONT_MAX, "font max", font_ptr);
	for (k = NULL_FONT; k <= font_ptr; incr(k)) {
		undump_qqqq(font_check[k]);
		undump_wd(font_size[k]);
		undump_wd(font_dsize[k]);
		undump(MIN_HALFWORD, MAX_HALFWORD, font_params[k]);
		undump_int(hyphen_char[k]);
		undump_int(skew_char[k]);
		undump(0, str_ptr, font_name[k]);
		undump(0, str_ptr, font_area[k]);
		undump(0, 255, font_bc[k]);
		undump(0, 255, font_ec[k]);
		undump_int(char_base[k]);
		undump_int(width_base[k]);
		undump_int(height_base[k]);
		undump_int(depth_base[k]);
		undump_int(italic_base[k]);
		undump_int(lig_kern_base[k]);
		undump_int(kern_base[k]);
		undump_int(exten_base[k]);
		undump_int(param_base[k]);
		undump(MIN_HALFWORD, hi_mem_min, font_glue[k]);
	}

	undump(0, HYPH_SIZE, hyph_count);
	for (k = 1; k <= hyph_count; incr(k)) {
		undump(0, HYPH_SIZE, j);
		undump(0, str_ptr, hyph_word[j]);
		undump(MIN_HALFWORD, MAX_HALFWORD, hyph_list[j]);
	}
	undump_size(0, TRIE_SIZE, "trie size", trie_max);
	for (k = 0; k <= trie_max; incr(k))
		undump_hh(trie[k]);
	undump(MIN_QUARTERWORD, MAX_QUARTERWORD, trie_op_ptr);
	for (k = MIN_QUARTERWORD + 1; k <= trie_op_ptr; incr(k))  {
		undump(0, 63, hyf_distance[k]);
		undump(0, 63, hyf_num[k]);
		undump(MIN_QUARTERWORD, MAX_QUARTERWORD, hyf_next[k]);
	}
	undump(BATCH_MODE, ERROR_STOP_MODE, interaction);
	undump(0, str_ptr, format_ident);
	undump_int(x);
	if (x != 6969 || feof(fmt_file)) goto bad_fmt;
	return TRUE;

bad_fmt:
	print("(Fatal format file error; I'm stymied)");
	return FALSE;
}

#ifdef INIT

#define	dump_ascii()	putc(str_pool[k], fmt_file)
#define	dump_int(W)		putw(W, fmt_file)
#define	dump_hh(W)		fwrite(&W, sizeof(W), 1, fmt_file);
#define	dump_wd(W)		fwrite(&W, sizeof(W), 1, fmt_file);
#define	dump_qqqq(W)	fwrite(&W, sizeof(W), 1, fmt_file);

store_fmt_file ()
{
	int		j;
	int		k;
	int		l;
	ptr		p;
	ptr		q;
	fourq	w;
	int		x;

	if (save_ptr != 0) {
		print_nl("! You can't \\dump inside a group");
		help1("`{...\\dump}' is a no-no.");
		succumb();
	}
	selector = NEW_STRING;
	print(" (preloaded format=");
	print_str(job_name);
	print_char(' ');
	print_val(abs(year) % 100);
	print_char('.');
	print_val(month);
	print_char('.');
	print_val(day);
	print_char(')');
	if (interaction == BATCH_MODE)
		selector = LOG_ONLY;
	else selector = TERM_AND_LOG;
	str_room(1);
	format_ident = make_string();
	pack_job_name(str_fmt);
	while ((fmt_file = w_open_out()) == NULL)
		prompt_file_name("format file name", str_fmt);
	print_nl("Beginning to dump on file ");
	print_str(w_make_name_string(fmt_file));
	flush_string();
	print_nl("");
	print_str(format_ident);

	dump_int(MEM_BOT);
	dump_int(MEM_TOP);
	dump_int(EQTB_SIZE);
	dump_int(HASH_PRIME);
	dump_int(HYPH_SIZE);

	dump_int(pool_ptr);
	dump_int(str_ptr);
	dump_int(null_str);
	for (k = 0; k <= str_ptr; incr(k))
		dump_int(str_start[k]);
	for (k = 0; k < pool_ptr; incr(k))
		dump_ascii();
	print_ln();
	print_int(str_ptr);
	print(" strings of total length ");
	print_int(pool_ptr);

	sort_avail();
	var_used = 0;
	dump_int(lo_mem_max);
	dump_int(rover);
	p = MEM_BOT;
	q = rover;
	x = 0;
	do {
		for (k = p; k <= q + 1; incr(k))
			dump_wd(mem[k]);
		x = x + q + 2 - p;
		var_used = var_used + q - p;
		p = q + node_size(q);
		q = rlink(q);
	} while (q != rover);
	var_used = var_used + lo_mem_max - p;
	dyn_used = mem_end + 1 - hi_mem_min;
	for (k = p; k <= lo_mem_max; incr(k))
		dump_wd(mem[k]);
	x = x + lo_mem_max + 1 - p;
	dump_int(hi_mem_min);
	dump_int(avail);
	for (k = hi_mem_min; k <= mem_end; incr(k))
		dump_wd(mem[k]);
	x = x + mem_end + 1 - hi_mem_min;
	for (p = avail; p != NULL; p = link(p))
		decr(dyn_used);
	dump_int(var_used);
	dump_int(dyn_used);
	print_ln();
	print_int(x);
	print(" memory locations dumped; current usage is ");
	print_int(var_used);
	print_char(',');
	print_int(dyn_used);

	k = ACTIVE_BASE;
	do {
		for (j = k; j < INT_BASE - 1; incr(j))
			if (equiv(j) == equiv(j + 1) &&
				eq_type(j) == eq_type(j + 1) &&
				eq_level(j) == eq_level(j + 1))
				goto found1;
		l = INT_BASE;
		goto done1;
	
	found1:
		incr(j);
		l = j;
		for (; j < INT_BASE - 1; incr(j))
			if (equiv(j) != equiv(j + 1) ||
				eq_type(j) != eq_type(j + 1) ||
				eq_level(j) != eq_level(j + 1))
				goto done1;
	
	done1:
		dump_int(l - k);
		for (; k < l; incr(k))
			dump_wd(eqtb[k]);
		k = j + 1;
		dump_int(k - l);
	} while (k != INT_BASE);

	do {
		for (j = k; j < EQTB_SIZE; incr(j))
			if (eqtb[j].i == eqtb[j + 1].i)
				goto found2;
		l = EQTB_SIZE + 1;
		goto done2;

	found2:
		incr(j);
		l = j;
		for (; j < EQTB_SIZE; incr(j))
			if (eqtb[j].i != eqtb[j + 1].i)
				goto done2;
	
	done2:
		dump_int(l - k);
		for (; k < l; incr(k))
			dump_wd(eqtb[k]);
		k = j + 1;
		dump_int(k - l);
	} while (k <= EQTB_SIZE);
	dump_int(par_loc);
	dump_int(write_loc);
	
	dump_int(hash_used);
	cs_count = FROZEN_CONTROL_SEQUENCE - 1 - hash_used;
	for (p = HASH_BASE; p <= hash_used; incr(p))
		if (text(p) != 0) {
			dump_int(p);
			dump_hh(hash[p]);
			incr(cs_count);
		}
	for (p = hash_used + 1; p < UNDEFINED_CONTROL_SEQUENCE; incr(p))
		dump_hh(hash[p]);
	dump_int(cs_count);
	print_ln();
	print_int(cs_count);
	print(" multiletter control sequences");

	dump_int(fmem_ptr);
	for (k = 0; k < fmem_ptr; incr(k))
		dump_wd(font_info[k]);
	dump_int(font_ptr);
	for (k = NULL_FONT; k <= font_ptr; incr(k)) {
		dump_qqqq(font_check[k]);
		dump_wd(font_size[k]);
		dump_wd(font_dsize[k]);
		dump_int(font_params[k]);
		dump_int(hyphen_char[k]);
		dump_int(skew_char[k]);
		dump_int(font_name[k]);
		dump_int(font_area[k]);
		dump_int(font_bc[k]);
		dump_int(font_ec[k]);
		dump_int(char_base[k]);
		dump_int(width_base[k]);
		dump_int(height_base[k]);
		dump_int(depth_base[k]);
		dump_int(italic_base[k]);
		dump_int(lig_kern_base[k]);
		dump_int(kern_base[k]);
		dump_int(exten_base[k]);
		dump_int(param_base[k]);
		dump_int(font_glue[k]);
		print_nl("\\font ");
		print_esc("");
		print_str(font_id_text(k));
		print_char('=');
		print_file_name(font_name[k], font_area[k], null_str);
		if (font_size[k] != font_dsize[k]) {
			print(" at ");
			print_scaled(font_size[k]);
			print("pt");
		}
	}
	print_ln();
	print_int(fmem_ptr - 7);
	print(" words of font info for ");
	print_int(font_ptr - FONT_BASE);
	print(" preloaded font");
	if (font_ptr != FONT_BASE + 1)
		print_char('s');
	dump_int(hyph_count);
	for (k = 0; k <= HYPH_SIZE; incr(k))
		if (hyph_word[k] != 0) {
			dump_int(k);
			dump_int(hyph_word[k]);
			dump_int(hyph_list[k]);
		}
	dump_int(trie_max);
	for (k = 0; k <= trie_max; incr(k))
		dump_hh(trie[k]);
	dump_int(trie_op_ptr);
	for (k = MIN_QUARTERWORD + 1; k <= trie_op_ptr; incr(k)) {
		dump_int(hyf_distance[k]);
		dump_int(hyf_num[k]);
		dump_int(hyf_next[k]);
	}
	print_ln();
	print_int(hyph_count);
	print(" hyphenation exception");
	if (hyph_count != 1)
		print_char('s');
	print_nl("Hyphenation trie of length ");
	print_int(trie_max);
	print(" has ");
	print_int(qo(trie_op_ptr));
	print(" op");
	if (trie_op_ptr != MIN_QUARTERWORD + 1)
		print_char('s');

	dump_int(interaction);
	dump_int(format_ident);
	dump_int(6969);
	w_close(fmt_file);
	tracing_stats = 0;
}
#endif
