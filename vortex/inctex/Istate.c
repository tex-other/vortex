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

#ifdef INCTEX
/*  This file is part of IncTeX 1.0
 *
 *  Copyright (C) 1992 by Regents of the University of California
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 *
 *  Plan: Save state checkpoint as diffs from initial .fmt state, tests show
 *  diff for any other page is minimal except for eqtb, dynamic
 *  mem and token lists.   D. Pan
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	<sys/file.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	"tex.h"
#include	"Imain.h"
#include	"file.h"
#include	"io.h"
#include	"dvi.h"
#include	"heap.h"
#include	"eq.h"
#include	"eqstack.h"
#include	"cond.h"
#include	"box.h"
#include	"texext.h"
#include	"tfm.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"evalstack.h"
#include	"hash.h"
#include	"cmds.h"
#include	"Icmp.h"
#include	"Idebug.h"

char inc_banner[] = "IncTeX, version 1.0";
/* These codes go at start of state file.  First word = state file version,
 * second word = checkpoint type, full, normal incremental, or super-compressed,
 * which means global var section is diff-ed against full checkpt's globals and
 * change list is saved. DLP
 */
char state_vers[] = "IncTeX state format: 120689ab";
#define		StateLayoutVersion	0x120689ab
#define		FullChkPtState		0x0f
#define		IncChkPtState		0x01
#define		CompressChkpt		0x08
#define		DiffListCode		0x88
#define		Array_NoDiff		0x11

#define ULONG unsigned long

ULONG		ChkPtTime  = 0;	/* Time .inc & .str/.font were written. DLP */

int		show_stuff = FALSE;	/* disable dynamic mem traces. DLP */
DIFF_CHUNK	*fdim_list = NULL;	/* list of font_info locs changed */
DIFF_CHUNK	*fdim_end = NULL;	/* last chunk in list */
int		fdim_listlen = 0;
DIFF_HUNK	*eqtb_list = NULL;	/* list of eqtb locs changed */
DIFF_HUNK	*eqtb_end = NULL;
int		eqtb_listlen = 0;
DIFF_HUNK	*hash_list = NULL;	/* hash locs changed. */
DIFF_HUNK	*hash_end = NULL;
int		hash_listlen = 0;
DIFF_HUNK	*xeq_list = NULL;	/* xeq_level locs changed. */
DIFF_HUNK	*xeq_end = NULL;
int		xeq_listlen = 0;
short		*global_chset = NULL,
		*eqtb_chset = NULL,
		*hash_chset = NULL,
		*xeq_chset = NULL;

byte_file	stc_file;	/* state check point file */
byte_file	str_file;	/* string pool file */
byte_file	font_file;	/* font array file */

extern	short	beg_save_globals;
extern	short	end_save_globals;

str     	str_ptr_high;	/* hi water mark for str_ptr */
ptr     	pool_ptr_high;	/* ...& pool_ptr; from append-arrays. DLP */
ptr     	fmem_ptr_high;	/* ...& fmem_ptr */
fnt     	font_ptr_high;	/* ...& font_ptr */
ptr     	fmem_last;	/* ...& fmem_ptr */
fnt     	font_last;	/* ...& font_ptr */
str		str_lowater  = 0;
ptr		pool_lowater = 0;
ptr             fmem_lowater = 0;
fnt             font_lowater = 0;
extern	int	fmem_lastcmp;	/* !! */
extern	int	font_lastcmp;	/* !! */
extern	str	str_lastcmp;	/* !! */
extern	ptr	pool_lastcmp;	/* !! */


int	max_saved_page = 0;

int	size_globals;
int	string_okay;		/* false if str corrupted */
int	string_empty = TRUE;	/* false if str succesfully loaded */
int	globals_pure = FALSE;	/* true if no formatting has been done yet */

#define	SMEM_SIZE	1	/* upper mem[] are 1 mword/1 cell (IncTeX) */

#define	SARRY_SIZE	1000
typedef	struct {		/* dynamic mem scan (save_single) */
		ptr	beg;
		ptr	end;
	} sary;

char	*global_copy = NULL;/* mbuf with copy of globals */

state_version()
{
	return(StateLayoutVersion);
}

#ifdef DEBUG
/* !!! */ extern 	ptr		cur_p;
#endif DEBUG

/* save context
 */
save_state()
{
	struct stat		st;
	char	fname[FILE_NAME_SIZE];

	/* time2 -- beginning of checkpointing;  */
	/* time2 - time1 = processing time excluding checkpointing */

	open_state_file(SAVE, total_pages);
	if (show_state) {
		(void) strcpy(fname, name_of_file);
		print(" {");
		print(fname);
		update_terminal();
	}


	save_state_code(stc_file);
	
	save_globals();	/* save common global var */
	
	save_arrays();	/* save eqtb,hash table,stack,fonts */
	
	save_mems();	/* save mem */

	close_state_file();
	if (total_pages > max_saved_page)
		max_saved_page = total_pages;
	if (show_state) {
#ifdef DEBUG
		dump_inputs();		/* 111 DLP */
#endif DEBUG
		stat(fname, &st);
		print(" ("); print_int(st.st_size); print(" bytes)}");
#ifdef DEBUG
		print_nl("Page hash index calls: "); print_int(hash_count);
		hash_count = 0;
#endif DEBUG
		update_terminal();
	}
#ifdef DEBUG
/* !!! */if (cur_p != 0) {
		print_nl("!! cur_p: ");print_int(cur_p); print_ln();
	}
#endif DEBUG
}

/* restore context
 */
load_state (no)
	int	no;
{
	int	t_tmp,		/* fix for log/terminal position - DLP */
		f_tmp,
		t_old,
		f_old,
		select_old,
		select_tmp,	/* save current output redirection - DLP */
		compressed,
		page;

	print_nl("Loading state after page ");
	print_int(no);
	print(" from file ");
	print(name_of_file);
	print("...");
	update_terminal();

	/* read state version */
	if (!read_state_code(&page, &compressed, stc_file))
		goto LOAD_BAD;
	/* check_state_type(page); OBSOLETE. DLP */

	t_old = term_offset;
	f_old = file_offset;
	select_old = selector;	/* save curr selector setting - DLP */

	if (!load_globals(compressed))
		goto LOAD_BAD;

	t_tmp = term_offset;	/* save log/terminal position from last run */
	f_tmp = file_offset;	/* if load_globals prints, they're off - DLP */
	select_tmp = selector;
	term_offset = t_old;	/* switch settings to curr msgs - DLP */
	file_offset = f_old;
	selector = select_old;

	if (!load_append_only())
		goto LOAD_BAD;

	if (!load_arrays())
		goto LOAD_BAD;

	if (!load_mems())
		goto LOAD_BAD;

	close_state_file();

#ifdef DEBUG
	if (show_state)		/* 111 DLP ***	*/
		{dump_inputs();print_ln();}
#endif DEBUG
	
	print("done"); print_ln();
	update_terminal();
	term_offset = t_tmp;	/* restore last run's settings - DLP */
	file_offset = f_tmp;
	selector = select_tmp;
	return(TRUE);
LOAD_BAD:
	history = FATAL_ERROR_STOP;
	qsc_check = FALSE;
	print("failed"); print_ln();
	update_terminal();
	exit(history);
}


load_append_only()
{
	if (string_okay) {
		if (string_empty) {
			string_empty = FALSE;
			if (! load_appends()) {
				string_okay = FALSE;
				return(FALSE);
			}
		}
		return(TRUE);
	} else
		return(FALSE);
}

/*	write checkpoint layout version (1 word)
 *	and checkpoint file type - full or incremental (1 word).
 *	and curr page count. DLP
 */
save_state_code(chkpt_file)
	byte_file	chkpt_file;
{
	int	code;

	code = StateLayoutVersion;
	fwrite(&code, sizeof(int), 1, chkpt_file);
	if (do_compression)
		code = CompressChkpt;
	else
		code = IncChkPtState;
	fwrite(&code, sizeof(int), 1, chkpt_file);
	fwrite(&total_pages, sizeof(int), 1, chkpt_file);
}

/*	read checkpoint layout version (1 word)
 *	and checkpoint file type - globals compressed (normal)
 *	or only arrays incrementally saved (1 word),
 *	and curr page count. DLP
 */
read_state_code(page, compress, chkpt_file)
	int 		*page,
 			*compress;
	byte_file	chkpt_file;
{
	int	code,
		ccode;
	/* int	i; */

	if (1 > fread(&code,  sizeof(int), 1, chkpt_file)) return(FALSE);
	if (1 > fread(&ccode, sizeof(int), 1, chkpt_file)) return(FALSE);
	if (1 > fread(page,   sizeof(int), 1, chkpt_file)) return(FALSE);
	*compress = ccode == CompressChkpt;
	if( code > StateLayoutVersion) {
		print_nl("Uh oh! Strange (new?) state file format.");
		print_nl("I recommend you reformat from scratch (-v option).");
		return(FALSE);
	} else if( code < StateLayoutVersion) {
		print_nl("Uh oh! Old state file format.");
		print_nl("I recommend you reformat from scratch (-v option).");
		return(FALSE);
	}
	return(TRUE);
}

/*	Open write mode if SAVE mode, else read
 */
open_state_file(md, n)
	int	md;
	int	n;
{
	char	stc_name[FILE_NAME_SIZE];

	if (md == SAVE) {	/* open for save_state() */
		if (qsc_check) {
			get_ext(-1, EXT_LASTSTATE);
			strcpy(stc_name, name_of_file);
			get_ext(n, EXT_STC);
			rename(name_of_file,/* to */ stc_name);
		} else
			get_ext(n, EXT_STC);
		if ((stc_file = fopen(name_of_file, "w")) == NULL)
			goto O_BAD;
	} else {
		/* Open for load_state() --
		 * String pool is not loaded yet, so create .stc file
		 * name from command arg. (from invoke line).
		 * No error msg if not found, might just be scanning for
		 * the nearest checkpoint we can find. 			*/
		sprintf(stc_name, "%s%s%d%s%s", base_fn, EXT_DEL, n,
			EXT_DEL, EXT_STC);
		strcpy(name_of_file, stc_name);
		if ((stc_file = fopen(stc_name, "r")) == NULL) 
			goto O_BAD;
	}
	return(TRUE);
O_BAD:
	print_nl("Failed to open state checkpoint file "); print(name_of_file);
	print(" for page "); print_int(n); print_char('.');
	return(FALSE);
}


/*	close state file
 */
close_state_file()
{
	b_close(stc_file);
}

/*	save context in global variables
 *	do_compression = TRUE if it should save globals as array. DLP
 *	Format: (full checkpoint)   file & term offset + globals + END_MARKER
 *		(compressed chkpt)  file & term offset + diff list + END_MARKER
 *	NOTE: diff list writes addr as offset (16 bits) from beginning byte
 */
save_globals()
{
	ULONG		bg,
			ed,
			offset; /* offset from start of globals */
	F_NODE		*cur_input_fp,
			*input_stack_fp[STACK_SIZE];
	alpha_file      read_file_fp[16];
	int		f_offset,
			t_offset,
			i,
			j,
			*sptr,	/* ptrs to place in globals */
			*cptr;

	
	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;
	globals_pure = FALSE;
	beg_save_globals = 0x5a5a;	/* ensure contents always same. DLP */
	/* Stash then clear .fp pointers in cur_input, input_stack.
	 * They are reallocated and change each run.  It doesn't matter if
	 * file_offset, term_offset vary (probably will) */
	fwrite(&term_offset, sizeof(int), 1, stc_file);
	fwrite(&file_offset, sizeof(int), 1, stc_file);

	cur_input_fp = cur_input.fp;
	cur_input.fp = 0;
	f_offset = file_offset;
	t_offset = term_offset;
	file_offset = 0;
	term_offset = 0;

	/* TeX always checks that max_in_stack/input_ptr != SAVE_STACK
	 * which is the ceiling on the stack */
	for (i = 0; i <= max_in_stack; i++) {
		input_stack_fp[i] = input_stack[i].fp;
		input_stack[i].fp = 0;
	}
	for (i = 0; i < 16; i++) {
		read_file_fp[i] = read_file[i];
		read_file[i] = NULL;
	}
	/* Not doing this screws up check 'cause garbage gets in unused part */
	for (i = strlen(name_of_file)+1;i<FILE_NAME_SIZE;i++)
		name_of_file[i] = '\0'; 	/* clear unused part of name */
	if (!do_compression) {
		fwrite(&beg_save_globals, size_globals, 1, stc_file);
		if (show_state) {
			print_ln();
			print_nl("global memory range: ");
			print_int((ULONG) &beg_save_globals);
			print(" - ");
			print_int((ULONG) &end_save_globals);
			print_nl("saved global vars (bytes): ");
			print_int(size_globals);
		}
	} else {	/* list the diffs from initial copy of globals */
			/* only globals & fonts list absolute addresses */
		if (global_copy == NULL) {
			print_nl("Compressed mode error: no initial globals!");
			history = FATAL_ERROR_STOP; exit(history);
		}
		
		
#ifdef DEBUGLISTGS
		if (show_state) {print_ln();print_nl("Global diff list: ");}
#endif DEBUGLISTGS
		for (	i=j=0,
			sptr=(int *)&beg_save_globals,cptr=(int *)global_copy;
			sptr < (int *) &end_save_globals;
			j++,sptr++,cptr++)
		   if (*sptr != *cptr) {
			fwrite(&sptr, sizeof(intptr), 1, stc_file);
			fwrite(sptr, sizeof(int), 1, stc_file);
			i++;
			if (qsc_check) 		/* flag value changed */
				global_chset[j] = ITEM_CHANGED;
#ifdef DEBUGLISTGS
			if (show_state) {
				print_int((int) sptr);print("==");
				print_int(*sptr);print(", ");
			}
#endif DEBUGLISTGS
		   };
		sptr = END_MARKER;
		fwrite(&sptr, sizeof(intptr), 1, stc_file);
		fwrite(&i,    sizeof(int), 1, stc_file);
		if (show_state) {
			print_nl("");
			print_nl("change list size (bytes) for glob mem: ");
			print_int(sizeof(intptr));print("+");
			print_int(i*(sizeof(intptr) + sizeof(int)));
			print_nl("change list len: "); print_int(i);
		}
	}

	i = END_MARKER;
	fwrite(&i, sizeof(int), 1, stc_file);
		
	/* restore fp's etc. DLP */
	cur_input.fp = cur_input_fp;
	for (i = 0; i <= max_in_stack; i++)
		input_stack[i].fp = input_stack_fp[i];
	for (i = 0; i < 16; i++)
		read_file[i] = read_file_fp[i];
	file_offset = f_offset;
	term_offset = t_offset;
}

/*	restore context in global variables
 *	compressed = TRUE if it should load globals as array. DLP
 */
load_globals(compressed)
	int		compressed;
{
	int		size,
			list_len,
			expected_len,
			f_offset,
			t_offset;
	ULONG		bg,
			ed;
	intptr		iaddr;
	shortptr	saddr;

#define READ1(A,B) if (fread(A, B, 1, stc_file) < 1) goto G_BAD;

	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;
	READ1(&term_offset, sizeof(int));
	READ1(&file_offset, sizeof(int));
	f_offset = file_offset;		/* so trace msgs don't affect it */
	t_offset = term_offset;
	file_offset = term_offset = 0;
	selector = TERM_AND_LOG;	/* why? globals not restored yet! */

	if (!compressed) { 	/* load full copy, then load diff's. DLP */
	  READ1(&beg_save_globals, size_globals);
	  if (show_state) {
	  	print_ln(); print_nl("global memory range: ");
	  	print_int(bg);print(" - ");print_int(ed);
	  	print_nl("read global vars (bytes): ");print_int(size_globals);
	  }
	} else {
	  if (!globals_pure)	/* make globals = init ref state version */
	  	bcopy(global_copy, /* to */ &beg_save_globals, size_globals);

	/* ---------- load diff if compressed = true ------ */
	  list_len = expected_len = size = 0;
#ifdef DEBUGLISTGS
	  if (show_state) print_nl("global diff list: ");
#endif DEBUGLISTGS
	  do {                    /* read list of changed globals */
		READ1(&iaddr, sizeof(intptr));
#ifdef DEBUGLISTGS
		if (show_state) {print_int((uint) iaddr);print("==");}
#endif DEBUGLISTGS
		if (((uint) iaddr) != END_MARKER) {
			if(((unsigned int) iaddr < (unsigned int) bg) ||
				((unsigned int) iaddr > (unsigned int) ed)) {
				print_nl(
				  "Checkpt data error! - global mem address must be between ");
				print_int((int) bg);print(" and ");print_int((int) ed);
				print_nl("Address (thrown away): "); print_int((uint) iaddr);
				READ1(&iaddr, sizeof(int));
				return(FALSE);
			} else
				READ1(iaddr, sizeof(int));
#ifdef DEBUGLISTGS
			if (show_state) {print_int(*iaddr);print(", ");}
#endif DEBUGLISTGS
			size = size + sizeof(intptr) + sizeof(int);
			list_len++;
               	}
       	  } while (((uint) iaddr) != END_MARKER);
	  READ1(&expected_len, sizeof(int));
	  if (expected_len != list_len) {
		print_nl("Error! global list size didn't match size given.");
		return(FALSE);
	  }
	  if (show_state) {
		print_nl("change list size (bytes) for global mem: ");
		print_int(sizeof(intptr)); print("+"); print_int(size);
		print_nl("change list len: "); print_int(list_len);
	  }
	}

	READ1(&expected_len, sizeof(int));
	if (expected_len != END_MARKER) {
		print_nl("Error! No end marker after global var section!");
		return(FALSE);
	}

	file_offset = f_offset;
	term_offset = t_offset;
	return(TRUE);
G_BAD:
	print_nl("State checkpoint file damaged (globals section)!");
	file_offset = f_offset;
	term_offset = t_offset;
	return(FALSE);
}
#undef READ1


/* need new space for list of state changes: get a new blank chunk to use....
   see if there's any chunks to reuse
   if not, allocate one and paste it to end of list.
   (list = forward & backward chained list, end ptrs are NULL. DLP
 */
#define USENEWHUNK(Z) { \
  if ( Z->nxt == NULL ) { \
	GETDIFFHUNK( Z->nxt );	\
	Z->nxt->nxt = NULL; \
	Z->nxt->prev = Z; \
  } \
  Z = Z->nxt; \
  Z->size = 0; \
}

#define USENEWCHUNK(Z) { \
  if ( Z->nxt == NULL ) { \
	GETDIFFCHUNK( Z->nxt );	\
	Z->nxt->nxt = NULL; \
	Z->nxt->prev = Z; \
  } \
  Z = Z->nxt; \
  Z->size = 0; \
}

int	trace_eq	= TRUE,	/* whether to trace eq table changes */
	trace_hash	= TRUE,	/* ...hash table changes */
	trace_xeq	= TRUE,	/* ...xeq_level table changes */
	trace_font	= TRUE;	/* ...font table changes */

int	save_font;
int	save_eq;
int	save_eq2;	/* sometimes have to stack 2 calls! */
hh	save_hash;
qword	save_xeq;

/*	empty out change list (change from full check pt) - just make sizes 0
 */
spill_list(l,e,s)
	DIFF_HUNK	*l,	/* head of list */
		   	**e;	/* ptr TO tail, going to change tail value! */
	int		*s;	/* list size */
{
	*e = l;		/* reset, tail = head */
	while (l != NULL) {
		l->size = 0;
		l = l->nxt;
	}
	*s = 0;		/* size = 0 */
}

spill_clist(l,e,s)
	DIFF_CHUNK	*l,
		   	**e;
	int		*s;
{
	*e = l;
	while (l != NULL) {
		l->size = 0;
		l = l->nxt;
	}
	*s = 0;
}

/*	record abs addr of changed font related word
 *	Note: all other note_...() rtns record array indexes, but
 *	there's just too many font-related arrays
 */
note_font(addr)
	intptr addr;
{
	if (!incremental || !trace_font) return;
	if( fdim_end == NULL ) {
		print_nl("Hey! Serious internal bug! fdim_end = NULL");
		GETDIFFCHUNK( fdim_end );
		fdim_end->nxt = NULL;
		fdim_end->prev = NULL;
		fdim_end->size = 0;
		fdim_list = fdim_end;
	}
	if( fdim_end->size >= DIFF_HUNK_SIZE ) { /* "list-chunk" full? */
		USENEWCHUNK(fdim_end);
		if (qsc_check) {
			USENEWCHUNK( fchset_end );
		}
	}
	fdim_end->listchunk[fdim_end->size] = addr;
	fdim_end->size++;
	fdim_listlen++;
	if (qsc_check) {	/* copy value at addr */
		fchset_end->listchunk[fchset_end->size] = (intptr) ITEM_CHANGED;
		fchset_end->size++;
	}
}

/*	save possibly changed eqtb location, similar to note_font(). DLP.
 */
note_eq(addr)
	int addr;
{	int duplicate;

	if (!incremental || !trace_eq) return;
	if ((addr<0) || (addr>EQTB_SIZE)) {
		print_nl("Hey! changed (eqtb) index "); print_int(addr);
	   	print(" not in allowed range!"); return;
	}
#ifdef DEBUG
	eqtb_diff[addr] = TRUE;		/* 111 for debugging */
#endif DEBUG
	/* check for duplicate - same as tail end? */
	if( eqtb_end == NULL ) {
		print_nl("Hey! Serious internal bug! eqtb_end = NULL");
		GETDIFFHUNK( eqtb_end );
		eqtb_end->nxt = NULL;
		eqtb_end->prev = NULL;
		eqtb_end->size = 0;
		eqtb_list = eqtb_end;
	}
	if (qsc_check) {
		duplicate = FALSE;
		add_set( eqtb_chset, addr, &duplicate );
		if (duplicate)
			return;
	} else if( peeklastchanges(addr,eqtb_end) )
		return;
	if( eqtb_end->size >= DIFF_HUNK_SIZE )  /* "list-chunk" full? */
		USENEWHUNK(eqtb_end);
	eqtb_end->listchunk[eqtb_end->size] = (shortptr) addr;
	eqtb_end->size++;
	eqtb_listlen++;
}


/*	save possibly changed hash location, similar to note_eq. DLP.
 *	hash table entries are never deleted, only changed in hash.c
 *	and one place in def.c by macro font_id_text().
 */
note_hash(addr)
	int addr;
{	int duplicate;
	if (!incremental || !trace_hash) return;
	if ((addr<0) || (addr>UNDEFINED_CONTROL_SEQUENCE)) {
		print_nl("Hey! changed (hash) index "); print_int(addr);
	   	print(" not in allowed range!"); return;
	}
#ifdef DEBUG
	hash_diff[addr] = TRUE;		/* 111 for debugging */
#endif DEBUG
	if( hash_end == NULL ) {
		print_nl("Hey! Serious internal bug! hash_end = NULL");
		GETDIFFHUNK( hash_end );
		hash_end->nxt = NULL;
		hash_end->prev = NULL;
		hash_end->size = 0;
		hash_list = hash_end;
	}
	if (qsc_check) {
		duplicate = FALSE;
		add_set( hash_chset, addr, &duplicate );
		if (duplicate)
			return;
	} else if( peeklastchanges(addr,hash_end) )
		return;
	if( hash_end->size >= DIFF_HUNK_SIZE )  /* "list-chunk" full? */
		USENEWHUNK(hash_end);
	hash_end->listchunk[hash_end->size] = (shortptr) addr;
	hash_end->size++;
	hash_listlen++;
}

/*	save possibly different xeq_level entry. DLP
 */
note_xeq(addr)
	int	addr;
{	int duplicate;
	if (!incremental || !trace_xeq) return;
	if ((addr<0) || (addr>=EQTB_SIZE+1-INT_BASE)) {
		print_nl("Hey! changed (xeq_level) index "); print_int(addr);
	   	print(" not in allowed range!"); return;
	}
#ifdef DEBUG
	xeq_diff[addr] = TRUE;		/* 111 for debugging */
#endif DEBUG
	if( xeq_end == NULL ) {
		print_nl("Hey! Serious internal bug! xeq_end = NULL");
		GETDIFFHUNK( xeq_end );
		xeq_end->nxt = NULL;
		xeq_end->prev = NULL;
		xeq_end->size = 0;
		xeq_list = xeq_end;
	}
	if (qsc_check) {
		duplicate = FALSE;
		add_set( xeq_chset, addr, &duplicate );
		if (duplicate)
			return;
	} else if( peeklastchanges(addr,xeq_end) )
		return;
	if( xeq_end->size >= DIFF_HUNK_SIZE )  /* "list-chunk" full? */
		USENEWHUNK(xeq_end);
	xeq_end->listchunk[xeq_end->size] = (shortptr) addr;
	xeq_end->size++;
	xeq_listlen++;
}

#define SET_ELEMBAD -2
#define SET_CHECKBAD -1
#define ITEM_CLEAR 0
#define ITEM_CHANGED 1
#define ITEM_EQUIV_CURR 2
#define ITEM_EQUIV_INIT 3
#define ITEM_UNEQUIV_CURR 4
#define ITEM_UNEQUIV_INIT 5
#define ITEM_MAXVAL ITEM_UNEQUIV_INIT

create_set( ptrmyset, slimit )
	short **ptrmyset;
	int   slimit;
{
	MALLOC(*ptrmyset,  short, slimit*sizeof(short));
	bzero(*ptrmyset,slimit*sizeof(short));
}

create_quiesc_sets()
{
	ULONG		bg,
			ed;
	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;
	create_set(&global_chset,size_globals/sizeof(int)+1);
	create_set(&eqtb_chset,  EQTB_SIZE+1 );
	create_set(&hash_chset,  UNDEFINED_CONTROL_SEQUENCE+1 );
	create_set(&xeq_chset,   EQTB_SIZE+1 - INT_BASE );
	GETDIFFCHUNK(fchset);
	fchset_end = fchset;
	fchset_end->nxt = fchset_end->prev = NULL;
	fchset_end->size = 0;
}

clear_set( myset, slimit )
	short myset[];
	int   slimit;
{
	bzero(myset,slimit*sizeof(short));
}

add_set( myset, addr, duplicate )
	short myset[];
	int addr;
	int *duplicate;
{	/* no limit check for now, note_eqtb/hash/xeq all do own checks. */
	if (addr<0) { fprintf(stderr,"\nError in add_set!\n"); return; }
	if (myset[addr] > ITEM_CLEAR) (*duplicate) = TRUE;
	myset[addr] = ITEM_CHANGED;
}

int
check_set_item( myset, addr, slimit )
	short myset[];
	int addr,
	    slimit;
{	int retval;

	if (addr>=slimit)
		return( SET_ELEMBAD );
	else if ((myset[addr]<ITEM_CLEAR) || (myset[addr]>ITEM_MAXVAL))
		return( SET_CHECKBAD );
	else return( myset[addr] );
}

int
reset( myset, slimit, setname )
	short myset[];
	int slimit;
{	int i;

	for (i=0;i<slimit;i++)
	  if (myset[i]<ITEM_CLEAR || myset[i]>ITEM_MAXVAL) {
		print_ln("!!");print(setname);print(" change set entry ");
		print_int(i);print(" bad");myset[i] = ITEM_CLEAR;
	  } else
		switch (myset[i]) {
		case ITEM_CLEAR:
		case ITEM_CHANGED:	break;
		case ITEM_UNEQUIV_CURR:
		case ITEM_EQUIV_CURR:   myset[i] = ITEM_CHANGED; break;
		case ITEM_UNEQUIV_INIT:
		case ITEM_EQUIV_INIT:   myset[i] = ITEM_CLEAR; break;
		}
}

int
check_and_reset( myset, slimit, setname )
	short myset[];
	int slimit;
	char *setname;
{	int i, ok;

	ok = TRUE;
	for (i=0;i<slimit;i++) {
	  if (myset[i]<ITEM_CLEAR || myset[i]>ITEM_MAXVAL) {
		print_ln("!!");print(setname);print(" change set entry ");
		print_int(i);print(" bad");ok = FALSE;myset[i] = ITEM_CLEAR;
	  } else {
		switch (myset[i]) {
		case ITEM_CLEAR:	break;
		case ITEM_CHANGED:	ok = FALSE;print_nl("!!");
					print(setname);print(" entry ");
					print_int(i);print(" never checked");
					/* could try to compare it against
					   the initial reference state value
					   if they have been copied.  For now,
					   this is not done for any structure */
					break;
		case ITEM_UNEQUIV_CURR:
		case ITEM_EQUIV_CURR:   myset[i] = ITEM_CHANGED;
					break;
		case ITEM_UNEQUIV_INIT:
		case ITEM_EQUIV_INIT:   myset[i] = ITEM_CLEAR;
					break;
		}
	  }
	}
	return(ok);
}

int
checkglob_and_reset( myset, slimit, setname )
	short myset[];
	int slimit;
	char *setname;
{	int i, ok, *sptr, *cptr;

	ok = TRUE;
	for (i=0, sptr = (int *)&beg_save_globals, cptr = (int *)global_copy;
	     sptr < (int *) &end_save_globals;
	     i++,sptr++,cptr++) {
	     /* !! i=i+sizeof(int),sptr++,cptr++) { */
	  if (myset[i]<ITEM_CLEAR || myset[i]>ITEM_MAXVAL) {
		fprintf(stderr,"%s change set entry %d bad\n",i);
		myset[i] = ITEM_CLEAR;
	  } else {
		switch (myset[i]) {
		case ITEM_CHANGED:	if (*sptr != *cptr) {
					  ok = FALSE;
					  if (TALK) {
						print_nl("Unchecked glob at ");
						print_int((uint) sptr);
						print(" is different.");
					  }
					}
					myset[i] = ITEM_CLEAR;	/* then...*/
		case ITEM_CLEAR:	break;
		case ITEM_UNEQUIV_INIT:
		case ITEM_UNEQUIV_CURR: ok = ok && FALSE;	/* then... */
		case ITEM_EQUIV_INIT:
		case ITEM_EQUIV_CURR:   myset[i] = ITEM_CLEAR;
					break;
		}
	  }
	}
	return(ok);
}

#define PEEKBACK 64

/* look at last few PEEKBACK change addr's in HUNK-size list for duplicates,
 * TRUE if addr is duplicate, else FALSE. DLP
 */
int
peeklastchanges(addr,end)
	int addr;
	DIFF_HUNK *end;
{
	int	cnt,
		i,
		lim;

	cnt = PEEKBACK;
	while ((end != NULL) && (cnt>0)) {	/* scan back from end */
		if( end->size <= cnt)		/* OK to scan whole chunk? */
			lim = 0;
		else
			lim = end->size - cnt;
		for (i=(end->size)-1;i>=lim;i--, cnt--)
			if( (int) (end->listchunk[i]) == addr) return(TRUE);
		end = end->prev;
    	}
    	return(FALSE);	/* well, didn't find it yet. */
}

/* save info appended to auxiliary font arrays.
 * changes via font_dimen are logged differently
 * via note_font() diff lists. DLP
 */
save_aux(size, hiwater, end, aux_file)
	int *size;
	fnt hiwater,
	    end;
	byte_file aux_file;
{
	int a;
	fnt segment;

	/* save static font info from font_lowater+1...font_ptr. DLP */
	segment = hiwater - end;		/* range to be saved */
	if (segment>0) {
	  /* Save info added to end of font-related arrays when font_ptr grows
	     font_used changes frequently when font_ptr does not,
	     and is saved in global var section! */
	  a = ftell(aux_file);
	  fwrite(&font_check[end+1],sizeof(qqqq) *segment, 1,aux_file);
	  fwrite(&font_size[end+1], sizeof(scal) *segment, 1,aux_file);
	  fwrite(&font_dsize[end+1],sizeof(scal) *segment, 1,aux_file);
	  fwrite(&font_params[end+1],sizeof(hword)*segment,1,aux_file);
	  fwrite(&font_name[end+1], sizeof(str)  *segment, 1,aux_file);
	  fwrite(&font_area[end+1], sizeof(str)  *segment, 1,aux_file);
	  fwrite(&font_bc[end+1],   sizeof(byte) *segment, 1,aux_file);
	  fwrite(&font_ec[end+1],   sizeof(byte) *segment, 1,aux_file);
/*****		font_used is kept in globals for now. DLP */
/*****	  fwrite(&font_used[end+1], sizeof(bool) *segment, 1,aux_file); */
	  fwrite(&font_glue[end+1], sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&hyphen_char[end+1],sizeof(int) *segment, 1,aux_file);
	  fwrite(&skew_char[end+1], sizeof(int)  *segment, 1,aux_file);
	  fwrite(&char_base[end+1], sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&width_base[end+1],sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&height_base[end+1],sizeof(ptr) *segment, 1,aux_file);
	  fwrite(&depth_base[end+1],sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&italic_base[end+1],sizeof(ptr) *segment, 1,aux_file);
	  fwrite(&lig_kern_base[end+1],sizeof(ptr)*segment,1,aux_file);
	  fwrite(&kern_base[end+1], sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&exten_base[end+1],sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&param_base[end+1],sizeof(ptr)  *segment, 1,aux_file);
	  *size = ftell(aux_file)-a;
	} else size = 0;
}

/*	load font aux arrays from font_sofar+1 to font_high
 */

load_aux(size_aux,font_high,font_sofar,datafile)
	int		*size_aux;
	fnt 		font_high;
	fnt		font_sofar;
	byte_file	datafile;
{
	fnt 		font_seg;

#define load_aux_seg(aux,elem,delta,file) \
	if (fread(&aux[font_sofar+1],sizeof(elem)*delta,1,file)<1) \
	   return(FALSE); \
	*size_aux += sizeof(elem)*delta

	font_seg = font_high - font_sofar;
	if( font_seg <=0 ) return(TRUE);
	load_aux_seg(font_check, qqqq, font_seg, datafile);
	load_aux_seg(font_size,  scal, font_seg, datafile);
	load_aux_seg(font_dsize, scal, font_seg, datafile);
	load_aux_seg(font_params,hword,font_seg, datafile);
	load_aux_seg(font_name,  str,  font_seg, datafile);
	load_aux_seg(font_area,  str,  font_seg, datafile);
	load_aux_seg(font_bc,    byte, font_seg, datafile);
	load_aux_seg(font_ec,    byte, font_seg, datafile);
/****	   font_used is saved in globals. DLP */
/****	load_aux_seg(font_used,  bool, font_seg, datafile); */
	load_aux_seg(font_glue,  ptr,  font_seg, datafile);
	load_aux_seg(hyphen_char,int,  font_seg, datafile);
	load_aux_seg(skew_char,  int,  font_seg, datafile);
	load_aux_seg(char_base,  ptr,  font_seg, datafile);
	load_aux_seg(width_base, ptr,  font_seg, datafile);
	load_aux_seg(height_base,ptr,  font_seg, datafile);
	load_aux_seg(depth_base, ptr,  font_seg, datafile);
	load_aux_seg(italic_base,ptr,  font_seg, datafile);
	load_aux_seg(lig_kern_base,ptr,font_seg, datafile);
	load_aux_seg(kern_base,  ptr,  font_seg, datafile);
	load_aux_seg(exten_base, ptr,  font_seg, datafile);
	load_aux_seg(param_base, ptr,  font_seg, datafile);
	return(TRUE);
}
#undef load_aux_seg

extern	ptr	font_glue_start[FONT_MAX];
/* Establish initial reference state point for checkpoint differencing.
 * Set lowater marks for str pool & font array segments that
 * are initialized by .fmt files.  Make copy of global vars
 * to do a diff against.
 */
set_init_ref_state()
{
	ULONG		bg,
			ed;
	int		i;
	F_NODE		*cur_input_fp,
			*input_stack_fp[STACK_SIZE];
	alpha_file      read_file_fp[16];

	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;

	fmem_lastcmp = fmem_lowater = fmem_last = fmem_ptr;
	font_lastcmp = font_lowater = font_last = font_ptr;
	pool_lastcmp = pool_lowater = pool_ptr;
	str_lastcmp  = str_lowater  = str_ptr;

	if (show_state) {
		print_nl("Premac high = ");  print_int(premac_hi);
		print_nl("Premac low = ");   print_int(premac_lo);
		print_nl("Str_pool low = "); print_int(pool_lowater);
		print_nl("Str_start low = ");print_int(str_lowater);
		print_nl("fmem_lowater = "); print_int(fmem_lowater);
		print_nl("font_lowater = "); print_int(font_lowater);
	  	print_nl("global memory range: ");
	  	print_int(bg);print(" - ");print_int(ed);
	}
	/* make copy of globals, but zero out file ptrs because
	   they change from run to run, make 1st addr constant value
	   so word 0 is never different in globals, that way an address
	   of 0 in global diffs means end of list. DLP */

	if (do_compression) {
		if (global_copy == NULL)
			MALLOC(global_copy, char, size_globals+20); /* !!! */
		if (global_copy == NULL) {
			print_nl("Error, couldn't allocate ");
			print_int(size_globals);
			print(" bytes of dynamic mem for globals.");
			update_terminal();
			history = FATAL_ERROR_STOP; exit(history);
		}
		cur_input_fp = cur_input.fp;	/* fp are MALLOC ptrs */
		for (i = 0; i < input_ptr; i++)
			input_stack_fp[i] = input_stack[i].fp;
		for (i = 0; i < 16; i++) {
			read_file_fp[i] = read_file[i];
			read_file[i]= NULL;
		}
		cur_input.fp = 0;
		for (i = 0; i < STACK_SIZE; i++)
			input_stack[i].fp = 0;
		for (i = strlen(name_of_file)+1;i<FILE_NAME_SIZE;i++)
			name_of_file[i] = '\0';/* clear unused part of name */
		beg_save_globals = 0x5a5a;  /* ensure 1st addr always same. */
		bcopy(&beg_save_globals, global_copy, size_globals);
		globals_pure = TRUE;
		/* restore fp's etc. DLP */
		cur_input.fp = cur_input_fp;
		for (i = 0; i < input_ptr; i++)
			input_stack[i].fp = input_stack_fp[i];
		for (i = 0; i < 16; i++)
			read_file[i] = read_file_fp[i];
	}
	bcopy(&font_glue[0], &font_glue_start[0], sizeof(font_glue_start));
#ifdef DEBUG
	if (show_state) dump_inputs();		 /*111 DLP */
#endif DEBUG
}

/*  save font segment from _.last mark to _.ptr mark in font_file. DLP
 */
save_fonts(fmem_ptr,fmem_last,font_ptr,font_last)
	ptr		fmem_ptr;
	ptr		fmem_last;
	fnt		font_ptr;
	fnt		font_last;
{
	ptr		fmem_seg;
	fnt		font_seg;
	int		size_aux = 0;

	fmem_seg = fmem_ptr - fmem_last;	/* fmem_ptr = first free loc */
	font_seg = font_ptr - font_last;	/* but font_ptr = first USED */

	if ((fmem_seg > 0) || (font_seg > 0)) {
		fwrite(&fmem_last, sizeof(ptr), 1, font_file);
		fwrite(&fmem_seg,  sizeof(ptr), 1, font_file);
		if (fmem_seg > 0)
			fwrite(&font_info[fmem_last],
				sizeof(mword)*fmem_seg,1,font_file);
		fwrite(&font_last, sizeof(fnt), 1, font_file);
		fwrite(&font_seg,  sizeof(fnt), 1, font_file);
		if (font_seg > 0)
			save_aux(&size_aux,font_ptr,font_last,font_file);
	}
	if (show_state) {
		print_nl("saved page font_info segment: ");
		print_int(2*sizeof(ptr));
		print("+");		print_int(sizeof(mword)*fmem_seg);
		print(" high = ");	print_int(fmem_ptr);
		print(" low = ");	print_int(fmem_last);
		print_nl("saved page aux font segment: ");
		print_int(2*sizeof(fnt));
		print("+");		print_int(size_aux);
		print(" high = ");	print_int(font_ptr);
		print(" low = ");	print_int(font_last);
	}
}

/*	save context in separate arrays
 *	checkpoint structure	(FC = Full Checkpoint) (INCR = Incremental)
 *	--------------------	-------------------------------------------
 *	- fmem limits		(curr - lowater)
 *	  aux font limits	(curr - lowater)
 *	  string pool limits	(curr - lowater)
 *	- save_stack size
 *	  save_stack		(full stack)
 *	- font save code	(diff list or array)
 *	  font diff list/array	(always diff list)
 *	- eqtb save code	(diff list or array)
 *	  eqtb diff list/array
 *	- hash save code
 *	  hash diff list/array
 *	- xeq_level save code
 *	  xeq_level diff list/array
 *
 *      sections marked by - are separated by END_MARKER code
 *
 *  Hash changes are estimated by tracing hash table lookups.
 *  There are lots on page 1 (style file control seq def's?), and
 *  so far it seems safe to throw away the hash table references
 *  on page 1 and just accumulate all lookups from page 2 on.
 *  Lists are accumulated until they take more room than just
 *  saving the whole array, which happens often with xeq_level,
 *  sometimes with eqtb, rarely with hash, not even anticipated for
 *  fonts.
 */
save_arrays()
{
	int		size_aux;
	int		i;
	int		len;
	intptr		iptr;
	DIFF_HUNK	*pt;
	DIFF_CHUNK	*ct;

#ifdef DEBUG
	compare_eqtb();		/* check didn't miss any eqtb changes. */
	compare_hash();		/* ... hash changes. */
	compare_xeq();		/* ... xeq_level changes. */
/****	compare_aux();****/	/* ... font arrays */
#endif DEBUG

/* _____Fonts ... save in *** append only *** file_____ */
	save_fonts(fmem_ptr,fmem_last,font_ptr,font_last);

	fwrite(&fmem_ptr, sizeof(ptr), 1, stc_file);	/* save marks in .stc */
	fwrite(&font_ptr, sizeof(fnt), 1, stc_file);

	fmem_last = fmem_ptr;		/* new chkpt water mark */
	font_last = font_ptr;

/* _____String Pool - write current limits_____ */
	fwrite(&pool_ptr,     sizeof(ptr), 1, stc_file);
	fwrite(&pool_lowater, sizeof(ptr), 1, stc_file);
	fwrite(& str_ptr,     sizeof(str), 1, stc_file);
	fwrite(& str_lowater, sizeof(str), 1, stc_file);
	if (show_state) {
		print_nl("Str_pool high = "); print_int(pool_ptr);
		print(" low = "); print_int(pool_lowater);
		print(" (bytes): ");
		print_int(sizeof(ascii)*(pool_ptr+1-pool_lowater));
		print_nl("Str_start high = "); print_int(str_ptr);
		print(" low = "); print_int(str_lowater);
		print(" (bytes): ");
		print_int(sizeof(ptr)*(str_ptr+1-str_lowater));
	}
	iptr = END_MARKER;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);

/* _____Save Stack - array limit = SAVE_SIZE_____ */
	fwrite(&save_ptr,sizeof(ptr),1,stc_file);
	fwrite(&save_stack[0],sizeof(mword)*save_ptr,1,stc_file);
	if (show_state) {
		print_nl("Save_stack size (bytes): ");
		print_int(sizeof(mword)*save_ptr); print(" Used up ");
		print_int(save_ptr); print(" of "); print_int(SAVE_SIZE);
	}
	iptr = END_MARKER;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);

/* _____Font diff list_____ Only globals & font lists are abs addresses */
	i = DiffListCode;	/* Fonts are ALWAYS saved as diff lists */
	fwrite(&i, sizeof(int), 1, stc_file);
	size_aux = len = 0;
	ct = fdim_list;
#ifdef DEBUG
	if (show_state) {
		print_nl("font mem change list (exp. size=");
		print_int(fdim_listlen); print("): ");
	}
#endif DEBUG
#define LISTITEMCOST (sizeof(intptr) + sizeof(int))
	fwrite(&fdim_listlen, sizeof(int), 1, stc_file);
	while (ct != NULL) {	/* save list of changed font dimensions */
		for (i=0;i<ct->size;i++) {
#ifdef DEBUG
			if (show_state){print_int(ct->listchunk[i]);print(" ");}
#endif DEBUG
			fwrite(&(ct->listchunk[i]), sizeof(intptr), 1,stc_file);
			fwrite(  ct->listchunk[i],  sizeof(int),    1,stc_file);
			size_aux += LISTITEMCOST;
		}
		ct = ct->nxt;
	}
	len = size_aux / LISTITEMCOST;
	if (show_state || (len != fdim_listlen)) {
		print_nl("change list size (bytes) for font mem: ");
		print_int(sizeof(intptr));print("+");print_int(size_aux);
		print(" len = "); print_int(len);
	}
	if (len != fdim_listlen) {
		print_nl("Internal Error! predicted len: ");
		print_int(fdim_listlen); print(" doesn't match count, page ");
		print_int(total_pages);
		fdim_listlen = len;
	}
	iptr = END_MARKER;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);

/* _____Eqtb_____ */
#undef  LISTITEMCOST
#define LISTITEMCOST (sizeof(shortptr) + sizeof(mword))
	if ((!trace_eq) || (eqtb_listlen >=
		(EQTB_SIZE*sizeof(mword)/LISTITEMCOST))) {
#ifdef DEBUG
	  if (trace_eq) print_nl("eqtb tracing stopped -- length=array size");
#endif DEBUG
	  trace_eq = FALSE;
	  i = Array_NoDiff;
	  fwrite(&i, sizeof(int), 1, stc_file);
	  fwrite(&eqtb[0], sizeof(mword)*(EQTB_SIZE+1), 1, stc_file);
	  if (show_state) {
		print_nl("eqtb (array, not diff list) saved (bytes): ");
		print_int(sizeof(mword)*(EQTB_SIZE+1));
	  }
	} else {
	  i = DiffListCode;
	  fwrite(&i, sizeof(int), 1, stc_file);
	  size_aux = len = 0;
	  pt = eqtb_list;
	  fwrite(&eqtb_listlen, sizeof(int), 1, stc_file);
#ifdef DEBUG
	  if (show_state) {
		print_nl("eq change list (exp. size=");
		print_int(eqtb_listlen); print("): ");
	}
#endif DEBUG
	while (pt != NULL) {	/* save list of changed eqtb entries */
		for (i=0;i<pt->size;i++) {
#ifdef DEBUGLIST
			if (show_state){print_int(pt->listchunk[i]);print(" ");}
#endif DEBUGLIST
			fwrite(&(pt->listchunk[i]),sizeof(shortptr),1,stc_file);
			fwrite(&eqtb[pt->listchunk[i]],
				sizeof(mword),1,stc_file);
			size_aux += LISTITEMCOST;
		}
		pt = pt->nxt;
	  }
	  len = size_aux / LISTITEMCOST;
	  if (show_state || (len != eqtb_listlen)) {
		print_nl("change list size (bytes) for eqtb: ");
		print_int(sizeof(intptr));print("+");print_int(size_aux);
		print(" len="); print_int(len);
	  	print(", array="); print_int(EQTB_SIZE);
	  }
	  if (len != eqtb_listlen) {
		print_nl("Internal Error! predicted len: ");
		print_int(eqtb_listlen); print(" doesn't match count, page ");
		print_int(total_pages);
		eqtb_listlen = len;
	  }
	}
	iptr = END_MARKER;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);

/* _____Hash table_____ */
#undef  LISTITEMCOST
#define LISTITEMCOST (sizeof(shortptr) + sizeof(hh))
	if ((!trace_hash) || (hash_listlen >=
		(UNDEFINED_CONTROL_SEQUENCE*sizeof(hh)/LISTITEMCOST))) {
#ifdef DEBUG
	  if (trace_hash) print_nl("hash tracing stopped -- length=array size");
#endif DEBUG
	  trace_hash = FALSE;
	  i = Array_NoDiff;
	  fwrite(&i, sizeof(int), 1, stc_file);
	  fwrite(&hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1),
		1, stc_file);
	  if (show_state) {
		print_nl("hash (array, not diff list) saved (bytes): ");
		print_int(sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
		print(", hash_used = ");
		print_int(hash_used);
	  }
	} else {
	  i = DiffListCode;
	  fwrite(&i, sizeof(int), 1, stc_file);
	  size_aux = len = 0;
	  pt = hash_list;
#ifdef DEBUGLIST
	  if (show_state) {
		print_nl("hash change list (exp. size=");
		print_int(hash_listlen); print("): ");
	  }
#endif DEBUGLIST
	  fwrite(&hash_listlen, sizeof(int), 1, stc_file);
	  while (pt != NULL) {	/* save list of changed hash entries */
		for (i=0;i<pt->size;i++) {
#ifdef DEBUGLIST
			if (show_state)
				{print_int(pt->listchunk[i]);print("_ ");}
#endif DEBUGLIST
			fwrite(&(pt->listchunk[i]),sizeof(shortptr),1,stc_file);
			fwrite(&hash[pt->listchunk[i]],
				sizeof(hh),1,stc_file);
			size_aux += LISTITEMCOST;
		}
		pt = pt->nxt;
	  }
	  len = size_aux / LISTITEMCOST;
	  if (show_state || (len != hash_listlen)) {
		print_nl("change list size (bytes) for hash: ");
		print_int(sizeof(intptr));print("+");print_int(size_aux);
		print(" len="); print_int(len);
	  	print(", array="); print_int(UNDEFINED_CONTROL_SEQUENCE);
	  }
	  if (len != hash_listlen) {
		print_nl("Internal Error! predicted len: ");
		print_int(hash_listlen); print(" doesn't match count, page ");
		print_int(total_pages);
		hash_listlen = len;
	  }
	}
	iptr = END_MARKER;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);

/* _____Xeq_level table_____ */
#undef  LISTITEMCOST
#define LISTITEMCOST (sizeof(shortptr) + sizeof(qword))
	if ((!trace_xeq) || (xeq_listlen >=
	  	((EQTB_SIZE+1 - INT_BASE)*sizeof(qword)/LISTITEMCOST))) {
#ifdef DEBUG
	  if (trace_xeq) print_nl("xeq tracing stopped -- length=array size");
#endif DEBUG
	  trace_xeq = FALSE;
	  i = Array_NoDiff;
	  fwrite(&i, sizeof(int), 1, stc_file);
	  fwrite(&xeq_level[0], sizeof(qword)*(EQTB_SIZE+1 - INT_BASE),
		1, stc_file);
	  if (show_state) {
		print_nl("xeq_level (array, not diff list) saved (bytes): ");
		print_int(sizeof(qword)*(EQTB_SIZE+1 - INT_BASE));
	  }
	} else {
	  i = DiffListCode;
	  fwrite(&i, sizeof(int), 1, stc_file);
	  size_aux = len = 0;
	  pt = xeq_list;
#ifdef DEBUGLIST
	  if (show_state) {
		print_nl("xeq_level change list (exp. size=");
		print_int(xeq_listlen); print("): ");
	  }
#endif DEBUGLIST
	  fwrite(&xeq_listlen, sizeof(int), 1, stc_file);
	  while (pt != NULL) {	/* save list of changed xeq entries */
		for (i=0;i<pt->size;i++) {
#ifdef DEBUGLIST
			if (show_state) {print_int(pt->listchunk[i]);print("_ ");}
#endif DEBUGLIST
			fwrite(&(pt->listchunk[i]),sizeof(shortptr),1,stc_file);
			fwrite(&xeq_level[pt->listchunk[i]],
				sizeof(qword),1,stc_file);
			size_aux += LISTITEMCOST;
		}
		pt = pt->nxt;
	  }
	  len = size_aux / LISTITEMCOST;
	  if (show_state || (len != xeq_listlen)) {
		print_nl("change list size (bytes) for xeq: ");
		print_int(sizeof(intptr));print("+");print_int(size_aux);
		print(" len="); print_int(len);
	  	print(", array="); print_int(EQTB_SIZE+1-INT_BASE);
	  }
	  if (len != xeq_listlen) {
		print_nl("Internal Error! predicted len: ");
		print_int(xeq_listlen); print(" doesn't match counted len.");
		xeq_listlen = len;
	  }
	}
	iptr = END_MARKER;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);
#undef LISTITEMCOST
#ifdef DEBUG
	save_eqhashxeq();
#endif DEBUG
}

/*	restore context in separate arrays
 */

extern DIFF_CHUNK	*fch_ptr; /* fdim_list = start of addr list */

load_arrays()
{
	ptr		tmpptr1,
			tmpptr2;
	str		tmpstr1,
			tmpstr2;
	fnt		tmpfnt;
 	intptr		iaddr;		/* long absolute addr */
	shortptr	saddr;		/* short addr === offset from start */
	int		size_aux,
			diffcode,
			expected_len,
			i;

#define READ1(A,B) if (fread(A, B, 1, stc_file) < 1) goto A_BAD;
#define CHECK(A,B,S) if (A != B) {print_nl(S); print_ln(); return(FALSE);}

/* ______Font Array, check limits______ */
/*	initial value should be loaded as array segments in load_append,
	diff lists will be loaded in a little while */
	READ1(&tmpptr1, sizeof(ptr));
	CHECK(tmpptr1,fmem_ptr,
		"!Error: font ptr check failed, state corrupted.");
	READ1(&tmpfnt, sizeof(fnt));
	CHECK(tmpfnt,font_ptr,
		"!Error: aux font ptr check failed, state corrupted.");

/* ______String Pool - read current limits______ */
	READ1(&tmpptr1, sizeof(ptr));	/* pool_ptr_high */
	READ1(&tmpptr2, sizeof(ptr));	/* pool_lowater  */
	READ1(&tmpstr1, sizeof(str));	/* str_ptr_high  */
	READ1(&tmpstr2, sizeof(str));	/* str_lowater   */
	if (show_state) {
		print_nl("Str_pool high = "); print_int(tmpptr1);
		print(" low = "); print_int(tmpptr2);
		print(" (bytes): ");
		print_int(sizeof(ascii)*(tmpptr1+1-tmpptr2));
	}
	CHECK(tmpptr1,pool_ptr,
		"!Error: pool_ptr_high changed from last run.");
	CHECK(tmpptr2,pool_lowater,
		"!Error: pool_lowater changed from last run.");
	if (show_state) {
		print_nl("Str_start high = "); print_int(tmpstr1);
		print(" low = "); print_int(tmpstr2);
		print(" (bytes): "); print_int(sizeof(ptr)*(tmpstr1+1-tmpstr2));
	}
	CHECK(tmpstr1,str_ptr,
		"!Error: str_start_high changed from last run");
	CHECK(tmpstr2,str_lowater,
		"!Error: str_lowater changed from last run");
	READ1(&iaddr, sizeof(intptr));
	CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged,no marker after font/string limit");

/* ______Save Stack, array limit = SAVE_SIZE_____ */
	READ1(&tmpptr1, sizeof(ptr));
	CHECK(tmpptr1,save_ptr,
		"!save_stack ptr check != save_ptr:");
	if (save_ptr > 0)
		READ1(&save_stack[0],sizeof(mword)*save_ptr);
	if (show_state) {
		print_nl("actual save_stack size (bytes): ");
		print_int(sizeof(mword)*save_ptr);
	}
	READ1(&iaddr, sizeof(intptr));
	CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged,no marker after save stack");

/* ______Font diff lists______ */
	/* WARNING: in quiescence mode might have to merge the current
	   and restored diff lists, depending on how quiescence is done. */
	spill_clist(fdim_list, &fdim_end, &fdim_listlen);
	if (qsc_check)
		spill_clist(fchset, &fchset_end, &i);
	size_aux = 0;
	READ1(&diffcode, sizeof(int));	/* ignore for fonts for now. DLP */
	READ1(&expected_len, sizeof(int));
#ifdef DEBUG
	if (show_state) {
		print_nl("font mem change list (exp. size=");
		print_int(expected_len);  print("): ");
	}
#endif DEBUG
	for (i=0;i<expected_len;i++) {	/* read changed font dimensions */
		READ1(&iaddr, sizeof(intptr));
		READ1(iaddr,  sizeof(int));
		note_font(iaddr);
		size_aux += sizeof(intptr) + sizeof(int);
#ifdef DEBUGLIST
		if (show_state) /* show addr of word to be reloaded */
			{print_int((uint) iaddr); print(" ");}
#endif DEBUGLIST
	}
	READ1(&iaddr, sizeof(intptr));
	if (show_state || (iaddr != END_MARKER)) {
		print_nl("change list size (bytes) for font mem: ");
		print_int(sizeof(intptr)); print("+"); print_int(size_aux);
		print(" len = "); print_int(fdim_listlen);
	}
	CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after font change list");

/* ______Eqtb table______ */
	READ1(&diffcode, sizeof(int));	/* ignore for fonts for now. DLP */
	if (diffcode != DiffListCode) {	/* then read in array mode */
	  READ1(&eqtb[0], sizeof(mword)*(EQTB_SIZE+1));
	  if (show_state) {
		print_nl("eqtb (array, not diff list) restored (bytes): ");
		print_int(sizeof(mword)*(EQTB_SIZE+1));
	  }
	  READ1(&iaddr, sizeof(intptr));
	  CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after eqtb: array");
	} else {
	  spill_list(eqtb_list, &eqtb_end, &eqtb_listlen);
	  if (qsc_check)
	      	for (i=0;i<EQTB_SIZE+1;i++) eqtb_chset[i] = ITEM_CLEAR;
	  size_aux = 0;
	  READ1(&expected_len, sizeof(int));
#ifdef DEBUGLIST
	  if (show_state) {
		print_nl("eqtb change list (exp. size=");
		print_int(expected_len);  print("): ");
	  }
#endif DEBUGLIST
	  for (i=0;i<expected_len;i++) {	/* read changed eqtb entries */
		READ1(&saddr, sizeof(shortptr));
		READ1(&eqtb[saddr], sizeof(mword));
		note_eq((int) saddr);
		size_aux += sizeof(shortptr) + sizeof(mword);
#ifdef DEBUGLIST
		if (show_state) {print_int(saddr); print(" ");}
#endif DEBUGLIST
	  }
	  READ1(&iaddr, sizeof(intptr));
	  if (show_state || (iaddr != END_MARKER)) {
		print_nl("change list size (bytes) for eqtb: ");
		print_int(sizeof(int)); print("+"); print_int(size_aux);
		print(" len = "); print_int(eqtb_listlen);
	  }
	  CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after eqtb: list");
	}

/* ______Hash table______ */
	READ1(&diffcode, sizeof(int));
	if (diffcode != DiffListCode) {	/* then read in array mode */
	  READ1(&hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
	  if (show_state) {
		print_nl("hash (array, not diff list) restored (bytes): ");
		print_int(sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
	  }
	  READ1(&iaddr, sizeof(intptr));
	  CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after hash: array");
	} else {
	  spill_list(hash_list, &hash_end, &hash_listlen);
	  if (qsc_check)
	      	for (i=0;i<UNDEFINED_CONTROL_SEQUENCE+1;i++)
			hash_chset[i] = ITEM_CLEAR;
	  size_aux = 0;
	  READ1(&expected_len, sizeof(int));
#ifdef DEBUGLIST
	  if (show_state) {
	 	 print_nl("hash change list (exp. size=");
		print_int(expected_len);  print("): ");
	  }
#endif DEBUGLIST
	  for (i=0;i<expected_len;i++) {	/* read changed hash items */
		READ1(&saddr, sizeof(shortptr));
		READ1(&hash[saddr], sizeof(hh));
		note_hash((int) saddr);
		size_aux += sizeof(shortptr) + sizeof(hh);
#ifdef DEBUGLIST
		if (show_state) {print_int(saddr); print(" ");}
#endif DEBUGLIST
	  }
	  READ1(&iaddr, sizeof(intptr));
	  if (show_state || (iaddr != END_MARKER)) {
		print_nl("change list size (bytes) for hash: ");
		print_int(sizeof(int)); print("+"); print_int(size_aux);
		print(" len = "); print_int(hash_listlen);
	  }
	  CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after hash: list");
	}

/* ______Xeq_level table______ */
	READ1(&diffcode, sizeof(int));
	if (diffcode != DiffListCode) {	/* then read in array mode */
	  READ1(&xeq_level[0], sizeof(qword)*(EQTB_SIZE+1 - INT_BASE));
	  if (show_state) {
		print_nl("xeq_level (array, not diff list) restored (bytes): ");
		print_int(sizeof(qword)*(EQTB_SIZE+1 - INT_BASE));
	  }
	  READ1(&iaddr, sizeof(intptr));
	  CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after eqtb: array");
	} else {
	  spill_list(xeq_list, &xeq_end, &xeq_listlen);
	  if (qsc_check)
	      	for (i=0;i<EQTB_SIZE+1 - INT_BASE;i++)
			xeq_chset[i] = ITEM_CLEAR;
	  size_aux = 0;
	  READ1(&expected_len, sizeof(int));
#ifdef DEBUGLIST
	  if (show_state) {
		print_nl("xeq_level change list (exp. size=");
		print_int(expected_len);  print("): ");
	  }
#endif DEBUGLIST
	  for (i=0;i<expected_len;i++) {	/* read changed xeq items */
		READ1(&saddr, sizeof(shortptr));
/*		qaddr = saddr + &xeq_level[0];	!!probably doesn't work 'cause
		   + adds one element of pointer size to it!!! DLP
		READ1(qaddr, sizeof(qword));
		note_xeq(qaddr);
 */		READ1(&xeq_level[saddr], sizeof(qword));
		note_xeq(saddr);
		size_aux += sizeof(shortptr) + sizeof(qword);
#ifdef DEBUGLIST
		if (show_state) {print_int((int) saddr); print(" ");}
/*		if (show_state) {print_int((int) qaddr); print(" ");}  */
#endif DEBUGLIST
	  }
	  READ1(&iaddr, sizeof(intptr));
	  if (show_state || (iaddr != END_MARKER)) {
		print_nl("change list size (bytes) for xeq: ");
		print_int(sizeof(intptr)); print("+"); print_int(size_aux);
		print(" len = "); print_int(xeq_listlen);
	  }
	  CHECK(iaddr,END_MARKER,
		"!Error: checkpoint damaged, no marker after xeq: list");
	}

	return(TRUE);
A_BAD:
	print_nl("State checkpoint file damaged (array section)!");
	return(FALSE);
}
#undef READ1
#undef CHECK

/* save mem structures
 */
save_mems()
{
	int i;
#ifdef offARRAY
	save_marray();	/* ||| DLP */
#endif offARRAY
	/* write single cell mem[] node, only actual used part */
	save_single();
	i = END_MARKER;
	fwrite(&i, sizeof(int), 1, stc_file);

	/* write token_list node, only actual part */
	save_token();
	i = END_MARKER;
	fwrite(&i, sizeof(int), 1, stc_file);

	/* write multi cell mem[] node, only actually used part */
	save_multi();
	i = END_MARKER;
	fwrite(&i, sizeof(int), 1, stc_file);
	
	if (show_state) {
		print_nl("single node: ");
		print_int(sizeof(mword)*(mem_end-hi_mem_min+1-single_empty()));
		print(" bytes");
		print_nl("tok_end= ");		print_int(tok_end);
		print("\ttok_low= ");		print_int(tok_low);
		print("\ttok_head= ");		print_int(tok_head);
		print("\ttok_empty= ");		print_int(tok_empty());
		print_nl("token_list region: ");
		print_int(sizeof(tok)*(tok_end-tok_low+1));
		print(" bytes");
		print_nl("token_list node: ");
		print_int(sizeof(tok)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
		print_nl("token_list link: ");
		print_int(sizeof(ptr)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
	}
}

/*	save string arrays
 */
int
save_strings()
{
	get_ext(-1, EXT_STR);
	if ((str_file = fopen(name_of_file, "w")) == NULL) {
		print_nl("Failed to open string file ");
		print(name_of_file); print_char('.'); print_ln();
		update_terminal();
		history = FATAL_ERROR_STOP;
		return(FALSE);
	}

	/* put in state version & time started */
	save_state_code(str_file);
	fwrite(&time0.tv_sec, sizeof(long), 1, str_file);

	/* write array high water marks. Save_strings assumes
	   it is being called at the end, and resets the
	   high water marks. DLP */
	/* 111 !!! has problems in quiescence mode if it skips over
	   pages to end */
	pool_ptr_high = pool_ptr;
	str_ptr_high  = str_ptr;
	fwrite(&pool_ptr_high, sizeof(ptr), 1, str_file);
	fwrite(&pool_lowater,  sizeof(ptr), 1, str_file);
	fwrite(&str_ptr_high,  sizeof(str), 1, str_file);
	fwrite(&str_lowater,   sizeof(str), 1, str_file);

	/* write whole str_pool array */
	fwrite(&str_pool[pool_lowater],
		sizeof(ascii)*(pool_ptr_high+1-pool_lowater), 1, str_file);
	
	/* write whole str_start array */
	fwrite(&str_start[str_lowater],
		sizeof(ptr)*(str_ptr_high+1-str_lowater), 1, str_file);
	
	if (show_state) {
		print_nl("Str_pool high = "); print_int(pool_ptr);
		print(" low = "); print_int(pool_lowater);
		print(" (bytes): ");
		print_int(sizeof(ascii)*(pool_ptr+1-pool_lowater));

		print_nl("Str_start high = "); print_int(str_ptr);
		print(" low = "); print_int(str_lowater);
		print(" (bytes): ");
		print_int(sizeof(ptr)*(str_ptr+1-str_lowater));
	}
	fclose(str_file);
	return(TRUE);
}

/*	open tmp font file. DLP
 */
int
open_font_file()
{
	/* string pool not ready yet */
	sprintf(name_of_file, "%s%s%s", base_fn, EXT_DEL, EXT_FONT_TMP);
	if ((font_file = fopen(name_of_file, "w")) == NULL) {
		print_nl("Failed to open interim font file ");
		print(name_of_file); print_char('.');
		return(FALSE);
	}
	/* put in state version & time started */
	save_state_code(font_file);
	fwrite(&time0.tv_sec, sizeof(long), 1, font_file);
	return(TRUE);
}

/*	make tmp font file permanent. DLP
 */
close_font_file()
{
	char	tmp_name[FILE_NAME_SIZE];

	b_close(font_file);
	get_ext(-1, EXT_FONT_TMP);
	strcpy(tmp_name,name_of_file);
	get_ext(-1, EXT_FONT);
	rename(tmp_name,name_of_file);
}

/*	restore string array, font arrays written at end
 */
load_appends()
{
	char	str_name[FILE_NAME_SIZE];
	int	page,
		compressed,
		size_aux;
	str	tmpstr;
	struct	timeval	when;
	ptr	fmem_seg,
		fmem_sofar,
		tmpptr;
	fnt	font_seg,
		font_sofar,
		tmpfnt;

#undef READ1
#define READ1(A,B) if (fread(A, B, 1, str_file) < 1) goto STR_BAD

	(void) strcpy(str_name, base_fn);
	(void) strcat(str_name, EXT_DEL);
	(void) strcat(str_name, EXT_STR);
	if ((str_file = fopen(str_name, "r")) == NULL) {
		print_nl("Failed to open string file ");
		print(str_name); print_char('.'); print_ln();
		update_terminal();
		return(FALSE);
	}

	/* read & check state version. DLP */
	if (!read_state_code(&page, &compressed, str_file))
		return(FALSE);
	if (page != last_max_pages) {
		print_nl("Warning, saved page count in INC/.str file (");
		print_int(page);
		print(") doesn't match count in INC/.inc file (");
		print_int(last_max_pages); print(")");
	}

	/* read time written */
	READ1(&when.tv_sec, sizeof(long));
	if (when.tv_sec != tsync.tv_sec) {
		print_nl("!Error: "); print(str_name);
		print(" was not written at the same time as the .inc file");
		print_nl("Must have crashed right before final save.");
		return(FALSE);
	}
	
	/* read high water marks. DLP */
	READ1(&pool_ptr_high, sizeof(ptr));
	READ1(&tmpptr, sizeof(ptr));
	if (tmpptr != pool_lowater) {
		print_nl("!Error: pool_lowater changed from last run.");
		print_nl("Rerun with -v if new system style has been loaded.");
		return(FALSE);
	}
	READ1(&str_ptr_high,  sizeof(str));
	READ1(&tmpstr, sizeof(str));
	if (tmpstr != str_lowater) {
		print_nl("!Error: str_lowater changed from last run.");
		print_nl("Rerun with -v if new system style has been loaded.");
		return(FALSE);
	}

	/* read str_pool array */
	READ1(&str_pool[pool_lowater],
		sizeof(ascii)*(pool_ptr_high+1-pool_lowater));
	
	/* read whole str_start array */
	READ1(&str_start[str_lowater],
		sizeof(ptr)*(str_ptr_high+1-str_lowater));
	
	if (show_state) {
		print_nl("Str_pool high = "); print_int(pool_ptr);
		print(" low = "); print_int(pool_lowater);
		print(" (bytes for highwater str_pool): ");
		print_int(sizeof(ascii)*(pool_ptr_high+1-pool_lowater));

		print_nl("Str_start high = "); print_int(str_ptr);
		print(" low = "); print_int(str_lowater);
		print(" (bytes read for highwater str_start): ");
		print_int(sizeof(ptr)*(str_ptr_high+1-str_lowater));
	}
	(void) fclose(str_file);

/* Fonts - must be read in BEFORE font diff lists! */

#undef READ1
#define READ1(A,B) if (fread(A, B, 1, str_file) < 1) goto FONT_BAD;

	(void) strcpy(str_name, base_fn);
	(void) strcat(str_name, EXT_DEL);
	(void) strcat(str_name, EXT_FONT);
	if ((str_file = fopen(str_name, "r")) == NULL) {
		print_nl("Failed to open font file ");
		print(str_name); print_char('.'); print_ln();
		update_terminal();
		return(FALSE);
	}

	/* read & check state version. DLP */
	if (!read_state_code(&page, &compressed, str_file))
		return(FALSE);
	if (page > 0) {
		print_nl("Warning, saved page count in INC/.fonts file (");
		print_int(page);
		print(") doesn't match count in INC/.inc file (");
		print_int(last_max_pages); print(")");
	}

	/* read time written */
	READ1(&when.tv_sec, sizeof(long));
	if (when.tv_sec != tsync.tv_sec) {
		print_nl("!Error: "); print(str_name);
		print(" was not written at the same time as the .inc file");
		print_nl("Must have crashed right before final save.");
		return(FALSE);
	}
	
	/* read high water marks. DLP */
	fmem_sofar = fmem_lowater;
	font_sofar = font_lowater;

	while ((fmem_sofar < fmem_ptr) || (font_sofar < font_ptr)) {
	  READ1(&tmpptr,   sizeof(ptr));
	  READ1(&fmem_seg, sizeof(ptr));
	  if (fmem_sofar != tmpptr) {
		print_nl("font mem segment starts at wrong place!");
		return(FALSE);
	  }
	  if (fmem_seg > 0)
		READ1(&font_info[fmem_sofar],sizeof(mword)*(fmem_seg));
	  if (show_state) {
		print_nl("font_info segment size loaded (bytes): ");
		print_int(2*sizeof(ptr));
		print("+");		print_int(sizeof(mword)*fmem_seg);
		print(" high = ");	print_int(fmem_sofar+fmem_seg);
		print(" low = ");	print_int(fmem_sofar);
		update_terminal();
	  }
	  fmem_sofar += fmem_seg;

	  READ1(&tmpfnt,   sizeof(fnt));
	  READ1(&font_seg, sizeof(fnt));
	  if (tmpfnt != font_sofar) {
		print_nl("aux font segment starts at wrong place!");
		return(FALSE);
	  }
	  size_aux = 0;
	  if (font_seg > 0)
	     if (!load_aux(&size_aux,font_sofar+font_seg,font_sofar,str_file))
	  	goto FONT_BAD;
	  if (show_state) {
		print_nl("font aux segment size loaded (bytes): ");
		print_int(2*sizeof(fnt));
		print("+");		print_int(size_aux);
		print(" high = ");	print_int(font_sofar+font_seg);
		print(" low = ");	print_int(font_sofar);
		update_terminal();
	  }
	  font_sofar += font_seg;
	}

	(void) fclose(str_file);

	if ((fmem_sofar != fmem_ptr) || (font_sofar != font_ptr)) {
		print_nl("Warning: fmem or aux font levels didn't line up!");
		print_nl("fmem load level: ");	print_int(fmem_sofar);
		print(";  fmem target: ");	print_int(fmem_ptr);
		print(";aux load level: ");	print_int(font_sofar);
		print(";  aux target: ");	print_int(font_ptr);
	}

	/* save any new font info from page 0 or the last page state loaded
	   up to this state checkpoint */
	save_fonts(fmem_ptr,fmem_last,font_ptr,font_last);

	fmem_last = fmem_ptr;		/* new chkpt water mark */
	font_last = font_ptr;

	return(TRUE);
STR_BAD:
	print_nl("Warning: string file damaged!");
	(void) fclose(str_file);
	return(FALSE);
FONT_BAD:
	print_nl("Warning: font file damaged!");
	(void) fclose(str_file);
	return(FALSE);
}

/*
 * save upper half mem[], only actual used part
 */
save_single()
{
	char	*map;
	char	*q;
	ptr	p;
	int	len;
	int	i;
	sary	s_array[SARRY_SIZE];
	int	s_indx = 0;
	int	l, h;
	ptr	end;		/* Avoid using last since that's an important
				 * global and bugs changing it are BAD */


	/* compute req'ed area for mem map of hi_mem_min-mem_end */
	/* Note: each bit in map corresponds to 2 mem[] cell     */
	len = ((mem_end - hi_mem_min + 1)/SMEM_SIZE + (sizeof(char)*8 - 1)) /
		     (sizeof(char)*8);
	map = (char *) malloc(len);
	/* mark as all mem[] cells are used */
	for (i = 0, q = map; i < len; i++)
		*(q++) = 0xff;
	
	p = avail;
	while(p != NULL) {
		/* go thru free list, reset unused mem map bit */
		off_memmap(p, 1, map, hi_mem_min, SMEM_SIZE);
		p = link(p);
	}

	l = hi_mem_min;
	while(get_used_area(&l, &h, map, hi_mem_min, mem_end)) {
		if (s_indx > SARRY_SIZE) {
			print_nl("Single array work space overflow.");
			print_ln();
			jump_out();
		}
		s_array[s_indx].beg = l;
		s_array[s_indx++].end = h;
		l = h + 1;
	}

	fwrite(&s_indx, sizeof(int), 1, stc_file); /* how many chunks */
	if (s_indx != 0) {
		for (i = 0; i < s_indx; i++) {
			if (show_stuff && show_state) {
				print_nl("used single mem: beg= ");
				print_int(s_array[i].beg);
				print(", end= ");
				print_int(s_array[i].end);
			}
			write_single(s_array[i].beg, s_array[i].end);
		}
	} else
		s_array[0].beg = mem_end+1;	/* single area is empty! DLP */
	if (show_state) {
		print_nl("single mem overhead: ");
		print_int(sizeof(int) + sizeof(ptr)*2*s_indx);
		print(" bytes");
	}
	/*free(map);*/		/* free mem map */
	/*return;*/		/* DLP */
	/* reset avail list, well, you gotta do what you gotta do. DLP */
	if (hi_mem_min == s_array[0].beg) {
		avail = s_array[0].end + 1;
		end = NULL;
	} else {
		/* free list exist before first used chunk */
		avail = hi_mem_min;
		build_single_free(NULL, avail, s_array[0].beg - SMEM_SIZE);
		end = s_array[0].beg - SMEM_SIZE;
	}
	if (end < mem_end) {
		for ( i = 1; i < s_indx; i++) {
			if (end > s_array[i].beg)
				continue;
			build_single_free(end, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			end = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != mem_end)
			build_single_free(end, s_array[s_indx-1].end + 1, mem_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("single node region: ");
		print_int(sizeof(mword)*(mem_end-hi_mem_min+1));
		print(" bytes");
		print_nl("empty single size: ");
		print_int(sizeof(mword)*single_empty());
		print(" bytes");
	}
	free(map);		/* free mem map */
}


/* save token_list node, only user defined part */
save_token()
{
	char	*map;
	char	*q;
	ptr	p;
	int	len;
	int	i;
	sary	s_array[SARRY_SIZE];
	int	s_indx = 0;
	int	l, h;
	int	sh;
	ptr	end;	/* to get token free lists mashed same, yech. DLP */

#ifdef DEBUG
/* 	Check premac area. DLP */
	if (show_state && (tokmem_copy != NULL)) {
		print_nl("Checking premac tokens, high="); print_int(premac_hi);
		print(" low = "); print_int(premac_lo);
	}
	if (tokmem_copy != NULL)
	  if (!bcmp(&tok_mem[premac_lo],tokmem_copy,(int) sizeof(tok)*(premac_hi-premac_lo+1)))
		print_nl("tok_mem in premac didn't checkout!");

	if (toklin_copy != NULL)
	  if (!bcmp(&tok_link[premac_lo],toklin_copy,(int) sizeof(ptr)*(premac_hi-premac_lo+1)))
		print_nl("tok_link in premac didn't checkout!");
	if (show_state)
		update_terminal();
#endif DEBUG

	if (show_state) {
	 print_nl("tok_mem[premac_lo-1] = "); print_int(tok_mem[premac_lo-1]);
	 print("  tok_link = "); print_int(tok_link[premac_lo-1]);
	}
	len = ((tok_end - tok_low + 1) + (sizeof(char)*8 - 1)) /
		     (sizeof(char)*8);
	map = (char *) malloc(len);
	/* mark as all tok_mem[] cells are used */
	for (i = 0, q = map; i < len; i++)
		*q++ = 0xff;
	
	p = tok_head;
	while(p != NULL) {
		/* reset unused mem map bit */
		off_memmap(p, 1, map, tok_low, 1);
		p = token_link(p);
	}

	l = tok_low;
	while(get_used_area(&l, &h, map, tok_low, tok_end)) {
		if (s_indx > SARRY_SIZE) {
			print_nl("Token_list array work space overflow.");
			print_ln();
			jump_out();
		}
		/* save only token_list defined by user */
		sh = h;
		if (l >= premac_lo && l <= premac_hi)
			l = premac_hi + 1;
		if (h >= premac_lo && h <= premac_hi)
			h = premac_lo - 1;
		if (h >= l) {
			s_array[s_indx].beg = l;
			s_array[s_indx++].end = h;
		}
		l = sh + 1;
	}

	fwrite(&s_indx, sizeof(int), 1, stc_file); /* how many chunks */
	if (s_indx != 0) {
		for (i = 0; i < s_indx; i++) {
			if (show_stuff && show_state) {
				print_nl("used tok_mem: beg= ");
				print_int(s_array[i].beg);
				print(", end= ");
				print_int(s_array[i].end);
			}
			write_token(s_array[i].beg, s_array[i].end);
		}
	} else
			s_array[0].beg = tok_end+1; /* token mem empty! DLP */
	if (show_state)  {
		print_nl("token_list overhead: ");
		print_int(sizeof(int) + sizeof(ptr)*2*s_indx);
		print(" bytes");
	}
	/*free(map);*/		/* free mem map */
	/* return;*/		/* DLP */
	/* mash token free list the same, blech pyew, DLP */
	if (tok_low == s_array[0].beg || tok_low == premac_lo) {
		tok_head = s_array[0].end + 1;
		end = NULL;
	} else {
		/* free list exist before first used chunk */
		tok_head = tok_low;
		build_token_free(NULL, tok_head, s_array[0].beg - SMEM_SIZE);
		end = s_array[0].beg - SMEM_SIZE;
	}
	if (end < tok_end) {
		for ( i = 1; i < s_indx; i++) {
			if (end > s_array[i].beg)
				continue;
			build_token_free(end, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			end = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != tok_end)
			build_token_free(end, s_array[s_indx-1].end + 1, tok_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("token_list region: ");
		print_int(sizeof(tok)*(tok_end-tok_low+1));
		print(" bytes");
		print_nl("empty token_list size: ");
		print_int(sizeof(tok)*tok_empty());
		print(" bytes");
	}
	free(map);		/* free mem map */
}
	
	
/*
 *	save lower half mem[], only actually used part
 */
save_multi()
{
	char	*map;
	char	*q;
	ptr	p;
	int	len;
	sary	m_array[SARRY_SIZE];
	int	m_indx = 0;
	int	i;
	int	l, h;
	ptr	end;
	
	
	len = ((lo_mem_max - MEM_MIN +1) + (sizeof(char)*8 -1)) /
		(sizeof(char)*8);
	map = (char *) malloc(len);
	/* mark as all mem[] cells are used */
	for (i = 0, q = map; i < len; i++)
		*q++ = 0xff;
	
	p = rover;
	do {
		off_memmap(p, (int) node_size(p), map, MEM_MIN, 1);
		p = rlink(p);
	} while(p != rover);
	
	l = MEM_MIN;
	while(get_multi_used(&l, &h, map)) {
		if (m_indx > SARRY_SIZE) {
			print_nl("Multi array work space overflow.");
			print_ln();
			jump_out();
		}
		m_array[m_indx].beg = l;
		m_array[m_indx++].end = h;
		l = h + 1;
	}
	fwrite(&m_indx, sizeof(int), 1, stc_file); /* how many chunks */
	if (m_indx != 0) {
		for (i = 0; i < m_indx; i++) {
			if (show_stuff && show_state) {
				print_nl("used multi mem: beg= ");
				print_int(m_array[i].beg);
				print(", end= ");
				print_int(m_array[i].end);
				}
			write_multi(m_array[i].beg, m_array[i].end);
		}
	} else
		m_array[0].beg = lo_mem_max+1;	/* multi area is empty! DLP */
	if (show_state)  {
		print_nl("multi mem overhead: ");
		print_int(sizeof(int) + sizeof(ptr)*2*m_indx);
		print(" bytes");
	}
	/*free(map); return;*/		/* DLP */
	/* mush free list to the same, DLP */
	if (m_array[0].beg == MEM_MIN) {
		rover = m_array[0].end + 1;
	} else {
		rover = MEM_MIN;
		build_multi_free(rover, rover, m_array[0].beg - 1);
	}
	end = rover;
	if (end <= lo_mem_max) {
		for (i = 1; i < m_indx; i++) {
			if (end > m_array[i].beg)
				continue;
			build_multi_free(end, m_array[i-1].end + 1, m_array[i].beg - 1);
			end = m_array[i-1].end + 1;
		}
		if (m_array[m_indx-1].end != lo_mem_max)
			build_multi_free(end, m_array[m_indx-1].end + 1, lo_mem_max);
	}
	if (show_state) {
		print_nl("multi node region: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1));
		print(" bytes");
		print_nl("empty multi size: ");
		print_int(sizeof(mword)*multi_empty());
		print(" bytes");
		print_nl("net multi used: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1 - multi_empty()));
		print(" bytes");
	}
	free(map); 		/* DLP */
}
	
off_memmap(p, size, map, base, fac)
	ptr	p;
	int	size;
	char	*map;
	ptr	base;
	int	fac;
{
	int	offset;
	int	pos;
	
	offset = ((p - base) / fac) / (sizeof(char)*8);
	pos = ((p - base) / fac) % (sizeof(char)*8);
	while(1) {
		*(map + offset) ^= 1 << pos;
		size--;
		if (size == 0)
			return;
		pos++;
		if (pos >= 8) {
			offset++;
			pos = 0;
		}
	}
}

get_used_area(bg, ed, map, base, max_limit)
	int	*bg;
	int	*ed;
	char	*map;
	ptr	base;
	ptr	max_limit;
{	
	int	p;
	
	p = *bg;
	p = get_beg_used(p, map, base, max_limit); /* get first mem[] pos of consq area */
	if (p ==0)
		return(FALSE);	/* no more found */
	*bg = p;
	p = get_end_used(p, map, base, max_limit); /* get last mem[] pos of consq area */
	*ed = p;
	return(TRUE);
}

get_beg_used(p, map, base, max_limit)
	int	p;
	char	*map;
	ptr	base;
	ptr	max_limit;
{
	int	offset;
	int	ooff;
	int	pos;
	int	mappos;

	offset = ooff = ((p - base) / SMEM_SIZE) / (sizeof(char)*8);
loop1:
	while(*(map + offset) == NULL)
		offset++;
	if (offset == ooff) {
		pos = ((p - base) / SMEM_SIZE) % (sizeof(char)*8);
		if ((unsigned)*(map + offset) < (unsigned)(1 << pos)) {
			offset++;
			goto loop1;
		}
	} else
		pos = 0;
	while((*(map + offset) & (1 << pos)) == NULL)
		pos++;
	mappos = base + (offset * ((sizeof(char))*8) + pos) * SMEM_SIZE;
	if (mappos > max_limit)
		return(0);	/* not found */
	else
		return(mappos);
}

get_end_used(p, map, base, max_limit)
	int	p;
	char	*map;
	ptr	base;
	ptr	max_limit;
{
	int	offset;
	int	ooff;
	int	pos;
	int	mappos;
	
	offset = ooff = ((p - base) / SMEM_SIZE) / (sizeof(char)*8);
loop2:	
	while(*(map + offset) == (char)0xff)
		offset++;
	if (offset == ooff)
		pos = ((p - base) / SMEM_SIZE) % (sizeof(char)*8);
	else
		pos = 0;
	while((*(map + offset) & (1 << pos)) != NULL) {
		pos++;
		if (pos >= 8)
			break;
	}
	if (pos >= 8) {
		offset++;
		goto loop2;
	}
	if (pos == 0) {
		offset--;
		pos = 7;
	} else
		pos--;
	mappos = base + (offset * ((sizeof(char))*8) + pos) * SMEM_SIZE + (SMEM_SIZE - 1);
	if (mappos > max_limit)
		return(max_limit);
	else
		return(mappos);
}

write_single(beg, end)
	ptr	beg;
	ptr	end;
{
	fwrite(&beg, sizeof(ptr), 1, stc_file);	/* beg address */
	fwrite(&end, sizeof(ptr), 1, stc_file); /* end address */
	fwrite(&mem[beg], (end - beg +1)*sizeof(mword), 1, stc_file);
	if (show_stuff && show_state) {
		print_nl("single mem write: ");
		print_int((end - beg +1)*sizeof(mword));
		print(" bytes");
	}
}

write_token(beg, end)
	ptr	beg;
	ptr	end;
{
	fwrite(&beg, sizeof(ptr), 1, stc_file);	/* beg address */
	fwrite(&end, sizeof(ptr), 1, stc_file); /* end address */
	fwrite(&tok_mem[beg], (end - beg +1)*sizeof(tok), 1, stc_file);
	fwrite(&tok_link[beg], (end - beg +1)*sizeof(ptr), 1, stc_file);
	if (show_stuff && show_state) {
		print_nl("tok_mem write: ");
		print_int((end - beg +1)*sizeof(tok));
		print(" bytes");
		print_nl("tok_link write: ");
		print_int((end - beg + 1)*sizeof(ptr));
		print(" bytes");
	}
}
get_multi_used(bg, ed, map)
	int	*bg;
	int	*ed;
	char	*map;
{
	int	p;
	
	p = *bg;
	p = get_multi_beg(p, map); /* get first mem[] pos of conseq area */
	if (p == MAX_HALFWORD)
		return(FALSE);	/* no more found */
	*bg = p;
	p = get_multi_end(p, map); /* get last mem[] pos of conseq area */
	*ed = p;
	return(TRUE);
}

get_multi_beg(p, map)
	int	p;
	char	*map;
{
	int	offset;
	int	ooff;
	int	pos;
	int	mappos;
	
	offset = ooff = (p - MEM_MIN) / (sizeof(char)*8);
loop1:
	while(*(map + offset) == NULL)
		offset++;
	if (offset == ooff) {
		pos = (p - MEM_MIN) % (sizeof(char)*8);
		if ((unsigned)*(map + offset) < (unsigned)(1 << pos)) {
			offset++;
			goto loop1;
		}
	} else
		pos = 0;
	while((*(map + offset) & (1 << pos)) == NULL)
		pos++;
	mappos = MEM_MIN + (offset * (sizeof(char)*8)) + pos;
	if (mappos > lo_mem_max)
		return(MAX_HALFWORD);	/* not found */
	else
		return(mappos);
}

get_multi_end(p, map)
	int	p;
	char	*map;
{
	int	offset;
	int	ooff;
	int	pos;
	int	mappos;
	
	offset = ooff = (p - MEM_MIN) / (sizeof(char)*8);
loop2:
	while(*(map + offset) == (char)0xff)
		offset++;
	if (offset == ooff)
		pos = (p - MEM_MIN) % (sizeof(char)*8);
	else
		pos = 0;
	while((*(map + offset) & (1 << pos)) != NULL) {
		pos++;
		if (pos >= 8)
			break;
	}
	if (pos >= 8) {
		offset++;
		goto loop2;
	}
	if (pos == 0) {
		offset--;
		pos = 7;
	} else
		pos--;
	mappos = MEM_MIN + (offset * (sizeof(char)*8)) + pos;
	if (mappos > lo_mem_max)
		return(lo_mem_max);
	else
		return(mappos);
}

write_multi(beg, end)
	ptr	beg;
	ptr	end;
{
	fwrite(&beg, sizeof(ptr), 1, stc_file);	/* beg address */
	fwrite(&end, sizeof(ptr), 1, stc_file); /* end address */
	fwrite(&mem[beg], (end - beg +1)*sizeof(mword), 1, stc_file);
	if (show_stuff && show_state) {
		print_nl("multi mem write: ");
		print_int((end - beg +1)*sizeof(mword));
		print(" bytes");
	}
}
	
	
/*	restore mem structures
 */
load_mems()
{
	int i;

#ifdef offARRAY
	load_marray();
#endif offARRAY
	
	load_single();	/* read single cell mem[] */
	(void) fread(&i, sizeof(int), 1, stc_file);
	if (i != END_MARKER)
		print_nl("Warning! No marker after dynamic single memory!");
	
	load_token();	/* restore token_list node */
	(void) fread(&i, sizeof(int), 1, stc_file);
	if (i != END_MARKER)
		print_nl("Warning! No marker after dynamic token memory!");
	
	load_multi();	/* read multi cell mem[] */
	(void) fread(&i, sizeof(int), 1, stc_file);
	if (i != END_MARKER)
		print_nl("Warning! No marker after dynamic multiword memory!");


	if (show_state) {
		print_nl("single node: ");
		print_int(sizeof(mword)*(mem_end-hi_mem_min+1-single_empty()));
		print(" bytes");
		print_nl("tok_end= ");		print_int(tok_end);
		print("\ttok_low= ");		print_int(tok_low);
		print("\ttok_head= ");		print_int(tok_head);
		print("\ttok_empty= ");		print_int(tok_empty());
		print_nl("token_list region: ");
		print_int(sizeof(tok)*(tok_end-tok_low+1));
		print(" bytes");
		print_nl("token_list node: ");
		print_int(sizeof(tok)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
		print_nl("token_list link: ");
		print_int(sizeof(ptr)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
	}
	return(TRUE);
}

/*	restore upper half mem
 */
load_single()
{
	sary	s_array[SARRY_SIZE];
	int	s_indx;
	int	i;
	ptr	beg, end;
	ptr	endfree;

	(void) fread(&s_indx, sizeof(int), 1, stc_file); /* get # of chunks */
	s_array[0].beg = mem_end+1;	/* in case single area is empty! DLP */
	for (i = 0; i < s_indx; i++) {	/* read chunk */
		fread(&beg, sizeof(ptr), 1, stc_file);
		fread(&end, sizeof(ptr), 1, stc_file);
		s_array[i].beg = beg;
		s_array[i].end = end;
#ifdef offARRAY
		(void) fseek(stc_file, (end - beg + 1)*sizeof(mword), SEEK_CUR);
#else
		(void) fread(&mem[beg],(end-beg+1)*sizeof(mword),1,stc_file);
#endif offARRAY
		if (show_stuff && show_state) {
			print_nl("restored single mem: beg= ");
			print_int(beg);
			print(", end= ");
			print_int(end);
			print_nl("single mem read: ");
			print_int((end - beg +1)*sizeof(mword));
			print(" bytes");
		}
	}
	if (show_state) {
		print_nl("single mem overhead: ");
		print_int(sizeof(int) + sizeof(ptr)*2*s_indx);
		print(" bytes");
	}
	if (hi_mem_min == s_array[0].beg) {
		avail = s_array[0].end + 1;
		endfree = NULL;
	} else {
		/* free list exist before first used chunk */
		avail = hi_mem_min;
		build_single_free(NULL, avail, s_array[0].beg - SMEM_SIZE);
		endfree = s_array[0].beg - SMEM_SIZE;
	}
	if (endfree < mem_end) {
		for ( i = 1; i < s_indx; i++) {
			if (endfree > s_array[i].beg)
				continue;
			build_single_free(endfree, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			endfree = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != mem_end)
			build_single_free(endfree, s_array[s_indx-1].end + 1, mem_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("single node: ");
		print_int(sizeof(mword)*(mem_end-hi_mem_min+1));
		print(" bytes");
		print_nl("empty single size: ");
		print_int(sizeof(mword)*single_empty());
		print(" bytes");
	}
}

/*  	restore token_list node 
 */
load_token()
{
	sary	s_array[SARRY_SIZE];
	int	s_indx;
	int	i;
	ptr	beg, end;
	ptr	endfree;

	if (show_state) {
		print_nl("Premac tokens high = "); print_int(premac_hi);
		print(" low = "); print_int(premac_lo);
	}

	fread(&s_indx, sizeof(int), 1, stc_file); /* get # of chunks to read */
	s_array[0].beg = tok_end+1;	/* in case token mem empty! DLP */
	for (i = 0; i < s_indx; i++) {	/* read chunk */
		fread(&beg, sizeof(ptr), 1, stc_file);
		fread(&end, sizeof(ptr), 1, stc_file);
		s_array[i].beg = beg;
		s_array[i].end = end;
		fread(&tok_mem[beg],  (end - beg + 1)*sizeof(tok), 1, stc_file);
		fread(&tok_link[beg], (end - beg + 1)*sizeof(ptr), 1, stc_file);
		if (show_stuff && show_state) {
			print_nl("restored tok_mem: beg= ");
			print_int(beg);
			print(", end= ");
			print_int(end);
			print_nl("single tok_mem: ");
			print_int((end - beg +1)*sizeof(tok));
			print(" bytes");
			print_nl("tok_link: ");
			print_int((end - beg +1)*sizeof(ptr));
			print(" bytes");
		}
	}
	if (tok_low == s_array[0].beg || tok_low == premac_lo) {
		tok_head = s_array[0].end + 1;
		endfree = NULL;
	} else {
		/* free list exist before first used chunk */
		tok_head = tok_low;
		build_token_free(NULL, tok_head, s_array[0].beg - SMEM_SIZE);
		endfree = s_array[0].beg - SMEM_SIZE;
	}
	if (endfree < tok_end) {
		for ( i = 1; i < s_indx; i++) {
			if (endfree > s_array[i].beg)
				continue;
			build_token_free(endfree, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			endfree = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != tok_end)
			build_token_free(endfree, s_array[s_indx-1].end + 1, tok_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("token_list node: ");
		print_int(sizeof(tok)*(tok_end-tok_low+1));
		print(" bytes");
		print_nl("empty token_list size: ");
		print_int(sizeof(tok)*tok_empty());
		print(" bytes");
		print_nl("tok_mem[premac_lo-1] = ");
		print_int(tok_mem[premac_lo-1]);
		print("  tok_link = "); print_int(tok_link[premac_lo-1]);
	}
}
	
/*	restore lower half mem
 */
load_multi()
{
	sary	m_array[SARRY_SIZE];
	int	m_indx;
	int	i;
	ptr	beg, end;
	ptr	endfree;
	
	(void) fread(&m_indx, sizeof(int), 1, stc_file); /* get # of chunks */
	m_array[0].beg = lo_mem_max+1;	/* in case single area is empty! DLP */
	for (i = 0; i < m_indx; i++) {	/* read chunk */
		(void) fread(&beg, sizeof(ptr), 1, stc_file);
		(void) fread(&end, sizeof(ptr), 1, stc_file);
		m_array[i].beg = beg;
		m_array[i].end = end;
#ifdef offARRAY
		(void) fseek(stc_file, (end - beg + 1)*sizeof(mword), SEEK_CUR);
#else
		(void) fread(&mem[beg], (end-beg+1)*sizeof(mword), 1, stc_file);
#endif offARRAY
		if (show_stuff && show_state) {
			print_nl("restored multi mem: beg= ");
			print_int(beg);
			print(", end= ");
			print_int(end);
			print_nl("multi mem read: ");
			print_int((end - beg +1)*sizeof(mword));
			print(" bytes");
		}
	}
	if (m_array[0].beg == MEM_MIN) {
		rover = m_array[0].end + 1;
	} else {
		rover = MEM_MIN;
		build_multi_free(rover, rover, m_array[0].beg - 1);
	}
	endfree = rover;
	if (endfree < lo_mem_max) {
		for (i = 1; i < m_indx; i++) {
			if (endfree > m_array[i].beg)
				continue;
			build_multi_free(endfree, m_array[i-1].end + 1, m_array[i].beg - 1);
			endfree = m_array[i-1].end + 1;
		}
		if (m_array[m_indx-1].end != lo_mem_max)
			build_multi_free(endfree, m_array[m_indx-1].end + 1, lo_mem_max);
	}
	if (show_state) {
		print_nl("multi node: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1));
		print(" bytes");
		print_nl("empty multi size: ");
		print_int(sizeof(mword)*multi_empty());
		print(" bytes");
		print_nl("net multi used: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1 - multi_empty()));
		print(" bytes");
	}
}
	
	
build_single_free(endfree, beg, end)
	ptr	endfree, beg, end;
{
	ptr	p;
	
	if (endfree)
		link(endfree) = beg;
	p = beg;
	while(p != end) {
		link(p) = p + SMEM_SIZE;
		info(p) = 0;		/* make free list always same! DLP */
		p = p + SMEM_SIZE;
	}
	link(end) = NULL;
	info(end) = 0	;		/* make free list always same! DLP */
	if (show_stuff && show_state) {
		print_nl("created free list: beg= ");
		print_int(beg);
		print(", end= ");
		print_int(end);
	}
}
	
build_token_free(endfree, beg, end)
	ptr	endfree, beg, end;
{
	ptr	p;
	
	if (endfree)
		token_link(endfree) = beg;
	p = beg;
	while(p < end) {
		/* skip preloaded macro token_list area */
		if (p >= premac_lo && p <= premac_hi) 
			p = premac_hi + SMEM_SIZE;
		else {
			token_link(p) = p + SMEM_SIZE;
			p = p + SMEM_SIZE;
		}
	}
	token_link(end) = NULL;
	if (show_stuff && show_state) {
		print_nl("created free token_list: beg= ");
		print_int(beg);
		print(", end= ");
		print_int(end);
	}
}
	
build_multi_free(endfree, beg, end)
	ptr	endfree, beg, end;
{
	link(beg) = EMPTY_FLAG;
	node_size(beg) = end - beg + 1;
	if (beg != rover) {
		llink(beg) = endfree;
		rlink(endfree) = beg;
		rlink(beg) = rover;
		llink(rover) = beg;
	} else {
		rlink(endfree) = rover;
		llink(endfree) = rover;
	}
	if (show_stuff && show_state) {
		print_nl("created multi free list: addr= ");
		print_int(beg);
		print(", size= ");
		print_int(end-beg+1);
	}
}
		
single_empty()
{
	int	sum;
	ptr	p;
	
	sum = SMEM_SIZE;
	p = link(avail);
	while(p) {
		sum += SMEM_SIZE;
		p = link(p);
	}
	return(sum);
}

tok_empty()
{
	int	sum;
	ptr	p;
	
	sum = SMEM_SIZE;
	p = token_link(tok_head);
	while(p) {
		sum += SMEM_SIZE;
		p = token_link(p);
	}
	return(sum);
}

multi_empty()
{
	int	sum = 0;
	ptr	p;
	
	p = rover;
	do {
		sum += node_size(p);
		p = rlink(p);
	} while(p != rover);
	return(sum);
}

#endif INCTEX
