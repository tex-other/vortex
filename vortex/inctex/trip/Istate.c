#ifdef INCTEX
/*
 *
 *  This file is part of
 *
 *  IncTeX  --	The Olivetti-Berkeley-Matsushita Incremental TeX
 *
 *  An Editor-Independent, Adaptive, Incremental TeX Formatter
 *
 *  Copyright (C) 1988 by Regents of the University of California
 *
 *  Authors:
 *	Pehong Chen, currently at
 *	Olivetti Research Center, Computer Systems Research Lab
 *	Menlo Park, CA  USA
 *	(chen@orc.olivetti.com)
 *
 *	Ikuo Minakata, currently at
 *	Matsushita Electric Industrial Co., Information Systems Research Lab
 *	Osaka,  Japan
 *	(min@renoir.berkeley.edu)
 *
 *	Prof. Michael A. Harrison
 *	University of California,  Computer Science Dept.
 *	Berkeley, CA  USA
 *	(harrison@berkeley.edu)
 *
 *	Derluen Pan
 *	University of California,  Computer Science Dept.
 *	Berkeley, CA  USA
 *	(pan@ucbarpa.berkeley.edu)
 *
 *  All rights reserved by the copyright holders.  See the copyright
 *  notice distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  Plan: Write full state checkpoint at page 1.  Experiments show the delta
 *  from any other checkpt from page 1 is minimal except for the dynamic
 *  mem and token lists.  For page n, n>1, save delta's from page 1.  As of
 *  version 0.9, only the font_info & aux font arrays are saved this way. D. Pan
 *
 */

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

int		show_stuff = FALSE; /* 111 disable few show_state traces. DLP */
FDIM_CHUNK	*fdim_list;	/* ptr to list font_info locs changed */
FDIM_CHUNK	*fdim_end;	/* ptr to last chunk in list */

byte_file	stc_file;	/* state check point file */
short		beg_save_globals;
short		end_save_globals;
byte_file	str_file;	/* string pool file */
byte_file	stk_file;	/* token list file */

int	max_saved_page = 0;
fnt	last_font_ptr  = 0;

int	size_globals;
int	string_okay;		/* false if str corrupted */
int	string_empty = TRUE;	/* false if str succesfully loaded */

#define	SMEM_SIZE	1	/* upper mem[] are 1 mword/1 cell (IncTeX) */

/*
 * save context
 */

save_state()
{
	struct stat		st;
	char	fname[FILE_NAME_SIZE];

	/* time2 -- beginning of checkpointing;  */
	/* time2 - time1 = processing time exclusing checkpointing */

	open_state_file(SAVE, total_pages);
	print(" {");
	strcpy(fname, name_of_file);
	if (do_compression)
		strcat(fname, ".Z");
	print(fname);
	/* for trip test */
/*
	update_terminal();
*/

	/* save common global var */
	save_globals();
	/* save eqtb, font info, etc */
	save_arrays();
	/* save mem */
	save_mems();

	close_state_file();
	if (do_compression)
		compress_file();
	if (total_pages > max_saved_page)
		max_saved_page = total_pages;
	stat(fname, &st);
	print(" (");
	print_int(st.st_size);
	print(" bytes)}");
	update_terminal();
}

/*
 * restore context
 */

load_state (no)
	int	no;
{
	int	t_tmp,		/* fix for log/terminal position - DLP */
		f_tmp,
		t_old,
		f_old,
		select_old,
		select_tmp;	/* save current output redirection */

	print_nl("Loading state of page ");
	print_int(no);
	print(" from file ");
	print(name_of_file);
	print("...");
	update_terminal();

	t_old = term_offset;
	f_old = file_offset;
	select_old = selector;	/* save curr selector setting - DLP */
	restore_globals();
	t_tmp = term_offset;	/* save log/terminal position, ok only if */
	f_tmp = file_offset;	/* restore_globals didn't call print - DLP */
	select_tmp = selector;
	term_offset = t_old;	/* now switch output settings - DLP */
	file_offset = f_old;
	selector = select_old;
	restore_arrays();
	restore_mems();

	close_state_file();
	
	if (! load_str_file()) {
		print("failed");
		update_terminal();
		return(FALSE);
	}

	if (do_compression)
		compress_file();

	print("done");
	update_terminal();
	term_offset = t_tmp;	/* restore position & selector - DLP */
	file_offset = f_tmp;
	selector = select_tmp;
	return(TRUE);
}


load_str_file ()
{
	if (string_okay) {
		if (string_empty) {
			string_empty = FALSE;
			if (! load_string()) {
				string_okay = FALSE;
				qsc_check = FALSE;
				return(FALSE);
			}
		}
	} else
		return(FALSE);
}


/*
 *	open state file
 */

open_state_file (md, n)
	int	md;
	int	n;
{
	char	stc_name[FILE_NAME_SIZE];

	if (md == SAVE) {	/* open for save_state() */
		/* file name can be generated from string pool */
		get_ext(n, EXT_STC);
		if ((stc_file = fopen(name_of_file, "w")) == NULL) {
			print_nl("Failed to open state checkpoint file ");
			print(name_of_file);
			print(" for page ");
			print_int(n);
			print_char('.');
			return(FALSE);
		}
	} else {		/* open for load_state() */
		/* string pool is not loaded yet, so create .stc/.str file */
		/* name from command arg. when program invoked		   */
		sprintf(stc_name, "%s%s%d%s%s", base_fn, EXT_DEL, n,
			EXT_DEL, EXT_STC);
		if ((stc_file = fopen(stc_name, "r")) == NULL) {
/*
 * message here seems a little too verbose.
 */
/*
			print_nl("Failed to open state checkpoint file ");
			print(stc_name);
			print(" for page ");
			print_int(n);
			print_char('.');
*/
			return(FALSE);
		}
	}
}

/*
 *	close state file
 */
close_state_file()
{
	b_close(stc_file);
}

/*
 *	save context in global variables
 */
save_globals()
{
	unsigned long	bg, ed;
	F_NODE		*cur_input_fp,
			*input_stack_fp[STACK_SIZE];
/* save term_offset so log output same (get space 'stead of new line) - DLP */
	int		f_offset,
			t_offset;
	int		i;

	
	bg = (unsigned long)&beg_save_globals;
	ed = (unsigned long)&end_save_globals;
	size_globals = ed - bg;
	/* Save, then clear .fp pointers in cur_input, input_stack.
	   They are reallocated every time and change each time.
	   file_offset, term_offset also vary, don't matter - DLP */
	cur_input_fp = cur_input.fp;
	cur_input.fp = 0;
	for (i = 0; i < STACK_SIZE; i++) {
		input_stack_fp[i] = input_stack[i].fp;
		input_stack[i].fp = 0;
	}
	f_offset = file_offset;
	t_offset = term_offset;
	file_offset = 0;
	term_offset = 0;
	fwrite(&beg_save_globals, size_globals, 1, stc_file);
	if(show_state) {
		print_nl("saved global vars: ");
		print_int(size_globals);
		print(" bytes");
	}
	/* restore fp's etc. DLP */
	cur_input.fp = cur_input_fp;
	for (i = 0; i < STACK_SIZE; i++)
		input_stack[i].fp = input_stack_fp[i];
	file_offset = f_offset;
	term_offset = t_offset;
}

/*
 *	restore context in global variables
 */
restore_globals()
{
	int	size;
	unsigned long	bg, ed;

	bg = (unsigned long)&beg_save_globals;
	ed = (unsigned long)&end_save_globals;
	size = ed - bg;
	fread(&beg_save_globals, size, 1, stc_file);
}

/*
 *	save aux font arrays as arrays
 */
save_all_aux()
{
	char		aux_name[FILE_NAME_SIZE];
	byte_file	aux_file;
	int		amount   = 0;
	fnt		tmp;

	/* string array not ready yet for get_ext */
	sprintf(aux_name, "%s%s%d%s%s", base_fn, EXT_DEL, 1, EXT_DEL, "aux");
	if ((aux_file = fopen(aux_name,"w")) == NULL) {
		fprintf(stderr,"Couldn't open font aux file: %s\n",aux_name);
		return;
	} else {
		print_nl("font aux file name: ");
		print(aux_name);
	}
	last_font_ptr = font_lowater;		/* save lo water mark */
	font_lowater = -1;			/* note: saves above lowater */
	save_aux(&amount, font_ptr, aux_file);	/* save whole arrays */
	font_lowater = last_font_ptr;		/* restore lo water mark */
	if (show_state) {
		print_nl("No. fonts saved: ");
		print_int(font_ptr);
		print(";related font arrays in bytes: ");
		print_int(sizeof(fnt));
		print("+"); print_int(amount-sizeof(fnt));
	}
 	fclose(aux_file);
	last_font_ptr = font_ptr;	/* save hi water mark */
}

save_aux(size, hiwater, aux_file)
int *size;
fnt hiwater;
byte_file aux_file;
{
	int a;
	fnt segment;

	/* ### save semi-static? font info from font_lowater+1 THRU font_ptr.  DLP */
	a = ftell(aux_file);
	segment = hiwater - font_lowater;	/* size of range to be saved */
	fwrite(&segment, sizeof(fnt), 1, aux_file);  /* save hiwater */
	if (segment>0) {
	  /* Save font-related arrays that change only when font_ptr grows
	     (font is added).
	     font_used and font_glue can change when font_ptr does not,
	     and are saved in global var section! */
	  fwrite(&font_check[font_lowater+1],sizeof(qqqq) *segment, 1,aux_file);
	  fwrite(&font_size[font_lowater+1], sizeof(scal) *segment, 1,aux_file);
	  fwrite(&font_dsize[font_lowater+1],sizeof(scal) *segment, 1,aux_file);
	  fwrite(&font_params[font_lowater+1],sizeof(hword)*segment,1,aux_file);
	  fwrite(&font_name[font_lowater+1], sizeof(str)  *segment, 1,aux_file);
	  fwrite(&font_area[font_lowater+1], sizeof(str)  *segment, 1,aux_file);
	  fwrite(&font_bc[font_lowater+1],   sizeof(byte) *segment, 1,aux_file);
	  fwrite(&font_ec[font_lowater+1],   sizeof(byte) *segment, 1,aux_file);
	  fwrite(&hyphen_char[font_lowater+1],sizeof(int) *segment, 1,aux_file);
	  fwrite(&skew_char[font_lowater+1], sizeof(int)  *segment, 1,aux_file);
	  fwrite(&char_base[font_lowater+1], sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&width_base[font_lowater+1],sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&height_base[font_lowater+1],sizeof(ptr) *segment, 1,aux_file);
	  fwrite(&depth_base[font_lowater+1],sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&italic_base[font_lowater+1],sizeof(ptr) *segment, 1,aux_file);
	  fwrite(&lig_kern_base[font_lowater+1],sizeof(ptr)*segment,1,aux_file);
	  fwrite(&kern_base[font_lowater+1], sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&exten_base[font_lowater+1],sizeof(ptr)  *segment, 1,aux_file);
	  fwrite(&param_base[font_lowater+1],sizeof(ptr)  *segment, 1,aux_file);
	}
	*size = ftell(aux_file)-a;
}

/*
 *	check address of font related value (word) about to change, sort of
 *	in order or probability.
 *	could used font_ptr instead of FONT_MAX for preciser range checks
 *	on the other hand this lets the check be decided at compile time. DLP
 *
 *	font_used, font_glue may be alloc separately from font_check...param_base
 *	which will all be in one block.
 */
#define uint unsigned int
#define check_mem_addr(addr)  if ( \
	(addr>=(uint)&font_info[0] && addr<=(uint)&font_info[fmem_ptr]) || \
	(addr>=(uint)&font_used[0] && addr<=(uint)&font_used[FONT_MAX]) || \
	(addr>=(uint)&font_glue[0] && addr<=(uint)&font_glue[FONT_MAX]) || \
	(addr>=(uint)&font_check[0] && addr<=(uint)&param_base[FONT_MAX]) \
	) ; else {print_nl("Hey! changed (font) mem address "); print_int(addr); \
	   print(" not in allowed sections!"); exit(-1);}

/*
 *	record font related value (word) about to change
 */
record_mem_change(addr)
intptr addr;
{
	uint add = (uint) addr;
	check_mem_addr(add);
	if( fdim_end == NULL )
		{print_nl("Hey! fdim_end = NULL");exit(-1);}
	if( fdim_end->size >= FDIM_CHUNK_SIZE ) {
		GETFONTCHUNK( fdim_end->nxt );
		fdim_end = fdim_end->nxt;
		fdim_end->nxt = NULL;
		fdim_end->size = 0;
	}
	fdim_end->listchunk[fdim_end->size] = addr;
	fdim_end->size++;
}

/*
 *	save context in separate arrays
 */
save_arrays()
{
	ptr		fmem_seg;
	fnt		font_seg;
	int		size_aux;
	int		i;
	intptr		iptr;
	FDIM_CHUNK	*pt;
	int		fullcheckpoint;

	fullcheckpoint = total_pages<2;

	/* 111 check font aux arrays. DLP */
/*
	compare_aux();
*/

	fmem_seg = fmem_ptr - fmem_lowater;	/* fmem_ptr = first free loc */
	fwrite(&fmem_seg,     sizeof(ptr), 1, stc_file);
	if (fmem_seg > 0)
		fwrite(&font_info[fmem_lowater], sizeof(mword)*(fmem_seg), 1, stc_file);
	if (show_state) {
		print_nl("font_info segment size saved: "); print_int(sizeof(ptr));
		print("+"); print_int(sizeof(mword)*fmem_seg);
		print(" bytes; high = "); print_int(fmem_ptr);
		print(" low = "); print_int(fmem_lowater);
	}
	if (fullcheckpoint)
		fmem_lowater = fmem_ptr;	/* new water mark for full chkpt */

	size_aux = 0;
	save_aux(&size_aux,font_ptr,stc_file);
	if (show_state) {
		print_nl("aux font segment size saved: "); print_int(sizeof(fnt));
		print("+"); print_int(size_aux-sizeof(fnt));
		print(" bytes; high = "); print_int(font_ptr);
		print(" low = "); print_int(font_lowater);
	}
	if (fullcheckpoint)
		font_lowater = font_ptr;	/* new water mark for full chkpt */

	size_aux = 0;
	pt = fdim_list;
	if (show_state) print_nl("font mem change list: ");
	while (pt != NULL) {	/* save list of changed font dimensions */
		for (i=0;i<pt->size;i++) {
			if (show_state) {print_int(pt->listchunk[i]);print(" ");}
			fwrite(&pt->listchunk[i], sizeof(intptr), 1, stc_file);
			fwrite( pt->listchunk[i], sizeof(int),   1, stc_file);
			size_aux += sizeof(intptr) + sizeof(int);
		}
		pt = pt->nxt;
	}
	iptr = END_FONT_CH_LIST;
	fwrite(&iptr, sizeof(intptr), 1, stc_file);
	if (show_state) {
		print_nl("change list size (bytes) for font mem: ");
		print_int(sizeof(intptr));
		print("+");print_int(size_aux);
	}

	fwrite(&eqtb[0], sizeof(mword)*(EQTB_SIZE+1), 1, stc_file);
	if (show_state) {
		print_nl("saved eqtb size: ");
		print_int(sizeof(mword)*(EQTB_SIZE+1));
		print(" bytes");
	}

	/* save save_stack */
/* always save whole thing for comparison - DLP |||
	if (save_ptr > 0)
		fwrite(&save_stack[0], sizeof(mword)*save_ptr, 1, stc_file);
 */
		fwrite(&save_stack[0], sizeof(mword)*SAVE_SIZE,1, stc_file);
	if (show_state) {
		print_nl("actual save_stack size: ");
		print_int(sizeof(mword)*save_ptr);
		print(" bytes");
	}

	/* save hash table */
	fwrite(&hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1), 1, stc_file);
	if (show_state) {
		print_nl("saved hash size: ");
		print_int(sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
		print(" bytes");
		print(" hash_used = ");
		print_int(hash_used);
	}
}


/*
 *	compare aux arrays up to last_font_ptr, against copy from beg of page
 */

#define compare_aux_seg(arr,kind) {kind buffer,*elem; errors=FALSE;first=TRUE; \
	for (elem=(kind *) arr,i=0;i<last;i++,elem++) { \
		fread(&buffer, sizeof(kind), 1, aux_file); \
		if (buffer != *elem) {errcnt++; \
			if (first || errcnt<16) {print_nl("Surprise! arr ["); \
				print_int(i); print("] has changed."); \
			errors = TRUE;first = FALSE; \
	}	}	} \
	if (errors>0) {print_nl("errors up to arr: ");print_int(errcnt);} \
}

compare_aux()
{
	byte_file	aux_file;
	char		*ext_aux = "aux";
	fnt		font_seg;
	fnt		last;
	int		errcnt,	first, i, errors;

	/* check aux arrays, initial saved in INC/___.1.aux; DLP */
	get_ext(1,ext_aux);
	if ((aux_file = fopen(name_of_file,"r")) == NULL) {
		print_nl("Couldn't open full checkpt file: ");
		print(name_of_file);
		return;
	}
	fread(&last,sizeof(fnt),1,aux_file);
	if (last-1 != last_font_ptr) {
		print_nl("Hey! font aux hi water ("); print_int(last);
		print(") != last_font_ptr(");print_int(last_font_ptr); print(")");
	}
	errcnt = 0;
	if (last > 0) {
	  compare_aux_seg(font_check, scal); /* really qqqq, but structs are a pain */
	  compare_aux_seg(font_size,  scal);
	  compare_aux_seg(font_dsize, scal);
	  compare_aux_seg(font_params,hword);
	  compare_aux_seg(font_name,  str);
	  compare_aux_seg(font_area,  str);
	  compare_aux_seg(font_bc,    byte);
	  compare_aux_seg(font_ec,    byte);
	  compare_aux_seg(hyphen_char,int);
	  compare_aux_seg(skew_char,  int);
	  compare_aux_seg(char_base,  ptr);
	  compare_aux_seg(width_base, ptr);
	  compare_aux_seg(height_base,ptr);
	  compare_aux_seg(depth_base, ptr);
	  compare_aux_seg(italic_base,ptr);
	  compare_aux_seg(lig_kern_base,ptr);
	  compare_aux_seg(kern_base,  ptr);
	  compare_aux_seg(exten_base, ptr);
	  compare_aux_seg(param_base, ptr);
	}
 	fclose(aux_file);
	if (show_state && (errcnt == 0)) print_nl("No surprises in aux fonts.");
}

/*
 *	load font aux arrays from font_lowater to font_lowater+delta
 */

#define load_aux_seg(aux,elem,delta,file) \
	fread(&aux[font_lowater+1],sizeof(elem)*delta,1,file); \
	*size_aux += sizeof(elem)*delta

load_aux(size_aux, font_seg, datafile)
int		*size_aux;
fnt 		font_seg;
byte_file	datafile;
{
	load_aux_seg(font_check, qqqq, font_seg, datafile);
	load_aux_seg(font_size,	 scal, font_seg, datafile);
	load_aux_seg(font_dsize, scal, font_seg, datafile);
	load_aux_seg(font_params,hword,font_seg, datafile);
	load_aux_seg(font_name,  str,  font_seg, datafile);
	load_aux_seg(font_area,  str,  font_seg, datafile);
	load_aux_seg(font_bc,    byte, font_seg, datafile);
	load_aux_seg(font_ec,    byte, font_seg, datafile);
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
}

/*
 *	restore context in separate arrays
 */
restore_arrays()
{
	byte_file	chk_file;			/* checkpt file. DLP */
	char		chk_name[FILE_NAME_SIZE];
	ptr		fmem_seg;
	fnt		font_seg;
	ptr		tmp;
	fnt		tmpseg;
	int		size_aux;
	int		done;
        unsigned long   bg, ed;
	FDIM_CHUNK	*pt;
	uint		itemp;
	char		*ext_aux = "aux";
	int		fullcheckpoint;

	fullcheckpoint = total_pages==1;	/* for now, how we tell. */

	if (total_pages>1) { /* now load delta from last full checkpoint */
	/* for now, last full checkpt is always INC/___.1.stc; DLP */
	/* string array not ready yet for get_ext */
	  sprintf(chk_name, "%s%s%d%s%s", base_fn, EXT_DEL, 1, EXT_DEL, "stc");
	  if ((chk_file = fopen(chk_name,"r")) == NULL)
		fprintf(stderr,"Couldn't open full checkpt file: %s\n",chk_name);
	  else {
		if (show_state) {
			print_nl("full checkpt file name: ");
			print(chk_name);
		}
		bg = (unsigned long)&beg_save_globals;
		ed = (unsigned long)&end_save_globals;
		size_globals = ed - bg;
		fseek(chk_file,size_globals,SEEK_CUR);
		fread(&fmem_seg,     sizeof(ptr), 1, chk_file);
		if (fmem_seg > 0)
			fread(&font_info[0], sizeof(mword)*fmem_seg, 1, chk_file);
		if (show_state) {
			print_nl("font info full checkpt segment (bytes): ");
			print_int(sizeof(ptr)); print("+");
			print_int(sizeof(mword)*fmem_seg);
			print(" bytes; high = "); print_int(fmem_seg);
			print(" low = 0 of course");
		}
		tmpseg = font_lowater;
		font_lowater = -1;	/* load aux from 0 */
		fread(&font_seg,     sizeof(fnt), 1, chk_file);
		size_aux = 0;
		if (font_seg > 0)
			load_aux(&size_aux, font_seg, chk_file);
		if (show_state) {
			print_nl("aux font full checkpt segment (bytes): ");
			print_int(sizeof(fnt)); print("+"); print_int(size_aux);
			print(" bytes; high = "); print_int(font_seg-1);
			print(" low = -1 of course");
		}
		font_lowater = tmpseg;
		fclose( chk_file );
	  }
	}

	fread(&fmem_seg,     sizeof(ptr), 1, stc_file);
	if (fmem_seg != fmem_ptr - fmem_lowater)
		print_nl("font mem segment improper size!");
	if (fmem_seg > 0)
		fread(&font_info[fmem_lowater], sizeof(mword)*(fmem_seg), 1, stc_file);
	if (show_state) {
		print_nl("font_info segment size loaded: "); print_int(sizeof(ptr));
		print("+");print_int(sizeof(mword)*fmem_seg);
		print(" bytes; high = "); print_int(fmem_ptr);
		print(" low = "); print_int(fmem_lowater);
	}

	fread(&font_seg,     sizeof(fnt), 1, stc_file);
	if (font_seg != font_ptr - font_lowater)
		print_nl("font aux segment improper size!");
	size_aux = 0;
	if (font_seg > 0)
		load_aux(&size_aux, font_seg, stc_file);
	if (show_state) {
		print_nl("font aux segment size loaded: "); print_int(sizeof(fnt));
		print("+"); print_int(size_aux);
		print(" bytes; high = "); print_int(font_ptr);
		print(" low = "); print_int(font_lowater);
	}

	pt = fdim_list;	/* WARNING: if quiescence execution EVER allows */
	pt->size = 0;		/* font arrays to change with quiescence true, */
	size_aux = 0;		/* then curr & saved change lists will have to be */
	done = FALSE;		/* MERGED so no changes get lost (ugh!). DLP */
	if (show_state)
		print_nl("font mem change list: ");
	do {			/* save list of changed font dimensions */
		if (pt->size>=FDIM_CHUNK_SIZE) {	/* time for next chunk */
			pt = pt->nxt;
			if (pt == NULL) {  		/* add chunk to end */
				GETFONTCHUNK(pt);
				pt->nxt = NULL;
				fdim_end->nxt = pt;
				fdim_end = pt;
			}
			pt->size = 0;
		}
		fread(&(pt->listchunk[pt->size]), sizeof(intptr), 1, stc_file);
		if (show_state) {	/* show addr of word about to be reloaded */
			print_int(pt->listchunk[pt->size]);
			print(" ");
		}
		if (pt->listchunk[pt->size]==END_FONT_CH_LIST)
			done = TRUE;
		else {
			itemp = (uint) pt->listchunk[pt->size];
			check_mem_addr(itemp);
			fread(pt->listchunk[pt->size],sizeof(int),1,stc_file);
			size_aux += sizeof(int) + sizeof(mword);
			(pt->size)++;
		}
	} while (!done);
	if (show_state) {
		print_nl("change list size (bytes) for font_info: ");
		print_int(sizeof(intptr));
		print("+"); print_int(size_aux);
	}

/*
	save_all_aux(); */	/* 111 for comparing with end-of-page */

	
	if (fullcheckpoint) {
		font_lowater = font_ptr;	/* water mark for full chkpt DLP */
		fmem_lowater = fmem_ptr;	/* didn't get put into globals */
	}

#ifdef NOTIN
	/* ### save other semi-static? font info; should save up to font_ptr.  DLP */
	fread(&tmp,     sizeof(fnt), 1, stc_file);
	if( tmp != font_ptr) {
	  fprintf(stderr, "warning: font count (font_ptr) %d not what I remember %d\n",
		tmp, font_ptr);
	  font_ptr = tmp;
	}
	fread(&font_check[0],	sizeof(qqqq) *font_ptr, 1, stc_file);
	fread(&font_size[0],	sizeof(scal) *font_ptr, 1, stc_file);
	fread(&font_dsize[0],	sizeof(scal) *font_ptr, 1, stc_file);
	fread(&font_params[0],	sizeof(hword)*font_ptr, 1, stc_file);
	fread(&font_name[0],	sizeof(str)  *font_ptr, 1, stc_file);
	fread(&font_area[0],	sizeof(str)  *font_ptr, 1, stc_file);
	fread(&font_bc[0],	sizeof(byte) *font_ptr, 1, stc_file);
	fread(&font_ec[0],	sizeof(byte) *font_ptr, 1, stc_file);
	fread(&hyphen_char[0],	sizeof(int)  *font_ptr, 1, stc_file);
	fread(&skew_char[0],	sizeof(int)  *font_ptr, 1, stc_file);
	fread(&char_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&width_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&height_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&depth_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&italic_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&lig_kern_base[0],sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&kern_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&exten_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	fread(&param_base[0],	sizeof(ptr)  *font_ptr, 1, stc_file);
	if (show_state) {
		print_nl("aux font arrays loaded");
	}
#endif NOTIN
	
	fread(&eqtb[0], sizeof(mword)*(EQTB_SIZE+1), 1, stc_file);
	if (show_state) {
		print_nl("restored eqtb size: ");
		print_int(sizeof(mword)*(EQTB_SIZE+1));
		print(" bytes");
	}
	
	/* restore save_stack */
/* always have whole thing for comparisons - DLP |||
	if (save_ptr > 0)
		fread(&save_stack[0], sizeof(mword)*save_ptr, 1, stc_file);
 */
		fread(&save_stack[0], sizeof(mword)*SAVE_SIZE,1, stc_file);
	if (show_state) {
		print_nl("actual save_stack size: ");
		print_int(sizeof(mword)*save_ptr);
		print(" bytes");
	}

	/* restore hash table */
	fread(&hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1), 1, stc_file);
	if (show_state) {
		print_nl("restored hash size: ");
		print_int(sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
		print(" bytes");
	}
}

/*
 *	save mem structures
 */
save_mems()
{
	int i;
	/* write single cell mem[] node, only actual used part */
	save_single();

	/* write token_list node, only actual part */
	save_token();

	/* write multi cell mem[] node, only actually used part */
	save_multi();
	
	if (show_state) {
		print_nl("single node: ");
		print_int(sizeof(mword)*(mem_end-hi_mem_min+1 - single_empty()));
		print(" bytes");
		print_nl("tok_end= ");		print_int(tok_end);
		print("\ttok_low= ");		print_int(tok_low);
		print("\ttok_empty= ");		print_int(tok_empty());
		print_nl("token_list node: ");
		print_int(sizeof(tok)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
		print_nl("token_list link: ");
		print_int(sizeof(ptr)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
		print_nl("multi node: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1 - multi_empty()));
		print(" bytes");
	}
}

/* 
 *	save string arrays
 */
save_string()
{
	get_ext(-1, EXT_STR);
	if ((str_file = fopen(name_of_file, "w")) == NULL) {
		print_nl("Failed to open string file ");
		print(name_of_file);
		print_char('.');
		print_ln();
		update_terminal();
		return(FALSE);
	}
	
	/* write whole str_pool array */
	fwrite(&str_pool[0], sizeof(ascii)*POOL_SIZE, 1, str_file);
	
	/* write whole str_start array */
	fwrite(&str_start[0], sizeof(ptr)*MAX_STRINGS, 1, str_file);
	
	if (show_state) {
		print_nl("str_pool write: ");
		print_int(sizeof(ascii)*POOL_SIZE);
		print(" bytes");
		print_nl("str_start write: ");
		print_int(sizeof(ptr)*MAX_STRINGS);
		print(" bytes");
	}

	/* sleazy, we know this gets written at wrapup_inc at end of formatting
	   so we save max page count here. DLP */
	fwrite(&total_pages, sizeof(int), 1, str_file);
	if (show_state) {
		print_nl("max page count: ");
		print_int(total_pages);
		print(" (4 bytes)");
	}
	fclose(str_file);
}

/*
 *	restore string array
 */
load_string()
{
	char	str_name[FILE_NAME_SIZE];

	strcpy(str_name, base_fn);
	strcat(str_name, EXT_DEL);
	strcat(str_name, EXT_STR);
	if ((str_file = fopen(str_name, "r")) == NULL) {
		print_nl("Failed to open string file ");
		print(str_name);
		print_char('.');
		print_ln();
		update_terminal();
		return(FALSE);
	}
	
	/* read whole str_pool array */
	fread(&str_pool[0], sizeof(ascii)*POOL_SIZE, 1, str_file);
	
	/* read while str_start array */
	fread(&str_start[0], sizeof(ptr)*MAX_STRINGS, 1, str_file);

	if (show_state) {
		print_nl("str_pool read: ");
		print_int(sizeof(ascii)*POOL_SIZE);
		print(" bytes");
		print_nl("str_start read: ");
		print_int(sizeof(ptr)*MAX_STRINGS);
		print(" bytes");
	}
	
	fread(&last_max_pages, sizeof(int), 1, str_file);
	if (show_state) {
		print_nl("max page count: ");
		print_int(last_max_pages);
		print(" (4 bytes)");
	}
	fclose(str_file);
	return(TRUE);
}
	
/*
 *	save preloaded macro token_list area
 */
save_premac()
{
	get_ext(-1, EXT_STK);
	if ((stk_file = fopen(name_of_file, "w")) == NULL) {
		print_nl("Failed to open token list file ");
		print(name_of_file);
		print_char('.');
		print_ln();
		update_terminal();
		return(FALSE);
	}
	
	/* write whole preloaded macro token_list */
	fwrite(&tok_mem[premac_lo], sizeof(tok)*(premac_hi-premac_lo+1),
	       1, stk_file);
	
	/* write token_list link */
	fwrite(&tok_link[premac_lo], sizeof(ptr)*(premac_hi-premac_lo+1),
	       1, stk_file);

	if (show_state) {
		print_nl("token_list write: ");
		print_int(sizeof(tok)*(premac_hi-premac_lo+1));
		print(" bytes");
		print_nl("token_list link write: ");
		print_int(sizeof(ptr)*(premac_hi-premac_lo+1));
		print(" bytes");
	}
	fclose(stk_file);
}

/*
 *	restore preloaded macro token_list
 */
load_premac()
{
	char	str_name[FILE_NAME_SIZE];

	strcpy(str_name, base_fn);
	strcat(str_name, EXT_DEL);
	strcat(str_name, EXT_STK);
	if ((stk_file = fopen(str_name, "r")) == NULL) {
		print_nl("Failed to open token list file ");
		print(str_name);
		print_char('.');
		print_ln();
		update_terminal();
		return(FALSE);
	}
	
	/* read whole whole preloaded macro token_list */
	fread(&tok_mem[premac_lo], sizeof(tok)*(premac_hi-premac_lo+1),
	      1, stk_file);
	
	/* read token_list link */
	fread(&tok_link[premac_lo], sizeof(ptr)*(premac_hi-premac_lo+1),
	      1, stk_file);

	if (show_state) {
		print_nl("token_list read: ");
		print_int(sizeof(tok)*(premac_hi-premac_lo+1));
		print(" bytes");
		print_nl("token_list link read: ");
		print_int(sizeof(ptr)*(premac_hi-premac_lo+1));
		print(" bytes");	 
	}
	fclose(stk_file);
	return(TRUE);
}
	
		
#define	SARRY_SIZE	1000
typedef	struct {
		ptr	beg;
		ptr	end;
	} sary;

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


	/* compute req'ed area for mem map of hi_mem_min-mem_end */
	/* Note: each bit in map corresponds to 2 mem[] cell     */
	len = ((mem_end - hi_mem_min + 1)/SMEM_SIZE + (sizeof(char)*8 - 1)) /
		     (sizeof(char)*8);
	map = (char*)malloc(len);
	/* mark as all mem[] cells are used */
	for (i = 0, q = map; i < len; i++)
		*q++ = 0xff;
	
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
		last = NULL;
	} else {
		/* free list exist before first used chunk */
		avail = hi_mem_min;
		build_single_free(NULL, avail, s_array[0].beg - SMEM_SIZE);
		last = s_array[0].beg - SMEM_SIZE;
	}
	if (last < mem_end) {
		for ( i = 1; i < s_indx; i++) {
			if (last > s_array[i].beg)
				continue;
			build_single_free(last, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			last = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != mem_end)
			build_single_free(last, s_array[s_indx-1].end + 1, mem_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("single node: ");
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
	ptr	last;	/* to get token free lists mashed same, yech. DLP */


	len = ((tok_end - tok_low + 1) + (sizeof(char)*8 - 1)) /
		     (sizeof(char)*8);
	map = (char*)malloc(len);
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
		last = NULL;
	} else {
		/* free list exist before first used chunk */
		tok_head = tok_low;
		build_token_free(NULL, tok_head, s_array[0].beg - SMEM_SIZE);
		last = s_array[0].beg - SMEM_SIZE;
	}
	if (last < tok_end) {
		for ( i = 1; i < s_indx; i++) {
			if (last > s_array[i].beg)
				continue;
			build_token_free(last, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			last = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != tok_end)
			build_token_free(last, s_array[s_indx-1].end + 1, tok_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("token head: ");
		print_int(tok_head);
		print_nl("token_list node: ");
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
	ptr	beg, end;	/* mush free list to the same, DLP */
	ptr	last;
	
	
	len = ((lo_mem_max - MEM_MIN +1) + (sizeof(char)*8 -1)) /
		(sizeof(char)*8);
	map = (char *)malloc(len);
	/* mark as all mem[] cells are used */
	for (i = 0, q = map; i < len; i++)
		*q++ = 0xff;
	
	p = rover;
	do {
		off_memmap(p, node_size(p), map, MEM_MIN, 1);
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
	last = rover;
	if (last < lo_mem_max) {
		for (i = 1; i < m_indx; i++) {
			if (last > m_array[i].beg)
				continue;
			build_multi_free(last, m_array[i-1].end + 1, m_array[i].beg - 1);
			last = m_array[i-1].end + 1;
		}
		if (m_array[m_indx-1].end != lo_mem_max)
			build_multi_free(last, m_array[m_indx-1].end + 1, lo_mem_max);
	}
	if (show_state) {
		print_nl("multi node: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1));
		print(" bytes");
		print_nl("empty multi size: ");
		print_int(sizeof(mword)*multi_empty());
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
	
	
/* 
 *	restore mem structures
 */
restore_mems()
{
	ptr tmp;

	/* read single cell mem[] */
	restore_single();
	
	/* restore token_list node */
	restore_token();
	
	/* read multi cell mem[] */
	restore_multi();
	/* try to get mem the same - DLP */
	/* crunch();*/	/* so multi-mem always looks the same. DLP */
	if (show_state) {
		print_nl("single node: ");
		print_int(sizeof(mword)*(mem_end-hi_mem_min+1 - single_empty()));
		print(" bytes");
		print_nl("tok_end= ");		print_int(tok_end);
		print("\ttok_low= ");		print_int(tok_low);
		print("\ttok_empty= ");		print_int(tok_empty());
		print_nl("token_list node: ");
		print_int(sizeof(tok)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
		print(" bytes");
		print_nl("token_list link: ");
		print_int(sizeof(ptr)*((tok_end-tok_low+1) -
				     (premac_hi-premac_lo+1) - tok_empty()));
	}
}

/*
 *	restore upper half mem
 */
restore_single()
{
	sary	s_array[SARRY_SIZE];
	int	s_indx;
	int	i;
	ptr	beg, end;
	ptr	last;

	fread(&s_indx, sizeof(int), 1, stc_file); /* get # of chunks to read */
	s_array[0].beg = mem_end+1;	/* in case single area is empty! DLP */
	for (i = 0; i < s_indx; i++) {	/* read chunk */
		fread(&beg, sizeof(ptr), 1, stc_file);
		fread(&end, sizeof(ptr), 1, stc_file);
		s_array[i].beg = beg;
		s_array[i].end = end;
		fread(&mem[beg], (end - beg + 1)*sizeof(mword), 1, stc_file);
		if(show_state) {
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
		last = NULL;
	} else {
		/* free list exist before first used chunk */
		avail = hi_mem_min;
		build_single_free(NULL, avail, s_array[0].beg - SMEM_SIZE);
		last = s_array[0].beg - SMEM_SIZE;
	}
	if (last < mem_end) {
		for ( i = 1; i < s_indx; i++) {
			if (last > s_array[i].beg)
				continue;
			build_single_free(last, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			last = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != mem_end)
			build_single_free(last, s_array[s_indx-1].end + 1, mem_end - (SMEM_SIZE - 1));
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

/*
 *  	restore token_list node 
 */
restore_token()
{
	sary	s_array[SARRY_SIZE];
	int	s_indx;
	int	i;
	ptr	beg, end;
	ptr	last;

	/* restore preloaded macro  token_list */
	load_premac();

	fread(&s_indx, sizeof(int), 1, stc_file); /* get # of chunks to read */
	s_array[0].beg = tok_end+1;	/* in case token mem empty! DLP */
	for (i = 0; i < s_indx; i++) {	/* read chunk */
		fread(&beg, sizeof(ptr), 1, stc_file);
		fread(&end, sizeof(ptr), 1, stc_file);
		s_array[i].beg = beg;
		s_array[i].end = end;
		fread(&tok_mem[beg],  (end - beg + 1)*sizeof(tok), 1, stc_file);
		fread(&tok_link[beg], (end - beg + 1)*sizeof(ptr), 1, stc_file);
		if(show_state) {
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
		last = NULL;
	} else {
		/* free list exist before first used chunk */
		tok_head = tok_low;
		build_token_free(NULL, tok_head, s_array[0].beg - SMEM_SIZE);
		last = s_array[0].beg - SMEM_SIZE;
	}
	if (last < tok_end) {
		for ( i = 1; i < s_indx; i++) {
			if (last > s_array[i].beg)
				continue;
			build_token_free(last, s_array[i-1].end + 1, s_array[i].beg - SMEM_SIZE);
			last = s_array[i].beg - SMEM_SIZE;
		}
		if (s_array[s_indx-1].end != tok_end)
			build_token_free(last, s_array[s_indx-1].end + 1, tok_end - (SMEM_SIZE - 1));
	}
	if (show_state) {
		print_nl("token head: ");
		print_int(tok_head);
		print_nl("token_list node: ");
		print_int(sizeof(tok)*(tok_end-tok_low+1));
		print(" bytes");
		print_nl("empty token_list size: ");
		print_int(sizeof(tok)*tok_empty());
		print(" bytes");
	}
}
	
/*
 *	restore lower half mem
 */
restore_multi()
{
	sary	m_array[SARRY_SIZE];
	int	m_indx;
	int	i;
	ptr	beg, end;
	ptr	last;
	
	fread(&m_indx, sizeof(int), 1, stc_file); /* get # of chunks to read */
	m_array[0].beg = lo_mem_max+1;	/* in case single area is empty! DLP */
	for (i = 0; i < m_indx; i++) {	/* read chunk */
		fread(&beg, sizeof(ptr), 1, stc_file);
		fread(&end, sizeof(ptr), 1, stc_file);
		m_array[i].beg = beg;
		m_array[i].end = end;
		fread(&mem[beg], (end - beg + 1)*sizeof(mword), 1, stc_file);
		if(show_state) {
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
	last = rover;
	if (last < lo_mem_max) {
		for (i = 1; i < m_indx; i++) {
			if (last > m_array[i].beg)
				continue;
			build_multi_free(last, m_array[i-1].end + 1, m_array[i].beg - 1);
			last = m_array[i-1].end + 1;
		}
		if (m_array[m_indx-1].end != lo_mem_max)
			build_multi_free(last, m_array[m_indx-1].end + 1, lo_mem_max);
	}
	if (show_state) {
		print_nl("multi node: ");
		print_int(sizeof(mword)*(lo_mem_max-MEM_MIN+1));
		print(" bytes");
		print_nl("empty multi size: ");
		print_int(sizeof(mword)*multi_empty());
		print(" bytes");
	}
}
	
	
build_single_free(last, beg, end)
	ptr	last, beg, end;
{
	ptr	p;
	
	if (last)
		link(last) = beg;
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
	
build_token_free(last, beg, end)
	ptr	last, beg, end;
{
	ptr	p;
	
	if (last)
		token_link(last) = beg;
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
	
build_multi_free(last, beg, end)
	ptr	last, beg, end;
{
	ptr	p;
	
	link(beg) = EMPTY_FLAG;
	node_size(beg) = end - beg + 1;
	if (beg != rover) {
		llink(beg) = last;
		rlink(last) = beg;
		rlink(beg) = rover;
		llink(rover) = beg;
	} else {
		rlink(last) = rover;
		llink(last) = rover;
	}
	if (show_stuff && show_state) {
		print_nl("created multi free list: addr= ");
		print_int(beg);
		print(", size= ");
		print_int(end-beg+1);
	}
}
		
compress_file()
{
	int	pid, wait_result, status;
	char	comp_name[FILE_NAME_SIZE + 1];
	struct	stat	st_buf;

	pid = vfork();
	if (pid == 0) {		/* child process */
		execl("/usr/ucb/compress", "compress", "-f", (char *)name_of_file, 0);
		print_nl( "Unable to invoke compress.");
		print_ln();
		jump_out();
	}
	do {
		wait_result = wait(&status);
	} while (wait_result != pid && wait_result != -1);

	if (show_state) {
		sprintf(comp_name, "%s.Z", name_of_file);
		stat(comp_name, &st_buf);
		fprintf(stderr, "\ncompressed file size: %d bytes",
			st_buf.st_size);
	}
}

uncompress_file (n)
	int	n;
{
	char	uncomp_name[FILE_NAME_SIZE + 1];
	int	pid, wait_result, status;

	get_ext(n, EXT_STC);
	sprintf(uncomp_name, "%s.%s", name_of_file, "Z");
	if (access(uncomp_name, F_OK) == -1)
		return;
	pid = vfork();
	if (pid == 0) {		/* child process */
		execl("/usr/ucb/uncompress", "uncompress", "-f", uncomp_name, 0);
		print_nl( "Unable to invoke compress.");
		print_ln();
		jump_out();
	}
	do {
		wait_result = wait(&status);
	} while (wait_result != pid && wait_result != -1);
}

flush_saved_state()
{
	int	i;

	for (i = 0; i <= max_saved_page; i++) {
		get_ext(i, EXT_STC);
		unlink(name_of_file);
		strcat(name_of_file, ".Z");
		unlink(name_of_file);
	}
	get_ext(-1, EXT_STR);
	unlink(name_of_file);
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
