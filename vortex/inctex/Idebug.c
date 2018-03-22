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
#ifdef DEBUG
/*  This file is part of IncTeX 1.0
 *  An Incremental TeX Formatter
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
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
#include	"Idebug.h"

mword	eqtb_copy[EQTB_SIZE+1];
int	eqtb_diff[EQTB_SIZE+1] = {0};
hh	hash_copy[UNDEFINED_CONTROL_SEQUENCE+1];
int	hash_diff[UNDEFINED_CONTROL_SEQUENCE+1] = {0};
qword	xeq_copy[EQTB_SIZE+1 - INT_BASE];
int	xeq_diff[EQTB_SIZE+1 - INT_BASE] = {0};

int	instate = 0;
tok	*tokmem_copy = NULL;
ptr	*toklin_copy = NULL;
int	hash_count = 0;

extern	byte_file	stc_file;	/* state check point file */
extern	fnt     	font_last;	/* ...& font_ptr */
extern	int		max_saved_page;

/* for tracking down in DBX where we are in save_state. DLP */
probestate()
{
	/*instate = ftell(stc_file);!!-!!*/
}

dump_multifree()
{
	ptr	p;

	p = rover;
	do {
		print_nl("free(");print_int(p); print("->");
		print_int(p-1+node_size(p));
		p = rlink(p);
	} while(p != rover);
	
	print_ln();
}

dump_tokenfree()
{
	ptr tlast,now;

	print_nl("Token free list:"); print_ln();
	tlast = NULL;
	now = tok_head;
	while (now != NULL) {
		print_int(now);
		tlast = now;
		now = tok_link[now];
		if (now == tlast+1) {
		  do {tlast = now;now=tok_link[now];}
		  while (now==tlast+1);
		  print(" - ");print_int(tlast);
		} else if (now == tlast-1) {
		  do {tlast = now;now=tok_link[now];}
		  while (now==tlast-1);
		  print(" - ");print_int(tlast);
		}
		print_ln();
	}
	if (show_state) { /*!!-!!*/
	 print_nl("END_TEMPLATE_TOKEN");print_int(END_TEMPLATE_TOKEN);
	 print_nl("RIGHT_BRACE_TOKEN + '}'");print_int(RIGHT_BRACE_TOKEN + '}');
	 print_nl("LEFT_BRACE_TOKEN + '{'");print_int(LEFT_BRACE_TOKEN + '{');
	 print_nl("ACTIVE_BASE");print_int(ACTIVE_BASE);
	 print_nl("CS_TOKEN_FLAG+FROZEN_END_GROUP");print_int(CS_TOKEN_FLAG+FROZEN_END_GROUP);
	 print_nl("MATH_SHIFT_TOKEN + '$'");print_int(MATH_SHIFT_TOKEN + '$');
	 print_nl("CS_TOKEN_FLAG+FROZEN_RIGHT");print_int(CS_TOKEN_FLAG+FROZEN_RIGHT);
	 print_nl("OTHER_TOKEN + '.'");print_int(OTHER_TOKEN + '.');
	 print_nl("CS_TOKEN_FLAG+FROZEN_DONT_EXPAND");print_int(CS_TOKEN_FLAG+FROZEN_DONT_EXPAND);
	 print_nl("END_WRITE_TOKEN");print_int(END_WRITE_TOKEN);
	 print_nl("CS_TOKEN_FLAG");print_int(CS_TOKEN_FLAG);
	 print_nl("CS_TOKEN_FLAG + FROZEN_CR");print_int(CS_TOKEN_FLAG+FROZEN_CR);
	 print_nl("END_TEMPLATE_TOKEN");print_int(END_TEMPLATE_TOKEN);
	}
}


dump_inputs()
{
	F_NODE	*f;
	int	i;

	print_nl("cur_input id ="); print_int(cur_input.fid);
	if (cur_input.fid >= 0)
	  if (virgin && cur_input.fp != NULL) {
		print(" fname = ");print(cur_input.fp->sn);
		print(" fid = ");print_int(cur_input.fp->id);
	  } else if (fnds != NULL) {
		print(" fname = ");print(fnds[cur_input.fid]->sn);
		print(" fid = ");print_int(fnds[cur_input.fid]->id);
	  }
	for (i=0;i<input_ptr;i++) {
		print_nl("id = ");print_int(input_stack[i].fid);
		if (input_stack[i].fid >= 0)
	  	  if (virgin && cur_input.fp != NULL) {
			print(" fname = ");print(input_stack[i].fp->sn);
			print(" fid = ");print_int(input_stack[i].fp->id);
		  } else if (fnds != NULL) {
			print(" fname = ");print(fnds[input_stack[i].fid]->sn);
			print(" fid = ");print_int(fnds[input_stack[i].fid]->id);
		  }
	}
	print_nl("f_nodes:");
	for (i = 0, f= f_bgn; ((i < f_max) && (f!= NULL));
	     i++, f= f->nxt) {
		print_nl("fname = ");print(f->sn);
		print(" fid = ");print_int(f->id);
	}
}

/*	copy hash table & eqtb
 */
save_eqhashxeq()
{
	int	i;

	/* save eqtb */
	for (i=0;i<=EQTB_SIZE;i++) eqtb_copy[i].i = eqtb[i].i;
	/* save hash */
	for (i=0;i<=UNDEFINED_CONTROL_SEQUENCE;i++)
		hash_copy[i].hh1 = hash[i].hh1;
	/* save xeq_level */
	for (i=0;i<=EQTB_SIZE+1-INT_BASE;i++) xeq_copy[i] = xeq_level[i];

}

/*	save aux font arrays as arrays
 */
save_all_aux()
{
	char		aux_name[FILE_NAME_SIZE];
	byte_file	aux_file;
	int		i, amount   = 0;
	fnt		tmp;

	/* string array not ready yet for get_ext */
	sprintf(aux_name, "%s%s%s", base_fn, EXT_DEL, "auxc");
	if ((aux_file = fopen(aux_name,"w")) == NULL) {
		fprintf(stderr,"Couldn't open font aux file: %s\n",aux_name);
		return;
	} else if (show_state) {
		print_nl("font aux file name: "); print(aux_name);
	}
	save_aux(&amount, font_ptr, -1, aux_file);	/* save whole arrays */
	if (show_state) {
		print_nl("No. fonts saved: "); print_int(font_ptr);
		print(";related font arrays in bytes: ");
		print_int(2*sizeof(fnt)); print("+"); print_int(amount);
	}
 	fclose(aux_file);
}

/*	compare aux arrays up to font_last, against copy from beg of page
 */
#define compare_aux_seg(arr,kind) {kind buffer,*elem; errors=FALSE;first=TRUE; \
	for (elem=(kind *) arr,i=0;i<=flast;i++,elem++) { \
		fread(&buffer, sizeof(kind), 1, aux_file); \
		if (buffer != *elem) {errcnt++; \
			if (first || errcnt<16) { \
				print_nl("Font change noted: arr ["); \
				print_int(i); print("] has changed."); \
			errors = TRUE;first = FALSE; \
	}	}	} \
	if (errors>0) {print_nl("changes up to arr: ");print_int(errcnt);} \
}

compare_aux()
{
	byte_file	aux_file;
	char		*ext_aux = "auxc";
	fnt		lowater;
	fnt		flast;
	int		errcnt,	first, i, errors;

	/* check aux arrays, initial saved in INC/___.1.aux; DLP */
	get_ext(-1,ext_aux);
	if ((aux_file = fopen(name_of_file,"r")) == NULL) {
		print_nl("Couldn't open full checkpt file: ");
		print(name_of_file);
		return;
	}
	errcnt = 0;
	flast = font_last;
	if (flast > 0) {
	  compare_aux_seg(font_check, scal); /*really qqqq/structs are a pain*/
	  compare_aux_seg(font_size,  scal);
	  compare_aux_seg(font_dsize, scal);
	  compare_aux_seg(font_params,hword);
	  compare_aux_seg(font_name,  str);
	  compare_aux_seg(font_area,  str);
	  compare_aux_seg(font_bc,    byte);
	  compare_aux_seg(font_ec,    byte);
	  compare_aux_seg(font_glue,  ptr);
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
 *	check address of font related value (word) about to change, sort of
 *	in order of probability.
 *	could used font_ptr instead of FONT_MAX for preciser range checks
 *	on the other hand this lets the check be decided at compile time. DLP
 *
 *	font_used, font_glue may be alloc separately from
 *	font_check...param_base which will all be in one block.
 *
 *	***WARNING: NOT PORTABLE, DEPENDS ON FORCING ALLOCATION***
 */
#define uint unsigned int
#define check_mem_addr(addr)  if ( \
	(addr>=(uint)&font_info[0] && addr<=(uint)&font_info[fmem_ptr]) || \
	(addr>=(uint)&font_used[0] && addr<=(uint)&font_used[FONT_MAX]) || \
	(addr>=(uint)&font_glue[0] && addr<=(uint)&font_glue[FONT_MAX]) || \
	(addr>=(uint)&font_check[0] && addr<=(uint)&param_base[FONT_MAX]) \
	) ;else {print_nl("Hey! changed (font) mem address ");print_int(addr); \
	   print(" not in allowed sections!"); return;}


/*	check eqtb has changed only in places noted in dynamic list
 */
compare_eqtb()
{
	int		errcnt = 0,
			extras = 0,
			used = 0,
			i;

	if (!trace_eq) return;
	/* check eqtb array; DLP */
	for (i=0;i<=EQTB_SIZE;i++) {
		if (eqtb[i].i != 0) used++;	/* Is this the right way? */
		if (eqtb_diff[i]) {
		  if (eqtb_copy[i].i == eqtb[i].i) {
		      /*print_nl("eqtb entry wasn't really changed: ");
			print_int(i); */
			extras++;
		  }
		  eqtb_diff[i] = FALSE;		/* clear for next round */
		} else {
		  if (eqtb_copy[i].i != eqtb[i].i) {
			print_nl("missed change to eqtb entry! - ");
			print_int(i);
			errcnt++;
		  }
		}
	}
	if (show_state) {
	  print_nl("Missed changes to eqtb: "); print_int(errcnt);
	  print(" Unneeded eqtb saves: ");      print_int(extras);
	  print(" Used eqtb entries: ");        print_int(used);
	  update_terminal();
	} else if (errcnt>0){
	  print_nl("!Missed changes to eqtb: "); print_int(errcnt);
	  update_terminal();
	}
}

/*	check hash has changed only in places noted in dynamic list
 */
compare_hash()
{
	int	errcnt = 0,
		extras = 0,
		used = 0,
		i;
	int	*c,*s;

	if (!trace_hash) return;
	/* check hash array; DLP */
	s = (int *) &hash[0];
	c = (int *) &hash_copy[0];
	for (i=0;i<=UNDEFINED_CONTROL_SEQUENCE;i++,s++,c++) {
		if (text(i)) used++;	/* count entries used */
		if (hash_diff[i]) {
		  if ( (*s) == (*c)) {
		      /*print_nl("hash entry wasn't really changed: ");
			print_int(i); */
			extras++;
		  }
		  hash_diff[i] = FALSE; 	/* clear for next round */
		} else {
		  if ( (*s) != (*c)) {
			print_nl("missed change to hash entry! - ");
			print_int(i);
			errcnt++;
		  }
		}
	}
	if (show_state) {
	  print_nl("Missed changes to hash: "); print_int(errcnt);
	  print(" Unneeded hash saves: ");      print_int(extras);
	  print(" Used hash entries: ");        print_int(used);
	  update_terminal();
	} else if (errcnt>0){
	  print_nl("!Missed changes to hash: "); print_int(errcnt);
	}
}


/*	check xeq_level has changed only in places noted in dynamic list
 */
compare_xeq()
{
	int	errcnt = 0,
		extras = 0,
		used = 0,
		i;
	qword	*c,*s;

	if (!trace_xeq) return;
	/* check xeq array; DLP */
	s = &xeq_level[0];
	c = &xeq_copy[0];
	for (i=0;i<=EQTB_SIZE+1 - INT_BASE;i++,s++,c++) {
		if ((*s) != LEVEL_ONE) used++;/* count nonnull entries */
		if (xeq_diff[i]) {
		  if ( (*s) == (*c)) {
		      /*print_nl("xeq entry wasn't really changed: ");
			print_int(i); */
			extras++;
		  }
		  xeq_diff[i] = FALSE; 	/* clear for next round */
		} else {
		  if ( (*s) != (*c)) {
			print_nl("missed change to xeq entry! - ");
			print_int(i);
			errcnt++;
		  }
		}
	}
	if (show_state) {
	  print_nl("Missed changes to xeq: "); print_int(errcnt);
	  print(" Unneeded xeq saves: ");      print_int(extras);
	  print(" Used xeq entries: ");        print_int(used);
	  update_terminal();
	} else if (errcnt>0){
	  print_nl("!Missed changes to xeq: "); print_int(errcnt);
	}
}

/*	save preloaded macro token_list area
 */
save_premac()
{
/* 	for checking predefined macro's in token area loadef from .fmt. DLP */
	if (tokmem_copy == NULL)
			MALLOC(tokmem_copy, tok, sizeof(tok)*(premac_hi-premac_lo+1));
	if (toklin_copy == NULL)
			MALLOC(toklin_copy, ptr, sizeof(ptr)*(premac_hi-premac_lo+1));
	bcopy(&tok_mem[premac_lo],  tokmem_copy, sizeof(tok)*(premac_hi-premac_lo+1));
	bcopy(&tok_link[premac_lo], toklin_copy, sizeof(ptr)*(premac_hi-premac_lo+1));
}

flush_saved_state()
{
	int	i;

	for (i = 0; i <= max_saved_page; i++) {
		get_ext(i, EXT_STC);
		unlink(name_of_file);
	}
	get_ext(-1, EXT_STR);
	unlink(name_of_file);
}

#ifdef ARRAY
/* 
 * save mem arrays as arrays. DLP
 */
save_marray(flag)
int	flag;
{		/* ||| dump mem array - DLP */
	char		mem_name[FILE_NAME_SIZE];
	byte_file	memfile;
	int		lm,
			ltm = sizeof(tok)*(TOK_MAX-TOK_MIN+1),
			ltl = sizeof(ptr)*(TOK_MAX-TOK_MIN+1);
	
	sprintf(mem_name, "%s%s%d%s%s", base_fn, EXT_DEL, total_pages,
			EXT_DEL, "mem");
	if ((memfile = fopen(mem_name,"w")) == NULL)
		fprintf(stderr,"Couldn't open mem array file: %s\n",mem_name);
	else {
	  print_nl("mem array file name: ");
	  print(mem_name);
	  if (flag == 0) {
		lm  = sizeof(mword)*(mem_end-hi_mem_min+1);
	  	print_nl("saving single mem\n");
		fwrite(&mem[hi_mem_min],lm  ,1,memfile);
/*		fwrite(tok_mem  ,ltm        ,1,memfile);
		fwrite(tok_link ,ltl        ,1,memfile);
 */		print("size single mem array="); print_int(lm);
		print(", tok_mem=");  print_int(ltm);
		print(", tok_link="); print_int(ltl);
	    } else {
		lm  = sizeof(mword)*(lo_mem_max-MEM_MIN+1);
	  	print_nl(stderr,"saving multi mem\n");
 		fwrite(&mem[MEM_MIN],   lm  ,1,memfile);
		print("size multi  mem array="); print_int(lm);
		print(", tok_mem=");  print_int(ltm);
		print(", tok_link="); print_int(ltl);
	    }
 	  fclose(memfile);
	}
}


load_marray(flag)
int	flag;
{
/* changes to load mem array - DLP */
	char		mem_name[FILE_NAME_SIZE];
	byte_file	memfile = NULL;
	int		lm,
			ltm = sizeof(tok)*(TOK_MAX-TOK_MIN+1),
			ltl = sizeof(ptr)*(TOK_MAX-TOK_MIN+1);
	
	sprintf(mem_name, "%s%s%d%s%s", base_fn, EXT_DEL, total_pages,
			EXT_DEL, "mem");
	if ((memfile = fopen(mem_name,"r")) == NULL)
		print_nl("Couldn't open mem array file: %s\n",mem_name);
	else {
	  print_nl("mem array file name: ");
	  print(mem_name);
	  if (flag == 0) {
		lm  = sizeof(mword)*(mem_end-hi_mem_min+1);
	  	print_nl(stderr,"loading single mem");
		print_ln();
		fread(&mem[hi_mem_min],lm  ,1,memfile);
/*		fread(tok_mem  ,ltm        ,1,memfile);
		fread(tok_link ,ltl        ,1,memfile);
 */		print("size single mem array="); print_int(lm);
		print(", tok_mem=");  print_int(ltm);
		print(", tok_link="); print_int(ltl);
	  } else {
		lm  = sizeof(mword)*(lo_mem_max-MEM_MIN+1);
	  	print_nl("loading multi mem"); print_ln();
 		fread(&mem[MEM_MIN],   lm  ,1,memfile);
		print("size multi  mem array="); print_int(lm);
		print(", tok_mem=");  print_int(ltm);
		print(", tok_link="); print_int(ltl);
	  }
 	  fclose(memfile);
	}
}

#ifdef NOUNDUMP
/* These routines for the preloaded macro's aren't needed anymore.
   save_premac, load_premac only needed if not using UNDUMP version, even so,
   should be done via loading FMT files. DLP
 */

#define	EXT_STK			"stk"
byte_file       stk_file;       /* token list file */


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
	if (fread(&tok_mem[premac_lo], sizeof(tok)*(premac_hi-premac_lo+1),
	      1, stk_file) < 1) return(FALSE);
	
	/* read token_list link */
	if (fread(&tok_link[premac_lo], sizeof(ptr)*(premac_hi-premac_lo+1),
	      1, stk_file) < 1) return(FALSE);

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
#endif NOUNDUMP
	
		
pr_singlefree()
{
	ptr	p;
	int	freesize;

	if (!show_state) return;
	p = avail;
	freesize = 0;
	print_nl("avail list: ");
	print_int(p);
	while(p != NULL) {
		/* go print free list */
		p = link(p);
		print(" ");
		print_int(p);
		freesize++;
	}
	print_nl("avail list size: ");
	print_int(freesize*sizeof(ptr));
	print("; avail = ");
	print_int(avail);
}

pr_tokenfree()
{
	ptr	p;
	int	freesize;

	if (!show_state) return;
	p = tok_head;
	freesize = 0;
	print_nl("token free list : ");
	print_int(p);
	while(p != NULL) { /* go print free list */
		p = token_link(p);
		print(" ");
		print_int(p);
		freesize++;
	}
	print_nl("token free list size: ");
	print_int(freesize*sizeof(ptr));
	print("; tok_head = ");
	print_int(tok_head);
}

pr_multifree()
{
	ptr	p;
	int	freesize;

	if (!show_state) return;
	p = rover;
	freesize = 0;
	print_nl("multi free list : ");
	print_int(p);
	do {
		p = rlink(p);
		print(" ");
		print_int(p);
		freesize++;
	} while(p != rover);
	print_nl("multi free list size: ");
	print_int(freesize*sizeof(mword));
	print("; rover = ");
	print_int(rover);
}

/*	OBSOLETE SUBRTN:
 *	open full checkpt file - page 1. DLP
 *	For now, last full checkpt is always INC/___.1.stc; DLP
 *	string array not ready yet for get_ext.
 */
open_full_chkpt (chk_file)
	byte_file	*chk_file;
{
	char	chk_name[FILE_NAME_SIZE];
	int	page,
		lastfull,
		lastcompressed;

	sprintf(chk_name, "%s%s%d%s%s", base_fn, EXT_DEL, 1, EXT_DEL, "stc");
	if ((*chk_file = fopen(chk_name,"r")) == NULL) {
		print_nl("Couldn't read full checkpt file: ");
		print(chk_name);
		exit( -5);
	} else {
		print_nl("- also reading full checkpt file: ");
		print(chk_name);
		print("...");
		read_state_code( &page, &lastfull, &lastcompressed, *chk_file);
		check_state_type(page, &lastfull);
	}
}

/*	read checkpoint layout version (1 word) ... obsolete
 *	and checkpoint file type - full or incremental (1 word). DLP
 */
check_state_type(page,fullcheckpoint)
	int 	page,
		*fullcheckpoint;
{
	if( page > 1) {	/* should be incr checkpt */
	    if( *fullcheckpoint ) {
		print_nl("Hey! Should be loading incremental checkpoint (pg .");
		print_int(page); print(").");
		print_nl("...trying to continue with incr chkpt load.");
		*fullcheckpoint = FALSE;
	    }
	}
}

#endif ARRAY


#ifdef CHECKSTATE
#define MIN(A,B) ((A > B) ? B : A)
#define ULONG unsigned long

#define		DiffListCode		0x88
#define		Array_NoDiff		0x11

/* dummy defines */
#define  CHECK			/**/
#define  COMPARE		/**/
#define  CHECKMARK1		/**/
#define  CHECKMARK2		/**/

typedef	struct {		/* dynamic mem scan (save_single) */
		ptr	beg;
		ptr	end;
	} sary;

#define	SARRY_SIZE	1000

extern	byte_file stc_file2;
extern	char	doc_fn[];
extern	short	beg_save_globals;
extern	short	end_save_globals;
extern	int	size_globals;
extern	char	*global_copy;/* mbuf with copy of globals */

comparecheckpts()
{
	int	i;

	for (;;) {
		printf("\nEnter state number (-1 to exit) for comparison:\n");
		scanf("%d", &i);
		if (i<0)  break;
		lookatstates(i);
	}
	printf("Goodbye!\n");
	exit(history);
}

int
open_o_state (n, ext)
	char		ext[];
{
	char		tmp[EXT_MAX];
	str		str_ext,
			str_dir;

	sprintf(tmp, "%s", "./s/");
	str_dir = make_str_given(tmp);
	if (job_name == 0)
		job_name = str_texput;
	sprintf(tmp, "%s%d%s%s", EXT_DEL, n, EXT_DEL, ext);
	str_ext = make_str_given(tmp);
	cur_area = str_dir;
	cur_name = job_name;
	cur_ext = str_ext;
	pack_cur_name();
	flush_string(); flush_string();	/* flush str_dir, str_ext */
	printf("Opening %s\n",name_of_file);
	if ((stc_file2 = fopen(name_of_file, "r")) == NULL) {
		printf("Cannot open %s\n",name_of_file);
		return(FALSE);
	}
}

int lookatstates (no)
	int	no;
{
	int	compressed,
		compressed2,
		page,
		page2;

	if (!open_state_file(LOAD, no)) return(FALSE);
	printf("Opening %s\n",name_of_file);
	if (!open_o_state(no, "stc")) return(FALSE);

	/* read state version */
	if (!read_state_code(&page, &compressed, stc_file))
		goto CST_BAD;
	if (!read_state_code(&page2, &compressed2, stc_file2))
		goto CST_BAD;
	if (page != page2)
		printf("page no's don't match!");
	if (compressed != compressed2)
		printf("compress codes don't match!");

	if (!compareglobals(compressed))
		goto CST_BAD;

	if (!comparearrays())
		goto CST_BAD;

	if (!comparemems())
		goto CST_BAD;

	close_state_file(LOAD);
	b_close(stc_file2);
	return(TRUE);
CST_BAD:
	history = FATAL_ERROR_STOP;
	printf("failed\n");
	close_state_file(LOAD);
	b_close(stc_file2);
	return(FALSE);
}

compareglobals(compressed)
	int		compressed;
{
	int		size,
			list_len,
			list_len2,
			expected_len,
			expected_len2,
			f_offset,
			t_offset,
			done,
			done2,
			i,
			i2,
			flag;
	ULONG		bg,
			ed;
	intptr		iaddr, iaddr2;
	shortptr	saddr;

#define READ1(A,B) if (fread(&A, B, 1, stc_file)  < 1) goto CG_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CG_BAD;
#include "Inewdef.h"

	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;
	READ1(i, sizeof(int));READ1(i, sizeof(int));/* term/file_offset */
	READ2(i, sizeof(int));READ2(i, sizeof(int));

	if (!compressed) { 	/* load full copy, then load diff's. DLP */
	  printf("- GLOBALS - raw\n");
	  if (global_copy == NULL)
			MALLOC(global_copy, int, size_globals);
	  if (fread(global_copy, size_globals, 1, stc_file) < 1) goto CG_BAD;
	  iaddr = (intptr) global_copy;
	  for (i=0;i<(size_globals/sizeof(int));i++) {
		READ2(i2, sizeof(int));
		if (*iaddr != i2)
			printf("offset[%d] %ld : %ld\n",
			(uint) i,(uint)*iaddr,(uint)i2);
		iaddr++;
	  }
	} else {
	  printf("- GLOBALS - list\n");
	/* ---------- load diff if compressed = true ------ */
	  done =  list_len =  expected_len =  0;
	  done2 = list_len2 = expected_len2 = 0;
	  do {
	    if (!done) {
		READ1(iaddr, sizeof(intptr));
		if (((uint) iaddr) == END_MARKER)
			done = TRUE;
		else {
			READ1(i, sizeof(int));
			list_len++;
		}
	    }
	    if (!done2) {
		READ2(iaddr2, sizeof(intptr));
		if (((uint) iaddr2) == END_MARKER)
			done2 = TRUE;
		else {
			READ2(i2, sizeof(int));
			list_len2++;
		}
	    }
	    if (!done && !done2) {flag = FALSE;
		if (flag = (iaddr != iaddr2))
			printf("a: %d %d", (uint) iaddr, (uint) iaddr2);
		if (i != i2) {
		  if (flag) printf("  v: %d %d", i, i2);
		  else      printf("a: %d  v: %d %d", (uint) iaddr, i, i2);
		  flag = TRUE;
		}
		if (flag) printf("\n");
	    } else if (!(done && done2)) {
		if (done)
			printf("2a: %d\n", (uint) iaddr2);
		else
			printf("1a: %d\n", (uint) iaddr);
               	}
       	  } while (!done || !done2);
	  READ1(expected_len, sizeof(int));
	  if (expected_len != list_len)
		printf("Error! global list len #1 didn't match size given.\n");
	  READ2(expected_len2, sizeof(int));
	  if (expected_len2 != list_len2)
		printf("Error! global list len #2 didn't match size given.\n");
	  printf("global change list len 1: %d\n",expected_len);
	  printf("global change list len 2: %d\n",expected_len2);
	}

	READ1(expected_len, sizeof(int));
	if (expected_len != END_MARKER)
		printf("Error! No end marker after #1 global var section!");
	READ2(expected_len, sizeof(int));
	if (expected_len != END_MARKER)
		printf("Error! No end marker after #2 global var section!");
	return(TRUE);
CG_BAD:
	printf("State checkpoint file damaged (globals section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2

comparearrays()
{
	ptr		fmem_seg,
			ptrx,
			tmpptr1,
			tmpptr2;
	str		tmpstr1,
			tmpstr2,
			strx;
	fnt		font_seg,
			fntx,
			tmpfnt2,
			tmpfnt1;
        ULONG   	bg,
			ed;
 	intptr		iaddr,
			iaddr2;		/* long absolute addr */
	shortptr	saddr,		/* short addr === offset from start */
			saddr2;
	int		size_aux,
			diffcode,
			diffcode2,
			expected_len,
			expected_len2,
			len,
			i,
			flag,
			v1,
			v2;
	qword		*qaddr,
			q1,
			q2;
	mword		m1,
			m2;
	hh		h1,
			h2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CA_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CA_BAD;
#include "Inewdef.h"

	printf("- ARRAYS --\n");
/* ______Font Array, check limits______ */
	CHECK(tmpptr1,tmpptr2,sizeof(ptr), "fmem_ptr check failed\n");
	CHECK(tmpfnt1,tmpfnt2,sizeof(fnt), "font_ptr check failed\n");

/* ______String Pool - read current limits______ */
	CHECK(tmpptr1, tmpptr2, sizeof(ptr), "pool_ptr_high different\n");
	CHECK(tmpptr1, tmpptr2, sizeof(ptr), "pool_lowater different\n");
	CHECK(tmpstr1, tmpstr2, sizeof(str), "str_ptr_high different\n");
	CHECK(tmpstr1, tmpstr2, sizeof(str), "str_lowater different\n");

	CHECKMARK1("after #1 water marks"); CHECKMARK2("after #2 water marks");

/* ______Save Stack, array limit = SAVE_SIZE_____ */
	CHECK(tmpptr1, tmpptr2, sizeof(ptr), "save_stack levels different\n");
	ptrx = MIN(tmpptr1,tmpptr2);
	if( ptrx > 0 ) {
	   for (i = 0;i<ptrx;i++) {
		READ1(v1,sizeof(mword)); READ2(v2,sizeof(mword));
		if (v1 != v2)
			printf("Stack[%d] %d != %d\n",i,v1,v2);
	   }
	}
	if( tmpptr1 > ptrx ) READ1(save_stack[0],sizeof(mword)*(tmpptr1-ptrx));
	if( tmpptr2 > ptrx ) READ2(save_stack[0],sizeof(mword)*(tmpptr2-ptrx));
	CHECKMARK1("after #1 save stack"); CHECKMARK2("after #2 save stack");

/* ______Font diff lists______ */
	printf("fonts\n");
	CHECK(i, diffcode, sizeof(int), "font diff codes not same\n");
	CHECK(expected_len,expected_len2,sizeof(int),"font list lens differ\n");
	len = MIN(expected_len,expected_len2);
	for (i=0;i<len;i++) {	/* read changed font dimensions */
		READ1(iaddr, sizeof(intptr)); READ2(iaddr2,sizeof(intptr));
		READ1(v1,  sizeof(int)); READ2(v2,  sizeof(int));
		flag = FALSE;
		if (flag = (iaddr != iaddr2))
			printf("a: %ld %ld", (uint) iaddr, (uint) iaddr2);
		if (v1 != v2) {
		  if (flag) printf("  v: %d %d", v1, v2);
		  else      printf("a: %ld  v: %d %d", (uint) iaddr, v1, v2);
		  flag = TRUE;
		}
		if (flag) printf("\n");
	}
	if( expected_len > len )
	   for (i=len;i<expected_len;i++) {
		READ1(iaddr, sizeof(intptr));
		READ1(v1,  sizeof(int));
		printf("a#1: %ld\n", (uint) iaddr);
	   }
	if( expected_len2 > len )
	   for (i=len;i<expected_len2;i++) {
		READ2(iaddr, sizeof(intptr));
		READ2(v1,  sizeof(int));
		printf("a#2: %ld\n", (uint) iaddr);
	   }
	CHECKMARK1("after font list"); CHECKMARK2("after font list");

/* ______Eqtb table______ */
	printf("eqtb\n");
	CHECK(diffcode, diffcode2, sizeof(int), "eqtb diff codes not same\n");
	if (diffcode == diffcode2) {
	   if (diffcode != DiffListCode) {	/* then read in array mode */
	  	READ1(eqtb[0], sizeof(mword)*(EQTB_SIZE+1));
	  	CHECKMARK1("after #1 eqtb/array");
		for (i=0;i<EQTB_SIZE+1;i++) {
	  		READ2(m2, sizeof(mword));
			if (m2.i != eqtb[i].i)
			   printf("Eqtb[%d] = %d %d\n",i,eqtb[i].i,m2.i);
		}
	  	CHECKMARK2("after #2 eqtb/array");
	   } else {
	      CHECK(expected_len,expected_len2,sizeof(int),
			"eqtb list lens differ\n");
	      len = MIN(expected_len,expected_len2);
	      for (i=0;i<len;i++) {
		READ1(saddr, sizeof(shortptr));	READ2(saddr2, sizeof(shortptr));
		READ1(v1, sizeof(mword));	READ2(v2, sizeof(mword));
		flag = FALSE;
		if (flag = (saddr != saddr2))
			printf("a: %d %d", (uint) saddr, (uint) saddr2);
		if (v1 != v2) {
		  if (flag) printf("  v: %d %d", v1, v2);
		  else      printf("a: %d  v: %d %d", (uint) saddr, v1, v2);
		  flag = TRUE;
		}
		if (flag) printf("\n");
	      }
	      if( expected_len > len )
	      for (i=len;i<expected_len;i++) {
		READ1(saddr, sizeof(shortptr)); READ1(v1,  sizeof(mword));
		printf("a#1: %d\n", (uint) saddr);
	      }
	      if( expected_len2 > len )
	      for (i=len;i<expected_len2 ;i++) {
		READ2(saddr, sizeof(shortptr)); READ2(v1,  sizeof(mword));
		printf("a#2: %d\n", (uint) saddr);
	      }
	      CHECKMARK1("after #1 eqtb list");CHECKMARK2("after #2 eqtb list");
	   }
	} else {
	   printf("Eqtb formats different: ");
	   if (diffcode != DiffListCode) {
		printf("#1=Array ");
	  	READ1(eqtb[0], sizeof(mword)*(EQTB_SIZE+1));
	  	CHECKMARK1("after #1 eqtb/array");
	   } else {
		printf("#1=List ");
	  	READ1(expected_len, sizeof(int));
	  	for (i=0;i<expected_len;i++) {
			READ1(saddr, sizeof(shortptr));
			READ1(eqtb[saddr], sizeof(mword));
		}
	  	CHECKMARK1("after #1 eqtb/list");
	   }
	   if (diffcode2 != DiffListCode) {
		printf("#2=Array ");
	  	READ2(eqtb[0], sizeof(mword)*(EQTB_SIZE+1));
	  	CHECKMARK2("after #2 eqtb/array");
	   } else {
		printf("#2=List ");
	  	READ2(expected_len, sizeof(int));
	  	for (i=0;i<expected_len;i++) {
			READ2(saddr, sizeof(shortptr));
			READ2(eqtb[saddr], sizeof(mword));
		}
	  	CHECKMARK2("after #2 eqtb/list");
	   }
	}

/* ______Hash table______ */
	printf("hash\n");
	CHECK(diffcode, diffcode2, sizeof(int), "hash diff codes not same\n");
	if (diffcode == diffcode2) {
	   if (diffcode != DiffListCode) {	/* then read in array mode */
	  	READ1(hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
	  	CHECKMARK1("after #1 hash/array");
		for (i=0;i<UNDEFINED_CONTROL_SEQUENCE+1;i++) {
	  		READ2(h2, sizeof(hh));
			if (h2.all != hash[i].all)
			   printf("Hash[%d] = %d %d\n",i,hash[i].all,h2.all);
		}
	  	CHECKMARK2("after #2 hash/array");
	   } else {
	      CHECK(expected_len,expected_len2,sizeof(int),
			"hash list lens differ\n");
	      len = MIN(expected_len,expected_len2);
	      for (i=0;i<len;i++) {
		READ1(saddr, sizeof(shortptr));	READ2(saddr2,sizeof(shortptr));
		READ1(h1,  sizeof(hh));		READ2(h2,  sizeof(hh));
		flag = FALSE;
		if (flag = (saddr != saddr2))
			printf("a: %d %d", (uint) saddr, (uint) saddr2);
		if (h1.all != h2.all) {
		  if (flag) printf("  v: %d %d", h1.all, h2.all);
		  else printf("a: %d  v: %d %d", (uint) saddr, h1.all, h2.all);
		  flag = TRUE;
		}
		if (flag) printf("\n");
	      }
	      if( expected_len > len )
	      for (i=len;i<expected_len;i++) {
		READ1(saddr, sizeof(shortptr));
		READ1(h1,  sizeof(hh));
		printf("a#1: %d\n", (uint) saddr);
	      }
	      if( expected_len2 > len )
	      for (i=len;i<expected_len2;i++) {
		READ2(saddr, sizeof(shortptr));
		READ2(h2,  sizeof(hh));
		printf("a#2: %d\n", (uint) saddr);
	      }
	      CHECKMARK1("after #1 hash list");CHECKMARK2("after #2 hash list");
	   }
	} else {
	   printf("Hash formats different: ");
	   if (diffcode != DiffListCode) {
		printf("#1=Array ");
	  	READ1(hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
	  	CHECKMARK1("after #1 hash/array");
	   } else {
		printf("#1=List ");
	  	READ1(expected_len, sizeof(int));
	  	for (i=0;i<expected_len;i++) {
			READ1(saddr, sizeof(shortptr));
			READ1(hash[saddr], sizeof(hh));
		}
	  	CHECKMARK1("after #1 hash/list");
	   }
	   if (diffcode2 != DiffListCode) {
		printf("#2=Array ");
	  	READ2(hash[0], sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1));
	  	CHECKMARK2("after #2 hash/array");
	   } else {
		printf("#2=List ");
	  	READ2(expected_len, sizeof(int));
	  	for (i=0;i<expected_len;i++) {
			READ2(saddr, sizeof(shortptr));
			READ2(hash[saddr], sizeof(hh));
		}
	  	CHECKMARK2("after #2 hash/list");
	   }
	}

/* ______Xeq_level table______ */
	printf("xeq\n");
	CHECK(diffcode, diffcode2, sizeof(int), "xeq diff codes not same\n");
	if (diffcode == diffcode2) {
	   if (diffcode != DiffListCode) {	/* then read in array mode */
	  	READ1(xeq_level[0], sizeof(qword)*(EQTB_SIZE+1 - INT_BASE));
	  	CHECKMARK1("after #1 xeq/array");
		for (i=0;i<EQTB_SIZE+1 - INT_BASE;i++) {
	  		READ2(q2, sizeof(qword));
			if (q2!= xeq_level[i])
			  printf("xeq[%d] = %d %d\n",i,xeq_level[i],q2);
		}
	  	CHECKMARK2("after #2 xeq/array");
	   } else {
	      CHECK(expected_len,expected_len2,sizeof(int),
			"xeq list lens differ\n");
	      len = MIN(expected_len,expected_len2);
	      for (i=0;i<len;i++) {
		READ1(saddr, sizeof(shortptr));	READ2(saddr2,sizeof(shortptr));
		READ1(q1,  sizeof(qword));	READ2(q2,  sizeof(qword));
		flag = FALSE;
		if (flag = (saddr != saddr2))
			printf("a: %d %d", (uint) saddr, (uint) saddr2);
		if (q1 != q2) {
		  if (flag) printf("  v: %d %d", q1, q2);
		  else      printf("a: %d  v: %d %d", (uint) saddr, q1, q2);
		  flag = TRUE;
		}
		if (flag) printf("\n");
	      }
	      if( expected_len > len )
	      for (i=len;i<expected_len;i++) {
		READ1(saddr, sizeof(shortptr));
		READ1(v1,  sizeof(qword));
		printf("a#1: %d\n", (uint) saddr);
	      }
	      if( expected_len2 > len )
	      for (i=len;i<expected_len2;i++) {
		READ2(saddr, sizeof(shortptr));
		READ2(v1,  sizeof(qword));
		printf("a#2: %d\n", (uint) saddr);
	      }
	      CHECKMARK1("after #1 xeq list"); CHECKMARK2("after #2 xeq list");
	   }
	} else {
	   printf("xeq formats different: ");
	   if (diffcode != DiffListCode) {
		printf("#1=Array ");
	  	READ1(xeq_level[0], sizeof(qword)*(EQTB_SIZE+1 - INT_BASE));
	  	CHECKMARK1("after #1 xeq/array");
	   } else {
		printf("#1=List ");
	  	READ1(expected_len, sizeof(int));
	  	for (i=0;i<expected_len;i++) {
			READ1(saddr, sizeof(shortptr));
			READ1(xeq_level[saddr], sizeof(qword));
		}
	  	CHECKMARK1("after #1 xeq/list");
	   }
	   if (diffcode2 != DiffListCode) {
		printf("#2=Array ");
	  	READ2(xeq_level[0], sizeof(qword)*(EQTB_SIZE+1 - INT_BASE));
	  	CHECKMARK2("after #2 xeq/array");
	   } else {
		printf("#2=List ");
	  	READ2(expected_len, sizeof(int));
	  	for (i=0;i<expected_len;i++) {
			READ2(saddr, sizeof(shortptr));
			READ2(xeq_level[saddr], sizeof(qword));
		}
	  	CHECKMARK2("after #2 xeq/list");
	   }
	}
	return(TRUE);
CA_BAD:
	printf("State checkpoint file damaged (array section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2

/*	restore mem structures
 */
comparemems()
{
	int	i,
		iaddr;
#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CMS_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CMS_BAD;
#include "Inewdef.h"

	printf("- MEMS -\n");
	comparesingle();
	CHECKMARK1("after #1 single"); CHECKMARK2("after #2 single");
	
	comparetoken( );
	CHECKMARK1("after #1 tokens"); CHECKMARK2("after #2 tokens");

	comparemulti( );
	CHECKMARK1("after #1 multiword"); CHECKMARK2("after #2 multiword");

	return(TRUE);
CMS_BAD:
	printf("Dynamic memory section damaged!\n");
	return(FALSE);

}
#undef READ1
#undef READ2

/*	restore upper half mem
 */
comparesingle()
{
	sary	s_array[SARRY_SIZE];
	sary	s_array2[SARRY_SIZE];
	int	s_indx;
	int	s_indx2;
	int	i, j, len;
	ptr	beg, end;
	ptr	beg2, end2;
	int	same,
		skipped;
	mword	m1, m2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CS_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CS_BAD;
#include "Inewdef.h"

	printf("single\n");
	CHECK(s_indx, s_indx2, sizeof(int), "# single chunks differ\n");
	len=MIN(s_indx, s_indx2);
	for (i = 0; i < len; i++) {	/* read chunk */
		same = TRUE;
		COMPARE(beg,beg2,sizeof(ptr),"BEG ");
		if (!same) printf("%d %d ", beg, beg2);
		COMPARE(end,end2,sizeof(ptr),"END ");
		s_array[i].beg = beg;s_array2[i].beg = beg2;
		s_array[i].end = end;s_array2[i].end = end2;
		if (end-beg != end2-beg2) {
			printf("SIZE %2d %2d ",end-beg,end2-beg2); same =FALSE;
		}
		if (!same) printf("chunk %d\n",i);
		if (end-beg == end2-beg2) {
		   same=TRUE;
		   for (j=beg;j<=end;j++)
		     if (same) {
			COMPARE(m1.i,m2.i,sizeof(mword), "difference in ");
			if (!same) printf("chunk %d@%d\n",i,beg);
		     } else {
			READ1(m1.i,sizeof(mword));READ2(m2.i,sizeof(mword));
		     }
		} else {
			fseek(stc_file, (end-beg+1)*sizeof(mword), SEEK_CUR);
			fseek(stc_file2,(end2-beg2+1)*sizeof(mword), SEEK_CUR);
		}
	}
	if (s_indx>len) {
	   skipped = 0;
	   for (i = len; i < s_indx; i++) {	/* skip chunk */
		READ1(beg,sizeof(ptr));READ1(end,sizeof(ptr));
		printf("#1a: %d %d\n", beg, end);
		s_array[i].beg = beg;
		s_array[i].end = end;
		fseek(stc_file, (end-beg+1)*sizeof(mword), SEEK_CUR);
		skipped +=      (end-beg+1)*sizeof(mword);
	   }
	   printf("#1: extra %d single chunks - (%d bytes)\n",s_indx-len,
		skipped);
	}
	if (s_indx2>len) {
	   skipped = 0;
	   for (i = len; i < s_indx2; i++) {
		READ2(beg2,sizeof(ptr));READ2(end2,sizeof(ptr));
		printf("#2a: %d %d\n", beg2, end2);
		s_array2[i].beg = beg2;
		s_array2[i].end = end2;
		fseek(stc_file2,(end2-beg2+1)*sizeof(mword), SEEK_CUR);
		skipped +=      (end2-beg2+1)*sizeof(mword);
	   }
	   printf("#2: extra %d single chunks - (%d bytes)\n",s_indx2-len,
		skipped);
	}
	return(TRUE);
CS_BAD:
	printf("State checkpoint file damaged (single section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2

/*  	restore token_list node 
 */
comparetoken()
{
	sary	s_array[SARRY_SIZE];
	sary	s_array2[SARRY_SIZE];
	int	s_indx;
	int	s_indx2;
	int	i, j, len;
	ptr	beg, end;
	ptr	beg2, end2;
	int	same,
		skipped;
	tok	t1, t2;
	ptr	p1, p2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CT_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CT_BAD;
#include "Inewdef.h"

	printf("token\n");
	CHECK(s_indx, s_indx2, sizeof(int), "# token chunks differ\n");
	len=MIN(s_indx, s_indx2);
	for (i = 0; i < len; i++) {	/* read chunk */
		same = TRUE;
		COMPARE(beg,beg2,sizeof(ptr),"BEG ");
		if (!same) printf("%d %d ",(uint) beg, (uint) beg2);
		COMPARE(end,end2,sizeof(ptr),"END ");
		s_array[i].beg = beg;s_array2[i].beg = beg2;
		s_array[i].end = end;s_array2[i].end = end2;
		if (end-beg != end2-beg2) {
			printf("SIZE %2d %2d ",end-beg,end2-beg2); same =FALSE;
		}
		if (!same) printf("chunk %d\n",i);
		if (end-beg == end2-beg2) {
		   same=TRUE;
		   for (j=beg;j<=end;j++)
		     if (same) {
			COMPARE(t1,t2,sizeof(tok), "tok_mem diff ");
		     	if (!same)
			   printf(" #%d in chunk %d size %d@%d\n",j,i,end-beg,(uint) beg);
		     } else {READ1(t1,sizeof(tok));READ2(t2,sizeof(tok));}
		   same=TRUE;
		   for (j=beg;j<=end;j++)
		     if (same) {
			COMPARE(p1,p2,sizeof(ptr), "tok_link diff ");
		     	if (!same)
			   printf(" #%d in chunk %d size %d@%d\n",j,i,end-beg,(uint) beg);
		     } else {READ1(p1,sizeof(ptr));READ2(p2,sizeof(ptr));}
		} else {
			fseek(stc_file,
			   (end-beg+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR);
			fseek(stc_file2,
			   (end2-beg2+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR);
		}
	}
	if (s_indx>len) {
	   skipped = 0;
	   for (i = len; i < s_indx; i++) {	/* skip chunk */
		READ1(beg,sizeof(ptr));READ1(end,sizeof(ptr));
		printf("#1a: %d %d\n", beg, end);
		s_array[i].beg = beg;
		s_array[i].end = end;
		fseek(stc_file,
			   (end-beg+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR);
		skipped += (end-beg+1)*(sizeof(tok)+sizeof(ptr));
	   }
	   printf("#1: extra %d single chunks - (%d bytes)\n",s_indx-len,
		skipped);
	}
	if (s_indx2>len) {
	   skipped = 0;
	   for (i = len; i < s_indx2; i++) {
		READ2(beg2,sizeof(ptr));READ2(end2,sizeof(ptr));
		printf("#2a: %d %d\n", beg2, end2);
		s_array2[i].beg = beg2; s_array2[i].end = end2;
		fseek(stc_file2,
			   (end2-beg2+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR);
		skipped += (end2-beg2+1)*(sizeof(tok)+sizeof(ptr));
	   }
	   printf("#2: extra %d single chunks - (%d bytes)\n",s_indx2-len,
		skipped);
	}
	return(TRUE);
CT_BAD:
	printf("State checkpoint file damaged (token section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2
	
/*	restore lower half mem
 */
comparemulti()
{
	sary	s_array[SARRY_SIZE];
	sary	s_array2[SARRY_SIZE];
	int	s_indx;
	int	s_indx2;
	int	i, j, len;
	ptr	beg, end;
	ptr	beg2, end2;
	int	same,
		skipped;
	mword	m1, m2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CM_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CM_BAD;
#include "Inewdef.h"

	printf("multi\n");
	CHECK(s_indx, s_indx2, sizeof(int), "# multi chunks differ\n");
	len=MIN(s_indx, s_indx2);
	for (i = 0; i < len; i++) {	/* read chunk */
		same = TRUE;
		COMPARE(beg,beg2,sizeof(ptr),"BEG ");
		if (!same) printf("%d %d ",(uint) beg, (uint) beg2);
		COMPARE(end,end2,sizeof(ptr),"END ");
		s_array[i].beg = beg;s_array2[i].beg = beg2;
		s_array[i].end = end;s_array2[i].end = end2;
		if (!same) {
			printf("SIZE %2d %2d ",end-beg,end2-beg2); same =FALSE;
		}
		if (!same) printf("chunk %d @%d @%d\n",i,(uint) beg,(uint)beg2);
		if (end-beg == end2-beg2) {
		   same=TRUE;
		   for (j=beg;j<=end;j++)
		     if (same) {
			COMPARE(m1.i,m2.i,sizeof(mword), "difference in ");
			if (!same) printf("chunk %d@%d\n",i,(uint) beg);
		     } else {
			READ1(m1.i,sizeof(mword));READ2(m2.i,sizeof(mword));
		     }
		} else {
			fseek(stc_file, (end-beg+1)*sizeof(mword), SEEK_CUR);
			fseek(stc_file2,(end2-beg2+1)*sizeof(mword), SEEK_CUR);
		}
	}
	if (s_indx>len) {
	   skipped = 0;
	   for (i = len; i < s_indx; i++) {	/* skip chunk */
		READ1(beg,sizeof(ptr));READ1(end,sizeof(ptr));
		printf("#1a: %d %d\n", beg, end);
		s_array[i].beg = beg;
		s_array[i].end = end;
		fseek(stc_file, (end-beg+1)*sizeof(mword), SEEK_CUR);
		skipped +=      (end-beg+1)*sizeof(mword);
	   }
	   printf("#1: extra %d multi chunks - (%d bytes)\n",s_indx-len,
		skipped);
	}
	if (s_indx2>len) {
	   skipped = 0;
	   for (i = len; i < s_indx2; i++) {
		READ2(beg2,sizeof(ptr));READ2(end2,sizeof(ptr));
		printf("#2a: %d %d\n", beg2, end2);
		s_array2[i].beg = beg2;
		s_array2[i].end = end2;
		fseek(stc_file2, (end2-beg2+1)*sizeof(mword), SEEK_CUR);
		skipped +=      (end2-beg2+1)*sizeof(mword);
	   }
	   printf("#2: extra %d multi chunks - (%d bytes)\n",s_indx2-len,
		skipped);
	}
	return(TRUE);
CM_BAD:
	printf("State checkpoint file damaged (multi section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2
#undef  CHECK
#undef  COMPARE
#undef  CHECKMARK1
#undef  CHECKMARK2

#endif CHECKSTATE

/* look for ptrs to mem node p in MEM, eqtb, save stack, hyph_list
 */
extern	ptr     hyph_list[HYPH_SIZE+1];

search_mem(p)
ptr p;
{
	int	q;

	for (q=MEM_MIN;q<=lo_mem_max;q++) {
	    if (link(q)==p) {
		print_nl("LINK(");print_int(q);print_char(')');
	    }
	    if (info(q)==p) {
		print_nl("INFO(");print_int(q);print_char(')');
	    }
	}
	for (q=hi_mem_min;q<=mem_end;q++) {
	    if (link(q) == p) {
		print_nl("LINK(");print_int(q);print_char(')');
	    }
	    if (info(q) == p) {
		print_nl("INFO(");print_int(q);print_char(')');
	    }
	}
	/* eqtb */
	for (q=ACTIVE_BASE;q<=(BOX_BASE+255);q++) {
	    if (equiv(q) == p) {
		print_nl("EQUIV(");print_int(q);print_char(')');
	    }
	}
	/* save stack */
	if (save_ptr>0)
	  for (q=0;q<save_ptr;q++) {
	    if (equiv_field(save_stack[q]) == p) {
		print_nl("SAVE(");print_int(q);print_char(')');
	    }
	  }
	/* hyph */
	for (q=0;q<=HYPH_SIZE;q++) {
	    if (hyph_list[q] == p) {
		print_nl("HYPH_LIST(");print_int(q);print_char(')');
	    }
	}
	print_ln();
}

search_eq()
{
	hword p;
	int n,i,q;

	print_nl("Glue_base: ");print_int(GLUE_BASE);
	print(   " Local_base: ");print_int(LOCAL_BASE);
	print(   " Box_base: ");print_int(BOX_BASE);
	print(   " Cur_font_loc: ");print_int(CUR_FONT_LOC);
	print_ln();

	/* reg 1 & 2 are tokens */
	/* reg 3 */
	for (n=GLUE_BASE;n<LOCAL_BASE;n++) {
	    p = equiv(n);
	    print_nl("EQ[");print_int(n);print("] gluespec ");print_int(p);
	}
	/* reg 4 */
	print_nl("EQ[");print_int(PAR_SHAPE_LOC);print("] ");
	print_int(equiv(PAR_SHAPE_LOC));
	for (n=BOX_BASE;n<CUR_FONT_LOC;n++) {
	    p = equiv(n);
	    print_nl("EQ[");print_int(n);print("] ");print_int(p);
	    show_node_list(p);
	}
	/* save stack */
	if (save_ptr>0)
	  for (n=save_ptr-1;n>=0;n--) {
	    if (save_type(n) == RESTORE_OLD_VALUE) {
		i=save_index(n);
		print_nl("SAVE("); print_int(n);print_char(')');
		print(" = EQUIV[");print_int(i);print_char(']');
		print_char('=');
		n--;
	      	print_int(equiv_field(save_stack[n]));
		/* local_base = par_shape_loc */
	      	if ((i>=GLUE_BASE && i<=LOCAL_BASE) ||
	       	    (i>=BOX_BASE  && i<CUR_FONT_LOC))
			print_char('*');
	    }
	  }
	  print_ln();
}
#endif DEBUG
#endif INCTEX
