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

/*  Icmp.c
 *  This file is part of IncTeX 1.0
 *  An Incremental TeX Formatter
 *
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";


#ifdef INCTEX

#include	"tex.h"
#include	"texext.h"
#include	"tfm.h"
#include	"math.h"
#include	"box.h"
#include	"boxlists.h"
#include	"token.h"
#include	"tokenlists.h"
#include	"tokenstack.h"
#include	"evalstack.h"
#include	"eqstack.h"
#include	"dvi.h"
#include	"page.h"
#include	"heap.h"
#include	"hyph.h"
#include	"boxlists.h"
#include	"par.h"
#include	"cond.h"
#include	"mlst-hlst.h"
#include	"Imain.h"
#include	"Icmp.h"

extern	val     depth_threshold;
extern	val     breadth_max;
extern	fnt     font_in_short_display;
extern	list    cur_list;
extern	list    nest[];
extern	ptr     nest_ptr;
extern	int     max_nest_stack;

extern	char	name_of_file[FILE_NAME_SIZE];
extern	int	parse_in_error;
extern	int	check_error;
extern	int	cur_parse_count;
extern	int	parse_number1[MEM_MAX-MEM_MIN+1];
extern	int	parse_number2[MEM_MAX-MEM_MIN+1];
extern	int	*cur_p_number;
/*!! extern	short  *cur_p_number[];*/
extern	mword   mem[MEM_MAX-MEM_MIN+1];
extern	mword	copy_of_mem[];
extern	mword   save_stack[SAVE_SIZE];
mword   copy_of_save_stack[SAVE_SIZE];
extern	ptr	font_glue[FONT_MAX];
ptr	copy_of_font_glue[FONT_MAX];
ptr	font_glue_start[FONT_MAX];

#define DIRFORCMP	"./s"
#define CHECK_FILE	15 
#define NODE_BODY	((short) -1)

#define nested_list_parse(N) \
    {append_char('.'); parse_node_list(N); flush_char();}

#define parse_count(P) *(cur_p_number+P)
/*!! #define parse_count(P) *cur_p_number[P]*/

#define upperhalf(P) (P & ~0xFFFF)
#define lowerhalf(P) (P &  0xFFFF)

/* Don't print mode line number,
 * Convert show_ligature so it does show_node_list instead of short_display.
 * Make a copy of print_spec so we can parse glue spec's, this is the only
 *	one that allows repeated visits to a node for now.
 */

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
int	s_indx, s_indx2,
	m_indx, m_indx2;
sary	s_array[SARRY_SIZE], s_array2[SARRY_SIZE],
	m_array[SARRY_SIZE], m_array2[SARRY_SIZE];

extern	char	doc_fn[];
extern	short	beg_save_globals;
extern	short	end_save_globals;
extern	int	size_globals;
extern	char	*global_copy;  /* mbuf with copy of globals */

extern
FILE		*parse_check_file;
extern
byte_file	stc_file;
byte_file	stc_file2;
char		*cur_globals;

extern	str	str_lowater;
extern	ptr	pool_lowater;
extern	fnt    	font_last;
extern	ptr    	fmem_last;
int		fmem_lastcmp;
int		font_lastcmp;
int		font_fatal_diff;
str		str_lastcmp;
ptr		pool_lastcmp;
int		fatal_append_diff;

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

DIFF_CHUNK      *glob2_list = NULL;	/* so we can restore glob entries */
DIFF_CHUNK	*glob2_end = NULL;
int		glob2_listlen = 0;
DIFF_CHUNK      *gval2_list = NULL;
DIFF_CHUNK	*gval2_end = NULL;
int		gval2_listlen = 0;
DIFF_CHUNK	*fch_ptr = NULL;
DIFF_CHUNK      *fchset = NULL;
DIFF_CHUNK	*fchset_end = NULL;
DIFF_CHUNK	*fchset_ptr = NULL;
int		fcount = 0;
DIFF_HUNK       *eq2_list = NULL;	/* so we can restore eqtb entries */
DIFF_HUNK	*eq2_end = NULL;
int		eq2_listlen = 0;
DIFF_CHUNK       *eval2_list = NULL;
DIFF_CHUNK	*eval2_end = NULL;
int		eval2_listlen = 0;

#define	testlen 20
#define IP (intptr)
intptr	testa[testlen] = {
IP 4,	IP 6,	IP 7,	IP 22,	IP 178,	IP 255,	IP 400,	IP 500,	IP 1001,IP 2001,
IP 22,	IP 643,	IP 77,	IP 2,	IP 78,	IP 52,	IP 41,	IP 1005,IP 4006,IP 21
	};
int	testb[testlen] = {
-33,	-33,	-33,	-33,	-33,	-33,	-33,	-33,	-33,	-33,
16,	16,	16,	16,	16,	16,	16,	16,	16,	16,
	};
#undef IP

int		showq;		/* whether to show quiesc logic */
int		explain;	/* whether to show quiesc logic */
int		n_qsc_msgs = 0;

/* codes for comparison */
#define	IGNOREVAL  0
#define	NODEMEMPTR 1
#define	LBREAKHALF 2
#define	LBREAKWORD 3
#define	NAMEOFFILE 4
#define LASTCMPTYPE NAMEOFFILE 

#define ENTERPTR(A)        var_loc[i]=A;var_len[i]=2;cmp_with[i]=NODEMEMPTR;i++;
#define ENTERHALFIGNORE(A) var_loc[i]=A;var_len[i]=2;cmp_with[i]=IGNOREVAL;i++;
#define ENTERWORDIGNORE(A) var_loc[i]=A;var_len[i]=4;cmp_with[i]=IGNOREVAL;i++;
#define ENTERLONGIGNORE(A) var_loc[i]=A;var_len[i]=4;cmp_with[i]=IGNOREVAL;i++;
#define ENTERHALFLBREAK(A) var_loc[i]=A;var_len[i]=2;cmp_with[i]=LBREAKHALF;i++;
#define ENTERWORDLBREAK(A) var_loc[i]=A;var_len[i]=4;cmp_with[i]=LBREAKWORD;i++;

#define N_LOCS 128

int		var_loc[ N_LOCS ],	/* addr of global var */
		var_len[ N_LOCS ],	/* length of global var */
		cmp_with[N_LOCS ];	/* code gives type of comparison */
int		max_var_locs,		/* number actually used */
		last_loc;		/* last checked */

enter_locs()
{
	int i = 0;
	int j;
	ENTERPTR((int)&cond_ptr);
	ENTERPTR((int)&cur_list.head_field);
	ENTERPTR((int)&cur_list.tail_field);
	for (j=0;j<NEST_SIZE;j++) {
	  ENTERPTR((int) &nest[j].head_field);
	  ENTERPTR((int) &nest[j].tail_field);
	}
	ENTERPTR((int)&passive);
	ENTERPTR((int)&just_box);
	ENTERPTR((int)&ha);
	ENTERPTR((int)&hb);
	ENTERPTR((int)&cur_box);
	ENTERPTR((int)&cur_mlist);
	ENTERPTR((int)&temp_ptr);
	ENTERPTR((int)&page_tail);
	ENTERHALFIGNORE((int)&best_page_break);
	ENTERHALFIGNORE((int)&best_bet);
	ENTERHALFIGNORE((int)&best_line);
	ENTERHALFIGNORE((int)&mem_end);
	ENTERHALFIGNORE((int)&lo_mem_max);
	ENTERHALFIGNORE((int)&hi_mem_min);
	ENTERHALFIGNORE((int)&avail);
	ENTERHALFIGNORE((int)&rover);
	ENTERHALFIGNORE((int)&tok_head);
	ENTERWORDIGNORE((int)&dyn_used);
	ENTERWORDIGNORE((int)&var_used);
	ENTERLONGIGNORE((int)&chk_cid);
	var_loc[i]=(int)&name_of_file[0];var_len[i]=sizeof(name_of_file);
	cmp_with[i]=NAMEOFFILE;i++;
	/* page break data not important right after a page break! */
	var_loc[i]=(int)&best_pl_line[0];var_len[i]=sizeof(hword)*4;
		cmp_with[i]=IGNOREVAL;i++;
	var_loc[i]=(int)&best_place[0];var_len[i]=sizeof(ptr)*4;
		cmp_with[i]=IGNOREVAL;i++;
	/* line numbers allowed to be different */
	var_loc[i]=(int)&line_stack[0];var_len[i]=sizeof(val)*MAX_IN_OPEN;
		cmp_with[i]=IGNOREVAL;i++;
	max_var_locs = i-1;
}

int
open_parse_file (ext)
	char		ext[];
{
	char	tmp[FILE_NAME_SIZE];

	sprintf(tmp, "%s%s", base_fn, ext);
	if (showq) {print_nl("Opening ");print(tmp);}/*!!*/
	if ((parse_check_file = fopen(tmp, "w")) == NULL)
		{print(" failed!!");exit(-1);}
}

int
open_lastchkpt()
{
	get_ext(-1, EXT_LASTSTATE);
	if (showq) {print_nl("Opening ");print(name_of_file);}/*!!*/
	if ((stc_file2 = fopen(name_of_file, "r")) == NULL) {
		print_nl("!!Cannot open ");print(name_of_file);
		return(FALSE);
	}
	return(TRUE);
}

int
comparestate (no)
	int	no;
{
	int	compressed,
		compressed2,
		page,
		page2;
	ULONG	bg,
		ed;
	int	i,ok;

	if (total_pages>last_max_pages)
		return(FALSE);
	if (fatal_append_diff) {
		if (EXPLAINIT)
			print_nl("Previous fatal string or font difference.");
		return(FALSE);
	}
	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;
	if (cur_globals == NULL)
		MALLOC(cur_globals, char, size_globals); /* !!! */
	beg_save_globals = 0x5a5a;  /* ensure 1st addr always same. */
	bcopy(&beg_save_globals, cur_globals, size_globals);
	bcopy(&mem[0], copy_of_mem, sizeof(mem));
	bcopy(&save_stack[0],&copy_of_save_stack[0],sizeof(save_stack));
	bcopy(&font_glue[0], &copy_of_font_glue[0], sizeof(font_glue));
	if (glob2_list == NULL) {
		enter_locs();
		GETDIFFCHUNK(glob2_list);
		glob2_end = glob2_list;
		glob2_end->nxt = glob2_end->prev = NULL;
		glob2_end->size = glob2_listlen = 0;
		GETDIFFCHUNK(gval2_list);
		gval2_end = gval2_list;
		gval2_end->nxt = gval2_end->prev = NULL;
		gval2_end->size = gval2_listlen = 0;
	} else {
		spill_clist(glob2_list, &glob2_end, &glob2_listlen);
		spill_clist(gval2_list, &gval2_end, &gval2_listlen);
	}
	if (eq2_list == NULL) {
		GETDIFFHUNK(eq2_list);
		eq2_end = eq2_list;
		eq2_end->nxt = eq2_end->prev = NULL;
		eq2_end->size = eq2_listlen = 0;
		GETDIFFCHUNK(eval2_list);
		eval2_end = eval2_list;
		eval2_end->nxt = eval2_end->prev = NULL;
		eval2_end->size = eval2_listlen = 0;
	} else {
		spill_list(eq2_list, &eq2_end, &eq2_listlen);
		spill_clist(eval2_list, &eval2_end, &eval2_listlen);
	}

	if (!open_lastchkpt()) return(FALSE);	/* open from previous run */
	if (!open_state_file(LOAD, no)) return(FALSE);

	/* read state version */
	if (!read_state_code(&page, &compressed, stc_file))
		goto CST_BAD;
	if (!read_state_code(&page2, &compressed2, stc_file2))
		goto CST_BAD;
	if (page != page2)
		print_nl("!!page no's don't match!");
	if (compressed != compressed2)
		print_nl("!!compress codes don't match!");

	cur_p_number = &parse_number1[0];
	/*!! cur_p_number = parse_number1;*/
	bzero((char *) &parse_number1[0], sizeof(parse_number1));
	bzero((char *) &parse_number2[0], sizeof(parse_number2));

	/* open check file */
	open_parse_file(".parse1");
	ok = parse_mem();	/* parse for current state */
	fclose(parse_check_file);
	if (!ok) goto CST_BAD;

	if (!read_globals(compressed)) {
		if (EXPLAINIT) print_nl("Loading previous globals failed.");
		goto CST_BAD;
	}

	if (!cmp_arrays()) {
		restore_eq();
		if (EXPLAINIT) print_nl("Arrays differ.");goto CST_BAD;
	}

	if (!cmp_mems()) {
		restore_eq();
		if (EXPLAINIT) print_nl("Mems differ.");goto CST_BAD;
	}
	
	cur_p_number = &parse_number2[0];
	/*!! cur_p_number = parse_number2;*/
	open_parse_file(".parse2");	/* parse previous state */
	ok = parse_mem();		/* globals & node mem copied in now */
	fclose(parse_check_file);
	restore_eq();
	if (!ok) goto CST_BAD;

	close_state_file();
	b_close(stc_file2);
	bcopy(cur_globals, &beg_save_globals, size_globals);
	bcopy(copy_of_mem, &mem[0], sizeof(mem));
	bcopy(&copy_of_save_stack[0],&save_stack[0],sizeof(save_stack));
	bcopy(&copy_of_font_glue[0], &font_glue[0], sizeof(font_glue));

	if (!cmp_parses(".parse1",".parse2")) {
		if (EXPLAINIT) print_nl("Dynamic mem parses don't match!");
		goto CMP_FAIL;
	}
	if (!check_full_parse()) {
	   if (EXPLAINIT) print_nl("Dynamic memory not fully parsed!");
	   goto CMP_FAIL;
	}
	if (!cmp_globals(compressed)) {
		if (EXPLAINIT) print_nl("Globals different");
		goto CMP_FAIL;
	}
	if (!cmp_appends()) {
		if (EXPLAINIT) print_nl("Appends different");
		goto CMP_FAIL;
	}
	n_qsc_msgs = 0; /* reset if quiescence detected */
	return(TRUE);

CST_BAD:
	bcopy(cur_globals, &beg_save_globals, size_globals);
	bcopy(copy_of_mem, &mem[0], sizeof(mem));
	bcopy(&copy_of_save_stack[0],&save_stack[0],sizeof(save_stack));
	bcopy(&copy_of_font_glue[0], &font_glue[0], sizeof(font_glue));
	close_state_file();
	b_close(stc_file2);
CMP_FAIL:
	if (EXPLAINIT) {
		n_qsc_msgs++;
		print_nl("State compare page ");print_int(no);print(" failed");
		print_ln();
	}
	return(FALSE);
}

int
cmp_parses(ext1,ext2)
	char	*ext1;
	char	*ext2;
{
	char	tmp[FILE_NAME_SIZE];
	byte_file	parse1;
	byte_file	parse2;
	int	ok;
	int	c1, c2;

	sprintf(tmp, "%s%s", base_fn, ext1);
	if (showq) {print_nl("Opening ");print(tmp);/*!!*/}
	if ((parse1 = fopen(tmp, "r")) == NULL)
		{print_nl("Opening ");print(tmp);print(" failed!!");exit(-1);}
	sprintf(tmp, "%s%s", base_fn, ext2);
	if (showq) {print_nl("Opening ");print(tmp);/*!!*/}
	if ((parse2 = fopen(tmp, "r")) == NULL)
		{print_nl("Opening ");print(tmp);print(" failed!!");exit(-1);}
	ok = TRUE;
	while (TRUE) {
		c1 = getc(parse1);
		c2 = getc(parse2);
		if ((c1 == EOF) && (c2 == EOF)) /* files identical */
			break;
		if ((c1 != c2) || (c1 == EOF) || (c2 == EOF)) {
			ok = FALSE;break;
		}
	}
	return(ok);
}

int
check_full_parse()
{
	int	ok, i, j;

	ok=scan_nodes(&s_array2[0],s_indx2,&m_array2[0],m_indx2,parse_number2);
	if (!ok && EXPLAINIT) print_nl("Incomplete parse of prev state.");
	if (!scan_nodes(&s_array[0],s_indx,&m_array[0],m_indx,parse_number1))
		ok = FALSE;
	if (!ok && EXPLAINIT) print_nl("Incomplete parse of curr state.");
	return(ok);
}

/* strict between (not including ends) */
#define free_between(L,H,T,I) { sec_ok = TRUE;\
	for (j=L+1,p=parse_number+L+1;j<H;p++,j++) if ((*p)!=0) sec_ok=FALSE; \
	if (!sec_ok && EXPLAINIT) {print_nl(T);print(" memory before block "); \
	  print_int(I);print(" is in use [");print_int(L);print_char('-'); \
	  print_int(H);print("] ##"); \
	  for (j=L+1,p=parse_number+L+1;j<H;p++,j++) {print_char(' ');print_int(*p);}\
	}; ok = ok && sec_ok;}

/* including ends */
#define full_between(L,H,T,I) { sec_ok = TRUE;\
	for (j=L,p=parse_number+L;j<=H;p++,j++) if ((*p)==0) sec_ok=FALSE; \
	if (!sec_ok && EXPLAINIT) {print_nl(T);print(" memory block "); \
	  print_int(I);print(" not parsed [");print_int(L);print_char('-'); \
	  print_int(H); print("] ##"); \
	  for (j=L,p=parse_number+L;j<=H;p++,j++) {print_char(' ');print_int(*p);}\
	}; ok = ok && sec_ok;}

int
scan_nodes(s_array, s_indx, m_array, m_indx, parse_number)
	sary  s_array[];
	int   s_indx;
	sary  m_array[];
	int   m_indx;
	int /*!!short*/ *parse_number;
{
	int	ok,
		sec_ok,
		i,
		j;
	int /*!!short*/	*p;

	ok=TRUE;
	if (s_array[0].beg > hi_mem_min)
		free_between(hi_mem_min-1,s_array[0].beg,"single",0);
	full_between(s_array[0].beg,s_array[0].end,"single",0);
	if (!sec_ok) {if (TALK) {print_nl("# blocks: ");print_int(s_indx);}
		  if (!showq) return(FALSE);
	}
	for (i=1;i<s_indx;i++) {
		free_between(s_array[i-1].end,s_array[i].beg,"single",i);
		full_between(s_array[i].beg,  s_array[i].end,"single",i);
		if (!sec_ok) {if (TALK) {print_nl("# blocks: ");print_int(s_indx);}
			if (!showq) return(FALSE);
		}
	}
	if (s_array[s_indx].end < mem_end)
	    free_between(s_array[s_indx-1].end,mem_end,"single",s_indx);
	if (!sec_ok) {if (TALK) {print_nl("# blocks: ");print_int(s_indx);}
		  if (!showq) return(FALSE);
	}

	if (m_array[0].beg > MEM_MIN)
		free_between(hi_mem_min-1,m_array[0].beg,"multi",0);
	full_between(m_array[0].beg,m_array[0].end,"multi",0);
	if (!sec_ok) {if (TALK) {print_nl("# blocks: ");print_int(m_indx);}
		  if (!showq) return(FALSE);
	}
	for (i=1;i<m_indx;i++) {
		free_between(m_array[i-1].end,m_array[i].beg,"multi",i);
		full_between(m_array[i].beg,  m_array[i].end,"multi",i);
		if (!sec_ok) {if (TALK) {print_nl("# blocks: ");print_int(m_indx);}
			if (!showq) return(FALSE);
		}
	}
	if (m_array[m_indx].end < lo_mem_max)
		free_between(m_array[m_indx-1].end,lo_mem_max,"multi",m_indx);
	if (!sec_ok && TALK) {print_nl("# blocks: ");print_int(m_indx);}
	return(ok);
}

#define uint	unsigned int

int
compare_ptr(iaddr2,i2)
	ptr *iaddr2;
	uint i2;
{
	int  retval = TRUE;

	if (upperhalf(i2) != 0) {	/* ptr val in upper halfword */
		i2 = upperhalf(i2);	/* shift it down into lower. */
		i2 = i2 >> 16;
		i2 = lowerhalf(i2);	/* mask off any sign extend bits */
	}
	if ((i2<MEM_MIN) || (i2>MEM_MAX)) {
		print_nl("!!Ptr val ");print_int(i2);print(" at ");
		print_int((uint) iaddr2);
		print(", global for prev state out of range.");
		return(FALSE);	/* some kind of internal error really? */
	}
	if ((*iaddr2<MEM_MIN) || (*iaddr2>MEM_MAX)) {
		print_nl("!!Ptr val ");print_int(*iaddr2);print(" at ");
		print_int((uint) *iaddr2);
		print(", global for curr state out of range.");
		return(FALSE);	/* some kind of internal error really? */
	}
	if ((parse_number2[i2]<0) || (parse_number1[*iaddr2]<0))
		retval = FALSE;
	else
		retval = parse_number2[i2] == parse_number1[*iaddr2];
	if (!retval && TALK) {
		print_nl("Glob ptr diff @"); print_int((int)iaddr2);
		print(" Parse # (curr):");print_int(parse_number1[*iaddr2]);
		print(" (prev):");print_int(parse_number1[i2]);
	}
	return(retval);
}

read_globals(compressed)
	int		compressed;
{
	int		size_globals,
			list_len, list_len2,
			expected_len, expected_len2,
			f_offset,
			t_offset,
			done, done2,
			ok, i, i2;
	ULONG		bg,
			ed;
	intptr		iaddr, iaddr2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file)  < 1) goto CG_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CG_BAD;

	ok = TRUE;
	bg = (ULONG) &beg_save_globals;
	ed = (ULONG) &end_save_globals;
	size_globals = ed - bg;
	READ1(i, sizeof(int));READ1(i, sizeof(int)); /* term/file_offset */
	READ2(i, sizeof(int));READ2(i, sizeof(int));

	if (!compressed) { 	/* load full copy, then load diff's. DLP */
	  if (showq) print_nl("- GLOBALS - raw");
	  if (global_copy == NULL)
		MALLOC(global_copy, char, size_globals);
	  if (fseek(stc_file, size_globals, SEEK_CUR)<0) goto CG_BAD;
	  iaddr = (intptr) global_copy;
	  for (i=0;i<(size_globals/sizeof(int));i++) {
		READ2(i2, sizeof(int));
		if ((*iaddr != i2) && showq) {
			if (EXPLAINIT) {
				print_nl("!!offset[");print_int(i);
				print("] ");print_int((uint)*iaddr);
				print(" : ");print_int(i2);
			}
			ok = FALSE;
		}
		iaddr++;
	  }
	} else {
	  if (showq) print_nl("- GLOBALS - list");
	/* ---------- load diff if compressed = true ------ */
	  done =  list_len =  expected_len =  0;
	  done2 = list_len2 = expected_len2 = 0;
	  do {
	    if (!done) {	/* skips list for curr state */
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
			list_len2++;
			if(((uint) iaddr2 < (uint) bg) ||
			   ((uint) iaddr2 > (uint) ed)) {
				print_nl("!!Global mem address out of range ");
				print_int((int) bg);print(" - ");
				print_int((int) ed);print_ln();
				print("Address ");print_int(list_len2);
				print_nl(" (thrown away): ");
				print_int((uint) iaddr2);
				READ2(iaddr2, sizeof(int));
			} else {
				if (fread(iaddr2,sizeof(int),1,stc_file2) < 1)
					goto CG_BAD;
				if( glob2_end == NULL ) {
					print_nl("Hey! glob2_end = NULL");
					GETDIFFCHUNK( glob2_end );
					GETDIFFCHUNK( gval2_end );
					glob2_end->nxt = glob2_end->prev = NULL;
					gval2_end->nxt = gval2_end->prev = NULL;
					glob2_end->size = gval2_end->size = 0;
					glob2_list = glob2_end;
					gval2_list = gval2_end;
				}
				if( glob2_end->size >= DIFF_HUNK_SIZE ) {
					USENEWCHUNK(glob2_end);
					USENEWCHUNK(gval2_end);
				}
				glob2_end->listchunk[glob2_end->size] = iaddr2;
				i2 = *iaddr2;
				gval2_end->listchunk[gval2_end->size] =
					(intptr) i2;
				glob2_end->size++; gval2_end->size++;
				glob2_listlen++;   gval2_listlen++;
			}
		}
	    }
       	  } while (!done || !done2);
	  READ1(expected_len, sizeof(int));
	  if (expected_len != list_len) {ok = FALSE;
		print_nl("!!Error! count off for curr globals list.");
	  }
	  READ2(expected_len2, sizeof(int));
	  if (expected_len2 != list_len2) {ok = FALSE;
		print_nl("!!Error! count off for prev globals list.");
	  }
	  if (showq) {
		print_nl("global change list len 1: ");print_int(expected_len);
	  	print_nl("global change list len 2: ");print_int(expected_len2);
	  }
	}

	READ1(expected_len, sizeof(int));
	if (expected_len != END_MARKER)
		print_nl("!!Error! No end mark after curr global var section!");
	READ2(expected_len, sizeof(int));
	if (expected_len != END_MARKER)
		print_nl("!!Error! No end mark after prev global var section!");
	return(ok);
CG_BAD:
	print_nl("!!State checkpoint file damaged (globals section)!");
	return(FALSE);
}
#undef READ1
#undef READ2

#define check_address(A,LOC,LEN) \
	((((unsigned long)A)>=LOC) && (((unsigned long)A)<LOC+LEN))

int
cmp_globals()
{
	DIFF_CHUNK	*ct, *vt;
	unsigned int	i2;
	intptr		iaddr2;
	ULONG		addrval,
			offset;
	int		i, j,
			ok,
			flag,
			found,
			startat;
	F_NODE		*cur_input_fp,
			*input_stack_fp[STACK_SIZE];

	ct = glob2_list;
	vt = gval2_list;
	last_loc = -1;
	cur_input_fp = cur_input.fp;
	for (i = 0; i < input_ptr; i++)
		input_stack_fp[i] = input_stack[i].fp;
	cur_input.fp = 0;
	for (i = 0; i < STACK_SIZE; i++)
		input_stack[i].fp = 0;
	ok = TRUE;
	while (ct != NULL) {
	   for (j=0;j<ct->size;j++) {
		i2 = (int) vt->listchunk[j];
		iaddr2 = ct->listchunk[j];
		offset = ((ULONG) iaddr2-(ULONG) &beg_save_globals)/sizeof(int);
		flag = check_set_item(global_chset, offset, size_globals+1);
		switch (flag) {
		case SET_ELEMBAD:  /* error */
			print_nl("Set elem spec bad on eqtb item #");
			print_int(offset);
			break;
		case SET_CHECKBAD:  /* error */
			print_nl("Set check bad on eqtb item #");
			print_int(offset);
			break;
		case ITEM_UNEQUIV_CURR:
		case ITEM_UNEQUIV_INIT:
		case ITEM_EQUIV_CURR:
		case ITEM_EQUIV_INIT: /* checked already */ break;
		case ITEM_CLEAR:
			/* just note that we're checking this */
			global_chset[offset] = ITEM_EQUIV_INIT; break;
		case ITEM_CHANGED:
			global_chset[offset] = ITEM_EQUIV_CURR; break;
		} /* switch */
		if (i2 != *iaddr2) {
		    if (upperhalf(i2) == upperhalf(*iaddr2)) {
			addrval = (ULONG) iaddr2;
			addrval = addrval + 2;
			iaddr2 = (intptr) addrval;
			i2 = lowerhalf(i2);
		    }
		    found = FALSE;
		    i = startat = (last_loc+1) % (max_var_locs+1);
		    do {
			if (check_address(iaddr2,var_loc[i],var_len[i])) {
				found = TRUE; break;
			}
		        i=(++i) % (max_var_locs+1);
		    } while (i != startat);
		    if (!found) { /* bad news, don't know who this item is */
			if (EXPLAINIT) {
				print_nl("Unknown global at ");
				print_int(iaddr2);print(" is different!");
			}
			if (!showq) return(FALSE);
			ok = FALSE;
		    } else {
			last_loc = i;
			if (cmp_with[i] > LASTCMPTYPE) {
			    print_nl("!!Intern Err: global compare for addr ");
			    print_int(iaddr2);print(" has unknown op # ");
			    print_int(cmp_with[i]);print(" selection entry ");
			    print_int(i);
			    cmp_with[i] = IGNOREVAL;
			} /*else printf("glob add %d ",iaddr2);*/
			switch (cmp_with[i]) {
			case IGNOREVAL:  /*printf("ignoreable\n");*/
					 break;
			case NODEMEMPTR: /*printf("ptr\n");*/
					 if (!compare_ptr(iaddr2,i2)) {
						if (!showq) return(FALSE);
						ok = FALSE;
					 }
					 break;
			case LBREAKHALF: if (TALK) print_nl(
					  "Can't parse line break hwords yet");
					 if (!showq) return(FALSE);
					 ok = FALSE;
					 break;
			case LBREAKWORD: if (TALK) print_nl(
					  "Can't parse line break hwords yet");
					 if (!showq) return(FALSE);
					 ok = FALSE;
					 break;
			case NAMEOFFILE: /* check ptr is in curr string range */
					 if ((int) iaddr2 <= (int)
					  &name_of_file[strlen(name_of_file)]) {
						if (TALK) print_nl(
						 "!!Name of file in globals is different.");
						if (!showq) return(FALSE);
						ok = FALSE;
					 }
					 break;
			}
		    }
		}
	   }
	   ct = ct->nxt;
	   vt = vt->nxt;
	}
	if (!checkglob_and_reset(global_chset, size_globals+1, "GLOBALS"))
		ok = FALSE;
	/* restore fp's etc. DLP */
	cur_input.fp = cur_input_fp;
	for (i = 0; i < input_ptr; i++)
		input_stack[i].fp = input_stack_fp[i];
	return(ok);
}

restore_eq()
{
	DIFF_HUNK	*ct;
	DIFF_CHUNK	*vt;
	int j;

	if (showq) print_nl("Restore_eq:");
	ct = eq2_list; vt = eval2_list;
	while (ct != NULL) {
	   for (j=0;j<ct->size;j++) {
		eqtb[(int) ct->listchunk[j]].i = (int) vt->listchunk[j];
		if (showq) {
			print_nl("->a: ");print_int((int) ct->listchunk[j]);
			print(" n: ");print_int(eqtb[(int) ct->listchunk[j]].i);
		}
	   }
	   ct = ct->nxt;
	   vt = vt->nxt;
	}
}

#define FONTCHANGENOTFOUND 0
#define DIFFERENTFONTCHANGE 1
#define SAMEFONTCHANGE 2

/* Returns whether the same font array item as iaddr2 was changed in current
 * state, and if curr value is same as val2
 */
int
cmp_fontelem( iaddr2, val2 )
	intptr iaddr2;
	unsigned /*int*/ val2;
{	int found,
	    rval,
	    i;
	
	found = FALSE;
	if (fch_ptr != NULL) {
		/* move curr ptr to next elem if any */
		if ((++fcount) >= fch_ptr->size) {	/* next list chunk */
			fch_ptr = fch_ptr->nxt;
			fchset_ptr = fchset_ptr->nxt;
			if (fch_ptr == NULL)
				fchset_ptr = NULL;
		   	else {
			  if (fch_ptr->size != fchset_ptr->size) {/* error */
			  	print_nl("!!fch & fchset lists out of synch!");
				fch_ptr = fchset_ptr = NULL;
		   	  }
			  fcount = 0;
			  if (fch_ptr->size <= 0)
				fch_ptr = fchset_ptr = NULL;
			}
		}
	}
	if (fch_ptr != NULL) {
		if ((fch_ptr->size>0) && (fch_ptr->listchunk[fcount]==iaddr2)) {
			found = TRUE;
			if (bcmp(iaddr2,&val2,4) == 0)
				rval = SAMEFONTCHANGE;
			else
				rval = DIFFERENTFONTCHANGE;
		}
	}
	if (!found) {	/* scan list from start */
		fch_ptr = fdim_list; fchset_ptr = fchset;
		while ((fch_ptr != NULL) && (!found)) {
		   if (fch_ptr->size <= 0)
			break;
		   for (i=0;i<fch_ptr->size;i++) {
			if (fch_ptr->listchunk[i] == iaddr2) {
				found = TRUE;
				fcount = i;
				/* state val same as current? */
				if (bcmp(iaddr2,&val2,4) == 0)
					rval = SAMEFONTCHANGE;
				else
					rval = DIFFERENTFONTCHANGE;
				goto FONTELEM_FOUND;
			} /* if */
		   }	  /* for */
		   fch_ptr = fch_ptr->nxt;
		   fchset_ptr = fchset_ptr->nxt;
		}	  /* while */
	}		  /* if !found */
FONTELEM_FOUND:
	if (!found)
		return(FONTCHANGENOTFOUND);
	else
		return(rval);
}

int
cmp_arrays()
{
	ptr		fmem_seg,
			ptrx,ptry,
			tmpptr1, tmpptr2;
	str		tmpstr1, tmpstr2,
			strx;
	fnt		font_seg,
			fntx,
			tmpfnt2, tmpfnt1;
        ULONG   	bg, ed;
 	intptr		iaddr, iaddr2;	/* long absolute addr */
	shortptr	saddr, saddr2;	/* short addr === offset from start */
	short		s;
	int		size_aux,
			diffcode, diffcode2,
			expected_len, expected_len2,
			len,
			i,
			flag,
			ok,
			v1, v2;
	qword		*qaddr,
			q1, q2;
	mword		m1, m2;
	hh		h1, h2;
	DIFF_CHUNK	*xfchset_ptr,
			*xfch_ptr;

#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CA_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CA_BAD;
#include "Icmpdef.h"

	ok = TRUE;
	if (showq) print_nl("-- ARRAYS --");
/* ______Font Array, check limits______ */
	CHECK(tmpptr1,tmpptr2,sizeof(ptr), "fmem_ptr check failed\n");
	CHECK(tmpfnt1,tmpfnt2,sizeof(fnt), "font_ptr check failed\n");

/* ______String Pool - read current limits______ */
	CHECK(tmpptr1, tmpptr2, sizeof(ptr), "pool_ptr_high different\n");
	CHECK(tmpptr1, tmpptr2, sizeof(ptr), "pool_lowater different\n");
	CHECK(tmpstr1, tmpstr2, sizeof(str), "str_ptr_high different\n");
	CHECK(tmpstr1, tmpstr2, sizeof(str), "str_lowater different\n");

	CHECKMARK1("after curr levels"); CHECKMARK2("after prev levels");

/* ______Save Stack, array limit = SAVE_SIZE_____ */
	CHECK(tmpptr1, tmpptr2, sizeof(ptr), "save_stack levels different\n");
	if (!ok)
		return(FALSE);
	if (tmpptr2 > 0)
	    READ2(save_stack[0],sizeof(mword)*tmpptr2);
	for (i=tmpptr2-1;i>=0;i--) {
	    if (save_type(i) != RESTORE_OLD_VALUE) {
	        if (copy_of_save_stack[i].i != save_stack[i].i) {
		  if (EXPLAINIT) {
		 	 print_nl("Save stack[");print_int(i);print("] ");
			 print_int(save_stack[i]);print("!=");
			 print_int(copy_of_save_stack[i]);
		  }
		  if (!showq) return(FALSE);
		  ok=FALSE;
		}
	    } else {	/* It's an eqtb value to be checked */
		ptrx = save_index(i);
		if (copy_of_save_stack[i].i != save_stack[i].i) {
		  if ((copy_of_save_stack[i].hh.hh2.b0 != RESTORE_OLD_VALUE)
		   || (ptrx != copy_of_save_stack[i].hh.hh2.rh)) {
		  	if (EXPLAINIT) {
		 	 print_nl("Save stack [");print_int(i);
			 print("] ");print_int(save_stack[i]);print("(*");
			 print_int(ptrx);print(") order different");
		   	}
		  	if (!showq) return(FALSE);
			ok = FALSE;
		  }
		}
		if (ptrx >= INT_BASE) /* check xeq_level */
		  if (save_level(i) != copy_of_save_stack[i].hh.hh2.b1) {
		     if (EXPLAINIT) {
			print_nl("Save level ");print_int(save_level(i));
			print("] <> ");
			print_int(copy_of_save_stack[i].hh.hh2.b1);
			print(" entry[");print_int(i);print("]=*");
			print_int(ptrx);
		     }
		     if (!showq) return(FALSE);
		     ok = FALSE;
		  }
		if (i>0)
		  i--;	/* get eqtb ptr entry */
		else {print_nl("!!Restore dropped off end of save stack!");
		  ok = FALSE;
		  return(FALSE);/* ? means save stack messed up ? */
		}
		if (copy_of_save_stack[i].i != save_stack[i].i)
		  if ((ptrx>=GLUE_BASE && ptrx<LOCAL_BASE) ||
		    (ptrx>=BOX_BASE  && ptrx<CUR_FONT_LOC) ||
		    (ptrx==PAR_SHAPE_LOC)) /* shape_loc < box_base actually */
			/* parse it later in parse_mem */;
		  else {
		    if (EXPLAINIT) {
		 	 print_nl("Diff non-ptr entry in save stack[");
			 print_int(i);print("] ");print_int(save_stack[i]);
			 print("(*"); print_int(ptrx);print(")");
		    }
		    if (!showq) return(FALSE);
		    ok = FALSE;
		  }
	    } /* else RESTORE_OLD */
	} /* end for */
	if (fseek(stc_file,sizeof(mword)*tmpptr1,SEEK_CUR)<0)
	    goto CA_BAD;
	CHECKMARK1("after curr savestack"); CHECKMARK2("after prev savestack");

/* ______Font diff lists______ */
	/* !!!!! Change later so reads font list 2, checks vs locations */
	if (showq) print_nl("fonts");
	/* change font glue to init from very beginning! */
	bcopy(&font_glue_start[0], &font_glue[0], sizeof(font_glue));
	CHECK(i, diffcode, sizeof(int), "font diff codes not same\n");
	READ1(expected_len,sizeof(int)); READ2(expected_len2,sizeof(int));
	fchset_ptr = fchset; fch_ptr = fdim_list;
	fcount = 0;
	for (i=0;i<expected_len2;i++) {	/* read prev font dimension changes */
		READ2(iaddr2,sizeof(intptr));
		READ2(v2,  sizeof(int));
		flag = cmp_fontelem(iaddr2,v2);
		if (flag == SAMEFONTCHANGE)
		  fchset_ptr->listchunk[fcount]=(intptr)ITEM_EQUIV_CURR;
		if (bcmp(iaddr2,&v2,4) == 0) { /* state val same as current? */
		  /* well, if it's in font glue, that's been reset to initial
		   * value, so comparison isn't to current, but we'll be
		   * parsing the prev font glue, so that's OK. */
		  if (flag == DIFFERENTFONTCHANGE)
		  	fchset_ptr->listchunk[fcount]=(intptr)ITEM_EQUIV_CURR;
		  /* else it's really ITEM_EQUIV_INIT but we aren't keeping
		     track of that for font arrays with this data structure */
		} else if (((ULONG) iaddr2>=(ULONG)&font_glue[0]) &&
		    	   ((ULONG) iaddr2<(ULONG)&font_glue[FONT_MAX])) {
		    /* see if prev font change was in font glue, install if so
		     * copy prev ptr in so we can parse & check the glue */
		  if (showq) {
			print_nl("->fontglue: ");print_int((int)iaddr2);
			print(" o: ");print_int(*((short *) iaddr2));
			print(" n: ");print_int(*((short *) &v2));
		  }
		  bcopy(&v2,iaddr2,4);
		  /* * ((int *)iaddr2) = * ((int *) &v2); */
		  if (flag == DIFFERENTFONTCHANGE)
		  	fchset_ptr->listchunk[fcount]=(intptr)ITEM_EQUIV_CURR;
		} else {	/* elem not in font glue, fatal difference */
		     if (flag == DIFFERENTFONTCHANGE)
		     	fchset_ptr->listchunk[fcount]=(intptr)ITEM_UNEQUIV_CURR;
		     if (showq) {
			print_nl("!!f: ");print_int((uint)iaddr2);print(" v: ");
			print_int(*iaddr2);print(" ");print_int(v2);
		     }
		     ok = FALSE;
		     if (EXPLAINIT) print_nl("Font dim compare failed.");
		}
	}
	/* now scan for any unchecked entries */
	xfchset_ptr = fchset;xfch_ptr = fdim_list;
	if (ok)
	  while ((xfchset_ptr != NULL) && (xfchset_ptr->size>0)) {
		for (i=0;i<xfchset_ptr->size;i++)
		      if ((int) xfchset_ptr->listchunk[i]==ITEM_CHANGED) {
			/* Is item duplicated earlier, with flag checked? */
			fchset_ptr = fchset;fch_ptr = fdim_list;fcount=0;
		        flag = cmp_fontelem(xfch_ptr->listchunk[i],0);
		        if ((int)fchset_ptr->listchunk[fcount]==ITEM_CHANGED) {
		     	   	if (EXPLAINIT) {
				  print_nl("!!not in prev fdim list: ");
				  print_int((uint) xfch_ptr->listchunk[i]);
				}
				ok = FALSE;
			}
		        xfchset_ptr->listchunk[i]=ITEM_CLEAR;
		      }
		xfchset_ptr = xfchset_ptr->nxt;xfch_ptr = xfch_ptr->nxt;
	  }
	xfchset_ptr = fchset;	/* now reset flags */
	while ((xfchset_ptr != NULL) && (xfchset_ptr->size>0)) {
		for (i=0;i<xfchset_ptr->size;i++)
		        xfchset_ptr->listchunk[i]=(intptr)ITEM_CHANGED;
		xfchset_ptr = xfchset_ptr->nxt;
	}
	if (fseek(stc_file,
		(sizeof(intptr)+sizeof(int))*expected_len,SEEK_CUR)<0)
	    goto CA_BAD;
	CHECKMARK1("after curr font list"); CHECKMARK2("after prev font list");

/* ______Eqtb table______ */
	if (showq) {
		if (ok) print(" ok"); else print(" bad"); print_nl("eqtb");
	} else if (!ok) {
		if (EXPLAINIT) print_nl("font change cmp failed.");
		return(FALSE);
	}
	CHECK(diffcode, diffcode2, sizeof(int), "eqtb diff codes not same\n");
	if (diffcode == diffcode2) {
	   if (diffcode != DiffListCode) {	/* then read in array mode */
		if (fseek(stc_file, sizeof(mword)*(EQTB_SIZE+1), SEEK_CUR)<0)
			goto CA_BAD;
	  	CHECKMARK1("after curr eqtb/array");
		for (i=0;i<EQTB_SIZE+1;i++) {
	  		READ2(m2, sizeof(mword));
			if (m2.i != eqtb[i].i)
		  	  if ((i>=GLUE_BASE && i<LOCAL_BASE) ||
		      		(i==PAR_SHAPE_LOC) ||
		      		(i>=BOX_BASE && i<CUR_FONT_LOC)) {
			  /* do ptr parse check, save curr value in temp list */
			  /* flag it OK for now, decide later in parse_eqtb */
			    eqtb[i].i = m2.i;
		  	    if( eq2_end == NULL ) {
				print_nl("Hey! eq2_end = NULL");
		 		GETDIFFHUNK( eq2_end );
				GETDIFFCHUNK( eval2_end );
				eq2_end->nxt = eq2_end->prev = NULL;
				eval2_end->nxt = eval2_end->prev = NULL;
				eq2_end->size = eval2_end->size = 0;
				eq2_list = eq2_end;
				eval2_list = eval2_end;
		  	    }
			    if( eq2_end->size >= DIFF_HUNK_SIZE ) {
				USENEWHUNK(eq2_end);
				USENEWCHUNK(eval2_end);
			    }
			    eq2_end->listchunk[eq2_end->size] = (shortptr) i;
			    eval2_end->listchunk[eval2_end->size] =
				(intptr) eqtb[i].i;
			    eq2_end->size++; eval2_end->size++;
			  } else {
			   	print_nl("Eqtb[");print_int(i);print("] = ");
				print_int(eqtb[i].i);print(" ");print_int(m2.i);
			  }
		}
	  	CHECKMARK2("after prev eqtb/array");
	   } else {
	      READ1(expected_len,sizeof(int)); READ2(expected_len2,sizeof(int));
	      for (i=0;i<expected_len2;i++) {
		READ2(saddr2,sizeof(shortptr)); READ2(v2, sizeof(mword));
		flag = check_set_item(eqtb_chset, (int) saddr2, EQTB_SIZE+1);
		switch (flag) {
		case SET_ELEMBAD:  /* error */
			print_nl("Set elem bad on eqtb item #");print_int(i);
			break;
		case SET_CHECKBAD:  /* error */
			print_nl("Set check bad on eqtb item #");print_int(i);
			break;
		case ITEM_UNEQUIV_CURR:
		case ITEM_UNEQUIV_INIT:
		case ITEM_EQUIV_CURR:
		case ITEM_EQUIV_INIT:  break; /* checked already */
		case ITEM_CLEAR:
		case ITEM_CHANGED:
		 if (eqtb[saddr2].i == v2) {
		  if (flag == ITEM_CLEAR)
			eqtb_chset[saddr2] = ITEM_EQUIV_INIT;
		  else
			eqtb_chset[saddr2] = ITEM_EQUIV_CURR;
		 } else if ((saddr2>=GLUE_BASE && saddr2<LOCAL_BASE) ||
		      (saddr2==PAR_SHAPE_LOC) ||
		      (saddr2>=BOX_BASE && saddr2<CUR_FONT_LOC)) {
			/* Is value in one of regions that point into dyn mem?
			 * do ptr parse check, save curr value in temp list
			 * flag ask OK for now, decide later in parse_eqtb */
			if (flag == ITEM_CLEAR)
				eqtb_chset[saddr2] = ITEM_EQUIV_INIT;
			else
				eqtb_chset[saddr2] = ITEM_EQUIV_CURR;
		  	if( eq2_end == NULL ) {
				print_nl("Hey! eq2_end = NULL");
		 		GETDIFFHUNK( eq2_end );
				GETDIFFCHUNK( eval2_end );
				eq2_end->nxt = eq2_end->prev = NULL;
				eval2_end->nxt = eval2_end->prev = NULL;
				eq2_end->size = eval2_end->size = 0;
				eq2_list = eq2_end;
				eval2_list = eval2_end;
		  	}
			if( eq2_end->size >= DIFF_HUNK_SIZE ) {
				USENEWHUNK(eq2_end);
				USENEWCHUNK(eval2_end);
			}
			eq2_end->listchunk[eq2_end->size] = (shortptr) saddr2;
			eval2_end->listchunk[eval2_end->size] =
				(intptr) eqtb[saddr2].i;
			eq2_end->size++; eval2_end->size++;
			eq2_listlen++;   eval2_listlen++;
		  	if (showq) {
			  print_nl("->ptr: ");print_int(saddr2);print(" o: ");
			  print_int(eqtb[saddr2].i);print(" n:");print_int(v2);
		  	}
			eqtb[saddr2].i = v2;
		  } else {	/* not in ptr section */
			if (flag == ITEM_CLEAR)
				eqtb_chset[saddr2] = ITEM_UNEQUIV_INIT;
			else
				eqtb_chset[saddr2] = ITEM_UNEQUIV_CURR;
		  	if (EXPLAINIT) {
			  print_nl("!!eq: ");print_int(saddr2);print(" o: ");
			  print_int(eqtb[saddr2].i);print(" n:");print_int(v2);
		  	}
		  	if (!showq) {
	      		  reset(eqtb_chset,EQTB_SIZE+1);
			  return(FALSE);
			}
		  	ok = FALSE;
		  }
		  break;
		} /* switch */
	      }   /* for */
	      if (!check_and_reset( eqtb_chset, EQTB_SIZE+1, "EQTB" )) {
		if (showq || (EXPLAINIT)) print_nl("!!eqtb bit reset failed");
		if (!showq) return(FALSE);
		ok = FALSE;
	      }
	      if (fseek(stc_file, (sizeof(shortptr)+sizeof(mword))
			*expected_len, SEEK_CUR)<0) goto CA_BAD;
	      CHECKMARK1("after curr eq list");CHECKMARK2("after prev eq list");
	   }
	} else {
	   if (EXPLAINIT) print_nl("!!Eqtb formats different: ");
	   if (diffcode != DiffListCode) {
		if (EXPLAINIT) print("curr eqtb=Array, prev= diff ");
		if (fseek(stc_file, sizeof(mword)*(EQTB_SIZE+1), SEEK_CUR)<0)
			goto CA_BAD;
	  	CHECKMARK1("after curr eqtb/array");
	   } else {
		if (EXPLAINIT) print("curr eqtb=List, prev= diff ");
	  	READ1(expected_len, sizeof(int));
	        if (fseek(stc_file, (sizeof(shortptr)+sizeof(mword))
			*expected_len,SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK1("after curr eqtb/list");
	   }
	   if (!showq) return(FALSE);
	   ok = FALSE;
	   if (diffcode2 != DiffListCode) {
		if (EXPLAINIT) print("prev eqtb=Array ");
		if (fseek(stc_file2, sizeof(mword)*(EQTB_SIZE+1), SEEK_CUR)<0)
			goto CA_BAD;
	  	CHECKMARK2("after prev eqtb/array");
	   } else {
		if (EXPLAINIT) print("prev eqtb=List ");
	  	READ2(expected_len, sizeof(int));
	        if (fseek(stc_file2,(sizeof(shortptr)+sizeof(mword))
			*expected_len, SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK2("after prev eqtb/list");
	   }
	}

/* ______Hash table______ */
	if (showq) {
		if (ok) print(" ok"); else print(" bad"); print_nl("hash");
	} else if (!ok) {
		if (EXPLAINIT) print_nl("eq cmp failed.");
		return(FALSE);
	}
	CHECK(diffcode, diffcode2, sizeof(int), "hash diff codes not same\n");
	if (diffcode == diffcode2) {
	   if (diffcode != DiffListCode) {	/* then read in array mode */
		if(fseek(stc_file,sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1),
			SEEK_CUR)<0)
			goto CA_BAD;
	  	CHECKMARK1("after curr hash/array");
		for (i=0;i<UNDEFINED_CONTROL_SEQUENCE+1;i++) {
	  		READ2(h2, sizeof(hh));
			if (h2.all != hash[i].all) {
			   if (showq) {
			     print_nl("Hash[");print_int(i);print("] = ");
			     print_int(hash[i].all);print(" ");
			     print_int(h2.all);
			   }
			   ok = FALSE;
			}
		}
	  	CHECKMARK2("after prev hash/array");
	   } else {
	      READ1(expected_len,sizeof(int)); READ2(expected_len2,sizeof(int));
	      for (i=0;i<expected_len2;i++) {
		READ2(saddr2,sizeof(shortptr));
		READ2(h2,  sizeof(hh));
		flag = check_set_item(hash_chset, (int) saddr2,
			UNDEFINED_CONTROL_SEQUENCE+1);
		switch (flag) {
		case SET_ELEMBAD:  /* error */
			print_nl("!!Set elem spec bad on hash item #");
			print_int(i);
			break;
		case SET_CHECKBAD:  /* error */
			print_nl("!!Set check bad on hash item #");print_int(i);
			break;
		case ITEM_EQUIV_CURR:
		case ITEM_EQUIV_INIT:
		case ITEM_UNEQUIV_CURR:
		case ITEM_UNEQUIV_INIT: /* checked already */ break;
		case ITEM_CLEAR:
			if (hash[saddr2].all == h2.all)
				hash_chset[saddr2] = ITEM_EQUIV_INIT;
			else {
				hash_chset[saddr2] = ITEM_UNEQUIV_INIT;
				if (showq || (EXPLAINIT)) {
		  		  print_nl("h prev: ");print_int((uint) saddr2);
				  print(" v: ");print_int(hash[saddr2].all);
				  print(" ");print_int(h2.all);
				}
				if (!showq) {
	      		  	  reset(
				    hash_chset,UNDEFINED_CONTROL_SEQUENCE+1);
			  	  return(FALSE);
				}
				ok = FALSE;
			}
			break;
		case ITEM_CHANGED:
			if (hash[saddr2].all == h2.all)
				hash_chset[saddr2] = ITEM_EQUIV_CURR;
			else {
				hash_chset[saddr2] = ITEM_UNEQUIV_CURR;
				if (showq || (EXPLAINIT)) {
		  		  print_nl("h diff: ");print_int((uint) saddr2);
				  print("  v: ");print_int(hash[saddr2].all);
				  print(" ");print_int(h2.all);
				}
				if (!showq) {
	      		  	  reset(
				    hash_chset,UNDEFINED_CONTROL_SEQUENCE+1);
			  	  return(FALSE);
				}
				ok = FALSE;
			}
			break;
		} /* switch */
	      }   /* for */
	      if (!check_and_reset(
			hash_chset, UNDEFINED_CONTROL_SEQUENCE+1,"HASH")) {
		if (showq || (EXPLAINIT)) print_nl("!!hash bit reset failed");
		if (!showq) return(FALSE);
		ok = FALSE;
	      }
	      if (fseek(stc_file, (sizeof(shortptr)+sizeof(hh))
			*expected_len, SEEK_CUR)<0) goto CA_BAD;
	      CHECKMARK1("after curr hash list");
	      CHECKMARK2("after prev hash list");
	   }
	} else {
	   if (EXPLAINIT) print_nl("Hash formats different: ");
	   if (diffcode != DiffListCode) {
		if (EXPLAINIT) print("curr hash=Array, prev= diff ");
		if (fseek(stc_file,sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1),
			SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK1("after curr hash/array");
	   } else {
		if (EXPLAINIT) print("curr hash=List, prev= diff ");
	  	READ1(expected_len, sizeof(int));
		if(fseek(stc_file,(sizeof(hh)+sizeof(shortptr))
			*expected_len,SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK1("after curr hash/list");
	   }
	   if (!showq) return(FALSE);
	   ok = FALSE;
	   if (diffcode2 != DiffListCode) {
		if (EXPLAINIT) print("prev hash=Array ");
		if(fseek(stc_file2,sizeof(hh)*(UNDEFINED_CONTROL_SEQUENCE+1),
			SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK2("after prev hash/array");
	   } else {
		if (EXPLAINIT) print("prev hash=List ");
	  	READ2(expected_len, sizeof(int));
		if(fseek(stc_file2,(sizeof(hh)+sizeof(shortptr))
			*expected_len,SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK2("after prev hash/list");
	   }
	}

/* ______Xeq_level table______ */
	if (showq) {
		if (ok) print(" ok"); else print(" bad"); print_nl("xeq");
	} else if (!ok) {
		if (EXPLAINIT) print_nl("hash cmp failed.");
		return(FALSE);
	}
	CHECK(diffcode, diffcode2, sizeof(int), "xeq diff codes not same\n");
	if (diffcode == diffcode2) {
	   if (diffcode != DiffListCode) {	/* then read in array mode */
		if (fseek(stc_file,sizeof(qword)*(EQTB_SIZE+1 - INT_BASE))<0)
			goto CA_BAD;
	  	CHECKMARK1("after curr xeq/array");
		for (i=0;i<EQTB_SIZE+1 - INT_BASE;i++) {
	  		READ2(q2, sizeof(qword));
			if (q2!= xeq_level[i]) {
			  print_nl("xeq[");print_int(i);print("] = ");
			  print_int(xeq_level[i]);print(" ");print_int(q2);
			  ok = FALSE;
			}
		}
	  	CHECKMARK2("after prev xeq/array");
	   } else {
	      READ1(expected_len,sizeof(int)); READ2(expected_len2,sizeof(int));
	      for (i=0;i<expected_len2;i++) {
		READ2(saddr2,sizeof(shortptr));
		READ2(q2,  sizeof(qword));
		flag = check_set_item(hash_chset, (int) saddr2,
			EQTB_SIZE+1 - INT_BASE);
		switch (flag) {
		case SET_ELEMBAD:  /* error */
			print_nl("Set elem spec bad on xeq item #");
			print_int(saddr2);
			break;
		case SET_CHECKBAD:  /* error */
			print_nl("Set check bad on xeq item #");
			print_int(saddr2);
			break;
		case ITEM_EQUIV_CURR:
		case ITEM_EQUIV_INIT:
		case ITEM_UNEQUIV_CURR:
		case ITEM_UNEQUIV_INIT: /* checked already */ break;
		case ITEM_CLEAR:
			if (xeq_level[saddr2] == q2)
				xeq_chset[saddr2] = ITEM_EQUIV_INIT;
			else {
				xeq_chset[saddr2] = ITEM_UNEQUIV_INIT;
				if (showq || (EXPLAINIT)) {
		  		  print_nl("x prev: ");print_int((uint) saddr2);
				  print(" v: ");
				  print_int((int) xeq_level[saddr2]);
				  print(" ");print_int((int) q2);
				}
				if (!showq) {
	      		  	  reset(hash_chset,EQTB_SIZE+1 - INT_BASE);
			  	  return(FALSE);
				} else ok = FALSE;
			}
			break;
		case ITEM_CHANGED:
			if (xeq_level[saddr2] == q2)
				xeq_chset[saddr2] = ITEM_EQUIV_CURR;
			else {
				xeq_chset[saddr2] = ITEM_UNEQUIV_CURR;
				if (showq || (EXPLAINIT)) {
		  		  print_nl("x diff: ");print_int((uint) saddr2);
				  print("  v: ");
				  print_int((int) xeq_level[saddr2]);
				  print(" ");print_int((int) q2);
				}
				if (!showq) {
	      		  	  reset(hash_chset,EQTB_SIZE+1 - INT_BASE);
			  	  return(FALSE);
				} else ok = FALSE;
			}
			break;
		} /* switch */
	      }   /* for */
	      if (!check_and_reset(xeq_chset, EQTB_SIZE+1 - INT_BASE,"XEQ")) {
		if (showq || (EXPLAINIT)) print_nl("!!xeq bit reset failed");
		ok = FALSE;
	      }
	      if (fseek(stc_file, (sizeof(shortptr)+sizeof(qword))
			*expected_len, SEEK_CUR)<0) goto CA_BAD;
	      CHECKMARK1("after curr xeq list");
	      CHECKMARK2("after prev xeq list");
	   }
	} else {
	   if (EXPLAINIT) print_nl("Xeq formats different: ");
	   if (diffcode != DiffListCode) {
	   	if (EXPLAINIT) print("curr xeq=Array, prev= diff ");
		if (fseek(stc_file,sizeof(qword)*(EQTB_SIZE+1 - INT_BASE))<0)
			goto CA_BAD;
	  	CHECKMARK1("after curr xeq/array");
	   } else {
	   	if (EXPLAINIT) print("curr=List, prev= diff ");
	  	READ1(expected_len, sizeof(int));
		if(fseek(stc_file,(sizeof(qword)+sizeof(shortptr))
			*expected_len,SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK1("after curr xeq/list");
	   }
	   if (!showq) return(FALSE);
	   ok = FALSE;
	   if (diffcode2 != DiffListCode) {
	   	if (EXPLAINIT) print("prev xeq=Array ");
		if (fseek(stc_file2,sizeof(qword)*(EQTB_SIZE+1 - INT_BASE))<0)
			goto CA_BAD;
	  	CHECKMARK2("after prev xeq/array");
	   } else {
	   	if (EXPLAINIT) print("prev=List ");
	  	READ2(expected_len, sizeof(int));
		if(fseek(stc_file2,(sizeof(qword)+sizeof(shortptr))
			*expected_len,SEEK_CUR)<0) goto CA_BAD;
	  	CHECKMARK2("after prev xeq/list");
	   }
	}
	if (showq) {
		if (ok) print(" ok"); else print(" bad"); print_ln();
	} else if (!ok && EXPLAINIT)
		print_nl("xeq cmp failed.");
	return(ok);
CA_BAD:
	print_nl("!!State checkpoint file damaged (array section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2

/*	restore mem structures
 */
int
cmp_mems()
{
	int	i,
		ok,
		iaddr;
#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CMS_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CMS_BAD;
#include "Icmpdef.h"

	if (showq) print_nl("- MEMS -");
	if (!(ok = readsingle())) {
		if (EXPLAINIT) print_nl("Single node mem read failed.");
		return(FALSE);}
	CHECKMARK1("after curr single"); CHECKMARK2("after prev single");
	if (!ok) return(FALSE);
	
	if (!(ok = cmp_token())) {
		if (EXPLAINIT) print_nl("Token mem different.");
		return(FALSE);}
	CHECKMARK1("after curr tokens"); CHECKMARK2("after prev tokens");
	if (!ok) return(FALSE);

	if (!(ok = readmulti())) {
		if (EXPLAINIT) print_nl("Multi node mem read failed.");
		return(FALSE);}
	/*skip multi's for stc_file: CHECKMARK1("after curr multiword");*/
	CHECKMARK2("after prev multiword");

	return(ok);
CMS_BAD:
	print_nl("!!Dynamic memory section damaged!\n");
	return(FALSE);
}
#undef READ1
#undef READ2

/*	restore upper half mem
 */
readsingle()
{
	int	i, j, len;
	ptr	beg, end;
	ptr	beg2, end2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file) < 1) goto CS_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CS_BAD;

	if (showq) print_nl("single");
	READ1(s_indx,sizeof(int));
	for (i = 0; i < s_indx; i++) {	/* read chunk */
		READ1(beg,sizeof(ptr));READ1(end,sizeof(ptr));
		s_array[i].beg = beg; s_array[i].end = end;
		if (fseek(stc_file, (end-beg+1)*sizeof(mword), SEEK_CUR)<0)
			goto CS_BAD;
	}
	READ2(s_indx2,sizeof(int));
	for (i = 0; i < s_indx2; i++) {	/* read chunk */
		READ2(beg2,sizeof(ptr));READ2(end2,sizeof(ptr));
		s_array2[i].beg = beg2; s_array2[i].end = end2;
		READ2(mem[beg2],(end2-beg2+1)*sizeof(mword));
	}
	return(TRUE);
CS_BAD:
	print_nl("!!State checkpoint file damaged (single section)!\n");
	return(FALSE);
}
#undef READ1
#undef READ2

/*  	compare token_list node 
 */
cmp_token()
{
	sary	s_array[SARRY_SIZE], s_array2[SARRY_SIZE];
	int	s_indx, s_indx2;
	int	i, j, len, ok;
	ptr	beg, end;
	ptr	beg2, end2;
	int	same,
		skipped;
	tok	t1, t2;
	ptr	p1, p2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file)  < 1) goto CT_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CT_BAD;
#include "Icmpdef.h"

	ok = TRUE;
	if (showq) print_nl("token");
	CHECK(s_indx, s_indx2, sizeof(int), "# token chunks differ\n");
	len=MIN(s_indx, s_indx2);
	for (i = 0; i < len; i++) {	/* read chunk */
		same = TRUE;
		COMPARE(beg,beg2,sizeof(ptr),"BEG ");	/* sets ok & same */
		if (!same && TALK) {
		   print_int((uint) beg);print_char(' ');print_int((uint) beg2);
		}
		COMPARE(end,end2,sizeof(ptr),"END ");
		s_array[i].beg = beg;s_array2[i].beg = beg2;
		s_array[i].end = end;s_array2[i].end = end2;
		if ((end-beg != end2-beg2) && TALK) {
		   print("SIZE ");print_int(end-beg);print_char(' ');
		   print_int(end2-beg2);print_char(' ');
		   ok = same = FALSE;
		}
		if (!same && TALK) {print("chunk ");print_int(i);print_ln();}
		if (end-beg == end2-beg2) {
		   same=TRUE;
		   for (j=beg;j<=end;j++)
		     if (same) {
			COMPARE(t1,t2,sizeof(tok), "tok_mem diff ");
		     	if (!same && TALK) {
			   print(" #");print_int(j);print(" in chunk ");
			   print_int(i);print(" size ");print_int(end-beg);
			   print_char('@');print_int(beg);print_ln();
			}
		     } else {READ1(t1,sizeof(tok));READ2(t2,sizeof(tok));}
		   same=TRUE;
		   for (j=beg;j<=end;j++)
		     if (same) {
			COMPARE(p1,p2,sizeof(ptr), "tok_link diff ");
		     	if (!same && TALK) {
			   print(" #");print_int(j);print(" in chunk ");
			   print_int(i);print(" size ");print_int(end-beg);
			   print_char('@');print_int(beg);print_ln();
			}
		     } else {READ1(p1,sizeof(ptr));READ2(p2,sizeof(ptr));}
		} else {
			if (fseek(stc_file,
			   (end-beg+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR)<0)
				goto CT_BAD;
			if (fseek(stc_file2,
			   (end2-beg2+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR)<0)
				goto CT_BAD;
		}
		if (!ok && !showq) return(FALSE);
	}
	if (s_indx>len) {
	   skipped = 0;ok = FALSE;
	   for (i = len; i < s_indx; i++) {	/* skip chunk */
		READ1(beg,sizeof(ptr));READ1(end,sizeof(ptr));
		s_array[i].beg = beg; s_array[i].end = end;
		if (fseek(stc_file,
			   (end-beg+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR)<0)
				goto CT_BAD;
		skipped += (end-beg+1)*(sizeof(tok)+sizeof(ptr));
	   }
	   if (TALK) {
	   	print("curr state: extra ");print_int(s_indx-len);
	   	print(" token chunks - (");print_int(skipped);
	   	print(" bytes)");print_ln();
	   }
	}
	if (s_indx2>len) {
	   skipped = 0;ok = FALSE;
	   for (i = len; i < s_indx2; i++) {
		READ2(beg2,sizeof(ptr));READ2(end2,sizeof(ptr));
		s_array2[i].beg = beg2; s_array2[i].end = end2;
		if (fseek(stc_file2,
			   (end2-beg2+1)*(sizeof(tok)+sizeof(ptr)),SEEK_CUR)<0)
				goto CT_BAD;
		skipped += (end2-beg2+1)*(sizeof(tok)+sizeof(ptr));
	   }
	   if (TALK) {
	   	print("prev state: extra ");print_int(s_indx2-len);
	   	print(" token chunks - (");print_int(skipped);
	   	print(") bytes)");print_ln();
	   }
	}
	return(ok);
CT_BAD:
	print_nl("!!State checkpoint file damaged (token section)!");
	return(FALSE);
}
#undef READ1
#undef READ2
	
/*	restore lower half mem
 */
extern	int		show_stuff;
readmulti()
{
	int	i, j, len;
	ptr	beg, end;
	ptr	beg2, end2;

#define READ1(A,B) if (fread(&A, B, 1, stc_file)  < 1) goto CM_BAD;
#define READ2(A,B) if (fread(&A, B, 1, stc_file2) < 1) goto CM_BAD;
/* read just from stc file 2, don't need anything from stc file 1 */

	if (showq) print_nl("multi");
	READ1(m_indx,sizeof(int));
	for (i = 0; i < m_indx; i++) {	/* read chunk */
		READ1(beg,sizeof(ptr));READ1(end,sizeof(ptr));
		m_array[i].beg = beg; m_array[i].end = end;
		if (fseek(stc_file,(end-beg+1)*sizeof(mword),SEEK_CUR) < 0)
		    goto CM_BAD;
	}
	READ2(m_indx2,sizeof(int));
	for (i = 0; i < m_indx2; i++) {
		READ2(beg2,sizeof(ptr));READ2(end2,sizeof(ptr));
		m_array2[i].beg = beg2; m_array2[i].end = end2;
		if (fread(&mem[beg2],(end2-beg2+1)*sizeof(mword),1,stc_file2)
		    < 1) goto CM_BAD;
		if (showq && show_stuff && show_state) {
			print_nl("restored multi mem: beg= ");
			print_int(beg2);
			print(", end= ");
			print_int(end2);
			print_nl("multi mem read: ");
			print_int((end2 - beg2 +1)*sizeof(mword));
			print(" bytes");
		}
	}
	return(TRUE);
CM_BAD:
	print_nl("State checkpoint file damaged (multi section)!");
	return(FALSE);
}
#undef READ1
#undef READ2
#undef  CHECK
#undef  COMPARE
#undef  CHECKMARK1
#undef  CHECKMARK2

#define AUX_LAYER_SIZE (sizeof(qqqq)+sizeof(scal)*2+sizeof(hword)+\
	sizeof(str)*2+sizeof(byte)*2 \
	/*font_used: +sizeof(bool)*/ +sizeof(ptr)*10+sizeof(int)*2)

/*	restore string array, font arrays written at end
 */
cmp_appends()
{
	int	page,
		compressed,
		size_aux;
	str	tmpstr,
		str_high;
	struct	timeval	when;
	ptr	fmem_seg,
		fmem_sofar,
		fmem_chunk,
		pool_high,
		tmpptr;
	fnt	font_seg,
		font_sofar,
		font_chunk,
		tmpfnt;
	ptr	instart;
	ascii	inpool;
	mword	mtmp;
	FILE	*str_copy;

	if (fatal_append_diff) {
		if (EXPLAINIT)
			print_nl("Previous fatal string or font difference.");
		return(FALSE);
	}
	
	if ((pool_ptr<=pool_lastcmp) && (str_ptr<=str_lastcmp)) {
	/* if (explain) {
		print_nl("String pool has not grown since last comparison.");
		print_nl("pool ptr = ");print_int(pool_ptr);
		print(" last = ");	print_int(pool_lastcmp);
		print_nl("str ptr = ");	print_int(str_ptr);
		print(" last = ");	print_int(str_lastcmp);
	   }
	*/ goto COMPARE_FONTS;
	}

#define READ1(A,B) if (fread(A, B, 1, str_copy) < 1) goto STR_BAD

	get_ext(-1, EXT_STR);
	if ((str_copy = fopen(name_of_file, "r")) == NULL) {
		print_nl("Failed to open string file ");print(name_of_file);
		print_char('.');print_ln();update_terminal(); goto CHECK_FAIL;
	} else if (showq) {print_nl(name_of_file);print(" opened.");}

	/* read & check state version. DLP */
	if (!read_state_code(&page, &compressed, str_copy))
		goto CHECK_FAIL;

	/* read time written */
	READ1(&when.tv_sec, sizeof(long));
	
	/* read high water marks. DLP */
	READ1(&pool_high, sizeof(ptr));
	READ1(&tmpptr,  sizeof(ptr));
	if (tmpptr != pool_lowater) {
		print_nl("!Error: pool_lowater changed from last run.");
		print_nl("Rerun with -v if new system style has been loaded.");
		goto CHECK_FAIL;
	}
	READ1(&str_high,  sizeof(str));
	READ1(&tmpstr, sizeof(str));
	if (tmpstr != str_lowater) {
		print_nl("!Error: str_lowater changed from last run.");
		print_nl("Rerun with -v if new system style has been loaded.");
		goto CHECK_FAIL;
	}

	/* !!! skip lowater to lastcmp later? */

	/* read str_pool array */
	for (tmpptr=pool_lowater;tmpptr<pool_ptr;tmpptr++) {
		READ1(&inpool,sizeof(ascii));
		if (inpool != str_pool[tmpptr]) {
			if (EXPLAINIT) {
				print_nl("!!str_pool[");print_int(tmpptr);
				print("] different!");
			}
			goto CHECK_FAIL;
		}
	}
	if (pool_ptr<pool_high+1)
		if (fseek(str_copy,
			sizeof(ascii)*(pool_high+1-pool_ptr),SEEK_CUR)<0)
			goto STR_BAD;
	
	/* read str_start array */
	for (tmpstr=str_lowater;tmpstr<str_ptr;tmpstr++) {
		READ1(&instart,sizeof(ptr));
		if (instart != str_start[tmpstr]) {
			if (EXPLAINIT) {
				print_nl("!!str_start[");print_int(tmpstr);
				print("] different!");
			}
			goto CHECK_FAIL;
		}
	}
	if (str_ptr<str_high+1)
		if (fseek(str_copy,
			sizeof(ptr)*(str_high+1-str_ptr),SEEK_CUR)<0)
			goto STR_BAD;
	
	if (show_state) {
		print_nl("Str_pool high = "); print_int(pool_ptr);
		print(" low = "); print_int(pool_lowater);
		print(" (bytes for highwater str_pool): ");
		print_int(sizeof(ascii)*(pool_high+1-pool_lowater));

		print_nl("Str_start high = "); print_int(str_ptr);
		print(" low = "); print_int(str_lowater);
		print(" (bytes read for highwater str_start): ");
		print_int(sizeof(ptr)*(str_high+1-str_lowater));
	}
	(void) fclose(str_copy);
	str_lastcmp = str_ptr;
	pool_lastcmp = pool_ptr;

/* Fonts - must be read in BEFORE font diff lists! */

COMPARE_FONTS:

#undef READ1
#define READ1(A,B) if (fread(A, B, 1, str_copy) < 1) goto FONT_BAD;

	if ((fmem_ptr<=fmem_lastcmp) && (font_ptr<=font_lastcmp)) {
	/* if (explain) {
		print_nl("Fonts have not grown since last comparison.");
		print_nl("font ptr = ");print_int(font_ptr);
		print(" last = ");	print_int(font_lastcmp);
		print_nl("fmem ptr = ");print_int(fmem_ptr);
		print(" last = ");	print_int(fmem_lastcmp);
	   }
	*/ return(TRUE);
	}

	get_ext(-1, EXT_FONT);
	if ((str_copy = fopen(name_of_file, "r")) == NULL) {
		print_nl("Failed to open font file ");print(name_of_file);
		print_char('.');print_ln();update_terminal(); goto CHECK_FAIL;
	} else if (showq) {print_nl(name_of_file);print(" opened.");}

	/* read & check state version. DLP */
	if (!read_state_code(&page, &compressed, str_copy))
		goto CHECK_FAIL;

	/* read time written */
	READ1(&when.tv_sec, sizeof(long));
	
	/* set low water marks. DLP */
	fmem_sofar = fmem_lowater;
	font_sofar = font_lowater;

	while ((fmem_sofar < fmem_ptr) || (font_sofar < font_ptr)) {
	  READ1(&tmpptr,   sizeof(ptr));
	  READ1(&fmem_seg, sizeof(ptr));
	  if (tmpptr != fmem_sofar) {
		print_nl("!!aux font segment starts at wrong place!");
		goto CHECK_FAIL;
	  }
	  if (fmem_sofar >= fmem_ptr)
	     print_nl(!!"Hmm, fmem_sofar has passed fmem_ptr in font check.");
	  else {
	  /* skipped checked section, important 'cause random changes logged
	   * in font change list may have been made and compare could fail */
	   if (fmem_sofar < fmem_lastcmp) {
	     fmem_chunk=MIN(fmem_seg,fmem_lastcmp-fmem_sofar);
	     if (fseek(str_copy,sizeof(mword)*fmem_chunk,SEEK_CUR)<0)
			goto FONT_BAD;
	     fmem_sofar += fmem_chunk;
	     fmem_seg   -= fmem_chunk;
	   }

	  /* check in any new section */
	   if (fmem_seg > 0) {
	     fmem_chunk=MIN(fmem_seg,fmem_ptr-fmem_sofar);
	     for (tmpptr=fmem_lastcmp;tmpptr<fmem_lastcmp+fmem_chunk;tmpptr++) {
		READ1(&mtmp,sizeof(mword));
		if (mtmp.i != font_info[tmpptr].i) {
			if (EXPLAINIT) {
				print_nl("!!font_info[");print_int(tmpptr);
				print("] different!");
			}
			goto CHECK_FAIL;
		}
	     } /* for */
	     fmem_sofar = fmem_lastcmp += fmem_chunk;
	     fmem_seg -= fmem_chunk;
	   }
	  } /* _sofar >= _ptr */

	  /* skip any 'future' section */
	  if (fmem_seg > 0)
		if (fseek(str_copy,sizeof(mword)*fmem_seg,SEEK_CUR)<0)
			goto FONT_BAD;

	  if (show_state) {
		print_nl("font_info segment size loaded (bytes): ");
		print_int(2*sizeof(ptr));
		print("+");		print_int(sizeof(mword)*fmem_chunk);
		print(" high = ");	print_int(fmem_lastcmp);
		print(" low = ");	print_int(fmem_lastcmp-fmem_seg);
		update_terminal();
	  }

	  READ1(&tmpfnt,   sizeof(fnt));
	  READ1(&font_seg, sizeof(fnt));
	  if (tmpfnt != font_sofar) {
		print_nl("!!aux font segment starts at wrong place!");
		goto CHECK_FAIL;
	  }
	  if (font_sofar >= font_ptr)
	     print_nl(!!"Hmm, font_sofar has passed font_ptr in font check.");
	  else {
	   if (font_sofar < font_lastcmp) {
	     font_chunk=MIN(font_seg,font_lastcmp-font_sofar);
	     if (fseek(str_copy,font_chunk*AUX_LAYER_SIZE,SEEK_CUR)<0)
			goto FONT_BAD;
	     font_sofar += font_chunk;
	     font_seg   -= font_chunk;
	   }

	   if (font_seg > 0) {
	     font_chunk=MIN(font_seg,font_ptr-font_sofar);
	     if (!check_aux(font_chunk,font_lastcmp,str_copy))
	  	goto FONT_BAD;
	     font_sofar = font_lastcmp += font_chunk;
	     font_seg -= font_chunk;
	   }
	  } /* _sofar >= _ptr */

	  if (font_seg > 0) {
	     if (fseek(str_copy,font_seg*AUX_LAYER_SIZE,SEEK_CUR)<0)
			goto FONT_BAD;
	     font_sofar += font_chunk;
	  }

	  if (show_state) {
		print_nl("font aux segment size loaded (bytes): ");
		print_int(2*sizeof(fnt));
		print("+");		print_int(font_chunk*AUX_LAYER_SIZE);
		print(" high = ");	print_int(font_lastcmp);
		print(" low = ");	print_int(font_lastcmp-font_chunk);
		update_terminal();
	  }
	} /* while */

	(void) fclose(str_copy);

	if ((fmem_lastcmp != fmem_ptr) || (font_lastcmp != font_ptr)) {
		print_nl("Warning: fmem or aux font levels didn't line up!");
		print_nl("fmem load level: ");	print_int(fmem_lastcmp);
		print(";  fmem target: ");	print_int(fmem_ptr);
		print(";aux load level: ");	print_int(font_lastcmp);
		print(";  aux target: ");	print_int(font_ptr);
	}
	return(TRUE);

STR_BAD:
	print_nl("Warning: string file damaged!");
CHECK_FAIL:
	fatal_append_diff = TRUE;
	(void) fclose(str_copy);
	return(FALSE);
FONT_BAD:
	print_nl("Warning: font file damaged!");
	goto CHECK_FAIL;
}

/*	check font aux arrays from font_sofar+1 to font_high
 */

/*static*/ mword checkbuf[FONT_MAX];	/* tmp buffer for reading in segment */

check_aux(font_high,font_sofar,datafile)
	fnt 		font_high;
	fnt		font_sofar;
	byte_file	datafile;
{
	fnt 		font_seg;

#define check_seg(SEG,ELEM,NUM) \
	if (fread(&checkbuf[0],sizeof(ELEM)*NUM,1,datafile)<1) return(FALSE); \
	if (bcmp(&checkbuf[0],&SEG[font_sofar+1],sizeof(ELEM)*NUM)!=0) \
		return(FALSE);

	font_seg = font_high - font_sofar;
	if( font_seg <=0 ) return(TRUE);
	check_seg(font_check, qqqq, font_seg);
	check_seg(font_size,  scal, font_seg);
	check_seg(font_dsize, scal, font_seg);
	check_seg(font_params,hword,font_seg);
	check_seg(font_name,  str,  font_seg);
	check_seg(font_area,  str,  font_seg);
	check_seg(font_bc,    byte, font_seg);
	check_seg(font_ec,    byte, font_seg);
/****	   font_used is saved in globals. DLP  */
/****	check_seg(font_used,  bool, font_seg); */
	check_seg(font_glue,  ptr,  font_seg);
	check_seg(hyphen_char,int,  font_seg);
	check_seg(skew_char,  int,  font_seg);
	check_seg(char_base,  ptr,  font_seg);
	check_seg(width_base, ptr,  font_seg);
	check_seg(height_base,ptr,  font_seg);
	check_seg(depth_base, ptr,  font_seg);
	check_seg(italic_base,ptr,  font_seg);
	check_seg(lig_kern_base,ptr,font_seg);
	check_seg(kern_base,  ptr,  font_seg);
	check_seg(exten_base, ptr,  font_seg);
	check_seg(param_base, ptr,  font_seg);
	return(TRUE);
}
#undef check_seg
#endif INCTEX
