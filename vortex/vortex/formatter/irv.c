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

#ifdef VORTEX

/*
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Development Environment
 *
 *  This file is part of the VorTeX incremental formatter,
 *
 *  Copyright (C) 1987 by	Ikuo Minakata	(min@renoir.berkeley.edu)
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */
#ifndef lint
static char	_rcsid_[] = "$Source:$ for VorTeX Incremental Formatter, Copyright (C) Ikuo Minakata 1987";
static char	_rcsver_[] = "$Revision: $";
static char	_rcsdate_[] = "$Date: $";
#endif !lint

#ifdef  VIEW
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/un.h>
#include	"protocol.h"
#endif
#include	"tex.h"
#include	"texext.h"
#include	"cmds.h"
#include	"heap.h"
#include	"char.h"
#include	"str.h"
#include	"eq.h"
#include	"hash.h"
#include	"evalstack.h"
#include	"eqstack.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"box.h"
#include	"pack.h"
#include	"cond.h"
#include	"io.h"
#include	"file.h"
#include	"tfm.h"
#include	"hyph.h"
#include	"dvi.h"
#include	"fmt.h"
#include	"error.h"
#include	"print.h"
#include	"page.h"
       
#include	"allir.h"
#include	"main.h"
#include	"macro.h"
#include	"irv.h"

#ifdef VIEW
#include	"texincl.c"
#endif

FILE  *fid;

dump_hier(p)
short	p;
{
short	i;
  if (p != 0)
	  if (p > 1) 
		fid = fopen("/dev/null", "w");
	else
	  fid = stderr;
  else {
	  if ((fid = fopen("#node#","w")) <= 0) {
	    printf("can't create #node# file\n");
	    exit(1);
	}
  }
  print_hier(par_node_top);
  if (fid != stderr)
	  fclose(fid);
}

print_hier(top)
struct _node *top;
{
struct _node  *par_tmp;
struct _node  *token_tmp;
  for (par_tmp = top ; par_tmp != NIL; par_tmp = ((struct _unode *)par_tmp)->_rt) {
    print_par_info(par_tmp);
    for (token_tmp = ((struct _unode *)par_tmp)->_dn; token_tmp != NIL; token_tmp = token_tmp->nd_rt) {
      switch (token_tmp->nd_type) {
      case NODE_WORD:
	print_info_word(token_tmp, 1);
	break;
      case NODE_SPACE:
	print_info_space(token_tmp, 1);
	break;
      case NODE_CSEQ:
      case NODE_DEF:
      case NODE_FDEF:
      case NODE_FONT:
	print_info_cseq(token_tmp, 1);
	break;
      case NODE_GROUP:
	print_info_group(token_tmp, 1);
	break;
      case NODE_MATH:
	print_info_math(token_tmp, 1);
	break;
      case NODE_DISPLAY:
	print_info_display(token_tmp, 1);
	break;
      case NODE_SPECIAL:
	print_info_special(token_tmp, 1);
	break;
      case NODE_INPUT:
	print_info_input(token_tmp, 1);
	break;
      case NODE_PAR:
	print_info_par(token_tmp, 1);
	break;
      default:
	print_info_unknown(token_tmp, 1);
	break;
      }
    }
  }
}

print_info_input(first, level)
struct _node	*first;
short  level;
{
  short i;
  for (i = 0; i <level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "INPUT: ");
  print_link_info(first);
  fprintf(fid, "\t\t>>> input filename: ");
  xprint_char(((struct _input *)first)->_bon);
  putc('\n', fid);
}

print_par_info(first)
struct _node  *first;
{
  fprintf(fid, "PAR:  ");
  print_link_info(first);
  putc('\n', fid);
}

print_info_word(first, level)
struct _node  *first;
short  level;
{
short i;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "WORD: ");
  print_link_info(first);
  fprintf(fid, "\t");
  xprint_char(((struct _unode *)first)->_dn);
  putc('\n', fid);
}

print_info_space(first, level)
struct _node  *first;
short  level;
{
short  i;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "SPACE:");
  print_link_info(first);
  fprintf(fid, "\t");
  xprint_char(((struct _space *)first)->_boc);
  putc('\n', fid);
}

print_info_cseq(first, level)
struct _node  *first;
short  level;
{
short  i;
struct _node  *tmp;
char   c;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "CSEQ: ");
  print_link_info(first);
  fprintf(fid, "\t");
  xprint_char(((struct _cseq *)first)->_bon);
  fprintf(fid, "\n\t\t** cseq parameter **\n\t\t");
  if (((struct _cseq *)first)->_boc != NIL) {
    putc ('"', fid);
    for (tmp = ((struct _cseq *)first)->_boc; tmp != ((struct _cseq *)first)->_eoc; tmp = tmp->nd_rt) {
      c = ((struct _char *)tmp)->_ch;
      if (c =='\n') {
	putc('\\', fid);
	c = 'n';
      }
      putc (c, fid);
      putc ('-', fid);
    }
    c = ((struct _char *)tmp)->_ch;
    if (c =='\n') {
      putc('\\', fid);
      c = 'n';
    }
    putc (c, fid);
    putc ('"', fid);
  } else
    fprintf(fid, "\tnone");
  putc ('\n', fid);
}

print_info_par(first, level)
struct _node  *first;
short  level;
{
short  i;
struct _node  *tmp;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "PAR:");
  print_link_info(first);
  putc('\n', fid);
  for (tmp = ((struct _unode *)first)->_dn; tmp != NIL; tmp = ((struct _unode *)tmp)->_rt) {
    switch (tmp->nd_type) {
    case NODE_WORD:
      print_info_word(tmp, level + 1);
      break;
    case NODE_SPACE:
      print_info_space(tmp, level + 1);
      break;
    case NODE_CSEQ:
    case NODE_DEF:
    case NODE_FDEF:
    case NODE_FONT:
      print_info_cseq(tmp, level + 1);
      break;
    case NODE_GROUP:
      print_info_group(tmp, level + 1);
      break;
    case NODE_MATH:
      print_info_math(tmp, level + 1);
      break;
    case NODE_DISPLAY:
      print_info_display(tmp, level + 1);
      break;
    case NODE_SPECIAL:
      print_info_special(tmp, level + 1);
      break;
    case NODE_INPUT:
      print_info_input(tmp, level + 1);
      break;
    case NODE_PAR:
      print_info_par(tmp, level + 1);
      break;
    default:
      print_info_unknown(tmp, level + 1);
      break;
    }
  }
}

print_info_group(first, level)
struct _node  *first;
short  level;
{
short  i;
struct _node  *tmp;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "GROUP:");
  print_link_info(first);
  putc('\n', fid);
  for (tmp = ((struct _group *)first)->_dn; tmp != NIL; tmp = ((struct _group *)tmp)->_rt) {
    switch (tmp->nd_type) {
    case NODE_WORD:
      print_info_word(tmp, level + 1);
      break;
    case NODE_SPACE:
      print_info_space(tmp, level + 1);
      break;
    case NODE_CSEQ:
    case NODE_DEF:
    case NODE_FDEF:
    case NODE_FONT:
      print_info_cseq(tmp, level + 1);
      break;
    case NODE_GROUP:
      print_info_group(tmp, level + 1);
      break;
    case NODE_MATH:
      print_info_math(tmp, level + 1);
      break;
    case NODE_DISPLAY:
      print_info_display(tmp, level + 1);
      break;
    case NODE_SPECIAL:
      print_info_special(tmp, level + 1);
      break;
    case NODE_INPUT:
      print_info_input(tmp, level + 1);
      break;
    case NODE_PAR:
      print_info_par(tmp, level + 1);
      break;
    default:
      print_info_unknown(tmp, level + 1);
      break;
    }
  }
}

print_info_math(first, level)
struct _node  *first;
short  level;
{
short  i;
struct _node  *tmp;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "MATH: ");
  print_link_info(first);
  putc('\n', fid);
  for (tmp = ((struct _math *)first)->_dn; tmp != NIL; tmp = ((struct _math *)tmp)->_rt) {
    switch (tmp->nd_type) {
    case NODE_WORD:
      print_info_word(tmp, level + 1);
      break;
    case NODE_SPACE:
      print_info_space(tmp, level + 1);
      break;
    case NODE_CSEQ:
    case NODE_DEF:
    case NODE_FDEF:
    case NODE_FONT:
      print_info_cseq(tmp, level + 1);
      break;
    case NODE_GROUP:
      print_info_group(tmp, level + 1);
      break;
    case NODE_MATH:
      print_info_math(tmp, level + 1);
      break;
    case NODE_DISPLAY:
      print_info_display(tmp, level + 1);
      break;
    case NODE_SPECIAL:
      print_info_input(tmp, level + 1);
      break;
    case NODE_INPUT:
      print_info_input(tmp, level + 1);
      break;
    case NODE_PAR:
      print_info_par(tmp, level + 1);
      break;
    default:
      print_info_unknown(tmp, level + 1);
      break;
    }
  }
}

print_info_display(first, level)
struct _node  *first;
short  level;
{
short  i;
struct _node  *tmp;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "DISPLAY:");
  print_link_info(first);
  putc('\n', fid);
  for (tmp = ((struct _math *)first)->_dn; tmp != NIL; tmp = ((struct _math *)tmp)->_rt) {
    switch (tmp->nd_type) {
    case NODE_WORD:
      print_info_word(tmp, level + 1);
      break;
    case NODE_SPACE:
      print_info_space(tmp, level + 1);
      break;
    case NODE_CSEQ:
    case NODE_DEF:
    case NODE_FDEF:
    case NODE_FONT:
      print_info_cseq(tmp, level + 1);
      break;
    case NODE_GROUP:
      print_info_group(tmp, level + 1);
      break;
    case NODE_MATH:
      print_info_math(tmp, level + 1);
      break;
    case NODE_DISPLAY:
      print_info_display(tmp, level + 1);
      break;
    case NODE_SPECIAL:
      print_info_input(tmp, level + 1);
      break;
    case NODE_INPUT:
      print_info_input(tmp, level + 1);
      break;
    case NODE_PAR:
      print_info_par(tmp, level + 1);
      break;
    default:
      print_info_unknown(tmp, level + 1);
      break;
    }
  }
}

print_info_special(first, level)
struct _node 	*first;
short	level;
{
  short	i;
  struct _node	 *tmp;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "SPECIAL: ");
  print_link_info(first);
  putc('\n', fid);
  fprintf(fid, "\t\t** special param **\n");
  tmp = ((_Cseq *) first)->_bon;
    switch (tmp->nd_type) {
    case NODE_WORD:
      print_info_word(tmp, level + 1);
      break;
    case NODE_SPACE:
      print_info_space(tmp, level + 1);
      break;
    case NODE_CSEQ:
    case NODE_DEF:
    case NODE_FDEF:
    case NODE_FONT:
      print_info_cseq(tmp, level + 1);
      break;
    case NODE_GROUP:
      print_info_group(tmp, level + 1);
      break;
    case NODE_MATH:
      print_info_math(tmp, level + 1);
      break;
    case NODE_DISPLAY:
      print_info_display(tmp, level + 1);
      break;
    case NODE_SPECIAL:
      print_info_special(tmp, level + 1);
      break;
    case NODE_INPUT:
      print_info_input(tmp, level + 1);
      break;
    default:
      print_info_unknown(tmp, level + 1);
      break;
    }
}

print_info_unknown(first, level)
struct _node  *first;
short  level;
{
short i;
  for (i = 0; i < level; i++)
    fprintf(fid, "\t\t");
  fprintf(fid, "UNKNOWN: ");
  print_link_info(first);
  fprintf(fid, "\t");
/*  xprint_char((struct _unode *)first->_dn); */
  putc('\n', fid);
}

print_link_info(first)
struct _node  *first;
{
  putc('^', fid);
  print_type(first->nd_up);
  putc('<', fid);
  print_type(first->nd_lt);
  putc('>', fid);
  print_type(first->nd_rt);
}

print_type(xnode)
struct _node  *xnode;
{
  if (xnode != NIL) {
    switch (xnode->nd_type) {
    case NODE_WORD:
      putc('W', fid);
      break;
    case NODE_PAR:
      putc('P', fid);
      break;
    case NODE_GROUP:
      putc('G', fid);
      break;
    case NODE_CSEQ:
    case NODE_DEF:
    case NODE_FDEF:
    case NODE_FONT:
      putc('C', fid);
      break;
    case NODE_SPACE:
      putc('S', fid);
      break;
    case NODE_MATH:
      putc('M', fid);
      break;
    case NODE_DISPLAY:
      putc('D', fid);
      break;
    case NODE_SPECIAL:
      putc('X', fid);
      break;
    case NODE_INPUT:
      putc('I', fid);
      break;
    default:
      putc('?', fid);
      break;
    }
  } else
    putc('?', fid);
}

xprint_char(first)
struct _char  *first;
{
struct _char  *tmp;
struct _node   *save;
char   c;
  save = first->_up;
  putc('"', fid);
  for (tmp = first;;) {
    c = tmp->_ch;
    if (c == '\n') {
      putc('\\', fid);
      c = 'n';
    }
    putc(c, fid);
    tmp = (struct _char *)tmp->_rt;
    if (tmp == NIL || tmp->_up != save)
      break;
    putc('-', fid);
  }
  putc('"', fid);
}
    

bool	xfree[MEM_MAX];
bool	was_free[MEM_MAX];
ptr	was_mem_end, was_lo_max, was_hi_min;
	
check_mem(print_locs)
bool	print_locs;
{
	ptr	p, q;
	bool	clobbered;
	
	for (p = MEM_MIN; p <= lo_mem_max; p++)
		xfree[p] = FALSE;
	for (p = hi_mem_min; p <= mem_end; p++)
		xfree[p] = FALSE;
	
	p = avail;
	q = NULL;
	clobbered = FALSE;
	while(p != NULL) {
		if ((p > mem_end) || (p < hi_mem_min))
			clobbered = TRUE;
		else
			if (xfree[p])
				clobbered = TRUE;
		if (clobbered) {
		        print_nl("AVAIL list clobbered at ");
			print_int(q);
			goto done1;
		}
		xfree[p] = TRUE;
		q = p;
		p = link(q);
	}
done1:
	p = rover;
	q = NULL;
	clobbered = FALSE;
	do {
		if((p >= lo_mem_max) || (p < MEM_MIN))
			clobbered = TRUE;
		else
			if ((rlink(p) >= lo_mem_max) || (rlink(p) <MEM_MIN))
				clobbered = TRUE;
			else
				if(!((is_empty(p)) || (node_size(p) < 2) ||
				  (p + node_size(p) > lo_mem_max) ||
				  (llink(rlink(p)) != p)))
			    		clobbered = TRUE;
		if (clobbered) {
			print_nl("Double-AVAIL list clobbered at ");
			print_int(q);
			goto done2;
		}
		for (q = p; q <= p + node_size(p) -1; q++) {
			if (xfree[q]) {
				print_nl("Doubly free location at ");
				print_int(q);
				goto done2;
			}
			xfree[q] = TRUE;
		}
		q = p;
		p = rlink(p);
	} while(p != rover);
done2:
	
	p = MEM_MIN;
	while(p <= lo_mem_max) {
		if(is_empty(p)) {
			print_nl("Bad flag at ");
			print_int(p);
		}
		while((p <= lo_mem_max) && (!xfree[p]))
			incr(p);
		while((p <= lo_mem_max) && (xfree[p]))
			incr(p);
	}
	
	if (print_locs) {
		print_nl("New busy locs:");
		for (p = MEM_MIN; p <= lo_mem_max; p++)
			if ((!xfree[p]) && ((p > was_lo_max) || (was_free[p]))) {
				print_char(" ");
				print_int(p);
			}
		for (p = hi_mem_min; p <= mem_end; p++)
			if ((!xfree[p]) && ((p < was_hi_min) || (p > was_mem_end) || (was_free[p]))) {
				print_char(" ");
				print_int(p);
			}
	}
	
	for (p = MEM_MIN; p <= lo_mem_max; p++)
		was_free[p] = xfree[p];
	for (p = hi_mem_min; p <= mem_end; p++)
		was_free[p] = xfree[p];
	was_mem_end = mem_end;
	was_lo_max = lo_mem_max;
	was_hi_min = hi_mem_min;
}
			
s_mem()
{
	FILE	*mid;
	ptr	p, q;
	int	s;
	
	p = avail;
	while(p != NIL) {
		info(p) = NULL;
		p = link(p);
	}
	p = rover;
	do {
		s = node_size(p);
		for (q = p + 2; q < p + s; q++)
			mem[q].i = NULL;
		p = rlink(p);
	} while(p != rover);
	mid = fopen("mem", "w");
	fwrite(&mem[0], sizeof(mword)*MEM_MAX, 1, mid);
	fclose(mid);
}
#endif
