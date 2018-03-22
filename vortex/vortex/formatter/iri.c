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


#include	"tex.h"
#include	"cmds.h"
#include	"heap.h"
#include	"arith.h"
#include	"char.h"
#include	"tfm.h"
#include	"eq.h"
#include	"eqstack.h"
#include	"hash.h"
#include	"token.h"
#include	"scan.h"
#include	"tokenstack.h"
#include	"evalstack.h"
#include	"box.h"
#include	"boxlists.h"
#include	"math.h"
#include	"mathlists.h"
#include	"cond.h"
#include	"def.h"
#include	"dvi.h"
#include	"pack.h"
#include	"page.h"
#include	"par.h"
#include	"print.h"
#include	"error.h"
#include	"eval.h"

#include	"allir.h"
#include	"main.h"
#include	"msg.h"
#include	"macro.h"
#include	"texext.h"
#include	"var.h"

extern	_Node	*get_rightmost_char();

/*
 *  make space node 
 */
make_space_node (first, last)
_Char	*first, *last;
{
	_Space	*new_space_node;
	_Char	*tmp;

	if (warming || irs_read_only)
		return;
	free_iri(first);	/* free discarded IRi's */

	if ((new_space_node = (_Space *) malloc(sizeof(_Space))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _space.");
		exit(-1);
	}

	new_space_node->_ty = NODE_SPACE;
	new_space_node->_boc = (_Node *) first;
	new_space_node->_eoc = (_Node *) last;

	iri_hier(new_space_node); /* setup IRi relations */

	token_node_last = (_Node *) new_space_node;
	if (first != last) {
		for(tmp = first; tmp != last; tmp = (_Char *) (tmp->_rt))
			tmp->_up = (_Node *) new_space_node;
		last->_up = (_Node *) new_space_node;
	} else 
		first->_up = (_Node *) new_space_node;
}

/*
 *  make word node 
 */
make_word_node (first, last)
_Char	*first, *last;
{
	_Unode	*new_word_node;
	_Char	*tmp;

	if (warming || irs_read_only)
		return(NIL);

	free_iri(first);	/* free discarded IRi's */
	if ((new_word_node = (_Unode *) malloc(sizeof(_Unode))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _unode.");
		exit(-1);
	}

	new_word_node->_ty = NODE_WORD;
	new_word_node->_dn = (_Node *) first;
	new_word_node->_re = NIL;

	iri_hier(new_word_node);

	token_node_last = (_Node *) new_word_node;
	if (first != last) {
		for(tmp = first; tmp != last; tmp = (_Char *) tmp->_rt)
			tmp->_up = (_Node *) new_word_node;
		last->_up = (_Node *) new_word_node;
	} else 
		first->_up = (_Node *) new_word_node;
}

/*
 *     make cs  node entry in IRi   
 */

_Cseq *
make_cs_node(first, last)
_Char		*first, *last;
{
	_Cseq		*new_cs_node;
	_Char		*tmp;

	if (irs_read_only)		/* if read from read-only IRs */
		return(NIL);

	free_iri(first);		/* free discarded IRi's */
	if ((new_cs_node = (_Cseq *)malloc(sizeof(_Cseq))) == NIL) {
		msg(STDERR, "Not enough core to malloc to struct _cseq");
		exit(-1);
	}
	new_cs_node->_ty = NODE_CSEQ;
	new_cs_node->_bon = (_Node *) first;
	new_cs_node->_eon = (_Node *) last;
	new_cs_node->_boc = NIL;
	new_cs_node->_eoc = NIL;
	new_cs_node->_def = NIL;

	iri_hier(new_cs_node);

	token_node_last = (_Node *) new_cs_node;
	for (tmp = first; tmp != last; tmp = (_Char *) tmp->_rt)
		tmp->_up = (_Node *) new_cs_node;
	last->_up = (_Node *) new_cs_node;
	return(new_cs_node);
}

make_group_node (first)
_Char	*first;
{
	_Group	*new_group_node;

	if (irs_read_only)
		return;

	free_iri(first);

	if ((new_group_node = (_Group *) malloc(sizeof(_Group))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _group.");
		exit(-1);
	}

	new_group_node->_ty = NODE_GROUP;

	iri_hier(new_group_node);

	cur_group_node = new_group_node;
	token_node_last = NIL;
	par_node_last = NIL;
	make_par_node();
	new_group_node->_dn = (_Node *) par_node_last;
	if (first->_up == NIL) {
		make_word_node(first, first);
		begin_of_word_token = 0;
	}
}

close_group_node (first)
_Char	*first;
{
	if(irs_read_only)
		return;
	if (cur_group_node == NIL) {
		fprintf(stderr, "unmatch '}'!\n");
		return;
	}
	make_word_node(first, first);
	close_gmd_common();
}

/*
 *         make math node            
 */
_Math *
make_math_node (first)
_Char		*first;
{
	_Math		*new_math_node;

	if (irs_read_only)		/* if read from other than IRs */
		return(NIL);

	free_iri(first);		/* free discarded IRi's */
	if ((new_math_node = (_Math *) malloc(sizeof(_Math))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _math.");
		exit(-1);
	}
	new_math_node->_ty = NODE_MATH;
	new_math_node->_dn = NIL;

	iri_hier(new_math_node);

	cur_group_node = (_Group *) new_math_node;
	token_node_last = NIL;
	par_node_last = NIL;
	make_par_node();
	new_math_node->_dn = (_Node *) par_node_last;

	/* create '$' word node, and link as 1st math group */
	make_word_node(first, first);
	begin_of_word_token = 0;

	/* ir_math is set to $ by make_word_node, get rid of $ here */
	ir_math = first->_rt;

	return (new_math_node);
}

/*
 *        make display node             
 */
make_display_node(math_node)
	_Math		*math_node;
{
	if (irs_read_only)
		return;
	math_node->_ty = NODE_DISPLAY;

	ir_math = ir_math->nd_rt;
}

/*
 *         close math node              
 */
close_math_node()
{
	_Char		*char_ptr;
	ptr	p;

	if (math_que_top == NIL) {
		msg(STDERR, "something strange in math nest(close)!!");
		return;
	}
	p = math_que_top;
	char_ptr = (_Char *)math_char_field(p);
	math_que_top = math_rlink(p);
	if (math_que_top != NIL)
		math_llink(math_que_top) = NIL;
	free_node(p, MATH_QUE_SIZE);

	if (char_ptr != NIL) {
		make_word_node(char_ptr, char_ptr); /* make ending '$' node */
		close_gmd_common();
	}
}

/*
 *        close display node            
 */
close_display_node()
{
	_Char		*char_ptr, *char_ptr1;
	ptr	p;

	if (math_que_top == NIL) {
		msg(STDERR, "something strange in display nest(close)!!");
		return;
	}
	p = math_que_top;
	char_ptr = (_Char *)math_char_field(p);
	math_que_top = math_rlink(p);
	if (math_que_top != NIL)
		math_llink(math_que_top) = NIL;
	free_node(p, MATH_QUE_SIZE);
	if (char_ptr != NIL) {
		make_word_node(char_ptr, char_ptr); /* make ending '$' node */
		if (math_que_top != NIL) {
			p = math_que_top;
			char_ptr1 = (_Char *)math_char_field(p);
			math_que_top = math_rlink(p);
			if (math_que_top != NIL)
				math_llink(math_que_top) = NIL;
			free_node(p, MATH_QUE_SIZE);
			if (char_ptr1 != NIL)
				make_word_node(char_ptr1, char_ptr1);
		}
		close_gmd_common();
	}
}

close_gmd_common()
{
	_Node	*tmp;
	_Node	*bg;
	
	begin_of_word_token = 0;
	token_node_last = (_Node *) cur_group_node;
		/* if no paragraph in current group */
/*
	if (cur_group_node->_ty == NODE_GROUP)
		bg = cur_group_node->_dn;
	else
		bg = ((_Math *)cur_group_node)->_dn;
	if (bg == (_Node *) par_node_last) {
		tmp = par_node_last->_dn;
		if (cur_group_node->_ty == NODE_GROUP)
			cur_group_node->_dn = tmp;
		else
			((_Math *)cur_group_node)->_dn = tmp;
		while(tmp != NIL) {
			tmp->nd_up = (_Node *) cur_group_node;
			tmp = tmp->nd_rt;
		}
		free(par_node_last);
	}
*/
	par_node_last = (_Unode *) cur_group_node->_up;
	cur_group_node = (_Group *)par_node_last->_up;
}

/*
 *    make par node entry in IRi
 */

make_par_node()
{
	_Unode		*new_par_node;

	if (irs_read_only)		/* reading from read-only IRs */
		return;
	if ((new_par_node = (_Unode *) malloc(sizeof(_Unode))) == NIL) {
		msg(STDERR, "Not enough core to malloc to struct _unode");
		exit(-1);
	}
	new_par_node->_ty = NODE_PAR;
	new_par_node->_up = (_Node *) cur_group_node;
	new_par_node->_dn = NIL;
	new_par_node->_re = NIL;
	if (par_node_last == NIL) {
		new_par_node->_lt = NIL;
	} else {
		new_par_node->_lt = (_Node *) par_node_last;
		par_node_last->_rt = (_Node *) new_par_node;
	}
	new_par_node->_rt = NIL;
/*
	if (first != NIL)
		first->_up = (_Node *) par_node_last;
*/
	par_node_last = new_par_node;
	token_node_last = NIL;
}

/*
 * setup IRs hierachy relation
 */
iri_hier(new_node)
_Node	*new_node;
{
	if (token_node_last == 0) {
		new_node->nd_lt = NIL;
		par_node_last->_dn = new_node;
	} else {
		new_node->nd_lt = (_Node *) token_node_last;
		token_node_last->nd_rt = new_node;
	}
	new_node->nd_up = (_Node *) par_node_last;
	new_node->nd_rt = NIL;
}
	
set_cs_param_field (cs_node, cnt)
_Cseq	*cs_node;	/* cseq node */
short		cnt;		/* token number oparam */
{
	_Char	*char_ptr;
	short		i;
      
	if (cnt <= 0)		/* no param */
		return;
	
	/* find cseq string right edge (begin of param */
	char_ptr = (_Char *) (cs_node->_bon);
	while (char_ptr->_up == (_Node *) cs_node)
		char_ptr = (_Char *) (char_ptr->_rt);

	/* if cseq is follwed by space(s) */
	if (char_ptr->_up != NIL && char_ptr->_up->nd_type == NODE_SPACE) {
		do {
			char_ptr = (_Char *) (char_ptr->_rt);
		} while ((char_ptr->_up != NIL) &&
			 (char_ptr->_up->nd_type == NODE_SPACE));
	}

	/* next non-blank char is begin of param */
	cs_node->_boc = (_Node *) char_ptr;  /* set begin of param */

	/* find right edge of 1st token */
	char_ptr = (_Char *) get_rightmost_char(char_ptr);
	for (i = 0; i < cnt - 1 ; i++) { /* count for 'count'-1 tokens */
		/* left edge of next token */
		char_ptr = (_Char *) (char_ptr->_rt);
		/* get right edge */
		char_ptr = (_Char *) get_rightmost_char(char_ptr);
	}
	cs_node->_eoc = (_Node *) char_ptr;	/* set end of param */
}

/*
 * release IRs nodes of a paragraph
 */
_Node	*biggest_subtree();

free_iri(first)
_Char 	*first;
{
	_Node	*tmp, *tmp1, *tmp2, *tmp_up, *tmp_lt, *tmp_x, *tmp_par, *free_beg;
	char	n_ty;
	int	i;
	
	if(first->_up == NIL)
		return;

#ifdef _IRI
	fprintf(stderr, "first free char: %c\n", first->_ch);
	fflush(stderr);
	fprintf(stderr, "first 30 char:\n");
	for (i = 0, tmp = (_Node *)first; i < 30; i++) {
		fprintf(stderr, "char: %c uplink: %x\n", tmp->nd_char, tmp->nd_up);
		fflush(stderr);
		tmp = tmp->nd_rt;
	}
#endif	
	tmp = tmp_par = free_beg = biggest_subtree(first);
	
	while(tmp != NIL) {
		tmp_x = tmp->nd_up;
		if (tmp_par != NIL) {
			n_ty = tmp_par->nd_type;
			tmp_up = tmp_par->nd_up;
			tmp_lt = tmp_par->nd_lt;
			for(tmp1 = tmp_par; tmp1 != NIL;) {
				tmp2 = tmp1->nd_rt;
				free_inode(tmp1, 1);
				tmp1 = tmp2;
			}
			
			if (n_ty == NODE_PAR && 
			    tmp_up == NIL && tmp_par == free_beg) {
				if (tmp_par == (_Node *)par_node_top) {
					par_node_last = NIL;
					make_par_node();
					par_node_top = par_node_last;
				}
			} else 	if (tmp_lt != NIL) 
					tmp_lt->nd_rt = NIL;
		}
		tmp = tmp_x;
		if (tmp != NIL)
			tmp_par = tmp->nd_rt;
	}
#ifdef _IRI
	tmp = (_Node *)first;
	do {
		if (tmp->nd_up != NIL) {
			fprintf(stderr, "uplink char: %c  addr: %x\n", tmp->nd_char, tmp);
			fflush(stderr);
		}
		tmp = tmp->nd_rt;
	} while(tmp != (_Node *)first);
#endif
}

_Node	*
biggest_subtree(first)
_Char	*first;
{
	_Node	*tmp, *tmp_par, *bg;
	
	tmp = first->_up;
	while((tmp_par = tmp->nd_up) != NIL) {
		switch(tmp_par->nd_type) {
			case NODE_GROUP:
				bg = ((_Group *)tmp_par)->_dn;
				break;
			case NODE_MATH:
			case NODE_DISPLAY:
				bg = ((_Math *)tmp_par)->_dn;
				break;
			case NODE_PAR:
				bg = ((_Unode *)tmp_par)->_dn;
				break;
			default:
				fprintf(stderr, "IRi hierarchy clobbered\n");
				break;
			}
		if (bg != tmp)
			break;
		tmp = tmp_par;
	}
	return(tmp);
}


free_inode(xnode, lv)
_Node 	*xnode;
short	lv;
{
	_Node 	*tmp, *p;
	_Node	*up, *bg;

	switch(xnode->nd_type) {
		case NODE_CHAR:
			break;
		case NODE_SYMBOL:
#ifdef DEBUG
			fprintf(stderr, "%d SYMBOL\t", lv); /* DEBUG */
			goto common_unode; /* DEBUG */
#endif
		case NODE_RULE:
#ifdef DEBUG
			fprintf(stderr, "%d RULE\t", lv); /* DEBUG */
			goto common_unode; /* DEBUG */
#endif
		case NODE_WORD:
#ifdef DEBUG
			fprintf(stderr, "%d WORD\t", lv); /* DEBUG */
			goto common_unode; /* DEBUG */
#endif
		case NODE_DEF:
#ifdef DEBUG
			fprintf(stderr, "%d DEF\t", lv);/* DEBUG */
			goto common_unode;/* DEBUG */
#endif
		case NODE_FDEF:		
#ifdef DEBUG
			fprintf(stderr, "%d FDEF\t",lv); /* DEBUG */
			goto common_unode; /* DEBUG */
#endif
		case NODE_CSEQ:
#ifdef DEBUG
			fprintf(stderr, "%d CSEQ\t", lv); /* DEBUG */
			goto common_unode; /* DEBUG */
#endif
		case NODE_FONT:
#ifdef DEBUG
			fprintf(stderr, "%d FONT\t", lv); /* DEBUG */
			goto common_unode;/* DEBUG */
#endif
		case NODE_SPECIAL:
#ifdef DEBUG
			fprintf(stderr, "%d SPECIAL\t", lv); /* DEBUG */
common_unode:					/* DEBUG */
#endif
			bg = ((_Unode *)xnode)->_dn;
			up = bg->nd_up;
			for (p = bg;; p = p->nd_rt) {
#ifdef DEBUG
				if (p->nd_type == NODE_CHAR) /* DEBUG */
					fprintf(stderr, "%c", p->nd_char); /* DEBUG */
#endif
				if (p->nd_char == EOF)
					break;
				if (p->nd_up && p->nd_up != up)
					break;
				p->nd_up = NIL;
			}
#ifdef DEBUG
			fprintf(stderr, "\n"); /* DEBUG */
			fflush(stderr);	/* DEBUG */
#endif
			free((char *)xnode);
			break;
		case NODE_INPUT:
#ifdef DEBUG
			fprintf(stderr, "%d INPUT\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
#endif
			for (tmp = ((_Input *)xnode)->_dn; tmp != NIL; tmp = tmp->nd_rt)
				free_inode(tmp, lv + 1);
			bg = ((_Input *)xnode)->_bon; /* IRs of "\input" */
			up = bg->nd_up;
			for (p = bg;; p = p->nd_rt) {
				if (p->nd_up && p->nd_up != up)
					break;
				p->nd_up = NIL;
			}
			free((char *)xnode);
			break;
		case NODE_SPACE:
#ifdef DEBUG
			fprintf(stderr, "%d SPACE\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
#endif
			bg = ((_Space *)xnode)->_boc;
			if (bg) {
				up = bg->nd_up;
				for (p = bg;; p = p->nd_rt) {
					if (p->nd_up && p->nd_up != up)
						break;
					p->nd_up = NIL;
				}
			}
			free((char *)xnode);
			break;
		case NODE_GROUP:
#ifdef DEBUG
			fprintf(stderr, "%d GROUP\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
#endif
			for (tmp = ((_Group *)xnode)->_dn; tmp != NIL; tmp = tmp->nd_rt)
				free_inode(tmp, lv +1);
			free((char *)xnode);
			break;
		case NODE_MATH:
#ifdef DEBUG
			fprintf(stderr, "%d MATH\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
			goto common_math; /* DEBUG */
#endif
		case NODE_DISPLAY:
#ifdef DEBUG
			fprintf(stderr, "%d DISPLAY\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
common_math:			/* DEBUG */
#endif
			for (tmp = ((_Math *)xnode)->_dn; tmp != NIL; tmp = tmp->nd_rt)
				free_inode(tmp, lv + 1);
			free((char *)xnode);
			break;
		case NODE_PAR:
#ifdef DEBUG
			fprintf(stderr, "%d PAR\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
#endif
			for (tmp = ((_Unode *)xnode)->_dn; tmp != NIL; tmp = tmp->nd_rt)
				free_inode(tmp, lv + 1);
			free((char *)xnode);
			break;
		default:
#ifdef DEBUG
			fprintf(stderr, "%d WHAT???\n", lv); /* DEBUG */
			fflush(stderr);	/* DEBUG */
#endif
			confusion("free IRi node");
		}
}


_Node *
get_rightmost_char(char_node)
_Node   *char_node;
{
  _Node  *tmp, *parent;
  _Char	*tmp1, *tmp2;
  short		no_bskip, cat;

  if (cat_code(((_Char *)char_node)->_ch) == COMMENT) { /* if this node is comment */
    parent = char_node->nd_up;	/* skip over this node */
    tmp = char_node;
    while (parent == tmp->nd_up)
      tmp = tmp->nd_rt;
    char_node = tmp;
  }

  if (char_node->nd_up == NIL)
    return(char_node);
  switch (char_node->nd_up->nd_type) {
  case NODE_WORD:
    tmp1 = (_Char *)char_node;
    if ((tmp2 = (_Char *)tmp1->_rt) != NIL) {
      if (cat_code(tmp1->_ch) == SUP_MARK && cat_code(tmp2->_ch) == SUP_MARK) {
	if ((tmp = (_Node *)tmp2->_rt) != NIL)
	  return (tmp);
      }
    }
    return(char_node);
    break;
  case NODE_SPACE:
    return(((_Space *)char_node->nd_up)->_eoc);
    break;
  case NODE_CSEQ:
  case NODE_DEF:
  case NODE_FDEF:
  case NODE_FONT:
  case NODE_SYMBOL:
  case NODE_RULE:
    tmp = char_node;
    while (tmp->nd_up == char_node->nd_up)
      if (tmp->nd_rt == NIL)
	return(tmp);
    else
      tmp = tmp->nd_rt;
    no_bskip = FALSE;
    tmp1 = (_Char *)char_node->nd_rt;
    if (tmp1->_ch == defed_char) /* if being catcode-ed */
      cat = catcode; /* use previous catcode */
    else
      cat = cat_code(tmp1->_ch);
    /* if '\' followed by other than latter or space, trailing spaces */
    /* should be counted as token */    if (cat != LETTER && cat != SPACER)
      no_bskip = TRUE;
    if (tmp->nd_up != NIL && no_bskip == FALSE) /* skip trailing spaces */
      while (tmp->nd_up != NIL && tmp->nd_up->nd_type == NODE_SPACE)
	if(tmp->nd_rt == NIL)
	  return (tmp);
      else
	tmp = tmp->nd_rt;
    return(tmp->nd_lt);
    break;
  default:
    fprintf(stderr, ">>>something strange in macro processing<<<\n");
    return(char_node);
    break;
  }
}

set_param_extent(cs_node)
_Cseq  *cs_node;
{
	_Node	*tmp;
	_Char	*char_ptr;

	if (cs_node->_boc == NIL) {
		char_ptr = (_Char *)cs_node->_bon;
				/* find right edge of cseq */
		while (char_ptr->_up == (_Node *)cs_node) {
			if (char_ptr->_rt == NIL) break;
				char_ptr = (_Char *)char_ptr->_rt;
		}
				/* if cseq is followed by space(s) */
		if (char_ptr->_up != NIL && char_ptr->_up->nd_type == NODE_SPACE) {
			do {
				char_ptr = (_Char*)char_ptr->_rt;
			} while (char_ptr->_up != NIL 
				&& char_ptr->_up->nd_type == NODE_SPACE);
		}
	cs_node->_boc = (_Node *)char_ptr;
	cs_node->_eoc = get_rightmost_char(char_ptr);
	return;
	}
	char_ptr = (_Char *)cs_node->_boc;
	if (char_ptr->_rt != NIL) {
		while (char_ptr->_up != NIL 
			&& char_ptr->_up->nd_type == NODE_SPACE) {
			char_ptr = (_Char *)char_ptr->_rt;
			if (char_ptr->_rt == NIL)
				break;
		}
	cs_node->_boc = (_Node *)char_ptr;  
	}
	tmp = ((_Char *)cs_node->_eoc)->_rt;
	while (((_Char *)tmp)->_up != NIL 
		&& (((_Char *)tmp)->_up)->nd_type == NODE_SPACE )
		tmp = tmp->nd_rt;
	cs_node->_eoc = get_rightmost_char(tmp);
}

#ifdef  DEBUG
print_cs_name(cs)
_Cseq *cs;
{
  _Node  *tmp;

  fprintf(stderr, "\tcs_name: ");
  putc ('"', stderr);
  tmp = cs->_bon;
  while (((_Char *)tmp)->_up == (_Node *)cs) {
    putc(((_Char *)tmp)->_ch, stderr);
    tmp = ((_Char *)tmp)->_rt;
    if (tmp == NIL)
      break;
  };
  putc ('"', stderr);
  putc ('\t', stderr);
}

print_extent(cs)
_Cseq *cs;
{
  _Node  *tmp;

  tmp = cs->_boc;
  while (tmp != cs->_eoc) {
    putc(((_Char *)tmp)->_ch, stderr);
    tmp  = ((_Char *)tmp)->_rt;
  }
  putc(((_Char *)tmp)->_ch, stderr);
  putc('\n', stderr);
}
#endif

#endif VORTEX
