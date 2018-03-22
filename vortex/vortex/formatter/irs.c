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

#ifdef VORTEX

#include "tex.h"
#include "cmds.h"
#include "heap.h"
#include "eq.h"
#include "hash.h"
#include "scan.h"
#include "io.h"
#include "char.h"
#include "box.h"
#include "cond.h"
#include "print.h"
#include "error.h"
#include "expand.h"
#include "align.h"
#include "tokenstack.h"
#include "token.h"

#include    	<stdio.h>
#include    	"allir.h"
#include    	"main.h"
#include    	"macro.h"
#include    	"var.h"
#include	"msg.h"

extern		make_space_node();
extern		make_word_node();
struct _cseq	*make_cs_node();
struct _char	*get_irs_ptr();
struct _char	*get_next_irs();
struct _char	*get_prev_irs();


/*
 *  get IRs pointer from buffer pointer
 */
struct _char *
get_irs_ptr (n)
	int		n;
{
	struct _char	*xptr;
	int		i;
/*
	for(xptr = irs_eol, i = 0; i < limit - n; i++)
		xptr = get_prev_irs(xptr);
*/
	for(xptr = irs_bol, i = start; i < n ; i++)
		xptr = get_next_irs(xptr);
	return(xptr);
}

/*
 *  get next IRs node
 */
struct _char *
get_next_irs(sptr)
	struct _char	*sptr;
{
	struct _char	*xptr;
	char		c;

	xptr = (struct _char *)sptr->_rt;
	c = sptr->_ch;
	if (cat_code(c) == SUP_MARK && c == xptr->_ch) 
		 /* "^^?"  */
		xptr = (struct _char *) ((struct _char *) xptr->_rt)->_rt;
	return(xptr);
}

/*
 *  get previous IRs node
 */
struct _char *
get_prev_irs(sptr)
	struct _char	*sptr;
{
	struct _char	*xptr, *yptr, *zptr;

	xptr = (struct _char *) sptr->_lt;
	yptr = (struct _char *) xptr->_lt;
	if (yptr!= NIL)
		zptr = (struct _char *) yptr->_lt;
	else
		zptr = NIL;
	if (zptr != NIL &&
	    (cat_code(zptr->_ch) == SUP_MARK &&
	    yptr->_ch == zptr->_ch))
		return(zptr);
	else
		return(xptr);
}

/*
 *     enque unprocessed cs node
 */
cs_node_enque(cs_node)
	struct _cseq		*cs_node;
{
	ptr	p;
      
	if (cs_node == NIL)		/* maybe reading from read only IRs */
		return;

	p = get_node(CS_QUE_SIZE);
	cs_llink(p) = cs_que_end;
	cs_rlink(p) = NIL;
	type(p) = CS_QUE_NODE;
	subtype(p) = NIL;
	if (cs_que_end != NIL && cs_que_top != NIL)
		cs_rlink(cs_que_end) = p;
	cs_node_field(p) = (int)cs_node;
	cs_que_end = p;
	if (cs_que_top == NIL)
		cs_que_top = cs_que_end;
}

/*
 *    enque unprocessed math node
 */

math_enque(math_char)
	struct _char		*math_char;
{
	ptr	p;

	p = get_node(MATH_QUE_SIZE);
	math_llink(p) = math_que_end;
	math_rlink(p) = NIL;
	type(p) = MATH_QUE_NODE;
	subtype(p) = NIL;
	if (math_que_end != NIL && math_que_top != NIL)
		math_rlink(math_que_end) = p;
	math_char_field(p) = (int)math_char;
	math_que_end = p;
	if (math_que_top == NIL)
		math_que_top = math_que_end;
}
	

#endif

