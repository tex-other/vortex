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
 *  Copyright (C) 1987 by the Regents of University of California and by
 *  Pehong Chen
 *  Computer Science Division
 *  571 Evans Hall
 *  University of California
 *  Berkeley, CA 94720
 *  USA
 *  phc@berkeley.edu
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */
#ifndef lint
static char	_rcsid_[] = "$Source:$ for VorTeX Incremental Formatter, Copyright (C) Pehong Chen 1987";
static char	_rcsver_[] = "$Revision: $";
static char	_rcsdate_[] = "$Date: $";
#endif !lint

#include	<sys/types.h>
#include	"tex.h"
#include	"scan.h"
#include	"box.h"
#include	"heap.h"
#include	"tokenstack.h"
#include	"dvi.h"
#include	"tfm.h"
#include	"eq.h"

#include	"allir.h"
#include	"main.h"
#include	"msg.h"

extern struct _char	*begin_of_word_token;
extern struct _cseq	*last_cs;
struct _node		*ir_math = NIL;
struct _node		*ir_esc = NIL;

make_lig_node (p)
	ptr		p;
{
	ptr 		q;
	_Unode		*ln;

	if ((ln = (_Unode *) malloc(sizeof(_Unode))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _lig.");
		exit(-1);
	}
	
	ln->_ty = NODE_LIG;
	ln->_ch = character(lig_char(p));
	q = lig_ptr(p);
	ln->_dn = ln->_lt = ir_char(q);
	ln->_up = ir_char(q)->nd_up;
	for (q; q != NULL; q = link(q)) {
		ir_char(q)->nd_up = (_Node *) ln;
		ln->_rt = ir_char(q);
	}
	
	ir_char(lig_trick) = (_Node *) ln;
}

static
skip_in_math()
{
	while ((ir_cur->nd_char <= ' ') || (ir_cur->nd_char == '$') ||
	       (ir_cur->nd_char == '%') || (ir_cur->nd_char == '#') ||
	       (ir_cur->nd_char == '&') || (ir_cur->nd_char == '~') ||
	       (ir_cur->nd_char == '{') || (ir_cur->nd_char == '}') ||
	       (ir_cur->nd_char == '_') || (ir_cur->nd_char == '^') ||
	       (ir_cur->nd_char == '\\')) {
		if (ir_cur->nd_char == '\\') {
			if ((ir_cur->nd_rt->nd_char == ',') ||
			    (ir_cur->nd_rt->nd_char == '>') ||
			    (ir_cur->nd_rt->nd_char == ';') ||
			    (ir_cur->nd_rt->nd_char == '!'))
				ir_cur = ir_cur->nd_rt->nd_rt;
			else {	/* regular math symbol */
				ir_cur->nd_up->nd_type = NODE_SYMBOL;
				return(TRUE);
			}
		} else if (ir_cur->nd_char == '%') {
			while (ir_cur->nd_char != '\n')
				ir_cur = ir_cur->nd_rt;
		} else {
			ir_cur = ir_cur->nd_rt;
		}
		if (ir_cur == NIL) {
			msg(STDERR, "math mode is seriously wrong...");
			exit(-1);
		}
	}
	return(FALSE);
}

handle_math (p)
	ptr		p;
{
	if (ir_esc != NIL) {
		ir_char(p) = ir_esc->nd_up;
		if ((ir_esc->nd_up != NIL) &&
		    (((_Cseq *) ir_esc->nd_up)->_eon != NIL))
			ir_cur = ((_Cseq *) ir_esc->nd_up)->_eon->nd_rt;
		else
			ir_cur = NIL;
		ir_esc = NIL;
	} else if (ir_cur != NIL) 
		if (skip_in_math()) {
			ir_char(p) = (_Node *) ir_cur->nd_up;
			if ((ir_cur != NIL) &&
			    (ir_cur->nd_up != NIL) &&
			    (((_Cseq *) ir_cur->nd_up)->_eon != NIL))
				ir_cur = ((_Cseq *) ir_cur->nd_up)->_eon->nd_rt;
			else
				ir_cur = NIL;
		} else {
			ir_char(p) = ir_cur;
			ir_cur = ir_cur->nd_rt;
		}
}

link_char_node (p)
	ptr		p;
{
	/* fixup IRs+IRi vs. IRt cross references */
	if (state != TOKEN_LIST) {
		if (in_math && (ir_cur != NIL)) {
			if (ir_math != NIL) {
				ir_cur = ir_math;
				ir_math = NIL;
			}
			/* handle_math(); */
		} else {
			ir_char(p) = (_Node *) irs_ptr;
		}
		goto done;
	}

	/* process token list */
	switch (token_type) {
	case PARAMETER:		/* reading actual parameters */
		if (in_math) /* && (ir_cur != NIL)) */ {
			/* handle_math(); */
		} else {
			/*
			 * ir_cur == NIL means there is no argument to the cseq
			 * otherwise, ir_cur should point to the nect char in
			 * argument list.
			 */
			if (ir_cur == NIL) {
				ir_cur = (_Node *) irs_ptr;
			}
			/* make sure ch in mem[] is the same as that in IRs */
			while (character(p) != ir_cur->nd_char) {
				ir_cur = ir_cur->nd_rt;
			}
			ir_char(p) = ir_cur;
			ir_cur = ir_cur->nd_rt;
		}
		break;
	case U_TEMPLATE:
	case V_TEMPLATE:
		if (in_math && (ir_cur != NIL)) {
			if (ir_esc != NIL) {
				ir_char(p) = ir_esc->nd_up;
				if ((ir_esc->nd_up != NIL) &&
				    (((_Cseq *) ir_esc->nd_up)->_eon != NIL))
					ir_cur = ((_Cseq *) ir_esc->nd_up)->_eon->nd_rt;
				else
					ir_cur = NIL;
			} else if (skip_in_math()) {
				ir_char(p) = (_Node *) ir_cur->nd_up;
				if ((ir_cur->nd_up != NIL) &&
				    (((_Cseq *) ir_cur->nd_up)->_eon != NIL))
					ir_cur = ((_Cseq *) ir_cur->nd_up)->_eon->nd_rt;
				else
					ir_cur = NIL;
			} else {
				ir_char(p) = ir_cur;
				ir_cur = ir_cur->nd_rt;

				}
/*			handle_math();*/
		} else {
			ir_char(p) = (_Node *) irs_ptr;
		}
		break;
	case BACKED_UP:
		if (begin_of_word_token != NIL) {
			/* 
			 * reading from back input (lookahead tokens),
			 * irs_next is now positioned at the beginning
			 * of next token, and therefore not very useful.
			 * back_input happens in cases like the first letter
			 * of a par or first letter after some cseq with
			 * optional arguments (e.g. \hskip 1in foo)
			 */
			ir_char(p) = (_Node *) begin_of_word_token;
		} else {
			/* reading things like \TeX */
			ir_char(p) = (_Node *) last_cs;
		}
		break;
	case MACRO:
		/* reading from expanded text (macros) */
		ir_char(p) = (_Node *) last_cs;
		if (in_math)
			ir_cur = last_cs->_eon->nd_rt;
		ir_esc = NIL;
		break;
	case INSERTED:
	case OUTPUT_TEXT:
	case EVERY_PAR_TEXT:
	case EVERY_MATH_TEXT:
	case EVERY_DISPLAY_TEXT:
	case EVERY_HBOX_TEXT:
	case EVERY_VBOX_TEXT:
	case EVERY_JOB_TEXT:
	case EVERY_CR_TEXT:
	case MARK_TEXT:
	case WRITE_TEXT:
	default:
		ir_char(p) = NIL;
		break;
	}
done:
#ifdef _IRL
	if (ir_char(p) != NIL)
		fprintf(stderr,"p=%d  c=%c  ir=%x  c=%c\n", p, character(p),
			ir_char(p), ir_char(p)->nd_char);
	else
		fprintf(stderr,"\np=%d  c=%c  ir=NIL", p, character(p));
	fflush(stderr);
#endif
	return(state);
}

#endif VORTEX
