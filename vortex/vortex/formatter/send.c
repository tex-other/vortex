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

#include	<stdio.h>
#include	<sys/types.h>
#include	"tex.h"
#include	"dvi.h"
#include	"tfm.h"
#include	"str.h"
#include	"eq.h"

#include	"allir.h"
#include	"tp_comm.h"
#include	"comm.h"
#include	"main.h"
#include	"msg.h"

/* Send a terminal char box */
#define	SEND_TBOX { \
	SEND_C(TP_CHAR); \
	SEND_L(((_Cbox *) bp)->_id); \
	SEND_L(((_Cbox *) bp)->_ch); \
	SEND_L(((_Cbox *) bp)->_ft); \
	SEND_L(((_Cbox *) bp)->_xb); \
	SEND_L(((_Cbox *) bp)->_yb); \
}

/* Send a nonterminal box or a terminal rule box */
#define	SEND_NBOX(N) { \
	SEND_C(N); \
	SEND_L(((_Ubox *) bp)->_id); \
	SEND_L(((_Ubox *) bp)->_xc); \
	SEND_L(((_Ubox *) bp)->_yc); \
	SEND_L(((_Ubox *) bp)->_wd); \
	SEND_L(((_Ubox *) bp)->_ht); \
}

#define move_terminal() { \
	if (bp->_rb != NIL) \
		bp = bp->_rb; \
	else if (bp->_wb->_rb != NIL) \
		bp = bp->_wb->_rb; \
	else if (bp->_qb->_rb != NIL) \
		bp = bp->_qb->_rb; \
	else { \
		SEND_C(TP_EOP); \
		return(cnt); \
	} \
}

send_page (bstr)
	char		*bstr;
{
	long		*lp;
	short		*sp;
	_Node		*bp;
	int		i;
	int		j;
	int		cnt = 0;
	int		f;
	char		*cp;
#ifdef __SEND
	_Char		*np;
#endif

	/* global magnification */
	SEND_L(mag);

	/* 10 count registers */
	for (i = 0; i < 10; i++) {
		SEND_L(pbox_curr->_ct[i]);
	}

	/* send font info */
	SEND_S(pbox_curr->_tf);

	for (i = 0; i < pbox_curr->_tf; i++) {
		f = pbox_curr->_ft[i];
		SEND_L(font_size[f]);
		SEND_S(f);
#ifdef _SEND
	fprintf(stderr, "\n<Font #%d: %d>", i+1, f);
	fflush(stderr);
#endif
		SEND_S((length(font_name[f])));
		for (j = str_start[font_name[f]]; j < str_start[font_name[f] + 1]; j++) {
			SEND_C(str_pool[j]);
		}
	}

#ifdef _SEND
	fprintf(stderr, "\n<Page %d>", pbox_curr->_no);
	fflush(stderr);
#endif
	bp = pbox_curr->_dn;
	while (bp != NIL) {
		switch (bp->nd_type) {
		case BOX_PAGE:
			msg(STDERR, "Shouldn't have a page box here.");
			return(cnt);

		case BOX_PAR:
#ifdef __SEND
			if ((((_Ubox *) bp)->_re != NIL) &&
			    (((_Ubox *) bp)->_re->nd_type == NODE_PAR)) {
				fprintf(stderr, "\n<Q>\n");
				fflush(stderr);
			} else {
				fprintf(stderr, "\n<Q:err>\n");
				fflush(stderr);
			}
#endif
			SEND_NBOX(TP_PAR);
			bp = ((_Ubox *) bp)->_dn;
#ifdef _SEND
			fprintf(stderr, "\n\n");
			fflush(stderr);
#endif
			break;

		case BOX_WORD:
#ifdef __SEND
			if ((((_Ubox *) bp)->_re != NIL) &&
			    (((_Ubox *) bp)->_re->nd_type == NODE_WORD)) {
				fprintf(stderr, " <W>");
				fflush(stderr);
			} else {
				fprintf(stderr, " <W:err>");
				fflush(stderr);
			}
#endif
			SEND_NBOX(TP_WORD);
			bp = ((_Ubox *) bp)->_dn;
#ifdef _SEND
			fprintf(stderr, " ");
			fflush(stderr);
#endif
			break;

		case BOX_CHAR:
		case BOX_HYPH:
		case BOX_LIG:
		case BOX_EXP:
		case BOX_MACRO:
#ifdef __SEND
if (((_Cbox *) bp)->_re == NIL)
	fprintf(stderr, "<N:%c>", bp->nd_char);
else
	switch (bp->nd_type) {
	case BOX_EXP:
		if (((_Cbox *) bp)->_re->nd_type == NODE_CSEQ)
			fprintf(stderr, "<E:%c>", ((_Cbox *) bp)->_re->nd_char);
		else
			fprintf(stderr, "<E:err>");
		break;
	case BOX_CHAR:
		if (((_Cbox *) bp)->_re->nd_type == NODE_CHAR)
			fprintf(stderr, "<C:%c>", ((_Cbox *) bp)->_re->nd_char);
		else
			fprintf(stderr, "<C:err:%c>", bp->nd_char);
		break;
	case BOX_LIG:
		if (((_Cbox *) bp)->_re->nd_type == NODE_LIG) {
			fprintf(stderr, "<L:");
			for (np = (_Char *) ((_Cbox *) bp)->_re->nd_lt;
			     np != (_Char *) ((_Cbox *) bp)->_re->nd_rt;
			     np = (_Char *) np->_rt)
				fprintf(stderr, "%c", np->_ch);
			fprintf(stderr, "%c", np->_ch);
			fprintf(stderr, ">");
		} else
			fprintf(stderr, "<L:err>");
		break;
	case BOX_MACRO:
		if (((_Cbox *) bp)->_re->nd_type == NODE_CSEQ) {
			fprintf(stderr, "<M");
			for (np = (_Char *) ((_Cseq *) ((_Cbox *) bp)->_re)->_bon;
			     np != (_Char *) ((_Cseq *) ((_Cbox *) bp)->_re)->_eon;
			     np = (_Char *) np->_rt)
				fprintf(stderr, "%c", np->_ch);
			fprintf(stderr, "%c", np->_ch);
			fprintf(stderr, ">");
		} else
			fprintf(stderr, "<M:err>");
		break;
	default:
		fprintf(stderr, "<U:err:%d>", ((_Cbox *) bp)->_re->nd_type);
		break;
	}
fflush(stderr);

#endif
			SEND_TBOX;
#ifdef _SEND
			fprintf(stderr, "%c", bp->nd_char);
			fflush(stderr);
#endif
			move_terminal();
			break;

		case BOX_RULE:
#ifdef __SEND
if (((_Cbox *) bp)->_re->nd_type == NODE_RULE) {
	fprintf(stderr, "<R:");
	for (np = (_Char *) ((_Cseq *) ((_Rbox *) bp)->_re)->_bon;
	     np != (_Char *) ((_Cseq *) ((_Rbox *) bp)->_re)->_eon;
	     np = (_Char *) np->_rt)
		fprintf(stderr, "%c", np->_ch);
	fprintf(stderr, "%c", np->_ch);
	fprintf(stderr, ">");
} else
	fprintf(stderr, "<R:err>");
fflush(stderr);
#endif
			SEND_C(TP_RULE);
			SEND_L(((_Rbox *) bp)->_id);
			SEND_L(((_Rbox *) bp)->_xc);
			SEND_L(((_Rbox *) bp)->_yc);
			SEND_L(((_Rbox *) bp)->_wd);
			SEND_L(((_Rbox *) bp)->_ht);
#ifdef _SEND
			fprintf(stderr, "<R>");
			fflush(stderr);
#endif
			move_terminal();
			break;

		case BOX_SPECIAL:
#ifdef __SEND
if (((_Cbox *) bp)->_re->nd_type == NODE_SPECIAL) {
	fprintf(stderr, "<S:");
	for (np = (_Char *) ((_Cseq *) ((_Sbox *) bp)->_re)->_bon;
	     np != (_Char *) ((_Cseq *) ((_Sbox *) bp)->_re)->_eon;
	     np = (_Char *) np->_rt)
		fprintf(stderr, "%c", np->_ch);
	fprintf(stderr, "%c", np->_ch);
	fprintf(stderr, ">");
} else
	fprintf(stderr, "<S:err>");
fflush(stderr);
#endif
			SEND_C(TP_SPECIAL);
			SEND_L(((_Sbox *) bp)->_id);
			SEND_L(((_Sbox *) bp)->_xb);
			SEND_L(((_Sbox *) bp)->_yb);
			SEND_L(0L);
			SEND_L(0L);
			SEND_S(((_Sbox *) bp)->_ta);
			for (j = 0, cp = ((_Sbox *) bp)->_ap;
			     j < ((_Sbox *) bp)->_ta;
			     j++, cp++) {
				SEND_C((*cp));
			}
#ifdef _SEND
			fprintf(stderr, "\n<S>");
			for (j = 0, cp = ((_Sbox *) bp)->_ap;
			     j < ((_Sbox *) bp)->_ta;
			     j++, cp++)
				fprintf(stderr, "%c", *cp);
			fflush(stderr);
#endif
			move_terminal();
			break;
		default:
			msg(STDERR, "Unknown box type %d.", bp->nd_type);
			return(cnt);
		}
	}
	msg(STDERR, "Page box ended prematurely.");
	return(cnt);
}

#endif
