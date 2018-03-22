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
#include	"str.h"
#include	"texext.h"

#include	"allir.h"
#include	"main.h"
#include	"msg.h"

extern long		qid;
extern long		wid;
extern long		cid;

extern short		empty_qbox;	/* true if new qbox is empty */
extern short		empty_wbox;	/* true if new wbox is empty */
extern struct _cbox	*cbox;		/* current char box */
extern struct _ubox	*wbox;		/* current word box */
extern struct _ubox	*qbox;		/* current par box */
extern struct _pbox	*pbox;		/* current new page box */
extern struct _pbox	*pbox_head;	/* head of page box list */

#ifdef _IRT
#include	<stdio.h>
FILE		*irt_fp;
char		*irt_fn = "#irt#";
int		fp_virgin = TRUE;
#endif

static int
fix_links (bp)
	_Node		*bp;
{
	bp->nd_up = (_Node *) wbox;
	if (wbox->_dn == NIL)
		wbox->_dn = bp;
	bp->nd_rt = NIL;
	if (cbox != NIL)
		cbox->_rt = bp;
	bp->nd_lt = (_Node *) cbox;
	cbox = (_Cbox *) bp;
	if ((wbox->_re == NIL) && (cbox->_re != NIL) &&
	    (cbox->_re->nd_up != NIL) &&
	    (cbox->_re->nd_up->nd_type == NODE_WORD)) {
		wbox->_re = cbox->_re->nd_up;
		((_Unode *) wbox->_re)->_re = (_Node *) wbox;
	}
	if ((qbox->_re == NIL) && (wbox->_re != NIL)) {
		qbox->_re = wbox->_re->nd_up;
		while ((qbox->_re != NIL) && (qbox->_re->nd_type != NODE_PAR)) {
			if (qbox->_re == qbox->_re->nd_up) {
				fprintf(stderr, "PAR node %x clobbered.\n", qbox->_re);
				break;
			}
			qbox->_re = qbox->_re->nd_up;
		}
		if (qbox->_re != NIL)
			((_Unode *) qbox->_re)->_re = (_Node *) qbox;
	}
}

make_char_box (p, f, c)
{
	_Cbox		*bp;
	_Char		*np;
	u_long		w, h, d;

	if (wbox == NIL) {
		msg(STDERR, "wbox shouldn't be nil while creating a cbox.");
		exit(-1);
	}
	
	if ((bp = (_Cbox *) malloc(sizeof(_Cbox))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _cbox.");
		exit(-1);
	}

	/* increment total number of boxes */
	(pbox->_tb)++;

	/* parent boxes are here to stay */
	empty_qbox = empty_wbox = FALSE;

	/* fill in attributes for current char box */
	bp->_ch = (char) c;
	bp->_xb = cur_h;
	bp->_yb = cur_v;
	bp->_ft = f;
	bp->_re = ir_char(p);
	
	/* set box id */
	if (++cid < W_BUMP)
		bp->_id = wbox->_id | cid;
	else {
		msg(STDERR, "Too many letters in a word.");
		exit(-1);
	}
	
	/* figure out box type */
	if (bp->_re == NIL) {
		if (bp->_ch == '-')
			bp->_ty = BOX_HYPH;
		else
			bp->_ty = BOX_EXP;
	} else 	if (bp->_re->nd_type == NODE_CHAR) {
		bp->_ty = BOX_CHAR;
		((_Char *) bp->_re)->_re = (_Node *) bp;
		pbox->_lc = bp->_re;
	} else if (bp->_re->nd_type == NODE_LIG) {
		bp->_ty = BOX_LIG;
		((_Unode *) bp->_re)->_re = (_Node *) bp;
		for (np = (_Char *) bp->_re->nd_lt;
		     np != NIL && np != (_Char *) bp->_re->nd_rt;
		     np = (_Char *) np->_rt)
			np->_re = (_Node *) bp;
		if (np != NIL)
			np->_re = (_Node *) bp;
	} else {
		bp->_ty = BOX_MACRO;
		((_Cseq *) bp->_re)->_re = (_Node *) bp;

		for (np = (_Char *) ((_Cseq *) bp->_re)->_bon;
		     (np != NIL) && (((_Cseq *) bp->_re)->_eon != NIL) &&
		     (np != (_Char *) ((_Cseq *) bp->_re)->_eon);
		     np = (_Char *) np->_rt)
			np->_re = (_Node *) bp;
		np->_re = (_Node *) bp;
/*		if ((bp->_re != NIL) && ((_Cseq *) bp->_re)->_boc != NIL)
			((_Char *) ((_Cseq *) bp->_re)->_boc)->_re = (_Node *) bp;
		if (((_Cseq *) bp->_re)->_eoc != NIL)
			((_Char *) ((_Cseq *) bp->_re)->_eoc)->_re = (_Node *) bp;
*/
	}

	/* update wbox attributes */
	w = char_width(f, char_info(f, c));
	h = char_height(f, height_depth(char_info(f, c)));
	d = char_depth(f, height_depth(char_info(f, c)));
	wbox->_xc = MIN2(wbox->_xc, cur_h);
	wbox->_yc = MIN2(wbox->_yc, cur_v - h);
	wbox->_wd = cur_h + w - wbox->_wd;
	wbox->_ht = MAX2(wbox->_ht, h + d);

	fix_links((_Node *) bp);

#ifdef _IRT
	fprintf(stderr,"char=%c  id =%x (%x)\n", c, bp->_id, CID(bp->_id));
	
	fprintf(irt_fp,"%c", c);
	fflush(irt_fp);
	if (ir_char(p) != NIL)
		fprintf(irt_fp,"\n[CHAR: id=%d  x=%d  y=%d  f=%d  c=%c  (p=%d ir=%x c=%c)]",
			bp->_id, bp->_xb, bp->_yb, bp->_ft, bp->_ch,
			p, ir_char(p), ir_char(p)->nd_char);
	else
		fprintf(irt_fp,"\n[CHAR: id=%d  x=%d  y=%d  f=%d  c=%c  (p=%d ir=NIL)]",
			bp->_id, bp->_xb, bp->_yb, bp->_ft, bp->_ch, p);
#endif
}

/*
 * We are essentially treating rule as char here 
 */
make_rule_box (p)
{
	_Rbox		*bp;
	_Char		*np;

	if (wbox == NIL) {
		msg(STDERR, "wbox shouldn't be nil while creating a cbox.");
		exit(-1);
	}
	
	if ((bp = (_Rbox *) malloc(sizeof(_Rbox))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _rbox.");
		exit(-1);
	}

	/* increment total number of boxes */
	(pbox->_tb)++;

	/* parent boxes are here to stay */
	empty_qbox = empty_wbox = FALSE;

	/* fill in attributes for current box */
	bp->_ty = BOX_RULE;
	bp->_ch = NULL;
	bp->_xc = cur_h;
	bp->_yc = cur_v;
	bp->_wd = rule_wd;
	bp->_ht = rule_ht;
	bp->_re = ir_rule(p);
	
	/* fix cross reference of IRs nodes under this rule */
	if (bp->_re != NIL) {
		bp->_re->nd_type = NODE_RULE;
		((_Cseq *) bp->_re)->_re = (_Node *) bp;

		for (np = (_Char *) ((_Cseq *) bp->_re)->_bon;
		     np != (_Char *) ((_Cseq *) bp->_re)->_eon;
		     np = (_Char *) np->_rt)
			np->_re = (_Node *) bp;
		if (np != NIL)
			np->_re = (_Node *) bp;

		for (np = (_Char *) ((_Cseq *) bp->_re)->_boc;
		     np != (_Char *) ((_Cseq *) bp->_re)->_eoc;
		     np = (_Char *) np->_rt)
			np->_re = (_Node *) bp;
		if (np != NIL)
			np->_re = (_Node *) bp;
	}

	/* set box id */
	if (++cid < W_BUMP)
		bp->_id = wbox->_id | cid;
	else {
		msg(STDERR, "Too many letters in a word.");
		exit(-1);
	}

	/* update wbox attributes */
	wbox->_xc = MIN2(wbox->_xc, cur_h);
	wbox->_yc = MIN2(wbox->_yc, cur_v - rule_ht);
	wbox->_wd = cur_h + rule_wd - wbox->_wd;
 	wbox->_ht = MAX2(wbox->_ht, rule_ht);

	fix_links((_Node *) bp);

#ifdef _IRT
/*
	fprintf(irt_fp,"RULE");
	fflush(irt_fp);
*/
	if (ir_rule(p) != NIL)
		fprintf(irt_fp,"\n[RULE: id=%d  x=%d  y=%d  w=%d  h=%d  (p=%d  ir=%x)]",
			bp->_id, bp->_xc, bp->_yc, bp->_wd, bp->_ht, p, ir_rule(p));
	else
		fprintf(irt_fp,"\n[RULE: id= %d  x=%d  y=%d  w=%d  h=%d  (p=%d  ir=NIL)]",
			bp->_id, bp->_xc, bp->_yc, bp->_wd, bp->_ht, p);
#endif
}

/*
 * We are essentially treating special as char here 
 */
make_special_box (p)
{
	int		j;
	_Sbox		*bp;
	_Char		*np;
	char		*ap;

	if (wbox == NIL) {
		msg(STDERR, "wbox shouldn't be nil while creating a cbox.");
		exit(-1);
	}
	
	if ((bp = (_Sbox *) malloc(sizeof(_Sbox))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _sbox.");
		exit(-1);
	}

	/* increment total number of boxes */
	(pbox->_tb)++;

	/* parent boxes are here to stay */
	empty_qbox = empty_wbox = FALSE;

	/* fill in attributes for current box */
	bp->_ty = BOX_SPECIAL;
	bp->_ch = NULL;
	bp->_xb = cur_h;
	bp->_yb = cur_v;
	bp->_re = ir_special(p);
	
	/* fix cross reference of IRs nodes under this rule */
	if (bp->_re != NIL) {
		((_Cseq *) bp->_re)->_re = (_Node *) bp;

		for (np = (_Char *) ((_Cseq *) bp->_re)->_bon;
		     np != (_Char *) ((_Cseq *) bp->_re)->_eon;
		     np = (_Char *) np->_rt)
			np->_re = (_Node *) bp;
		if (np != NIL)
			np->_re = (_Node *) bp;

		for (np = (_Char *) ((_Cseq *) bp->_re)->_boc;
		     np != (_Char *) ((_Cseq *) bp->_re)->_eoc;
		     np = (_Char *) np->_rt)
			np->_re = (_Node *) bp;
		if (np != NIL)
			np->_re = (_Node *) bp;
	}
	
	/* set box id */
	if (++cid < W_BUMP)
		bp->_id = wbox->_id | cid;
	else {
		msg(STDERR, "Too many letters in a word.");
		exit(-1);
	}
	
	/* copy arg list */
	bp->_ta = cur_length();
	if ((bp->_ap = (char *) calloc(bp->_ta, 1)) == NIL) {
		msg(STDERR, "Not enough core to calloc %d chars.", bp->_ta);
		exit(-1);
	}
	ap = bp->_ap;
	for (j = str_start[str_ptr]; j < pool_ptr; j++, ap++)
		*ap = str_pool[j];

	/* update pbox attribute */
	(pbox->_ts)++;
	pbox->_ta += bp->_ta;

	/* update wbox attributes */
	wbox->_xc = MIN2(wbox->_xc, cur_h);
	wbox->_yc = MIN2(wbox->_yc, cur_v);
/*
	wbox->_wd = wbox->_wd;
 	wbox->_ht = wbox->_ht;
*/

	fix_links((_Node *) bp);

#ifdef _IRT
/*
	fprintf(irt_fp,"SPECIAL");
	fflush(irt_fp);
*/
	if (ir_special(p) != NIL)
		fprintf(irt_fp,"\n[SPECIAL: id=%d  x=%d  y=%d (p=%d  ir=%x)]",
			bp->_id, bp->_xb, bp->_yb, p, ir_special(p));
	else
		fprintf(irt_fp,"\n[SPECIAL: id= %d  x=%d  y=%d (p=%d  ir=NIL)]",
			bp->_id, bp->_xb, bp->_yb, p);
#endif
}

make_word_box ()
{
	_Ubox		*wb;

	if (qbox == NIL) {
		msg(STDERR, "qbox shouldn't be nil while creating a wbox.");
		exit(-1);
	}
	
	if (empty_wbox)
		return(0);

	if ((wb = (_Ubox *) malloc(sizeof(_Ubox))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _ubox.");
		exit(-1);
	}
	empty_wbox = TRUE;

	/* increment total number of boxes */
	(pbox->_tb)++;

	/* fill in attributes for current word box */
	wb->_ty = BOX_WORD;
	wb->_ch = NULL;
	wb->_xc = INFINITY;
	wb->_yc = INFINITY;
	wb->_wd = cur_h;
	wb->_ht = 0;
	wb->_re = NIL;

	/* set box id */
	cid = 0;
	if ((wid += W_BUMP) < Q_BUMP)
		wb->_id = qbox->_id | wid;
	else {
		msg(STDERR, "Too many words in a paragraph.");
		exit(-1);
	}

	/* fix up/down pointers */
	wb->_up = (_Node *) qbox;
	wb->_dn = NIL;
	if (qbox->_dn == NIL)
		qbox->_dn = (_Node *) wb;

	/* fix left/right pointers */
	wb->_rt = NIL;
	wb->_lt = (_Node *) wbox;
	if (wbox != NIL) {
		wbox->_rt = (_Node *) wb;
		/* fix current par box (qbox) attributes */
		qbox->_xc = MIN2(qbox->_xc, wbox->_xc);
		qbox->_yc = MIN2(qbox->_yc, wbox->_yc);
		qbox->_wd = MAX2(qbox->_wd, wbox->_xc + wbox->_wd);
		qbox->_ht = MAX2(qbox->_ht, wbox->_yc + wbox->_ht);
	}

	/* newly created box becomes wbox globally */
	wbox = wb;

	/* start a new char */
	cbox = NIL;
#ifdef _IRT
	fprintf(stderr,"WORD  id =%x (%x)\n", wb->_id, WID(wb->_id));
	fprintf(irt_fp,"\n[WORD: id=%d]", wb->_id);
#endif
}

#define	fix_par_page() { \
	qbox->_wd = qbox->_wd - qbox->_xc; \
	qbox->_ht = qbox->_ht - qbox->_yc; \
	pbox->_xc = MIN2(pbox->_xc, qbox->_xc); \
	pbox->_yc = MIN2(pbox->_yc, qbox->_yc); \
	pbox->_wd = MAX2(pbox->_wd, qbox->_xc + qbox->_wd); \
	pbox->_ht = MAX2(pbox->_ht, qbox->_yc + qbox->_ht); \
}

make_par_box ()
{
	_Ubox		*qb;

	if (pbox == NIL) {
		msg(STDERR, "pbox shouldn't be nil while creating a qbox.");
		exit(-1);
	}
	
	if (empty_qbox)
		return(0);

	if ((qb = (_Ubox *) malloc(sizeof(_Ubox))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _ubox.");
		exit(-1);
	}
	empty_qbox = TRUE;

	/* clean up last wbox */
	if (empty_wbox) {
		(pbox->_tb)--;
		empty_wbox = FALSE;
		if (wbox->_lt != NIL)
			wbox->_lt->nd_rt = NIL;
		free(wbox);
	}

	/* increment total number of boxes */
	(pbox->_tb)++;

	/* fill in attributes for current par box */
	qb->_ty = BOX_PAR;
	qb->_ch = NULL;
	qb->_xc = qb->_yc = INFINITY;
	qb->_wd = qb->_ht = 0;
	qb->_re = NIL;

	/* set box id */
	wid = 0;
	if ((qid += Q_BUMP) < P_BUMP)
		qb->_id = pbox->_id | qid;
	else {
		msg(STDERR, "Too many paragraphs in a page.");
		exit(-1);
	}

	/* fix up/down pointers */
	qb->_up = (_Node *) pbox;
	qb->_dn = NIL;
	if (pbox->_dn == NIL)
		pbox->_dn = (_Node *) qb;

	/* fix left/right pointers */
	qb->_rt = NIL;
	qb->_lt = (_Node *) qbox;
	if (qbox != NIL) {
		qbox->_rt = (_Node *) qb;
		fix_par_page();
	}
	qbox = qb;
	wbox = NIL;
#ifdef _IRT
	fprintf(stderr,"PAR  id =%x (%x)\n", qb->_id, QID(qb->_id));
	fprintf(irt_fp,"\n[PAR: id=%d]", qb->_id);
#endif
}

make_page_box()
{
	_Pbox		*pb;
	int		j;

#ifdef _IRT
	if (fp_virgin) {
		if ((irt_fp = fopen(irt_fn, "w")) == NIL) {
			fprintf(stderr, "Can't open %s.", irt_fn);
			exit(-1);
		}
		fp_virgin = FALSE;
	}
#endif

	if (pbox == NIL){
		if ((pbox = (_Pbox *) malloc(sizeof(_Pbox))) == NIL) {
			msg(STDERR, "Not enough core to malloc struct _pbox.");
			exit(-1);
		}
		pbox->_up = pbox->_dn = pbox->_lt = pbox->_rt = NIL;
		pbox_head = pbox;
	} else {
		if (pbox->_no + 1 == starting_page) {
			if (pbox->_rt == NIL) {
				if ((pb = (_Pbox *) malloc(sizeof(_Pbox))) == NIL) {
					msg(STDERR, "Not enough core to malloc struct _pbox.");
					exit(-1);
				}
				pb->_rt = pb->_dn = pb->_lc = NIL;
				pbox->_rt = (_Node *) pb;
				pb->_lt = (_Node *) pbox;
				pbox = pb;
			} else {
				pbox = (_Pbox *) pbox->_rt;
				free_IRt(pbox->_dn);
				pbox->_dn = pbox->_lc = NIL;
			}
		} else if (starting_page < pbox->_no / 2) {
			if ((pbox = pbox_head) == NIL) {
				msg(STDERR, "Pbox list header shouldn't be nil.");
				exit(-1);
			}
			for (j = 1; j < starting_page; j++)
				pbox = (_Pbox *) pbox->_rt;
			free_IRt(pbox->_dn);
			pbox->_dn = pbox->_lc = NIL;
		} else if (starting_page <= pbox->_no) {
			for (j = starting_page; j < pbox->_no; j++)
				pbox = (_Pbox *) pbox->_lt;
			free_IRt(pbox->_dn);
			pbox->_dn = pbox->_lc = NIL;
		} else {
			msg(STDERR, "Impossible starting page, too large.");
			exit(-1);
		}
	}

	/* fill in rest of attributes */
	pbox->_ty = BOX_PAGE;
	pbox->_ch = NULL;
	pbox->_ec = irs_next->_lt;			/* extended context */
	pbox->_xc = pbox->_yc = INFINITY;
	pbox->_wd = pbox->_ht = 0;
	pbox->_tb = pbox->_tf = pbox->_tn = pbox->_ts = pbox->_ta = 0;
	pbox->_id = (total_pages + 1) << P_SHIFT;
	pbox->_ok = FALSE;
	for (j = 0; j < FT_MAX; j++)
		pbox->_ft[j] = 0;

	/* start a new par */
	empty_qbox = empty_wbox = FALSE;
	qbox = NIL;
	qid = wid = cid = 0;
#ifdef _IRT
	fprintf(stderr,"PAGE  id =%x (%x)\n", pbox->_id, PID(pbox->_id));
	fprintf(irt_fp,"\n[PAGE: %d]", total_pages+1);
#endif
}
	
finish_page_box ()
{
	int 		j;
	_Char		*ec;
	_Char		*lc;
	_Node		*np;

	/* fix last qbox and pbox */
	fix_par_page();
	pbox->_wd = pbox->_wd - pbox->_xc;
	pbox->_ht = pbox->_ht - pbox->_yc;
	pbox->_no = total_pages;

	/* clean up last wbox */
	if (empty_wbox) {
		(pbox->_tb)--;
		if (wbox->_lt != NIL)
			wbox->_lt->nd_rt = NIL;
		free(wbox);
	}

	/* clean up last qbox */
	if (empty_qbox) {
		(pbox->_tb)--;
		if (qbox->_lt != NIL)
			qbox->_lt->nd_rt = NIL;
		free(qbox);
	}
	
	/* record count registers */
	for (j = 0; j < 10; j++) {
		pbox->_ct[j] = count(j);
	}

	/* set last char node in contribute list as id of pbox */
	if (total_pages > 1) {
		ec = (_Char *) ((_Pbox *) pbox->_lt)->_ec;
		lc = (_Char *) ((_Pbox *) pbox->_lt)->_lc;

		while (ec != lc) {
			if (ec->_re != NIL) {
				pbox->_id = ((_Cbox *) ec->_re)->_id;
				break;
			}
			if (ec->_ch != EOF)
				ec = (_Char *) ec->_lt;
			else {
				/* inculded file? */
				np = ec->_up;
				while ((np != NIL) &&
				       (np->nd_type != NODE_INPUT))
					np = np->nd_up;
				if (np == NIL)
					break;
				else
					ec = (_Char *) ((_Input *) np)->_bon;
			}
		}
	}
}

free_IRt (b)
	_Node		*b;
{
	_Ubox		*qb = (_Ubox *) b;
	_Ubox		*qb1;
	_Ubox		*wb;
	_Ubox		*wb1;
	_Cbox		*cb;
	_Cbox		*cb1;

	while (qb != NIL) {
		wb = (_Ubox *) qb->_dn;
		while (wb != NIL) {
			cb = (_Cbox *) wb->_dn;
			while (cb != NIL) {
				cb1 = cb;
				cb = (_Cbox *) cb->_rt;
				free(cb1);
			}
			wb1 = wb;
			wb = (_Ubox *) wb->_rt;
			free(wb1);
		}
		qb1 = qb;
		qb = (_Ubox *) qb->_rt;
		free(qb1);
	}
}

#endif VORTEX
