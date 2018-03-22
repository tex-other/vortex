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
#include	"allir.h"
#include	"main.h"
#include	"msg.h"
#include	"comm.h"

extern _Pbox		*pbox_curr;
extern _Pbox		*pbox_head;
extern _File		*file_root;


/* 
 * clear_page ()
 *	Traverse every page in the chain and clear the ok flag. 
 *
 */
int
clear_page ()
{
	_Pbox		*pb;

	for (pb = pbox_head;  pb != NIL;  pb = (_Pbox *) pb->_rt)
		pb->_ok = FALSE;
}


int
find_page (no)
{
	short		forward = TRUE;

	if ((pbox_curr == NIL) || (no <= pbox_curr->_no / 2))
		pbox_curr = pbox_head;
	else if (pbox_curr->_no < no)
		pbox_curr = (_Pbox *) pbox_curr->_rt;
	else /* (pbox_curr->_no / 2) < no < page_curr->_no */
		forward = FALSE;

	if (forward)		
		while ((pbox_curr != NIL) && (pbox_curr->_no != no))
			pbox_curr = (_Pbox *) pbox_curr->_rt;
	else
		while ((pbox_curr != NIL) && (pbox_curr->_no != no))
			pbox_curr = (_Pbox *) pbox_curr->_lt;
	
	if (pbox_curr == NIL) {
		msg(STDERR, "Failed to find page %d.", no);
		return(-1);
	} else
		return(1);
}

_Node *
find_target (tid)
{
	_Node		*bp;
	short		pid;
	short		qid;
	short		wid;
	short		cid;
	short		j;

	pid = PID(tid);
	fprintf(stderr, "irt_to_irs: pid=%d\n", pid);
	if (find_page(pid) == -1)
		return(NIL);

	if ((qid = QID(tid)) == 0)
		return((_Node *) pbox_curr);
	fprintf(stderr, "irt_to_irs: qid=%d\n", qid);

	for (bp = pbox_curr->_dn, j = 1; bp != NIL && j < qid; bp = bp->_rb, j++);
	if (bp == NIL) {
		msg(STDERR, "Failed to find par %d in page %d",
		    qid, pid);
		return(NIL);
	}

	if ((wid = WID(tid)) == 0)
		return(bp);
	fprintf(stderr, "irt_to_irs: wid=%d\n", wid);

	for (bp = ((_Ubox *)bp)->_dn, j = 1; bp != NIL && j < wid; bp = bp->_rb, j++);
	if (bp == NIL) {
		msg(STDERR, "Failed to find word %d in par %d in page %d",
		    wid, qid, pid);
		return(NIL);
	}

	if ((cid = CID(tid)) == 0)
		return(bp);
	fprintf(stderr, "irt_to_irs: cid=%d\n", cid);

	for (bp = ((_Ubox *)bp)->_dn, j = 1; bp != NIL && j < cid; bp = bp->_rb, j++);
	if (bp == NIL) {
		msg(STDERR, "Failed to find terminal node %d in word %d in par %d in page %d",
		    cid, wid, qid, pid);
		return(NIL);
	}

	if (((_Cbox *) bp)->_re == NIL) {
		msg(STDERR, "tid %d doesn't have matching sid", tid);
		return(NIL);
	} else {
		msg(STDERR, "found tid %d (`%c')", tid, bp->nd_char);
		return(bp);
	}
}

irt_to_irs (id, data, bstr)
	char		*data;
	char		*bstr;
{
	long		tid;
	long		sid;
	long		*lp;
	int		cnt = 0;
	int		code;
	long		offset;
	_Node		*bp;
	_Node		*find_target();
	_File		*fp;
	_Char		*cp;
	
	/* process argument */
	tid = ntohl(* (u_long *) data);
	fprintf(stderr, "irt_to_irs: tid=0x%x\n", tid);

	if ((bp = find_target(tid)) == NIL) {
		fprintf(stderr, "irt_to_irs: failed to find tid=%d.\n", tid);
		code = NODE_NONE;
	} else {
		switch (bp->nd_type) {
		case BOX_PAGE:
			code = NODE_NONE;
			break;

		case BOX_PAR:
		case BOX_WORD:
			if (((_Ubox *) bp)->_re == NIL)
				code = NODE_NONE;
			else
				code = ((_Ubox *) bp)->_re->nd_type;
			break;

		case BOX_CHAR:
			fprintf(stderr, "irt_to_irs: char=%c  ie=%c\n", bp->nd_char, ((_Cbox *) bp)->_re->nd_type);
			if (((_Cbox *) bp)->_re == NIL)
				code = NODE_NONE;
			else
				code = ((_Cbox *) bp)->_re->nd_type;
			break;

		case BOX_SPECIAL:
			code = NODE_SPECIAL;
			break;

		case BOX_RULE:
			code = NODE_RULE;
			break;

		default:
			msg(STDERR, "Unknown box type %d", bp->nd_type);
			break;
		}
	}

	if (code != NODE_NONE) {
		sid = ((_Char *) ((_Cbox *) bp)->_re)->_id;
		fprintf(stderr, "irt_to_irs: found sid=0x%x\n", sid);
	
		/* find the file */
		if ((fp = ir_find_file(FID(sid), file_root)) == NIL) {
			fprintf(stderr, "irt_to_irs: fid %d not found.\n", FID(sid));
			SEND_SL(0L);
			return(cnt);
		}
		fprintf(stderr, "irt_to_irs: found file fp=0x%x\n", fp);
		
		/* find the char */
		for (offset = 0, cp = fp->hd;
		     cp != NIL && cp->_ch != EOF;
		     offset++, cp = (_Char *) cp->_rt )
			if (cp->_id == sid) {
				fprintf(stderr, "irt_to_irs: tid=0x%x  sid=0x%x fid=%d  offset=%d\n",
					tid, sid, FID(sid), offset);
				SEND_SL(offset);
				ts_send(TSC_RETURN, cnt, FID(sid), bstr);
				return(cnt);
			}
		fprintf(stderr, "irt_to_irs: sid 0x%x not in IRs", sid);
		SEND_SL(0L);
	} else {
		/* let source editor time out */
		fprintf(stderr, "irt_to_irs: tid=%d doesn't have a matching sid.\n", tid);
		SEND_SL(0L);
	}
	return(cnt);
}
	
irs_to_irt (fid, data, bstr)
	char		*data;
	char		*bstr;
{
	long		*lp;
	int		cnt = 0;
	long		offset;
	long		tid;
	_File		*fp;
	_File		*ir_find_file();
	_Char		*cp;
	_Char		*find_source();
	int		n;
	
	offset = ntohl(* (u_long *) data);
	fprintf(stderr, "irs_to_irt: fid=%d  offset=%d\n", fid, offset);

	/* find the file */
	if ((fp = ir_find_file(fid, file_root)) == NIL) {
		fprintf(stderr, "irs_to_irt: fid %d not found.", fid);
		SEND_SL(0L);
		return(cnt);
	}
	fprintf(stderr, "irs_to_irt: found file fp=0x%x\n", fp);
	
	/* find the char */
	for (n = 0, cp = fp->hd;
	     n < offset && cp != NIL && cp->_ch != EOF;
	     n++, cp = (_Char *) cp->_rt )
		fprintf(stderr, "%c", cp->_ch);
	fprintf(stderr, "\n");
	
	fprintf(stderr, "irs_to_irt: found cp=0x%x\n", cp);
	if (cp == NIL) {
		fprintf(stderr, "irs_to_irt: offset %d too big.\n", offset);
		SEND_SL(0L);
		return(cnt);
	}

	fprintf(stderr, "irs_to_irt: found irs node `%c' (sid=0x%x).\n",
		cp->_ch, cp->_id);
	if (cp->_re == NIL) {
		fprintf(stderr, "irs_to_irt: failed to find a matching tid.\n");
		SEND_SL(0L);
	} else {
		switch (cp->_re->nd_type) {
		case BOX_CHAR:
			tid = ((_Cbox *) cp->_re)->_id;
			break;
		case BOX_SPECIAL:
			tid = ((_Sbox *) cp->_re)->_id;
			break;
		case BOX_RULE:
			tid = ((_Rbox *) cp->_re)->_id;
			break;
		case BOX_WORD:
		case BOX_PAR:
			tid = ((_Ubox *) cp->_re)->_id;
			break;
		case BOX_PAGE:
			tid = ((_Pbox *) cp->_re)->_id;
			break;
		default:
			msg(STDERR, "Unknown box type %d", cp->_re->nd_type);
			SEND_SL(0L);
			return(cnt);
		}
		fprintf(stderr, "irs_to_irt: found matching tid=0x%x\n", tid);
		SEND_SL(tid);
		fprintf(stderr, "irs_to_irt: found matching tid=0x%x\n", ntohl(* (long *) bstr));
		ts_send(TSC_RETURN, cnt, fid, bstr);
	}
	return(cnt);
}

#endif
