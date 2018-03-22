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
 *  This file is part of the VorTeX incremental formatter.
 *
 *  Copyright (C) 1987 by the Regents of University of California and by
 *  Pehong Chen
 *  Computer Science Division
 *  571 Evans Hall
 *  University of California, Berkeley
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
#include	"allir.h"
#include	"main.h"
#include	"msg.h"
#include	"ts_comm.h"

extern char		name_of_file[FILE_NAME_SIZE];
extern int		cur_name;		/* defined in TeX */
extern _File		*file_root;		/* root of file tree IRf */
extern _File		*file_curr;		/* current file for reading */
extern _File		*file_update;		/* last file updated */

extern _Char		*irs_bol;
extern _Char		*irs_eol;
extern int		irs_read_only;

_File *
ir_find_file (id, root)
	_File		*root;
{
	if (root == NIL)
		return(NIL);
	else if (id == root->id)
		return(root);
	else if (id < root->id) {
		if (root->lt == NIL)
			return(NIL);
		else
			return(ir_find_file(id, root->lt));
	} else if (root->rt == NIL) {
		/* id greater than root->id */
		return(NIL);
	} else {
		return(ir_find_file(id, root->rt));
	}
}
	
static
ir_free_IRs (bp, ep)
	_Char		*bp, *ep;
{
	_Char		*cp;

	for (;;) {
		cp = bp;
		free(cp);
		if (bp == ep)
			break;
		else
			bp = (_Char *) bp->_rt;
	}
}

ir_close_file (fid)
{
	msg(MDBUG, "Closing file fid=%d", fid);
	ir_delete_file(fid, file_root);
}

static int
ir_delete_file (id, root)
	_File		*root;
{
	_File		*cp;
	_Char		*tail;
	int		code;

	if (id == root->id) {
		ir_free_IRs(root->hd, (_Char *) root->hd->_lt);
		return(1);
	} else if (id < root->id) {	/* id less than root->id */
		if (root->lt == NIL) {
			msg(STDERR, "File (id = %d) not found in IR.", id);
			return(-1);
		} else {
			if ((code = ir_delete_file(id, root->lt)) == 1) {
				root->lt = NIL;
				return(0);
			} else
				return(code);
		}
	} else if (root->rt == NIL) {	/* id greater than root->id */
		msg(STDERR, "File (id = %d) not found in IR.", id);
		exit(-1);
	} else if ((code = ir_delete_file(id, root->lt)) == 1) {
		root->rt = NIL;
		return(0);
	} else
		return(code);
}

_File *
ir_insert_file (id, root)
	_File		*root;
{
	_File		*cp;
	_Char		*tail;

	if (id == root->id) {
		tail = (_Char *) root->hd->_lt;
		ir_free_IRs(root->hd, tail->_lt);
		tail->_lt = tail->_rt = (_Node *) tail;
		root->hd = root->pt = tail;
		return(root);
	} else if (id < root->id) {
		if (root->lt == NIL) {
			if ((cp = (_File *) malloc(sizeof(_File))) == NIL) {
				msg(STDERR, "Not enough core to malloc struct _file.");
				exit(-1);
			}

			if ((cp->fn = (char *) malloc(strlen(name_of_file))) == NIL) {
				msg(STDERR, "Not enough core to malloc filename string.");
				exit(-1);
			}
			strcpy(cp->fn, name_of_file);

			cp->id = id;
			cp->lt = cp->rt = NIL;
			cp->hd = cp->pt = NIL;
			root->lt = cp;
			return(cp);
		} else
			return(ir_insert_file(id, root->lt));
	} else if (root->rt == NIL) {
		/* id greater than root->id */
		if ((cp = (_File *) malloc(sizeof(_File))) == NIL) {
			msg(STDERR, "Not enough core to malloc struct _file.");
			exit(-1);
		}

		if ((cp->fn = (char *) malloc(strlen(name_of_file))) == NIL) {
			msg(STDERR, "Not enough core to malloc filename string.");
			exit(-1);
		}
		strcpy(cp->fn, name_of_file);

		cp->id = id;
		cp->lt = cp->rt = NIL;
		cp->hd = cp->pt = NIL;
		root->rt = cp;
		return(cp);
	} else {
		return(ir_insert_file(id, root->rt));
	}
}

static
ir_fill_IRs (count, data)
	char		*data;
{
	_Char		*eof;

	if ((file_update->pt == NIL) ||
	    (file_update->hd == NIL) ||
	    (file_update->pt != file_update->hd) ||
	    (file_update->pt->_ch != EOF)) {
		/* create an empty node containing EOF */
		if ((eof = (_Char *) malloc(sizeof(_Char))) == NIL) {
			msg(STDERR, "Not enough core to malloc struct _char.");
			exit(-1);
		}
		eof->_ty = NODE_CHAR;
		eof->_ch = EOF;
		eof->_id = 0L;
	
		/* link to itself */
		eof->_lt = eof->_rt = (_Node *) eof;
		file_update->hd = file_update->pt = eof;
	}

	ir_insert_chars(file_update, count, data, FALSE);
#ifdef _IRF
	irs_traverse();
#endif
}

static int
irs_traverse()
{
	_Char		*cp;

	cp = file_update->hd;
	while (cp->_ch != EOF) {
		fprintf(stderr, "%c", cp->_ch);
		if (!cp->_up)	/* DEBUG */
			putc('?', stderr);
		cp = (_Char *) cp->_rt;
	}
}

ir_open_file (fid, len, data)
	char		*data;
{
	msg(MDBUG, "Opening file fid=%d len=%d", fid, len);
	if (file_root == NIL) {
		if ((file_root = (_File *) malloc(sizeof(_File))) == NIL) {
			msg(STDERR, "Not enough core to malloc struct _file.");
			exit(-1);
		}

		if ((file_root->fn = (char *) malloc(strlen(name_of_file))) == NIL) {
			msg(STDERR, "Not enough core to malloc filename string.");
			exit(-1);
		}
		strcpy(file_root->fn, name_of_file);

		file_root->id = fid;
		file_root->lt = file_root->rt = NIL;
		file_root->hd = file_root->pt = NIL;
		file_update = file_root;
	} else if (fid != file_update->id)
		file_update = ir_insert_file(fid, file_root);

	ir_fill_IRs(len/U_LONG, data);
	file_curr = file_update;

	irs_eol = file_curr->hd;
}

static int
ir_find_point (fid, flag, offset)
{
	int		i;

	if (fid != file_update->id) {
		if ((file_update = ir_find_file(fid, file_root)) == NIL) {
			/* no such file */
			msg(STDERR, "No such file %d registered.\n", fid);
			return(-1);
		}
	}
	
	switch (flag) {
	case IRS_CUR:
		break;
	case IRS_BOF:
		file_update->pt = file_update->hd;
		break;
	case IRS_EOF:
		file_update->pt = (_Char *) file_update->hd->_lt;
		break;
	default:
		msg(STDERR, "Unknown flag %d for insertion.", flag);
		return(-1);
	}
	
	if (offset > 0)
		for (i = 0; i < offset; i++) {
			if (file_update->pt->_ch == EOF) {
				break;
			}
			file_update->pt = (_Char *) file_update->pt->_rt;
		}
	else
		for (i = 0; i < -offset; i++) {
			if (file_update->pt == file_update->hd) {
				break;
			}
			file_update->pt = (_Char *) file_update->pt->_lt;
		}
}

/*
 * Insert COUNT number of DATA, each a u_long, before FP->PT.
 *
 */
ir_insert_chars (fp, count, data, check)
	_File		*fp;		/* file pointer */
	char		*data;
{
	int		i;
	u_long		wd;
	_Char		*cp, *pp;

	if (count <= 0)
		return(-1);

	if ((pp = (_Char *) malloc(sizeof(_Char))) == NIL) {
		msg(STDERR, "Not enough core to malloc struct _char.");
		exit(-1);
	}
	pp->_id = ntohl(*(u_long *) data);
	data += U_LONG;
	pp->_ty = NODE_CHAR;
	pp->_ch = (char) (pp->_id & A_MASK);
	pp->_rt = pp->_up = pp->_re = NIL;

#ifdef _IRF
	if (fp->pt->_lt == NIL)
		msg(STDERR, "Pre-INSERT : left=NIL, curr=%c, right=%c", fp->pt->_ch, fp->pt->_rt->nd_char);
	else if (fp->pt->_rt == NIL)
		msg(STDERR, "Pre-INSERT : left=%c, curr=%c, right=NIL", fp->pt->_lt->nd_char, fp->pt->_ch);
	else
		msg(STDERR, "Pre-INSERT : left=%c, curr=%c, right=%c", fp->pt->_lt->nd_char, fp->pt->_ch, fp->pt->_rt->nd_char);
#endif

	/* fix up links */
	if (fp->hd == fp->pt)
		fp->hd = pp;
	pp->_lt = fp->pt->_lt;
	pp->_lt->nd_rt = (_Node *) pp;

	if (check) {
		set_starting_page((_Char *) pp->_lt);
#ifdef _IRF
	        msg(STDERR, "INSERT : starting_page reset to %d", starting_page);
#endif
	}

	for (i = 1; i < count; i++) {
		if ((cp = (_Char *) malloc(sizeof(_Char))) == NIL) {
			msg(STDERR, "Not enough core to malloc struct _char.");
			exit(-1);
		}
		cp->_id = ntohl(*(u_long *) data);
		data += U_LONG;
		cp->_ty = NODE_CHAR;
		cp->_ch = (char) (cp->_id & A_MASK);
		cp->_lt = (_Node *) pp;
		pp->_rt = (_Node *) cp;
/*
		pp->_up = pp->_re = NIL;
*/
		cp->_up = cp->_re = NIL;
		pp = cp;
	}
	
	pp->_rt = (_Node *) fp->pt;
	fp->pt->_lt = (_Node *) pp;

#ifdef _IRF	
	if (fp->pt->_lt == NIL)
		msg(STDERR, "Post-INSERT : left=NIL, curr=%c, right=%c", fp->pt->_ch, fp->pt->_rt->nd_char);
	else if (fp->pt->_rt == NIL)
		msg(STDERR, "Post-INSERT : left=%c, curr=%c, right=NIL", fp->pt->_lt->nd_char, fp->pt->_ch);
	else
		msg(STDERR, "Post-INSERT : left=%c, curr=%c, right=%c", fp->pt->_lt->nd_char, fp->pt->_ch, fp->pt->_rt->nd_char);
#endif
}

ir_insert (fid, len, data)
	char		*data;
{
	u_long		flag;
	u_long		offset;
	u_long		count;

	flag = ntohl(*(u_long *) data);
	data += U_LONG;
	offset = ntohl(*(long *) data);
	data += U_LONG;
	count = ntohl(*(u_long *) data);
	data += U_LONG;

#ifdef _IRF
	msg(STDERR, "INSERT: fid=%d, flag=%d, offset=%d, count=%d", fid, flag, offset, count);
#endif
	ir_find_point(fid, flag, offset);
#ifdef _IRF
	msg(STDERR, "BEFORE INSER CHAR");
	irs_traverse();
#endif	
	ir_insert_chars(file_update, count, data, TRUE);
#ifdef _IRF
	irs_traverse();
#endif
}

ir_delete_chars (fp, count)
	_File		*fp;		/* file pointer */
{
	int		i;
	_Char		*bp, *cp;

	if ((count <= 0) || (fp->pt->_ch == EOF))
		return(-1);

#ifdef _IRF
	if (fp->pt->_lt == NIL)
		msg(STDERR, "Pre-DELETE : left=NIL, curr=%c, right=%c", fp->pt->_ch, fp->pt->_rt->nd_char);
	else if (fp->pt->_rt == NIL)
		msg(STDERR, "Pre-DELETE : left=%c, curr=%c, right=NIL", fp->pt->_lt->nd_char, fp->pt->_ch);
	else
		msg(STDERR, "Pre-DELETE : left=%c, curr=%c, right=%c", fp->pt->_lt->nd_char, fp->pt->_ch, fp->pt->_rt->nd_char);
#endif

	cp = fp->pt;
	bp = (_Char *) cp->_lt;
	for (i = 0; i < count; i++) {
		if (cp->_ch == EOF)
			break;
                fix_first_ptr(cp);
		cp = (_Char *) cp->_rt;
		free(cp->_lt);
	}
	bp->_rt = (_Node *) cp;
	cp->_lt = (_Node *) bp;
	fp->pt = cp;

#ifdef _IRF
	if (fp->pt->_lt == NIL)
		msg(STDERR, "Post-DELETE : left=NIL, curr=%c, right=%c", fp->pt->_ch, fp->pt->_rt->nd_char);
	else if (fp->pt->_rt == NIL)
		msg(STDERR, "Post-DELETE : left=%c, curr=%c, right=NIL", fp->pt->_lt->nd_char, fp->pt->_ch);
	else
		msg(STDERR, "Post-DELETE : left=%c, curr=%c, right=%c", fp->pt->_lt->nd_char, fp->pt->_ch, fp->pt->_rt->nd_char);
#endif

	set_starting_page(bp);
#ifdef _IRF
        msg(STDERR, "DELETE : starting_page reset to %d", starting_page);
#endif
}

static int
fix_first_ptr (cp)
	_Char		*cp;
{
	if (cp->_up == NIL)
		return;
	switch (cp->_up->nd_type) {
	case NODE_WORD:
	case NODE_LIG:
		if (((_Unode *) cp->_up)->_dn == (_Node *) cp)
			((_Unode *) cp->_up)->_dn = cp->_rt;
		break;
	case NODE_MATH:
	case NODE_DISPLAY:
		if (((_Math *) cp->_up)->_dn == (_Node *) cp)
			((_Math *) cp->_up)->_dn = cp->_rt;
		break;
	case NODE_CSEQ:
	case NODE_DEF:
	case NODE_FDEF:
	case NODE_FONT:
	case NODE_RULE:
	case NODE_SYMBOL:
	case NODE_SPECIAL:
		if (((_Cseq *) cp->_up)->_bon == (_Node *) cp)
			((_Cseq *) cp->_up)->_bon = cp->_rt;
		if (((_Cseq *) cp->_up)->_eon == (_Node *) cp)
			((_Cseq *) cp->_up)->_eon = cp->_lt;
		break;
	case NODE_INPUT:
		if (((_Input *) cp->_up)->_bon == (_Node *) cp)
			((_Input *) cp->_up)->_bon = cp->_rt;
		break;
	case NODE_GROUP:
		if (((_Group *) cp->_up)->_dn == (_Node *) cp)
			((_Group *) cp->_up)->_dn = cp->_rt;
		break;
	case NODE_SPACE:
		if (((_Space *) cp->_up)->_boc == ((_Space *) cp->_up)->_eoc) {
			((_Space *) cp->_up)->_boc = NIL;
			((_Space *) cp->_up)->_eoc = NIL;
		} else {
			if (((_Space *) cp->_up)->_boc == (_Node *) cp)
				((_Space *) cp->_up)->_boc = cp->_rt;
			if (((_Space *) cp->_up)->_eoc == (_Node *) cp)
				((_Space *) cp->_up)->_eoc = cp->_lt;
		}
		break;
	default:
		msg(STDERR, "Wrong IRi node type %d", cp->_up->nd_type);
		break;
	}
}

		
ir_delete (fid, len, data)
	char		*data;
{
	u_long		flag;
	u_long		offset;
	u_long		count;

	flag = ntohl(*(u_long *) data);
	data += U_LONG;
	offset = ntohl(*(long *) data);
	data += U_LONG;
	count = ntohl(*(u_long *) data);
	data += U_LONG;

#ifdef _IRF
	msg(STDERR, "DELETE : fid=%d, flag=%d, offset=%d, count=%d", fid, flag, offset, count);
#endif

	ir_find_point(fid, flag, offset);
	ir_delete_chars(file_update, count);
#ifdef _IRF
	irs_traverse();
#endif
}

char
ir_getc ()
{
	char		c;

	if (irs_eol->_ch == EOF)
		return(EOF);
	c = irs_eol->_ch;
/*
	fprintf(stderr, "%c", c);
*/
	irs_eol = (_Char *) irs_eol->_rt;
	return(c);
}

#endif VORTEX
