/* 
 * Copyright (c) 1987 The Regents of the University of California.
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
 *  RCS Info: $Header: proof.c,v 0.1 87/05/01 12:25:19 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the source editor/user interface written
 *  by John Coker for the VorTeX project under the direction of
 *  Prof. Michael A. Harrison of the University of California at
 *  Berkeley.
 *
 *  Copyright (c) 1987 John L. Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  John Coker and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  proof.c - proof buffer management routines
 */
static char _ID[] = "@(#)proof.c for VorTeX, Copyright (c) 1987 John Coker";

#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"
#include "document.h"
#include "gl_comm.h"
#include "ps_comm.h"

extern int		text_input();		/* handle an input key code */

extern int		dvi_resize(),		/* handle a resize event */
			dvi_expose(),		/* update exposed area */
			dvi_destroy(),		/* destroy a proof buffer */
			dvi_open(),		/* edit this buffer */
			dvi_close();		/* no longer editing it */
extern unsigned char	*dvi_modeline();	/* format a mode line */

proof_init(bufp)
	struct buffer	*bufp;
{
	struct proof	*prfp;

	/* paranoia may come in handy some day... */
	ASSERT(bufp->bu_type == BUFF_PROOF);

	/* all proof buffer have same handlers */
	bufp->bu_input = text_input;
	bufp->bu_resize = dvi_resize;
	bufp->bu_expose = dvi_expose;
	bufp->bu_paint = NULL;
	bufp->bu_mline = dvi_modeline;
	bufp->bu_event = NULL;
	bufp->bu_destroy = dvi_destroy;
	bufp->bu_open = dvi_open;
	bufp->bu_close = dvi_close;

	if (bufp->bu_sdata != NULL) {
		vfree(bufp->bu_sdata);
		bufp->bu_sdata = NULL;
	}
	if (bufp->bu_pdata == NULL) {
		prfp = (struct proof *)valloc(sizeof (struct proof));
		bufp->bu_pdata = prfp;
	} else {
		/* use the already existing one */
		prfp = bufp->bu_pdata;
	}
	bzero(prfp, sizeof (struct proof));

	return (0);
}

dvi_resize(winp)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct proof	*prfp;
	unsigned short	rect[4];

	bufp = winp->wi_buffer;
	ASSERT(bufp != NULL);
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* make up display area rectangle */
	rect[0] = htons(winp->wi_left);
	rect[1] = htons(winp->wi_top);
	rect[2] = htons(winp->wi_width);
	rect[3] = htons(winp->wi_height);

	/* tell the proof editor about the new window size */
	sendproof(PSC_RESIZE, winp->wi_xwindow, sizeof (rect), (char *)rect);

	return (0);
}

dvi_expose(winp, rect)
	struct window	*winp;
	struct rect	*rect;
{
	unsigned short	parms[4];

	parms[0] = htons(rect->re_left);
	parms[1] = htons(rect->re_top);
	parms[2] = htons(rect->re_width);
	parms[3] = htons(rect->re_height);

	sendproof(PSC_EXPOSE, winp->wi_xwindow, sizeof (parms), (char *)parms);

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-buffer-destroy-hook
 *  Desc: This variable may be set globally to a function which will
 *	be called each time a proof buffer is destroyed.
 *
 *	The function will be called with one argument, the name of
 *	the buffer being created (not necessarily the current buffer).
 *  Side: This function should not generate any errors as it may be
 *	called at a sensitive time.
 *  SeeA: proof-document kill-proof-editor proof-buffer-create-hook
 */
MKSTRING(PROOF_DESTROY, "proof-buffer-destroy-hook");

dvi_destroy(bufp)
	struct buffer	*bufp;
{
	struct value	hook;
	struct window	*winp;
	struct proof	*prfp;

	/* run the user's proof-buffer-destroy-hook if applicable */
	hook = get_variable(PROOF_DESTROY, bufp);
	if (funcp(hook)) {
		struct value	name, bname;

		/* run the given function with the buffer name */
		name.vl_type = LISP_SYMBOL;
		ssymbol(name.vl_data, save_symbol(PROOF_DESTROY));
		bname.vl_type = LISP_STRING;
		sstring(bname.vl_data, bufp->bu_name);
		call_function(name, hook, bname);
	}

	PROTECT();
	/* destroy the proof buffer specific data */
	if ((prfp = bufp->bu_pdata) != NULL) {
		vfree(prfp);
		bufp->bu_pdata = NULL;
	}

	/* kill all windows which were looking at this buffer */
	forallbwins(winp) {
		if (winp->wi_buffer == bufp)
			switchbuffer(winp, buffer_list);
	}
	UNPROTECT();

	return (0);
}

dvi_open(winp)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct proof	*prfp;
	unsigned short	rect[4];
	unsigned long	pxls[6];
	unsigned char	data[sizeof (rect) + sizeof (pxls)];
	long		docid, pageno;
	short		pos[2];

	/* make sure we can do this */
	if ((bufp = winp->wi_buffer) == NULL || bufp->bu_type != BUFF_PROOF)
		return (1);
	if ((prfp = bufp->bu_pdata) == NULL)
		return (1);

	/* tell the proof editor about this window */
	rect[0] = htons(winp->wi_left);
	rect[1] = htons(winp->wi_top);
	rect[2] = htons(winp->wi_width);
	rect[3] = htons(winp->wi_height);
	debug(DPROOF, "New proof window %d has area =%dx%d+%d+%d.",
	      winp->wi_index,
	      winp->wi_width, winp->wi_height, winp->wi_left, winp->wi_top);

	/* make up the pixel and pixmap values */
	pxls[0] = htonl(winp->wi_fgpixel);
	pxls[1] = NULL;
	pxls[2] = htonl(winp->wi_bgpixel);
	pxls[3] = NULL;
	pxls[4] = htonl(winp->wi_hlpixel);
	pxls[5] = NULL;

	/* send the initial window information */
	bcopy((char *)rect, data, sizeof (rect));
	bcopy((char *)pxls, data + sizeof (rect), sizeof (pxls));
	sendproof(PSC_CREATE, winp->wi_xwindow, sizeof (data), data);

	/* send the messages to open the window and proof this file */
	sendproof(PSC_BSTART, winp->wi_xwindow, 0, NULL);

	docid = htonl(prfp->pb_document);
	sendproof(PSC_DOCUMENT, winp->wi_xwindow,
		  sizeof (docid), (char *)&docid);

	pageno = htonl(prfp->pb_curpage);
	sendproof(PSC_GOTOABS, winp->wi_xwindow,
		  sizeof (pageno), (char *)&pageno);

	pos[0] = pos[1] = 0;
	sendproof(PSC_MOVEABS, winp->wi_xwindow, sizeof (pos), (char *)pos);

	sendproof(PSC_BEND, winp->wi_xwindow, 0, NULL);

	return (0);
}

dvi_close(winp)
	struct window	*winp;
{
	struct buffer	*bufp;

	/* tell the proof editor this window is gone */
	if ((bufp = winp->wi_buffer) != NULL && bufp->bu_type == BUFF_PROOF) {
		sendproof(PSC_DESTROY, winp->wi_xwindow, 0, NULL);
		debug(DPROOF, "Closed proof window %d.", winp->wi_index);
	}

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-buffer-create-hook
 *  Desc: This variable may be set globally to a function which will
 *	be called each time a proof buffer is created.  Its primary
 *	use is for setting up special variables and key bindings for
 *	proof buffer editing.
 *
 *	The function will be called with one argument, the name of
 *	the buffer being created (not necessarily the current buffer).
 *  Side: This function should not generate any errors as it may be
 *	called at a sensitive time.
 *  SeeA: proof-document start-proof-editor proof-buffer-destroy-hook
 */
MKSTRING(PROOF_CREATE, "proof-buffer-create-hook");

struct window *
makeproof(docp, page)
	struct document	*docp;
{
	extern char	*PROOFGEOMETRY;
	extern int	proof_socket;
	struct value	hook;
	struct window	*winp;
	struct buffer	*bufp;
	struct proof	*prfp;
	char		nbuf[100];
	register int	n;
	struct string	*name;

	ASSERT(proof_socket > 0);
	ASSERT(docp != NULL);
	if (page < 1)
		page = 1;

	/* find an unused name for this buffer */
	strcpy(nbuf, "*proof*");
	name = save_string(nbuf, strlen(nbuf));
	for (n = 2; (bufp = buffer_get(name, FALSE)) != NULL; n++) {
		sprintf(nbuf, "*proof<%d>*", n);
		name = save_string(nbuf, strlen(nbuf));
	}

	/* make the new proof buffer */
	bufp = buffer_create(name, BUFF_PROOF, FLAG_NONE);
	ASSERT(bufp != NULL);
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* set up the given values in proof info. */
	prfp->pb_curpage = page;
	prfp->pb_document = docp->dc_rootID;

	/* make the X window for the proof buffer */
	winp = makewindow(PROOFGEOMETRY, NULL, bufp);

	/* run the user's proof-buffer-create-hook if applicable */
	hook = get_variable(PROOF_CREATE, bufp);
	if (funcp(hook)) {
		struct value	name, bname;

		/* run the given function with the buffer name */
		name.vl_type = LISP_SYMBOL;
		ssymbol(name.vl_data, save_symbol(PROOF_CREATE));
		bname.vl_type = LISP_STRING;
		sstring(bname.vl_data, bufp->bu_name);
		call_function(name, hook, bname);
	}

	return (winp);
}
