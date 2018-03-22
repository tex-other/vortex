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
 *  RCS Info: $Header: proofcmds.c,v 0.1 87/05/01 12:25:36 john Locked $
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
 *  proofcmds.c - proof editor commands (vLisp functions)
 */
static char _ID[] = "@(#)proofcmds.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"
#include "document.h"
#include "gl_comm.h"
#include "ps_comm.h"
#include "ts_comm.h"

/*
 *  DOCUMENTATION
 *
 *  Name: proof-editor-host
 *  Desc: This variable holds the default host name on which to
 *	start the proof editor.  At startup, this is set to
 *	\lit{"localhost"}, which specifies that the proof editor
 *	be run on the current machine.
 *  SeeA: start-proof-editor formatter-host
 */
MKSTRING(PROOFHOST_NAME, "proof-editor-host");

/*
 *  DOCUMENTATION
 *
 *  Name: proof-editor-port
 *  Desc: This variable holds the internet port number at which
 *	to establish the proof editor connection.  At startup, this
 *	is set to an installation dependant value according to the
 *	local network configuration.
 *  SeeA: start-proof-editor formatter-port
 */
MKSTRING(PROOFPORT_NAME, "proof-editor-port");

/*
 *  DOCUMENTATION
 *
 *  Name: proof-editor-program
 *  Desc: This variable holds the full path name of the program
 *	run to start the proof editor.  At startup, this is set to
 *	an installation dependant binary file.
 *  SeeA: start-proof-editor formatter-program
 */
MKSTRING(PROOFPROG_NAME, "proof-editor-program");

/*
 *  DOCUMENTATION
 *
 *  Name: start-proof-editor
 *  Call: (start-proof-editor [ 'hostname ])
 *  Retu: t or nil
 *  Desc: This function starts up the proof editor on the specified
 *	host if necessary and connects to it.  The function returns
 *	t if a sucessful connection was made and nil otherwise.  If
 *	there is a serious low-level communications failure, an
 *	error may occur.
 *
 *	If the proof editor is already running when this function
 *	is called, the current proof editor is killed (as with
 *	\sym{kill-proof-editor}) and a new one is started.
 *  Side: If we're already connected to the formatter and a sucessful
 *	connection is made to the proof editor, a connection between
 *	the formatter and the proof editor is established.
 *
 *	Note that if the host name is unspecified, the value (a
 *	string host name) is gotten from \sym{proof-editor-host}.
 *	used.  The program name (a string UNIX path name) and the
 *	port number (a fixnum) are used from the global variables
 *	\sym{proof-editor-program} and \sym{proof-editor-port}.
 *  SeeA: kill-proof-editor start-formatter proof-editor-host
 *	proof-editor-port proof-editor-program
 */

DEFUN(dostartproof, "start-proof-editor", FLAG_NONE, "")
{
	extern int	proof_socket;
	struct value	arg;
	char		*host = NULL, hbuf[64];

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string host name");
		makecstring(gstring(arg.vl_data), hbuf, sizeof (hbuf));
		host = hbuf;
	}
	if (proof_socket > 0)
		quitproof(0);

	if (startproof(host) != 0)
		return (v_nil);
	else
		return (v_t);
}

startproof(hostname)
	char	*hostname;
{
	extern int	proof_socket, format_socket;
	struct value	arg;
	char		host[64], prog[1024];
	int		conn, port;

	if (proof_socket > 0)
		error("You already have a proof editor running!");

	if (hostname == NULL) {
		arg = get_variable(PROOFHOST_NAME, NULL);
		if (eq(arg, NOVALUE))
			error("%Y is unset and no hostname was specified!",
			      PROOFHOST_NAME);
		if (!stringp(arg))
			error("%Y is not set to a string host name!",
			      PROOFHOST_NAME);
		makecstring(gstring(arg.vl_data), host, sizeof (host));
		hostname = host;
	}
	if (*hostname == '\0')
		error("Empty string specified for proof editor host name!");

	arg = get_variable(PROOFPORT_NAME, NULL);
	if (eq(arg, NOVALUE))
		error("Variable %Y is unset; have no port!", PROOFHOST_NAME);
	if (!fixnump(arg))
		error("%Y is not set to a fixnum port!", PROOFHOST_NAME);
	port = gfixnum(arg.vl_data);
	if (port < 1024)
		error("Invalid port number %d for proof editor.", port);

	arg = get_variable(PROOFPROG_NAME, NULL);
	if (eq(arg, NOVALUE))
		error("Variable %Y is unset; no program!", PROOFPROG_NAME);
	if (!stringp(arg))
		error("%Y is not set to a string file name!", PROOFPROG_NAME);
	makecstring(gstring(arg.vl_data), prog, sizeof (prog));
	if (*prog == '\0')
		error("Empty string specified for proof editor program!");

	/* establish a connection with the proof editor */
	conn = connproof(prog, hostname, port);
	if (conn < 0)
		return (-1);

	/* tell proof editor and formatter to rendezvous */
	if (format_socket > 0)
		connection();

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: kill-proof-editor
 *  Call: (kill-proof-editor)
 *  Retu: nil
 *  Desc: This functions kills the proof editor if one is running.
 *	if no proof editor is running, this function does nothing.
 *  Side: If a formatter is running, it loses its connection to
 *	the proof editor also.
 *  SeeA: start-proof-editor kill-formatter
 */

DEFUN(dokillproof, "kill-proof-editor", FLAG_NONE, "")
{
	extern int	proof_socket;

	if (proof_socket <= 0)
		return (v_nil);

	quitproof(0);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-document
 *  Call: (proof-document 'document [ 'pageno ])
 *  Retu: pageno
 *  Desc: This function creates a proof window on the given document
 *	at the specified physical pages, or at page one if none is
 *	specified.
 *  Side: If a proof editor is not running when this function is
 *	called, one is started.  See \sym{start-proof-editor} for
 *	more details on this.
 *  SeeA: make-document start-proof-editor
 */

DEFUN(doproofdoc, "proof-document", FLAG_NONE, "D\np")
{
	struct window	*makeproof();
	struct value	arg, ret;
	int		docid, page = 1;
	struct document	*docp;
	struct window	*winp;

	if (proof_socket <= 0 && startproof(NULL) != 0)
		error("Failed to start proof editor; can't proof documents.");

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a document number");
	docid = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (fixnump(arg))
			page = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(2, "a page number");
		if (page <= 0)
			error("Invalid physical page number %d!", page);
	}

	if ((docp = getdocument(docid)) == NULL)
		error("Invalid document ID %d to proof.", docid);

	/* make a proof buffer window */
	winp = makeproof(docp, page);
	ASSERT(winp != NULL);
	debug(DPROOF, "Window %d assigned to new proof buffer (page %d).",
	      winp->wi_index, page);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, page);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-goto-page
 *  Call: (proof-goto-page 'pageno [ 'window ])
 *  Retu: pageno
 *  Desc: This function changes the page being viewed in the current
 *	window (or the window specified by the second argument if there
 *	is one) to the specified physical page number.
 *
 *	The physical page number has nothing to do with the page number
 *	TeX prints on the bottom of each page, it refers to the order
 *	of the pages as laid out in the document.  The first page
 *	formatter is page one, the second page two.
 *
 *	If the specified page does not exist, the closest possible
 *	page is gone to.  Thus, both zero and one go to the first
 *	page and numbers greater than the length of the document go
 *	to the end.
 *  SeeA: proof-next-page proof-logical-page proof-document
 */

DEFUN(doprfgotopage, "proof-goto-page", FLAG_NONE, "nGoto page: ")
{
	struct value	arg, ret;
	int		num;
	unsigned long	pageno;
	struct window	*winp = current_window;
	struct buffer	*bufp;
	struct proof	*prfp;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum page number");
	num = gfixnum(arg.vl_data);
	if (num < 1)
		num = 1;

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a window index");
		winp = getwindow(gfixnum(arg.vl_data));
	}
	bufp = windowbuffer(winp, BUFF_PROOF);

	/* get the proof buffer data */
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* send off the request to the proof editor */
	pageno = htonl(num);
	sendproof(PSC_GOTOABS, winp->wi_xwindow,
		  sizeof (pageno), (char *)&pageno);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, num);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-next-page
 *  Call: (proof-next-page 'count [ 'window ])
 *  Retu: pageno
 *  Desc: This function changes the page being viewed in the current
 *	window (or the window specified by the second argument if there
 *	is one) forward or backward by the specified count.
 *
 *	The physical page number has nothing to do with the page number
 *	TeX prints on the bottom of each page, it refers to the order
 *	of the pages as laid out in the document.  The first page
 *	formatter is page one, the second page two.
 *  SeeA: proof-goto-page proof-logical-page proof-document
 */

DEFUN(doprfnextpage, "proof-next-page", FLAG_NONE, "p")
{
	struct value	arg, ret;
	int		num;
	long		count;
	struct window	*winp = current_window;
	struct buffer	*bufp;
	struct proof	*prfp;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum count");
	num = gfixnum(arg.vl_data);
	if (num < 1)
		num = 1;

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!fixnump(arg))
			BADARGN(2, "a window index");
		winp = getwindow(gfixnum(arg.vl_data));
	}
	bufp = windowbuffer(winp, BUFF_PROOF);

	/* get the proof buffer data */
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* send off the request to the proof editor */
	count = htonl(num);
	sendproof(PSC_GOTOREL, winp->wi_xwindow,
		  sizeof (count), (char *)&count);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, num);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-move-absolute
 *  Call: (proof-move-absolute 'xpos 'ypos [ 'window ])
 *  Retu: nil
 *  Desc: This function moves the current window (or the window
 *	specified by the second argument if there is one) to the
 *	given proof editor position.
 *
 *	A proof buffer window position is specified in units of
 *	screen pixels.  These routines are normally not needed
 *	by the user.  You should probably be using \sym{proof-goto-line}.
 *  SeeA: proof-goto-line proof-move-relative proof-document
 */

DEFUN(doprfmoveabs, "proof-move-absolute", FLAG_NONE, "nMove to X: \nnY: ")
{
	struct value	arg, ret;
	int		xpos, ypos;
	short		coords[2];
	struct window	*winp = current_window;
	struct buffer	*bufp;
	struct proof	*prfp;

	CHECKAC(2, 3);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum position");
	xpos = gfixnum(arg.vl_data);
	arg = EVALARGN(2);
	if (!fixnump(arg))
		BADARGN(2, "a fixnum position");
	ypos = gfixnum(arg.vl_data);

	if (GETACOUNT() > 2) {
		arg = EVALARGN(3);
		if (!fixnump(arg))
			BADARGN(3, "a window index");
		winp = getwindow(gfixnum(arg.vl_data));
	}
	bufp = windowbuffer(winp, BUFF_PROOF);

	/* get the proof buffer data */
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* send off the request to the proof editor */
	coords[0] = htons(xpos);
	coords[1] = htons(ypos);
	sendproof(PSC_MOVEABS, winp->wi_xwindow,
		  sizeof (coords), (char *)coords);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-move-relative
 *  Call: (proof-move-relative 'xpos 'ypos [ 'window ])
 *  Retu: nil
 *  Desc: This function moves the current window (or the window
 *	specified by the second argument if there is one) in the
 *	given proof editor relative to the current position.
 *
 *	A proof buffer window position is specified in units of
 *	screen pixels.  These routines are normally not needed
 *	by the user.  You should probably be using
 *	\sym{proof-next-line}.
 *  SeeA: proof-next-line proof-move-absolute proof-document
 */

DEFUN(doprfmoverel, "proof-move-relative", FLAG_NONE, "nMove by X: \nnY: ")
{
	struct value	arg, ret;
	int		xpos, ypos;
	short		coords[2];
	struct window	*winp = current_window;
	struct buffer	*bufp;
	struct proof	*prfp;

	CHECKAC(2, 3);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum offset");
	xpos = gfixnum(arg.vl_data);
	arg = EVALARGN(2);
	if (!fixnump(arg))
		BADARGN(2, "a fixnum offset");
	ypos = gfixnum(arg.vl_data);

	if (GETACOUNT() > 2) {
		arg = EVALARGN(3);
		if (!fixnump(arg))
			BADARGN(3, "a window number");
		winp = getwindow(gfixnum(arg.vl_data));
	}
	bufp = windowbuffer(winp, BUFF_PROOF);

	/* get the proof buffer data */
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* send off the request to the proof editor */
	coords[0] = htons(xpos);
	coords[1] = htons(ypos);
	sendproof(PSC_MOVEREL, winp->wi_xwindow,
		  sizeof (coords), (char *)coords);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-select
 *  Call: (proof-select)
 *  Retu: nil
 *  Desc: This function causes the area under or the current mouse
 *	position (it must be invoked interactively) to be selected
 *	at the next higher level of structure.
 *  SeeA: proof-select-more proof-document
 */

DEFUN(doprfselect, "proof-select", FUNC_VISUAL, "")
{
	extern int	mouse_window;
	extern int	mouse_Xpos, mouse_Ypos;
	struct value	arg, ret;
	unsigned short	coords[2];
	struct window	*winp;
	struct buffer	*bufp;
	struct proof	*prfp;

	/* make sure a button or key was pressed */
	CHECKAC(0, 0);
	if (mouse_window < 0)
		error("No position for selection; is this bound to a key?");

	/* get the window used by last input event */
	winp = getwindow(mouse_window);
	ASSERT(winp != NULL);
	bufp = windowbuffer(winp, BUFF_PROOF);

	/* get the proof buffer data */
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* send off the request to the proof editor */
	coords[0] = htons(mouse_Xpos);
	coords[1] = htons(mouse_Ypos);
	debug(DPROOF, "Selecting at %d, %d on proof window %d.",
	      mouse_Xpos, mouse_Ypos, winp->wi_index);
	sendproof(PSC_SELECT, winp->wi_xwindow,
		  sizeof (coords), (char *)coords);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-select-more
 *  Call: (proof-select-more)
 *  Retu: nil
 *  Desc: This function completes the selection started with
 *	\sym{proof-select} to include the area under the
 *	mouse cursor (it must be invoked interactively).  All
 *	structure at the level established at the start of the
 *	selection through the area under the mouse becomes the
 *	current proof editor selection.
 *  SeeA: proof-select
 */

DEFUN(doselmore, "proof-select-more", FUNC_VISUAL, "")
{
	extern int	mouse_window;
	extern int	mouse_Xpos, mouse_Ypos;
	struct value	arg, ret;
	unsigned short	coords[2];
	struct window	*winp;
	struct buffer	*bufp;
	struct proof	*prfp;

	/* make sure a button or key was pressed */
	CHECKAC(0, 0);
	if (mouse_window < 0)
		error("No position for selection; is this bound to a key?");

	/* get the window used by last input event */
	winp = getwindow(mouse_window);
	ASSERT(winp != NULL);
	bufp = windowbuffer(winp, BUFF_PROOF);

	/* get the proof buffer data */
	prfp = bufp->bu_pdata;
	ASSERT(prfp != NULL);

	/* send off the request to the proof editor */
	coords[0] = htons(mouse_Xpos);
	coords[1] = htons(mouse_Ypos);
	debug(DPROOF, "Extending selection to %d, %d on window %d.",
	      mouse_Xpos, mouse_Ypos, winp->wi_index);
	sendproof(PSC_SELECTMORE, winp->wi_xwindow,
		  sizeof (coords), (char *)coords);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-selected-region
 *  Call: (proof-selected-region)
 *  Retu: list
 *  Desc: This function returns the region selected by the user
 *	using the proof editor.  The first element in the list is
 *	the source buffer name, the other two are offsets into the
 *	buffer defining the region.
 *
 *	If no region has been selected with the proof editor, this
 *	function returns nil.
 *  SeeA: proof-select
 */
extern struct buffer	*select_buffer;
extern unsigned long	select_first, select_last;

DEFUN(doprfselregion, "proof-selected-region", FLAG_NONE, "")
{
	struct value	buf, beg, end;

	CHECKAC(0, 0);

	/* do we have a selection? */
	if (select_buffer == NULL)
		return (v_nil);

	/* make a list out of these parts */
	buf.vl_type = LISP_STRING;
	sstring(buf.vl_data, select_buffer->bu_name);
	beg.vl_type = LISP_FIXNUM;
	sfixnum(beg.vl_data, select_first);
	end.vl_type = LISP_FIXNUM;
	sfixnum(end.vl_data, select_last);
	return cons(buf, cons(beg, cons(end, v_nil)));
}

/*
 *  DOCUMENTATION
 *
 *  Name: proof-moveto
 *  Call: (proof-moveto 'offset [ 'buffer [ 'window ] ])
 *  Retu: offset
 *  Desc: This function scrolls the specified proof window to the
 *	given offset in the buffer.  If no buffer and window are
 *	specified, the last used source and proof window buffers
 *	are used.
 *  SeeA: proof-select
 */
#define TIMEOUT	10

DEFUN(doprfmoveto, "proof-moveto", FUNC_VISUAL, ".")
{
	struct window	*curprfwindow(), *cursrcwindow();
	struct value	arg;
	unsigned long	args[2], posn, box;
	struct window	*swin, *pwin;
	struct buffer	*bufp;
	struct source	*srcp;
	int		len, fileID;
	char		*data;

	CHECKAC(1, 3);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a fixnum position");
	posn = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a buffer name string");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	} else {
		swin = cursrcwindow();
		bufp = swin->wi_buffer;
	}
	ASSERT(bufp != NULL);
	if (bufp->bu_type != BUFF_SOURCE ||
	    (bufp->bu_flags & BUFF_TEXFILE) == 0)
		error("Second argument must be a TeX source buffer.");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	if (GETACOUNT() > 2) {
		arg = EVALARGN(3);
		if (!fixnump(arg))
			BADARGN(3, "a window number");
		pwin = getwindow(gfixnum(arg.vl_data));
	} else {
		pwin = curprfwindow();
	}
	if (pwin == NULL || pwin->wi_type != WIN_BUFFER ||
	    pwin->wi_buffer->bu_type != BUFF_PROOF)
		error("Third argument must specify a proof buffer window.");

	/* now get the source buffer info. from the formatter */
	debug(DITEX, "Translating buffer %S, offset %d for proof editor...",
	      bufp->bu_name, posn);
	args[0] = htonl(TS_SRC2TGT);
	args[1] = htonl(posn);
	sendformat(TSC_EXECUTE, srcp->sb_fileID, sizeof (args), (char *)args);
	if (waitformat(TSC_RETURN, NULL, &len, &data, TIMEOUT) == 0)
		error("Position query timed out; %s failed!", GETFNAME());
	if (len != sizeof (unsigned long) || data == NULL)
		ierror("Return from position query has wrong length!");
	box = ntohl((unsigned long *)data);

	debug(DPROOF, "Buffer %S, offset %d translated to box ID %d.",
	      bufp->bu_name, posn, box);
	sendproof(PSC_POSITION, pwin->wi_xwindow, len, data);

	return (v_nil);
}
