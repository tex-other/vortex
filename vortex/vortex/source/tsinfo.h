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
 *  RCS Info: $Header: tsinfo.h,v 0.1 87/04/30 20:55:35 john Locked $
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
 *  tsinfo.h - formatter communications table
 */
 
#ifndef _TSINFO_
#define _TSINFO_

/*
 *  We maintain a table of all request which may be sent or received
 *  on the source/formatter connection.  Those which we expect to
 *  receive have a handler function to do whatever should be done.
 */
struct tsinfo {
	char		*ts_name;	/* name of request for printing */
	int		ts_code;	/* request code for pk_request */
	int		(*ts_handle)();	/* function to handle request */
};

extern int	ts_flush(),		/* flush data on this channel */
		ts_listen(),		/* return port listening at */
		ts_quit(),		/* peer is quitting */
		ts_abort(),		/* peer is aborting */
		ts_error(),		/* peer reports an error */
		ts_closedoc(),		/* close a document */
		ts_closefile(),		/* close a document file */
		ts_input(),		/* request an input file */
		ts_output(),		/* request an output file */
		ts_message(),		/* request a user message */
		ts_texerr();		/* TeX error occurred */

static struct tsinfo	tsinfo_table[] = {
	{ NULL,		0,		NULL },
	{ "VERIFY",	GLC_VERIFY,	NULL, },
	{ "GOAWAY",	GLC_GOAWAY,	NULL, },
	{ "WELCOME",	GLC_WELCOME,	NULL, },
	{ "LISTENAT",	GLC_LISTENAT,	NULL, },
	{ "LISTENING",	GLC_LISTENING,	ts_listen, },
	{ "CONNECT",	GLC_CONNECT,	NULL, },
	{ "FLUSH",	GLC_FLUSH,	ts_flush },
	{ "QUIT",	GLC_QUIT,	ts_quit },
	{ "ABORT",	GLC_ABORT,	ts_abort },
	{ "ERROR",	GLC_ERROR,	ts_error },
	{ "FORMAT",	TSC_FORMAT,	NULL, },
	{ "CLOSEDOC",	TSC_CLOSEDOC,	ts_closedoc, },
	{ "OPENFILE",	TSC_OPENFILE,	NULL, },
	{ "CLOSEFILE",	TSC_CLOSEFILE,	ts_closefile, },
	{ "INSERT",	TSC_INSERT,	NULL, },
	{ "DELETE",	TSC_DELETE,	NULL, },
	{ "TEXINPUT",	TSC_TEXINPUT,	ts_input },
	{ "TEXOUTPUT",	TSC_TEXOUTPUT,	ts_output },
	{ "TEXMESSAGE",	TSC_TEXMESSAGE,	ts_message },
	{ "TEXERROR",	TSC_TEXERROR,	ts_texerr },
	{ "EXECUTE",	TSC_EXECUTE,	NULL },
	{ "RETURN",	TSC_RETURN,	NULL }
};
static int	tsinfo_count = sizeof (tsinfo_table) / sizeof (*tsinfo_table);

extern struct tsinfo	*formatinfo();

#endif !_TSINFO_
