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
 *  RCS Info: $Header: psinfo.h,v 0.1 87/04/30 20:55:05 john Locked $
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
 *  psinfo.h - proof editor communications table
 */
 
#ifndef _PSINFO_
#define _PSINFO_

/*
 *  We maintain a table of all request which may be sent or received
 *  on the source/proof editor connection.  Those which we expect to
 *  receive have a handler function to do whatever should be done.
 */
struct psinfo {
	char		*ps_name;	/* name of request for printing */
	int		ps_code;	/* request code for pk_request */
	int		(*ps_handle)();	/* function to handle request */
};

extern int	ps_flush(),
		ps_listen(),
		ps_quit(),
		ps_abort(),
		ps_error(),
		ps_bstart(),
		ps_bend(),
#ifdef notyet
		ps_newview(),
#endif notyet
		ps_docpages(),
		ps_selection();

static struct psinfo	psinfo_table[] = {
	{ "INVALID",	0,		NULL },
	{ "VERIFY",	GLC_VERIFY,	NULL },
	{ "GOAWAY",	GLC_GOAWAY,	NULL },
	{ "WELCOME",	GLC_WELCOME,	NULL },
	{ "LISTENAT",	GLC_LISTENAT,	NULL },
	{ "LISTENING",	GLC_LISTENING,	ps_listen },
	{ "CONNECT",	GLC_CONNECT,	NULL },
	{ "FLUSH",	GLC_FLUSH,	ps_flush },
	{ "QUIT",	GLC_QUIT,	ps_quit },
	{ "ABORT",	GLC_ABORT,	ps_abort },
	{ "ERROR",	GLC_ERROR,	ps_error },
	{ "CREATE",	PSC_CREATE,	NULL, },
	{ "DESTROY",	PSC_DESTROY,	NULL, },
	{ "RESIZE",	PSC_RESIZE,	NULL, },
	{ "EXPOSE",	PSC_EXPOSE,	NULL, },
	{ "BSTART",	PSC_BSTART,	ps_bstart },
	{ "BEND",	PSC_BEND,	ps_bend },
	{ "MOVEABS",	PSC_MOVEABS,	NULL, },
	{ "MOVEREL",	PSC_MOVEREL,	NULL, },
#ifdef notyet
	{ "NEWVIEW",	PSC_NEWVIEW,	ps_newview },
#endif notyet
	{ "GOTOABS",	PSC_GOTOABS,	NULL, },
	{ "GOTOREL",	PSC_GOTOREL,	NULL, },
	{ "LOGICAL",	PSC_LOGICAL,	NULL, },
	{ "DOCUMENT",	PSC_DOCUMENT,	NULL, },
	{ "DOCPAGES",	PSC_DOCPAGES,	ps_docpages },
	{ "SELECT",	PSC_SELECT,	NULL, },
	{ "SELECTMORE",	PSC_SELECTMORE,	NULL, },
	{ "SELECTION",	PSC_SELECTION,	ps_selection, },
	{ "POSITION",	PSC_POSITION,	NULL, },
};
static int	psinfo_count = sizeof (psinfo_table) / sizeof (*psinfo_table);

extern struct psinfo	*proofinfo();

#endif !_PSINFO_
