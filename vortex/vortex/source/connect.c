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
 *  RCS Info: $Header$
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
 *  connect.c - connect proof editor and formatter
 */
static char _ID[] = "@(#)connect.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "document.h"
#include "gl_comm.h"

/*
 *  DOCUMENTATION
 *
 *  Name: connection-port
 *  Desc: This variable holds the port number at which the proof
 *	editor and formatter will try to rendezvous.  When the
 *	second of the two other processes starts, it will attempt
 *	to connect to the other at this port.
 *  SeeA: start-proof-editor start-formatter
 */
MKSTRING(CONNPORT_NAME, "connection-port");

/*
 *  DOCUMENTATION
 *
 *  Name: make-connection
 *  Call: (make-connection)
 *  Retu: t or nil
 *  Desc: This function causes a connection between the proof editor
 *	and the formatter to be established.  Both of these remote
 *	programs must have already been started.
 *  Side: Generally, this command is not needed, since when the
 *	second process starts up, it will attempt to connect to
 *	the other automatically.
 *  SeeA: start-proof-editor start-formatter connection-port
 */

DEFUN(domakeconn, "make-connection", FLAG_NONE, "")
{
	CHECKAC(0, 0);

	if (connection() == 0)
		return (v_t);
	else
		return (v_nil);
}

#define PROOF_LISTEN

connection()
{
	extern int	format_socket, proof_socket;
	struct value	arg;
	int		port;
	unsigned long	data[2];

	if (proof_socket <= 0)
		error("Can't connect them; proof editor isn't running!");
	if (format_socket <= 0)
		error("Can't connect them; formatter isn't running!");

	arg = get_variable(CONNPORT_NAME, NULL);
	if (eq(arg, NOVALUE))
		error("Variable %Y is unset; have no port!", CONNPORT_NAME);
	if (!fixnump(arg))
		error("%Y is not set to a fixnum port!", CONNPORT_NAME);
	port = gfixnum(arg.vl_data);
	if (port < 1024)
		error("Invalid port number %d for connction.", port);

	data[0] = htonl(port);
	data[1] = htonl(LISTEN_TIMEOUT);
#ifdef PROOF_LISTEN
	message("Connecting formatter to proof editor...");
	sendproof(GLC_LISTENAT, NULL, sizeof (data), (char *)data);
#else !PROOF_LISTEN
	message("Connecting proof editor to formatter...");
	sendformat(GLC_LISTENAT, ANYFILE, sizeof (data), (char *)data);
#endif PROOF_LISTEN

	return (0);
}

