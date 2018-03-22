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
 *  RCS Info: $Header: formatcmds.c,v 0.1 87/05/01 12:14:49 john Locked $
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
 *  formatcmds.c - basic formatter commands (vLisp functions)
 */
static char _ID[] = "@(#)formatcmds.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "document.h"
#include "gl_comm.h"
#include "ts_comm.h"

/*
 *  DOCUMENTATION
 *
 *  Name: formatter-host
 *  Desc: This variable holds the default host name on which to
 *	start the formatter.  At startup, this is set to
 *	\lit{"localhost"}, which specifies that the formatter
 *	be run on the current machine.
 *  SeeA: start-formatter proof-editor-host
 */
MKSTRING(FORMATHOST_NAME, "formatter-host");

/*
 *  DOCUMENTATION
 *
 *  Name: formatter-port
 *  Desc: This variable holds the internet port number at which
 *	to establish the formatter connection.  At startup, this
 *	is set to an installation dependant value according to the
 *	local network configuration.
 *  SeeA: start-formatter proof-editor-port
 */
MKSTRING(FORMATPORT_NAME, "formatter-port");

/*
 *  DOCUMENTATION
 *
 *  Name: formatter-program
 *  Desc: This variable holds the full path name of the program
 *	run to start the formatter.  At startup, this is set to
 *	an installation dependant binary file.
 *  SeeA: start-formatter proof-editor-program
 */
MKSTRING(FORMATPROG_NAME, "formatter-program");

/*
 *  DOCUMENTATION
 *
 *  Name: start-formatter
 *  Call: (start-formatter [ 'hostname ])
 *  Retu: t or nil
 *  Desc: This function starts up the formatter on the specified
 *	host if necessary and connects to it.  The function returns
 *	t if a sucessful connection was made and nil otherwise.  If
 *	there is a serious low-level communications failure, an
 *	error may occur.
 *  Side: If we're already connected to the proof editor and a
 *	sucessful connection is made to the formatter, a connection
 *	between the formatter and the proof editor is established.
 *
 *	Note that if the host name is unspecified, the value (a
 *	string host name) is gotten from \sym{formatter-host}.
 *	used.  The program name (a string UNIX path name) and the
 *	port number (a fixnum) are used from the global variables
 *	\sym{formatter-program} and \sym{formatter-port}.
 *  SeeA: kill-formatter start-proof-editor formatter-host
 *	formatter-port formatter-program
 */

DEFUN(dostartformat, "start-formatter", FLAG_NONE, "")
{
	extern int	format_socket;
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

	if (format_socket > 0)
		quitformat(0);

	if (startformat(host) != 0)
		return (v_nil);
	else
		return (v_t);
}

startformat(hostname)
	char	*hostname;
{
	extern int	proof_socket, format_socket;
	struct value	arg;
	char		host[64], prog[1024];
	int		conn, port;

	if (format_socket > 0)
		error("You already have a formatter running!");

	if (hostname == NULL) {
		arg = get_variable(FORMATHOST_NAME, NULL);
		if (eq(arg, NOVALUE))
			error("%Y is unset and no hostname was specified!",
			      FORMATHOST_NAME);
		if (!stringp(arg))
			error("%Y is not set to a string host name!",
			      FORMATHOST_NAME);
		makecstring(gstring(arg.vl_data), host, sizeof (host));
		hostname = host;
	}
	if (*hostname == '\0')
		error("Empty string specified for formatter host name!");

	arg = get_variable(FORMATPORT_NAME, NULL);
	if (eq(arg, NOVALUE))
		error("Variable %Y is unset; have no port!", FORMATHOST_NAME);
	if (!fixnump(arg))
		error("%Y is not set to a fixnum port!", FORMATHOST_NAME);
	port = gfixnum(arg.vl_data);
	if (port < 1024)
		error("Invalid port number %d for formatter.", port);

	arg = get_variable(FORMATPROG_NAME, NULL);
	if (eq(arg, NOVALUE))
		error("Variable %Y is unset; no program!", FORMATPROG_NAME);
	if (!stringp(arg))
		error("%Y is not set to a string file name!", FORMATPROG_NAME);
	makecstring(gstring(arg.vl_data), prog, sizeof (prog));
	if (*prog == '\0')
		error("Empty string specified for formatter program!");

	/* accept a connection from the formatter */
	conn = connformat(prog, hostname, port);
	if (conn < 0)
		return (-1);

	/* tell proof editor and formatter to rendezvous */
	if (proof_socket > 0)
		connection();

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: kill-formatter
 *  Call: (kill-formatter)
 *  Retu: nil
 *  Desc: This functions kills the formatter if one is running.
 *	if no formatter is running, this function does nothing.
 *  Side: If a proof editor is running, it loses its connection to
 *	the formatter also (but doesn't necessarily die itself).
 *  SeeA: start-formatter kill-proof-editor
 */

DEFUN(dokillformat, "kill-formatter", FLAG_NONE, "")
{
	extern int	format_socket;

	if (format_socket <= 0)
		return (v_nil);

	quitformat(0);

	return (v_nil);
}
