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
 *  RCS Info: $Header: proofops.c,v 0.1 87/05/01 12:25:52 john Locked $
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
 *  proofops.c - proof editor request handlers
 */
static char _ID[] = "@(#)proofops.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "buffer.h"
#include "window.h"
#include "gl_comm.h"
#include "ps_comm.h"
#include "ts_comm.h"
#include "document.h"

extern int	proof_socket;

/* ARGSUSED */
ps_flush(code, win, len, data)
	struct window	*win;
	char		*data;
{
	message("Flushing input on proof editor connection...");

	return (0);
}

/* ARGSUSED */
ps_listen(code, docp, len, data)
	struct document	*docp;
	char		*data;
{
	extern char	*alloca();
	extern int	format_socket;
	extern char	proof_host[];
	struct value	arg;
	int		port, hlen;

	if (len != sizeof (unsigned long))
		ierror("Listen request with wrong data length!");

	port = ntohl(*(unsigned long *)data);
	debug(DCONN, "Proof editor is listening for formatter at %d@%s.",
	      port, proof_host);

	if (format_socket < 0) {
		debug(DCONN|DPROOF|DITEX,
		      "Proof editor is listening; formatter isn't running!");
		return (1);
	}

	hlen = strlen(proof_host);
	len = hlen + 2 * sizeof (unsigned long);
	data = alloca(len);
	((unsigned long *)data)[0] = htonl(port);
	((unsigned long *)data)[1] = htonl(CONNECT_TIMEOUT);
	bcopy(proof_host, data + (2 * sizeof (unsigned long)), hlen);
	sendformat(GLC_CONNECT, NULL, len, data);

	return (0);
}

/* ARGSUSED */
ps_quit(code, win, len, data)
	struct window	*win;
	char		*data;
{
	shutdown(proof_socket, 2);
	close(proof_socket);
	proof_socket = -1;

	message("Our proof editor quit on us; no more proofing.");
	return (0);
}

/* ARGSUSED */
ps_abort(code, win, len, data)
	struct window	*win;
	char		*data;
{
	shutdown(proof_socket, 2);
	close(proof_socket);
	proof_socket = -1;

	quitproof(1);

	message("Our proof editor aborted; no more proofing.");
	return (0);
}

/* ARGSUSED */
ps_error(code, win, len, data)
	struct window	*win;
	char		*data;
{
	struct string	fake;
	char		errbuf[128];

	if (len <= 0) {
		/* no error message sent */
		debug(DPROOF, "Proof error with no error message!");
		error("Unspecified proofing error occured!");
	} else {
		fake.st_buffer = (unsigned char *)data;
		fake.st_length = len;
		makepstring(&fake, errbuf, sizeof (errbuf));
		debug(DPROOF, "Proof error: \"%s\".", errbuf);
		error("(proof editor) %s", errbuf);
	}
}

/* ARGSUSED */
ps_bstart(code, win, len, data)
	struct window	*win;
	char		*data;
{
}

/* ARGSUSED */
ps_bend(code, win, len, data)
	struct window	*win;
	char		*data;
{
}

/* ARGSUSED */
ps_newview(code, win, len, data)
	struct window	*win;
	char		*data;
{
}

/* ARGSUSED */
ps_docpages(code, win, len, data)
	struct window	*win;
	char		*data;
{
}

struct buffer	*select_buffer = NULL;
unsigned long	select_first, select_last;

#define TIMEOUT	10

/* ARGSUSED */
ps_selection(code, win, len, data)
	struct window	*win;
	char		*data;
{
	unsigned long	*posn, first, last;
	unsigned long	args[2];
	struct buffer	*bufp, *selbuf;
	int		selfirst, sellast;

	/* unset current selection */
	select_buffer = NULL;
	select_first = select_last = 0;

	if (len == 0 || data == NULL)
		return;
	if (len != 2 * sizeof (long))
		ierror("Selection request with wrong data length!");

	/* get the position information from the data */
	posn = (unsigned long *)data;
	first = ntohl(posn[0]);
	last = ntohl(posn[1]);
	debug(DCONN|DPROOF, "Box IDs %d through %d selected.", first, last);

	/* get the first character in the selection from formatter */
	message("Translating proof selection through formatter...");
	args[0] = htonl(TS_TGT2SRC);
	args[1] = htonl(first);
	sendformat(TSC_EXECUTE, ANYFILE, sizeof (args), (char *)args);
	if (waitformat(TSC_RETURN, &bufp, &len, &data, TIMEOUT) == 0)
		error("First position query timed out; selection failed.");
	if (len != sizeof (unsigned long) || data == NULL)
		ierror("Return from first position query has wrong length!");
	if (bufp == NULL)
		ierror("Return from first position query has no buffer!");
	selfirst = ntohl(*((unsigned long *)data));
	selbuf = bufp;
	debug(DPROOF, "Box ID %d translates to buffer %S, offset %d.",
	      first, bufp->bu_name, selfirst);
	vfree(data);

	/* get the last selected position */
	if (last == first) {
		/* just user the first buffer position plus one */
		sellast = selfirst + 1;
	} else {
		/* now get the last character in the selection */
		args[0] = htonl(TS_TGT2SRC);
		args[1] = htonl(last);
		sendformat(TSC_EXECUTE, ANYFILE, sizeof (args), (char *)args);
		if (waitformat(TSC_RETURN, &bufp, &len, &data, TIMEOUT) == 0)
			error("Second query timed out; selection failed.");
		if (len != sizeof (unsigned long))
			ierror("Return from second posn query wrong length!");
		if (bufp == NULL)
			ierror("Return from second posn query has no buffer!");
		sellast = ntohl(*((unsigned long *)data)) + 1;
		debug(DPROOF, "Box ID %d translates to buffer %S, offset %d.",
		      last, bufp->bu_name, sellast);
		vfree(data);
		if (bufp != selbuf || sellast <= selfirst)
			error("Selection spans source files or structure.");
	}

	select_buffer = selbuf;
	select_first = selfirst;
	select_last = sellast;
	debug(DPROOF, "Proof selection is %d to %d in buffer %S.",
	      selfirst, sellast, selbuf->bu_name);
}
