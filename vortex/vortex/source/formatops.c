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
 *  RCS Info: $Header: formatops.c,v 0.1 87/05/01 12:15:09 john Locked $
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
 *  formatops.c - internal formatter request handlers
 */
static char _ID[] = "@(#)formatops.c for VorTeX, Copyright (c) 1987 John Coker";

#include <signal.h>
#include <sys/file.h>
#include "vse.h"
#include "buffer.h"
#include "document.h"
#include "gl_comm.h"
#include "ts_comm.h"

extern int	format_socket;

/* ARGSUSED */
ts_flush(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	message("Flushing input on formatter connection...");

	return (0);
}

ts_listen(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	extern char	*alloca();
	extern int	proof_socket;
	extern char	format_host[];
	struct value	arg;
	int		port, hlen;

	ASSERT(len == sizeof (unsigned long));
	port = ntohl(*(unsigned long *)data);
	debug(DCONN, "Formatter is listening for proof editor at %d@%s.",
	      port, format_host);

	if (proof_socket < 0) {
		debug(DCONN|DITEX|DPROOF,
		      "Formatter is listening; proof editor isn't running!");
		return (1);
	}

	hlen = strlen(format_host);
	len = hlen + 2 * sizeof (unsigned long);
	data = alloca(len);
	((unsigned long *)data)[0] = htonl(port);
	((unsigned long *)data)[1] = htonl(CONNECT_TIMEOUT);
	bcopy(format_host, data + (2 * sizeof (unsigned long)), hlen);
	sendproof(GLC_CONNECT, NULL, len, data);

	return (0);
}

/* ARGSUSED */
ts_quit(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	shutdown(format_socket, 2);
	close(format_socket);
	format_socket = -1;

	message("Our formatter quit on us; no more formatting.");
	return (0);
}

/* ARGSUSED */
ts_abort(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	shutdown(format_socket, 2);
	close(format_socket);
	format_socket = -1;

	quitformat(1);

	message("Our formatter aborted; no more formatting.");
	return (0);
}

/* ARGSUSED */
ts_error(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	struct string	fake;
	char		errbuf[128];

	if (len <= 0) {
		/* no error message sent */
		debug(DITEX, "Format error with no error message!");
		error("Unspecified formatting error occured!");
	} else {
		fake.st_buffer = (unsigned char *)data;
		fake.st_length = len;
		makepstring(&fake, errbuf, sizeof (errbuf));
		debug(DITEX, "Format error: \"%s\".", errbuf);
		error("(formatter) %s", errbuf);
	}
}

/* ARGSUSED */
ts_closedoc(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	int		docid;
	struct document	*next, *last;

	/* get the document ID to search for */
	docid = docp->dc_rootID;

	last = NULL;
	for (next = document_list; next != NULL; next = next->dc_next) {
		if (next->dc_rootID == docid)
			break;
		last = next;
	}
	if (next == NULL) {
		debug(DITEX, "Told to close document which doesn't exist!");
		return (1);
	}

	PROTECT();
	/* remove the document from the list */
	if (last == NULL)
		document_list = next->dc_next;
	else
		last->dc_next = next->dc_next;
	vfree(next);
	UNPROTECT();

	message("The formatter has closed document %d for you.", docid);
	return (0);
}

/* ARGSUSED */
ts_closefile(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	return (1);
}

/* ARGSUSED */
ts_input(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	struct buffer	*findfile();
	char		file[1024];
	register char	*fp, *dp, *fend, *dend;
	struct string	tmp;

	dend = data + len;
	dp = data;
	fend = file + sizeof (file) - 1;
	fp = file;
	while (dp < dend && fp < fend) {
		if (*dp != '\0' && *dp != '\200')
			*fp++ = *dp++ & 0177;
	}
	*fp = '\0';
	if (fp - file < 1 && bufp == NULL) {
		static char	ERR[] = "No file name for input request!";

		sendformat(GLC_ERROR, ANYFILE, strlen(ERR), ERR);
		debug(DITEX, "Formatter sent an input request but no file!\n");
	} else if (fp - file > 0 && bufp == NULL) {
		debug(DITEX, "The formatter wants to input \"%s\".", file);

		/* find this file in a buffer and lock the buffer */
		tmp.st_length = fp - file;
		tmp.st_buffer = (unsigned char *)file;
		if ((bufp = findfile(&tmp)) == NULL) {
			debug(DITEX, "Couldn't find \"%S\" for formatter!",
			      &tmp);
			strcat(file, ": not found!");
			sendformat(GLC_ERROR, ANYFILE, strlen(file), file);
			return (1);
		}
		if (documentfile(current_document, bufp) < 0) {
			static char	ERR[] = "Too many files already!";

			sendformat(GLC_ERROR, ANYFILE, strlen(file), file);
			error("Too many document files already!");
		}
	}

	/* send this buffer to the formatter */
	send_buffer(bufp, format_socket);

	return (0);
}

ts_output(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	return (0);
}

/* ARGSUSED */
ts_message(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	if (len > 0 && data != NULL)
		message("TeX: %.*s", len, data);
	return (1);
}

/* ARGSUSED */
ts_texerr(code, docp, bufp, len, data)
	struct document	*docp;
	struct buffer	*bufp;
	char		*data;
{
	char	*errmesg;
	int	errline;

	ASSERT(docp != NULL);
	if (len <= sizeof (long)) {
		/* no error message sent */
		error("Unspecified TeX error occurred!");
	}

	/* extract the error line/message */
	errline = ntohl(*(long *)data);
	errmesg = data + sizeof (long);

	/* generate the error */
	error("TeX: !%d %.*s", errline, len - sizeof (long), errmesg);

	/* NOTREACHED */
}
