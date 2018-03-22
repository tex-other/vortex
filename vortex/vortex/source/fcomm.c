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
 *  RCS Info: $Header: fcomm.c,v 0.1 87/05/01 12:10:49 john Locked $
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
 *  fcomm.c - formatter communications routines
 */
static char _ID[] = "@(#)fcomm.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <netdb.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include "vse.h"
#include "buffer.h"
#include "window.h"
#include "document.h"
#include "process.h"
#include "gl_comm.h"
#include "ts_comm.h"
#include "tsinfo.h"
#include "channel.h"

/*
 *  The basic packet for the source <-> formatter communications.
 *  Note that the document parameter is passed for all requests,
 *  but only used for some of them.
 */
struct packet {
	u_short		pk_request;
	u_short		pk_datalen;
	u_long		pk_fileID;
};

/*
 *  We expect to read this structure from the formatter as the
 *  first thing after we establish the IPC connection.  Note that
 *  this is the basic packet structure plus the identifier and
 *  protocol version numbers we use to verify that our peer
 *  deserves our attention.  The document parameter is not used
 *  here, but is passed in all communications.
 */
struct verify {
	u_short		vr_request;
	u_short		vr_datalen;
	u_long		pk_fileID;
	u_long		vr_ident;
	u_short		vr_gl_vrsn;
	u_short		vr_ts_vrsn;
};
#define VERIFY_DATALEN	(sizeof (struct verify) - sizeof (struct packet))

/*
 *  We have to buffer incomming requests when we're waiting for a
 *  specific answer to a query.  This only happens when we're looping
 *  on packet reads in waitformat().  The queued requests get handled
 *  by the next call to recvformat();
 */
#define MAXREQBUFS	100

struct reqbuf {
	int		rb_request;
	struct buffer	*rb_buffer;
	int		rb_length;
	char		*rb_data;
	int		(*rb_handle)();
};

static struct reqbuf	request_buffer[MAXREQBUFS],
			*request_first = request_buffer,
			*request_last = request_buffer;
int		format_buffered = FALSE;


static char	RSH[] = "/usr/ucb/rsh";

char		format_host[1025];

#define MAXARGS		10

#ifdef DEBUG
static char	FOUT[] = "./tex.log";
#else
static char	FOUT[] = "/tmp/tex_%s.log";
#endif

static int
execformat(prog, host, port)
	char	*prog, *host;
{
	extern char	*strsave();
	char		*user = "nobody";
	struct process	*proc;
	struct passwd	*pwptr;
	char		local[64], nbuf[20], fbuf[256];
	char		**argv, *path;
	int		fd;

	if (gethostname(local, sizeof (local)) < 0)
		perror("Can't get hostname");
	sprintf(nbuf, "%d", port);

	/* make up the internal process structure */
	proc = (struct process *)valloc(sizeof (struct process));
	bzero((char *)proc, sizeof (struct process));
	proc->pr_argv = (char **)valloc(MAXARGS * sizeof(char *));

	/* set up command line to execute */
	argv = proc->pr_argv;
	if (strcmp(host, "localhost") && strncmp(host, local, strlen(host))) {
		path = RSH;
		*argv++ = strsave("rsh");
		*argv++ = strsave(host);
		*argv++ = strsave("-n");
	} else {
		/* just execute the given program directly */
		path = fixpath(prog);
	}
	*argv++ = strsave(prog);
	*argv++ = strsave(local);
	*argv++ = strsave(nbuf);
	if (debugging(DITEX))
		*argv++ = strsave("-d");
	*argv = NULL;
	proc->pr_argc = argv - proc->pr_argv;

	/* send formatter output to log file */
	if ((pwptr = getpwuid(getuid())) != NULL && pwptr->pw_name != NULL)
		user = pwptr->pw_name;
	sprintf(fbuf, FOUT, user);
	(void)unlink(fbuf);
	fd = open(fbuf, O_WRONLY|O_CREAT, 0666);

	/* fork the process using the process mechanisms */
	strncpy(proc->pr_comm, path, sizeof (proc->pr_comm) - 1);
	start_process(proc, FALSE, -1, fd);
	debug(DCONN|DITEX, "Started \"%s\" on %s [pid %d].",
	      prog, host, proc->pr_pid);
	close(fd);

	strcpy(format_host, host);
	return (proc->pr_pid);
}

static char	BADPACKET[] = "First packet isn't a VERIFY";
static char	BADDATALEN[] = "VERIFY packet's datalen is wrong";
static char	BADIDENT[] = "Wrong identification code";
static char	BADVERSION[] = "Wrong protocol version";

int	format_socket = -1;

connformat(prog, host, port)
	char	*prog, *host;
{
	extern int		errno;
	struct sockaddr_in	sin, peer;
	int			sock, conn;
	struct verify		verify;
	struct packet		packet;
	int			nfds, peerlen;
	unsigned long		rmask[FDM_SIZE];
	struct timeval		tmbuf;
	register int		p;

	/* find a usable socket for proof connection */
	sock = -1;
	for (p = 0; p < MAX_PORT_TRIES; p++) {
		/* make a new socket */
		if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
			perror("Can't make an internet stream socket");

		/* attempt to bind to it */
		bzero(&sin, sizeof (struct sockaddr_in));
		sin.sin_port = htons(port);
		if (bind(sock, (char *)&sin, sizeof (sin)) == 0)
			break;

		/* close the socket; try the next port number */
		close(sock);
		sock = -1;
		port++;
	}
	if (sock < 0)
		error("Couldn't find a port for the formatter connection!");

	/* mark this socket as listening for a connection */
	if (listen(sock, 1) < 0) {
		close(sock);
		perror("Couldn't listen on socket %d (port %d)",
		       socket, port);
	}
	debug(DCONN, "Listening for a connection on port %d...", port);
	message("Starting formatter on %s...", host);

	/* start the proof editor running */
	if (execformat(prog, host, port) < 0) {
		shutdown(sock, 2);
		close(sock);
		error("Couldn't start the formatter program!");
	}

#ifdef FORMAT_TIMEOUT
	/* wait for a while for the connection to arrive */
	tmbuf.tv_usec = (FORMAT_TIMEOUT % 1000) * 1000;
	tmbuf.tv_sec = FORMAT_TIMEOUT / 1000;
	bzero(rmask, sizeof (rmask));
	FDM_SET(rmask, sock);
	nfds = select(NOFILE, rmask, NULL, NULL, &tmbuf);
	if (nfds <= 0 || !isset(rmask, sock)) {
		shutdown(sock, 2);
		close(sock);
		error("Formatter response timed out; no formatting.");
	}
#endif FORMAT_TIMEOUT

	/* accept the connection */
	peerlen = sizeof (peer);
	if ((conn = accept(sock, (char *)&peer, &peerlen)) < 0) {
		shutdown(sock, 2);
		close(sock);
		perror("Couldn't accept a connection on port %d", port);
	}
	close(sock);

	/* verify that this is the proper process */
	debug(DCONN, "Connected to formatter; waiting for verification...");
	errno = 0;
	if (read(conn, &verify, sizeof (verify)) != sizeof (verify)) {
		close(conn);
		if (errno == 0)
			error("Formatter verification is too short!");
		else
			perror("Read error for formatter verification");
	}

	if (ntohs(verify.vr_request) != GLC_VERIFY) {
		goaway(conn, BADPACKET);
		return (-1);
	}
	if (ntohs(verify.vr_datalen) != VERIFY_DATALEN) {
		goaway(conn, BADDATALEN);
		return (-1);
	}
	if (ntohl(verify.vr_ident) != GLC_IDENT) {
		goaway(conn, BADIDENT);
		return (-1);
	}
	if (ntohs(verify.vr_gl_vrsn) != GLC_VERSION) {
		debug(DCONN|DITEX,
		      "Bad global protocol version (got %d, expect %d).",
		      ntohs(verify.vr_gl_vrsn), GLC_VERSION);
		goaway(conn, BADVERSION);
		return (-1);
	}
	if (ntohs(verify.vr_ts_vrsn) != TSC_VERSION) {
		debug(DCONN|DITEX,
		      "Bad formatter protocol version (got %d, expect %d).",
		      ntohs(verify.vr_ts_vrsn), TSC_VERSION);
		goaway(conn, BADVERSION);
		return (-1);
	}

	/* send the welcome packet--they're in */
	packet.pk_request = htons(GLC_WELCOME);
	packet.pk_datalen = 0;
	write(conn, (char *)&packet, sizeof (packet));

	debug(DCONN|DITEX,
	      "Successfully connected to formatter (sent WELCOME).");
	message("Connection established with TeX formatter.");

	return (format_socket = conn);
}

sendformat(request, fileID, length, data)
	char	*data;
{
	struct packet	packet;
	struct tsinfo	*info;
	int		len;

	/* find the format request corresponding to this number */
	if ((info = formatinfo(request)) == NULL)
		error("Sending unknown request %d to formatter!", request);

	/* make sure we have a connection */
	if (format_socket <= 0) {
		debug(DITEX|DCONN, "Can't send %s with no formatter!",
		      info->ts_name);
		return (-1);
	}

	/* send of the packet header */
	packet.pk_request = htons(request);
	packet.pk_fileID = htons(fileID);
	packet.pk_datalen = htons(length);
	len = write(format_socket, (char *)&packet, sizeof (packet));
	if (len != sizeof (packet)) {
		quitproof(1);
		perror("Write error on packet header to formatter connection");
	}

	/* send trailing data if there is any */
	if (length > 0 && data != NULL) {
		if (write(format_socket, data, length) != length)
			perror("Write error on formatter packet data");
	}

	debug(DCONN, "Sent request %s with %d bytes of data to formatter.",
	      info->ts_name, length);

	return (0);
}

recvformat()
{
	extern char	*alloca();
	struct packet	packet;
	char		*data;
	int		len, left, n;
	char		*addr;
	struct tsinfo	*info;
	struct document	*docp;
	struct buffer	*bufp;
	struct reqbuf	*req;

	/* handle any buffered requests */
	while (request_first < request_last) {
		req = request_first++;
		format_buffered = request_last - request_first;
		if (req->rb_request > 0 && req->rb_handle != NULL) {
			(*req->rb_handle)(req->rb_request,
					  req->rb_buffer,
					  req->rb_length, req->rb_data);
		}
	}
	if (request_first >= request_last) {
		request_first = request_last = request_buffer;
		format_buffered = 0;
	}

	/* read packet header itself */
	len = read(format_socket, &packet, sizeof (struct packet));
	if (len < 0) {
		debug(DCONN,
		      "Read error for formatter header; connection closed.");
		ts_abort(0, 0, 0, 0);
		return (-1);
	} else if (len < sizeof (struct packet)) {
		debug(DCONN,
		      "EOF on read for formatter header; connection closed.");
		ts_abort(0, 0, 0, 0);
		return (-1);
	}

	/* convert packet into host byte order */
	packet.pk_request = ntohs(packet.pk_request);
	packet.pk_datalen = ntohs(packet.pk_datalen);
	packet.pk_fileID = ntohs(packet.pk_fileID);

	/* read trailing packet data */
	if (packet.pk_datalen <= 0) {
		data = NULL;
		len = 0;
	} else {
		if ((data = alloca(packet.pk_datalen)) == NULL) {
			error("Not enough stack space to buffer %d bytes!",
			      packet.pk_datalen);
		}
		addr = data;
		len = 0;
		left = packet.pk_datalen;
		while (left > 0) {
			n = read(format_socket, addr, left);
			if (n < 0)
				perror("Read error for formatter packet data");
			if (n == 0)
				break;
			addr += n;
			left -= n;
			len += n;
		}
	}

	/* find specified document file */
	if (packet.pk_fileID == 0) {
		/* no document specified */
		bufp = NULL;
	} else {
		if ((docp = getdocument(packet.pk_fileID)) == NULL) {
			debug(DITEX,
			      "Unknown document file %d from formatter!",
			      packet.pk_fileID);
			docp = NULL;
		}
		if ((bufp = docfilebuf(packet.pk_fileID)) == NULL) {
			debug(DITEX, "Unknown document file %d (document %d)!",
			      packet.pk_fileID, packet.pk_fileID);
			bufp = NULL;
		}
	}

	/* get the connection information for this request */
	if ((info = formatinfo(packet.pk_request)) == NULL) {
		error("Unknown request code %d from formatter!",
		      packet.pk_request);
	}

	debug(DITEX, "Formatter request %s with %d bytes of data.",
	      info->ts_name, len);

	/* handle this request as appropriate */
	if (info->ts_handle == NULL) {
		error("No receive handler for request %s from formatter!",
		      info->ts_name);
	}
	(*info->ts_handle)(packet.pk_request, docp, bufp, len, data);

	return (0);
}

waitformat(reqmatch, bufp, lenp, datap, timeout)
	struct buffer	**bufp;
	int		*lenp;
	char		**datap;
{
	struct packet	packet;
	char		*data;
	int		len, left, n;
	char		*addr;
	struct buffer	*buf;
	struct tsinfo	*info, *match;
	struct document	*docp;
	struct reqbuf	*req;

	/* get the connection information for the request to match */
	if (reqmatch <= 0)
		match = NULL;
	else if ((match = formatinfo(reqmatch)) == NULL)
		error("Unknown request code %d to wait for!", reqmatch);

	/* check buffered requests */
	for (req = request_first; req < request_last; req++) {
		if (req->rb_request > 0 &&
		    (match == NULL || match->ts_code == req->rb_request)) {
			if (bufp != NULL)
				*bufp = req->rb_buffer;
			*lenp = req->rb_length;
			*datap = req->rb_data;
			n = req->rb_request;
			req->rb_request = -1;
			format_buffered--;
			return (n);
		}
	}

	/* read packet header itself */
top:	if (timeout > 0) {
		struct timeval	tvbuf;
		int		nr;
		unsigned long	mask[FDM_SIZE];

		debug(DITEX, "Waiting %d second%s for %s formatter reply...",
		      timeout, PLURAL(timeout),
		      (match == NULL) ? "any" : match->ts_name);

		tvbuf.tv_sec = timeout;
		tvbuf.tv_usec = 0;
		bzero(mask, sizeof (mask));
		FDM_SET(mask, format_socket);
		nr = select(NOFILE, mask, NULL, NULL, &tvbuf);
		if (nr == 0 || FDM_ISCLR(mask, format_socket))
			return (0);
	} else {
		/* no select, just do a blocking read */
		debug(DCONN|DITEX, "Blocking on %s formatter reply...",
		      (match == NULL) ? "any" : match->ts_name);
	}
	len = read(format_socket, &packet, sizeof (struct packet));
	if (len < 0) {
		debug(DCONN|DITEX,
		      "Read error for formatter header; connection closed.");
		ts_abort(0, 0, 0, 0);
		return (-1);
	} else if (len < sizeof (struct packet)) {
		debug(DCONN|DITEX,
		      "EOF on read for formatter header; connection closed.");
		ts_abort(0, 0, 0, 0);
		return (-1);
	}

	/* convert packet into host byte order */
	packet.pk_request = ntohs(packet.pk_request);
	packet.pk_datalen = ntohs(packet.pk_datalen);
	packet.pk_fileID = ntohs(packet.pk_fileID);

	/* read trailing packet data */
	if (packet.pk_datalen <= 0) {
		data = NULL;
		len = 0;
	} else {
		data = valloc(packet.pk_datalen);
		addr = data;
		len = 0;
		left = packet.pk_datalen;
		while (left > 0) {
			n = read(format_socket, addr, left);
			if (n < 0)
				perror("Read error for formatter packet data");
			if (n == 0)
				break;
			addr += n;
			left -= n;
			len += n;
		}
	}

	/* get the connection information for this request */
	if ((info = formatinfo(packet.pk_request)) == NULL) {
		error("Unknown request code %d from formatter!",
		      packet.pk_request);
	}

	/* find specified document file */
	if (packet.pk_fileID == 0) {
		/* no document specified */
		buf = NULL;
	} else {
		if ((buf = docfilebuf(packet.pk_fileID)) == NULL) {
			debug(DITEX, "Unknown document file %d (document %d)!",
			      packet.pk_fileID, packet.pk_fileID);
			buf = NULL;
		}
	}

	debug(DITEX, "Formatter request %s with %d bytes of data.",
	      info->ts_name, len);

	if (match == NULL || packet.pk_request == reqmatch) {
		/* return this request to our caller */
		if (bufp != NULL)
			*bufp = buf;
		*lenp = len;
		*datap = data;
		return (packet.pk_request);
	} else {
		/* save this packet for later processing */
		if (request_last >= request_buffer + MAXREQBUFS) {
			request_first = request_last = request_buffer;
			vfree(data);
			error("Too many buffered input requests; dropped!");
		}
		if (info->ts_handle == NULL) {
			vfree(data);
			ierror("We shouldn't receive a %s request!",
			       info->ts_name);
		}
		req = request_last++;
		format_buffered = request_last - request_first;
		req->rb_request = packet.pk_request;
		req->rb_buffer = buf;
		req->rb_length = len;
		req->rb_data = data;
		req->rb_handle = info->ts_handle;
		debug(DCONN, "Buffered a %s request while waiting for a %s.",
		      info->ts_name, match->ts_name);

		/* read next packet and process it */
		goto top;
	}
	/* NOTREACHED */
}

quitformat(status)
{
	struct buffer	*bufp;
	register int	count = 0;

	/* close the current document if there is one */
	if (current_document != NULL)
		closedoc(current_document->dc_rootID);

	/* remove stigma from TeX file buffers */
	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
		if (bufp->bu_flags & BUFF_TEXFILE) {
			bufp->bu_flags &= ~BUFF_TEXFILE;
			count++;
		}
	}
	if (count > 0) {
		debug(DITEX, "Removed restrictions on %d TeX file buffer%s.",
			count, PLURAL(count));
	}

	/* send quit message as appropriate */
	if (format_socket > 0) {
		/* send QUIT or ABORT */
		if (status == 0)
			sendformat(GLC_QUIT, ANYFILE, 0, NULL);
		else
			sendformat(GLC_ABORT, ANYFILE, 0, NULL);
		sleep(1);

		/* close down socket */
		shutdown(format_socket, 2);
		close(format_socket);
		format_socket = -1;

		debug(DCONN|DITEX,
		      "Closed down formatter connection (sent %s).",
		      status == 0 ? "QUIT" : "ABORT");
	}
}

static int
goaway(sock, mesg)
	char	*mesg;
{
	struct packet	packet;
	int		len;

	len = strlen(mesg);
	debug(DCONN|DITEX, "%s; formatter connection aborted.", mesg);

	/* tell our connection he loses */
	packet.pk_request = htons(GLC_GOAWAY);
	packet.pk_fileID = 0;
	packet.pk_datalen = htons(len);
	write(sock, (char *)&packet, sizeof (packet));
	write(sock, mesg, len);

	/* close down the socket and return */
	shutdown(sock, 2);
	close(sock);
}

struct tsinfo *
formatinfo(request)
{
	struct tsinfo	*info;

	if (request <= 0 || request >= tsinfo_count)
		return (NULL);

	info = &tsinfo_table[request];
	if (info->ts_code != request) {
		/* whoops! */
		ierror("Formatter communications table wrong for request %d!",
		       request);
	}

	return (info);
}
