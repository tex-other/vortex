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
 *  RCS Info: $Header: pcomm.c,v 0.1 87/05/01 12:23:04 john Locked $
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
 *  pcomm.c - proof editor communications routines
 */
static char _ID[] = "@(#)pcomm.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <pwd.h>
#include <netdb.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include "vse.h"
#include "buffer.h"
#include "window.h"
#include "process.h"
#include "gl_comm.h"
#include "ps_comm.h"
#include "psinfo.h"
#include "channel.h"

/*
 *  The basic packet for the source <-> proof editor communications.
 *  The xwindow parameter probably should be an X Window, but we
 *  don't want to depend on including <X/Xlib.h> everywhere, so
 *  we just assume it fits in a long.
 */
struct packet {
	u_short		pk_request;
	u_short		pk_datalen;
	u_long		pk_xwindow;
};

/*
 *  We expect to read this structure from the proof editor as the
 *  first thing after we establish the IPC connection.  Note that
 *  this is the basic packet structure plus the identifier and
 *  protocol version numbers we use to verify that our peer
 *  deserves our attention.  The xwindow parameter is not used
 *  here, but is passed in all communications.
 */
struct verify {
	u_short		vr_request;
	u_short		vr_datalen;
	u_long		vr_xwindow;
	u_long		vr_ident;
	u_short		vr_gl_vrsn;
	u_short		vr_ps_vrsn;
};
#define VERIFY_DATALEN	(sizeof (struct verify) - sizeof (struct packet))

/*
 *  We have to buffer incomming requests when we're waiting for a
 *  specific answer to a query.  This only happens when we're looping
 *  on packet reads in waitproof().  The queued requests get handled
 *  by the next call to recvproof();
 */
#define MAXREQBUFS	100

struct reqbuf {
	int		rb_request;
	struct window	*rb_window;
	int		rb_length;
	char		*rb_data;
	int		(*rb_handle)();
};

static struct reqbuf	request_buffer[MAXREQBUFS],
			*request_first = request_buffer,
			*request_last = request_buffer;
int		proof_buffered = FALSE;


static char	RSH[] = "/usr/ucb/rsh";

char		proof_host[1025];

#define MAXARGS		10

#ifdef DEBUG
static char	POUT[] = "./vpe.log";
#else
static char	POUT[] = "/tmp/vpe_%s.log";
#endif

static int
execproof(prog, host, port)
	char	*prog, *host;
{
	extern char	*strsave();
	extern char	Xdispname[];
	char		*user = "nobody";
	struct passwd	*pwptr;
	struct process	*proc;
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

	/* set up the argument line to execute */
	argv = proc->pr_argv;
	if (strcmp(host, "localhost") && strncmp(host, local, strlen(host))) {
		path = RSH;
		*argv++ = strsave("rsh");
		*argv++ = strsave(host);
		*argv++ = strsave("-n");
	} else {
		/* execute the given program directly */
		path = fixpath(prog);
	}
	*argv++ = strsave(prog);
	*argv++ = strsave(local);
	*argv++ = strsave(nbuf);
	*argv++ = strsave(Xdispname);
	*argv = NULL;
	proc->pr_argc = argv - proc->pr_argv;

	/* send proof editor output to log file */
	if ((pwptr = getpwuid(getuid())) != NULL && pwptr->pw_name != NULL)
		user = pwptr->pw_name;
	sprintf(fbuf, POUT, user);
	(void)unlink(fbuf);
	fd = open(fbuf, O_WRONLY|O_CREAT, 0666);

	/* fork the process using our process mechanisms */
	strncpy(proc->pr_comm, path, sizeof (proc->pr_comm) - 1);
	start_process(proc, FALSE, -1, fd);
	debug(DCONN|DPROOF, "Started \"%s\" on %s [pid %d].",
	      prog, host, proc->pr_pid);
	close(fd);

	strcpy(proof_host, host);
	return (proc->pr_pid);
}

static char	BADPACKET[] = "First packet isn't a VERIFY";
static char	BADDATALEN[] = "VERIFY packet's datalen is wrong";
static char	BADIDENT[] = "Wrong identification code";
static char	BADVERSION[] = "Wrong protocol versions";

int	proof_socket = -1;

connproof(prog, host, port)
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
		error("Couldn't find a port for the proof editor connection!");

	/* mark this socket as listening for a connection */
	if (listen(sock, 1) < 0) {
		close(sock);
		perror("Couldn't listen on socket %d (port %d)",
		       socket, port);
	}
	debug(DCONN, "Listening for a connection on port %d...", port);

	/* start the proof editor running */
	message("Starting proof editor on %s...", host);
	if (execproof(prog, host, port) < 0) {
		shutdown(sock, 2);
		close(sock);
		error("Couldn't start the proof editor program!");
	}

#ifdef PROOF_TIMEOUT
	/* wait for a while for the connection to arrive */
	tmbuf.tv_usec = (PROOF_TIMEOUT % 1000) * 1000;
	tmbuf.tv_sec = PROOF_TIMEOUT / 1000;
	bzero(rmask, sizeof (rmask));
	FDM_SET(rmask, sock);
	nfds = select(NOFILE, rmask, NULL, NULL, &tmbuf);
	if (nfds <= 0 || !isset(rmask, sock)) {
		close(sock);
		shutdown(sock, 2);
		error("Proof editor response timed out; no proofing.");
	}
#endif PROOF_TIMEOUT

	/* accept the connection */
	peerlen = sizeof (peer);
	if ((conn = accept(sock, (char *)&peer, &peerlen)) < 0) {
		shutdown(sock, 2);
		close(sock);
		perror("Couldn't accept a connection on port %d", port);
	}
	close(sock);

	/* verify that this is the proper process */
	debug(DCONN, "Connected to proof editor; waiting for verification...");
	errno = 0;
	if (read(conn, &verify, sizeof (verify)) != sizeof (verify)) {
		close(conn);
		if (errno == 0)
			error("Proof editor verification is too short!");
		else
			perror("Read error for proof editor verification");
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
		debug(DCONN|DPROOF,
		      "Wrong global protocol version (got %d, expect %d).",
		      ntohs(verify.vr_gl_vrsn), GLC_VERSION);
		goaway(conn, BADVERSION);
		return (-1);
	}
	if (ntohs(verify.vr_ps_vrsn) != PSC_VERSION) {
		debug(DCONN|DPROOF,
		      "Wrong proof protocol version (got %d, expect %d).",
		      ntohs(verify.vr_ps_vrsn), PSC_VERSION);
		goaway(conn, BADVERSION);
		return (-1);
	}

	/* send the welcome packet--they're in */
	packet.pk_request = htons(GLC_WELCOME);
	packet.pk_datalen = 0;
	write(conn, (char *)&packet, sizeof (packet));

	debug(DCONN|DITEX,
	      "Successfully connected to proof editor (sent WELCOME).");
	message("Connection established with proof editor.");

	return (proof_socket = conn);
}

sendproof(request, window, length, data)
	u_long	window;
	char	*data;
{
	struct packet	packet;
	struct psinfo	*info;
	int		len;

	/* find the proof request corresponding to this number */
	if ((info = proofinfo(request)) == NULL)
		error("Sending unknown request %d to proof editor!", request);

	/* make sure we have a connection */
	if (proof_socket <= 0) {
		debug(DPROOF|DCONN, "Can't send %s with no proof editor!",
		      info->ps_name);
		return (-1);
	}

	/* send of the packet header */
	packet.pk_request = htons(request);
	packet.pk_xwindow = htonl(window);
	packet.pk_datalen = htons(length);
	len = write(proof_socket, (char *)&packet, sizeof (packet));
	if (len != sizeof (packet)) {
		quitproof(1);
		perror("Write error on packet header to proof connection");
	}

	/* send trailing data if there is any */
	if (length > 0 && data != NULL) {
		if (write(proof_socket, data, length) != length)
			perror("Write error on proof packet data");
	}

	debug(DCONN, "Sent request %s with %d bytes of data to proof editor.",
	      info->ps_name, length);

	return (0);
}

recvproof()
{
	extern char	*alloca();
	struct packet	packet;
	struct window	*winp;
	char		*data;
	int		len, left, n;
	char		*addr;
	struct psinfo	*info;
	Window		xwin;
	struct reqbuf	*req;

	/* handle any buffered requests */
	while (request_first < request_last) {
		req = request_first++;
		proof_buffered = request_last - request_first;
		if (req->rb_request > 0 && req->rb_handle != NULL) {
			(*req->rb_handle)(req->rb_request,
					  req->rb_window,
					  req->rb_length, req->rb_data);
		}
	}
	if (request_first >= request_last) {
		request_first = request_last = request_buffer;
		proof_buffered = 0;
	}

	/* read packet header itself */
	len = read(proof_socket, &packet, sizeof (struct packet));
	if (len < 0) {
		debug(DCONN,
		      "Read error for proof header; connection closed.");
		ps_abort(0, 0, 0, 0);
		return (-1);
	} else if (len < sizeof (struct packet)) {
		debug(DCONN,
		      "EOF on read for proof header; connection closed.");
		ps_abort(0, 0, 0, 0);
		return (-1);
	}

	/* convert packet into host byte order */
	packet.pk_request = ntohs(packet.pk_request);
	packet.pk_datalen = ntohs(packet.pk_datalen);
	packet.pk_xwindow = ntohl(packet.pk_xwindow);

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
			n = read(proof_socket, addr, left);
			if (n < 0)
				perror("Read error for proof packet data");
			if (n == 0)
				break;
			addr += n;
			left -= n;
			len += n;
		}
	}

	/* find the associated window */
	if (packet.pk_xwindow == 0) {
		/* no window associated */
		winp = NULL;
	} else {
		xwin = (Window)packet.pk_xwindow;
		for (winp = window_list; winp != NULL; winp = winp->wi_next)
			if (winp->wi_xwindow == xwin)
				break;
		if (winp == NULL) {
			/* can't find the specified window in the list */
			debug(DPROOF, "Unknown window 0x%x from proof editor!",
			      (unsigned int)xwin);
			winp = NULL;
		}
	}

	/* get the connection information for this request */
	if ((info = proofinfo(packet.pk_request)) == NULL) {
		error("Unknown request code %d from proof editor!",
		      packet.pk_request);
	}
	debug(DPROOF, "Proof editor request %s with %d bytes of data.",
	      info->ps_name, len);

	/* get handler function for the request */
	if (info->ps_handle == NULL) {
		error("No receive handler for request %s from proof editor!",
		      info->ps_name);
	}

	return (*info->ps_handle)(packet.pk_request, winp, len, data);
}

waitproof(reqmatch, winp, lenp, datap, timeout)
	struct window	**winp;
	int		*lenp;
	char		**datap;
{
	struct packet	packet;
	char		*data;
	int		len, left, n;
	char		*addr;
	struct window	*win;
	struct psinfo	*info, *match;
	struct document	*docp;
	struct reqbuf	*req;
	Window		xwin;

	/* get the connection information for the request to match */
	if (reqmatch <= 0)
		match = NULL;
	else if ((match = proofinfo(reqmatch)) == NULL)
		error("Unknown request code %d to wait for!", reqmatch);

	/* check buffered requests */
	for (req = request_first; req < request_last; req++) {
		if (req->rb_request > 0 &&
		    (match == NULL || match->ps_code == req->rb_request)) {
			if (winp != NULL)
				*winp = req->rb_window;
			*lenp = req->rb_length;
			*datap = req->rb_data;
			n = req->rb_request;
			req->rb_request = -1;
			proof_buffered--;
			return (n);
		}
	}

	/* read packet header itself */
top:	if (timeout > 0) {
		struct timeval	tvbuf;
		int		nr;
		unsigned long	mask[FDM_SIZE];

		debug(DITEX,
		      "Waiting %d second%s for %s proof editor reply...",
		      timeout, PLURAL(timeout),
		      (match == NULL) ? "any" : match->ps_name);

		tvbuf.tv_sec = timeout;
		tvbuf.tv_usec = 0;
		bzero(mask, sizeof (mask));
		FDM_SET(mask, proof_socket);
		nr = select(NOFILE, mask, NULL, NULL, &tvbuf);
		if (nr == 0 || FDM_ISCLR(mask, proof_socket))
			return (0);
	} else {
		/* no select, just do a blocking read */
		debug(DCONN|DITEX, "Blocking on %s proof exitor reply...",
		      (match == NULL) ? "any" : match->ps_name);
	}

	len = read(proof_socket, &packet, sizeof (struct packet));
	if (len < 0) {
		debug(DCONN|DPROOF,
		      "Read error for proof header; connection closed.");
		ts_abort(0, 0, 0, 0);
		return (-1);
	} else if (len < sizeof (struct packet)) {
		debug(DCONN|DPROOF,
		      "EOF on read for proof header; connection closed.");
		ts_abort(0, 0, 0, 0);
		return (-1);
	}

	/* convert packet into host byte order */
	packet.pk_request = ntohs(packet.pk_request);
	packet.pk_datalen = ntohs(packet.pk_datalen);
	packet.pk_xwindow = ntohs(packet.pk_xwindow);

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
			n = read(proof_socket, addr, left);
			if (n < 0)
				perror("Read error for proof packet data");
			if (n == 0)
				break;
			addr += n;
			left -= n;
			len += n;
		}
	}

	/* find the associated window */
	if (packet.pk_xwindow == 0) {
		/* no window associated */
		win = NULL;
	} else {
		xwin = (Window)packet.pk_xwindow;
		for (win = window_list; win != NULL; win = win->wi_next)
			if (win->wi_xwindow == xwin)
				break;
		if (win == NULL) {
			/* can't find the specified window in the list */
			debug(DPROOF, "Unknown window 0x%x from proof editor!",
			      (unsigned int)xwin);
			win = NULL;
		}
	}

	/* get the connection information for this request */
	if ((info = proofinfo(packet.pk_request)) == NULL) {
		error("Unknown request code %d from proof editor!",
		      packet.pk_request);
	}

	debug(DITEX, "Proof editor request %s with %d bytes of data.",
	      info->ps_name, len);

	if (match == NULL || packet.pk_request == reqmatch) {
		/* return this request to our caller */
		if (winp != NULL)
			*winp = win;
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
		if (info->ps_handle == NULL) {
			vfree(data);
			ierror("We shouldn't receive a %s request!",
			       info->ps_name);
		}
		req = request_last++;
		proof_buffered = request_last - request_first;
		req->rb_request = packet.pk_request;
		req->rb_window = win;
		req->rb_length = len;
		req->rb_data = data;
		req->rb_handle = info->ps_handle;
		debug(DCONN, "Buffered a %s request while waiting for a %s.",
		      info->ps_name, match->ps_name);

		/* read next packet and process it */
		goto top;
	}
	/* NOTREACHED */
}

quitproof(status)
{
	struct window	*winp = NULL;
	struct buffer	*bufp;
	int		count;

	/* kill all existing proof windows */
	count = 0;
	do {
		for (winp = window_list; winp != NULL; winp = winp->wi_next) {
			if (winp->wi_type != WIN_BUFFER)
				continue;
			bufp = winp->wi_buffer;
			if (bufp != NULL && bufp->bu_type == BUFF_PROOF) {
				killwindow(winp, TRUE);
				count++;
				break;
			}
		}
	} while (winp != NULL);
	if (count > 0) {
		debug(DPROOF, "Killed %d proof editor window%s.",
		      count, PLURAL(count));
	}		

	/* kill existing proof buffers */
	count = 0;
	do {
		for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
			if (bufp->bu_type == BUFF_PROOF) {
				buffer_remove(bufp, TRUE);
				count++;
				break;
			}
		}
	} while (bufp != NULL);
	if (count > 0) {
		debug(DPROOF, "Killed %d proof editor buffer%s.",
		      count, PLURAL(count));
	}		

	/* send quit message to proof editor as appropriate */
	if (proof_socket > 0) {
		/* send QUIT or ABORT */
		if (status == 0)
			sendproof(GLC_QUIT, NULL, 0, NULL);
		else
			sendproof(GLC_ABORT, NULL, 0, NULL);
		sleep(1);

		/* close down socket */
		shutdown(proof_socket, 2);
		close(proof_socket);
		proof_socket = -1;
		/* close down socket */

		debug(DCONN|DPROOF,
		      "Closed down proof editor connection (sent %s).",
		      status == 0 ? "QUIT" : "ABORT");
	}
}

static char	DEBUGMESG[] = "%s; proof connection aborted.";

static int
goaway(sock, mesg)
	char	*mesg;
{
	struct packet	packet;
	int		len;

	len = strlen(mesg);
	debug(DCONN|DPROOF, DEBUGMESG, mesg);

	/* tell our connection he loses */
	packet.pk_request = htons(GLC_GOAWAY);
	packet.pk_xwindow = 0;
	packet.pk_datalen = htons(len);
	write(sock, (char *)&packet, sizeof (packet));
	write(sock, mesg, len);

	/* close down the socket and return */
	shutdown(sock, 2);
	close(sock);
}

struct psinfo *
proofinfo(request)
{
	struct psinfo	*info;

	if (request <= 0 || request >= psinfo_count) {
		debug(DPROOF, "Request number %d is out of range 1-%d!",
		      psinfo_count - 1);
		return (NULL);
	}

	info = &psinfo_table[request];
	if (info->ps_code != request) {
		/* whoops! */
		debug(DPROOF,
		      "Proof communications table wrong for %s, number %d!",
		      info->ps_name, request);
		return (NULL);
	}

	return (info);
}
