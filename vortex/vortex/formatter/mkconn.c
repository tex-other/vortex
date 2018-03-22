/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifdef VORTEX

/*
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents.
 *
 *  This file is part of the VorTeX proof editor 
 *  written by Jeffrey W. McCarrell for the VorTeX project
 *  under the direction of Prof. Michael A. Harrison
 *  of the University of California at Berkeley.
 *
 *  Copyright (c) 1987 by Jeffrey W. McCarrell
 *  and The Regents of the University of California.
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  jwm@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  Modified by Pehong Chen (phc@berkeley.edu) for the formatter.
 */

/*
 * All the routines for making the connections to the source and proof
 * editors here.
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<netdb.h>
#include	<signal.h>
#include	<X/Xlib.h>
#include	<sys/param.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<sys/ioctl.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<net/if.h>
#include	"comm.h"
#include	"mdep.h"
#include	"failcodes.h"
#include	"msg.h"

char	*p_etheraddr();

typedef struct T_verify {
	u_short		req;
	u_short		len;
	u_long		id;
	u_long		ident;
	u_short		gl_version;
	u_short		loc_version;
} verify;

/* allocate various buffers and such here. */
int		ts_sock = -1,
		tp_sock = -1;
static int	conn_timeout;		/* timed out flag. */

mktsconn(hname, port)
	char	*hname;
	int	port;
{
	struct hostent		*host;
	struct sockaddr_in	sin;
	int			len;
	char			*data;
	verify			vpkt;
	gl_hdr			pkt;
	char			errmsg[1024];

	/* get host entry and set up socket address */
	if ((host = gethostbyname(hname)) == NULL) {
		msg(SDBUG, "Unknown host \"%s\" to talk to!",
			hname);
		return(-1);
	}
	bzero((char *)&sin, sizeof (sin));
	bcopy((char *)host->h_addr, (char *)&sin.sin_addr, host->h_length);
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);

	/* make the socket and connect to the server */
	if ((ts_sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		msg(SDBUG|PERROR, "Can't make an internet socket");
		return(-1);
	}
	if (connect(ts_sock, &sin, sizeof (sin)) < 0) {
		msg(SDBUG|PERROR, "Can't connect to %d@%s", port, hname);
		close_socks(IFC_CLOSE_TS);
		return(-1);
	}
	signal(SIGPIPE, SIG_IGN);

	/* send back the verify packet */
	vpkt.req = htons(GLC_VERIFY);
	vpkt.len = htons(2 * sizeof (u_long));
	vpkt.id  = htonl(0L);
	vpkt.ident = htonl(GLC_IDENT);
	vpkt.gl_version = htonl(GLC_VERSION);
	vpkt.loc_version = htonl(TSC_VERSION);

#if 0
	msg(SDBUG, "verify packet version == %d TSC_VERSION == %d",
	  vpkt.loc_version, TSC_VERSION);
#endif 0

	if (write(ts_sock, (char *)&vpkt, sizeof (vpkt)) != sizeof (vpkt)) {
		msg(SDBUG|PERROR, "Write error to socket");
		close_socks(IFC_CLOSE_TS);
		return(-1);
	}

	/* wait for welcome packet */
	if (read(ts_sock, (char *)&pkt, sizeof (pkt)) != sizeof (pkt)) {
		msg(SDBUG|PERROR, "Read error for first packet");
		close_socks(IFC_CLOSE_TS);
		return(-1);
	}
	pkt.req = ntohs(pkt.req);
	pkt.len = ntohs(pkt.len);
	if (pkt.req == GLC_GOAWAY) {
		if (pkt.len > sizeof (errmsg))
			pkt.len = sizeof (errmsg) - 1;
		if (pkt.len > 0)
			len = read(ts_sock, errmsg, pkt.len);
		errmsg[len] = '\0';
		msg(SDBUG, "We're told \"%s\"!", errmsg);
		close_socks(IFC_CLOSE_TS);
		return(-1);
	} else if (pkt.req != GLC_WELCOME) {
		msg(SDBUG, "First packet request code is %d!", pkt.req);
		close_socks(IFC_CLOSE_TS);
		return(-1);
	}
	msg(SDBUG, "VorTeX incremental formatter successfully connected to source editor.");
	return(0);
}

/* lowest level routine to close down our socket connections. */
close_socks(which)
	int	which;
{
	if ((which == IFC_CLOSE_ALL || which == IFC_CLOSE_TS) &&
	  ts_sock != -1) {
		msg(SDBUG, "[Doing post_format before shutdown ts_sock %d]", ts_sock);
		post_format();
		msg(SDBUG, "[Flushing state files before shutdown ts_sock %d]", ts_sock);
		flush_saved_state();
		msg(SDBUG, "[doing shutdown 2 on ts_sock %d]", ts_sock);
		shutdown(ts_sock, 2);
		close(ts_sock);
		ts_sock = -1;
	}

	if ((which == IFC_CLOSE_ALL || which == IFC_CLOSE_TP) &&
	  tp_sock != -1) {
		msg(PDBUG, "[Clearing ok flag for every page before shutdown tp_sock %d]", tp_sock);
		clear_page();
		msg(PDBUG, "[doing shutdown 2 on tp_sock %d]", tp_sock);
		shutdown(tp_sock, 2);
		close(tp_sock);
		tp_sock = -1;
	}
	return(0);
}


/* how many port numbers we will try to bind to before failing. */
#define PORT_LIMIT	(100)

/*
 * handle GLC_LISTENAT requests.
 */
gl_listenat(id, len, data)
	u_long			id;
	u_short			len;
	char			*data;
{
	struct sockaddr_in	sin;
	int			listen_sock,
				peerlen,
				nfds;
	u_long			portno,
				timeo;
	u_short			sport;
	int			rmask[FDM_SIZE];
	char			emsg[1024];
	gl_hdr			pkt;
	verify			vpkt;
	struct timeval		timeout;

	/* get the port number, timeout out of the data. */
	portno = *(u_long *)data;
	portno = ntohl(portno);
	data += sizeof(u_long);
	timeo = *(u_long *)data;
	timeo = ntohl(timeo);

	/* find a place to listen at. */
	for (sport = (u_short) portno; sport < portno + PORT_LIMIT; sport++) {
		bzero((char*)&sin, sizeof(sin));
		sin.sin_family = AF_INET;
		sin.sin_port = htons(sport);

		/* make the socket to listen at. */
		if ((listen_sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			msg(PDBUG|PERROR,
			  "Couldn't make ProofEditor listen sock at %d",
			  sport);
			return(-1);
		}

		/* now bind the socket to the address. */
		if (bind(listen_sock, (char*)&sin, sizeof(sin)) < 0) {
			close(listen_sock);
			continue;
		}
		break;
	}

	/* see if we were able to bind to a socket. */
	if (sport >= portno + PORT_LIMIT) {
		msg(FATAL, "Can't find a port to bind to in range %d to %d",
		  portno, portno + PORT_LIMIT - 1);
	}

	/* send the reply packet with the address we found. */
	portno = (u_long) sport;
	tp_send(GLC_LISTENAT, id, sizeof(u_long), &portno);

	if (listen(listen_sock, 5) < 0) {
		msg(PDBUG|PERROR, "listen on sock %d failed!", listen_sock);
		return(-1);
	}

 	msg(PDBUG, "listening at port %d on sock %d", sport, listen_sock);

	/* timeout the listen after a reasonable amount of time. */
	timeout.tv_sec = timeo / 1000;
	timeout.tv_usec = (timeo % 1000) * 1000;

 	bzero((char *) rmask, sizeof(rmask));
 	FDM_SET(rmask, listen_sock);
 	nfds = select(NOFILE, rmask, (int *) 0, (int *) 0, &timeout);

	if (nfds == -1) {
		/*
		 * either the timeout expired or we got a signal.  In any
		 * case return.
		 */
		msg(PDBUG, "GLC_LISTEN for ProofEditor socket timed-out/interrupted.");
		close(listen_sock);
		return(-1);
	}
	if (nfds == 0) {
		/* timed out. */
		msg(PDBUG, "GLC_LISTEN for ProofEditor timed out.");
		close(listen_sock);
		return(-1);
	}
 	if (FDM_ISCLR(rmask, listen_sock)) {
		/* shouldn't happen. */
		msg(PDBUG, "GLC_LISTEN for ProofEditor sock confused?");
		close(listen_sock);
		return(-1);
	}

 	/* we don't care who the peer is. */
 	if ((tp_sock = accept(listen_sock, (char *) 0, 0)) < 0) {
		msg(PERROR|PDBUG, "[GLC_LISTEN: accept fails at %d on %d",
 		  portno, listen_sock);
		close(listen_sock);
		return(-1);
	}
	close(listen_sock);

	signal(SIGPIPE, SIG_IGN);

	/*
	 * We must wait for the verify packet and send the welcome packet.
	 */
	if (read(tp_sock, (char *)&vpkt, sizeof(vpkt)) != sizeof(vpkt)) {
		msg(PDBUG|PERROR, "Read err for verify pkt on ProofEditor sock %d", tp_sock);
		goto badstuff;
	}
	vpkt.req = ntohs(vpkt.req);
	vpkt.len = ntohs(vpkt.len);
	vpkt.ident = ntohl(vpkt.ident);
 	vpkt.gl_version = ntohl(vpkt.gl_version);
 	vpkt.loc_version = ntohl(vpkt.loc_version);
	if (vpkt.req != GLC_VERIFY) {
		strcpy(emsg, "first packet from ProofEditor not GLC_VERIFY!");
		msg(PDBUG|LITERAL, emsg);
		goto badverify;
	}
	if (vpkt.len != (2 * sizeof (u_long))) {
		strcpy(emsg, "first packet from ProofEditor has bad length!");
		msg(PDBUG|LITERAL, emsg);
		goto badverify;
	}
	if (vpkt.ident != GLC_IDENT) {
		strcpy(emsg, "bad GLC_IDENT from ProofEditor");
		msg(PDBUG|LITERAL, emsg);
		goto badverify;
	}
	if (vpkt.gl_version != GLC_VERSION) {
		sprintf(emsg, "bad GLC version %d, expecting %d",
		  vpkt.gl_version, GLC_VERSION);
		msg(PDBUG|LITERAL, emsg);
		goto badverify;
	}
	if (vpkt.loc_version != TPC_VERSION) {
		sprintf(emsg, "bad TPC version %d; expected %d",
		  vpkt.loc_version, TPC_VERSION);
		msg(PDBUG|LITERAL, emsg);
		goto badverify;
	}

	/* setup the welcome packet. */
	pkt.req = htons(GLC_WELCOME);
	pkt.id = pkt.len = 0;
	if (write(tp_sock, (char*)&pkt, sizeof(pkt)) < 0) {
		msg(PDBUG|PERROR, "Write error on welcome pkt to ProofEditor");
		goto badstuff;
	}
	/* Success at last! */
	msg(PDBUG, "Successfully connect to ProofEditor (WELCOME sent on sock %d).",
	  tp_sock);
	return(0);

badverify:
	/* the verify packet was bad, so send a GLC_GOAWAY. */
	pkt.req = htons(GLC_GOAWAY);
	pkt.len = htons(strlen(emsg));
	if (write(tp_sock, (char*)&pkt, sizeof(pkt)) < 0) {
		/* more errors, but ignore them for now.*/
	}
	if (write(tp_sock, emsg, pkt.len) < 0) {
		/* more errors, but ignore them for now.*/
	}
	/* fall through... */

badstuff:
	/* there was an error somewhere, cleanup. */
	shutdown(tp_sock, 2);
	close(tp_sock);
	tp_sock = -1;
	return(-1);
}


/*
 * Initiate a connection to the ProofEditor process. data points to
 *
 * u_long	Inet port number
 * string	hostname
 */
gl_connect(id, len, data)
	u_short		       len;
	u_long		       id;
	char		       *data;
{
	char			*hostname;
	u_long			portno,
				timeo;
	struct hostent		*host;
	struct sockaddr_in	sin;
	gl_hdr			pkt;
	verify			vpkt;
	int			dlen;
	char			errmsg[1024];
 	int			conn_to();

	if (len < (2 * sizeof(u_long)) + 2) {
		msg(PDBUG, "[gl_connect len too short: %d]", len);
		return(-1);
	}

	/* get the host data out of the	packet. */
 	portno = *(u_long *) data;
 	portno = htonl(portno);
 	data += sizeof(u_long);
 	timeo = *(u_long *) data;
 	timeo = htonl(timeo);
	hostname = data + sizeof(u_long);

	/* get host entry and setup socket address. */
	if ((host = gethostbyname(hostname)) == (struct hostent *) 0) {
		msg(PDBUG, "[Unknown host \"%s\" to connect to!]", hostname);
		return(-1);
	}

	bzero((char *)&sin, sizeof(sin));
	bcopy((char *) host->h_addr, (char*)&sin.sin_addr, host->h_length);
	sin.sin_family = AF_INET;
	sin.sin_port = htons(portno);

	/* make the socket and connect to the ProofEditor process. */
	if ((tp_sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		msg(PDBUG|PERROR, "Can't make ProofEditor internet socket");
		return(-1);
	}
	/* set up an alarm to check for timeout. */
	(void) signal(SIGALRM, conn_to);
	conn_timeout = 0;
	ualarm(timeo * 1000);

	if (connect(tp_sock, &sin, sizeof(sin)) < 0) {
		if (errno == EINTR && conn_timeout) {
			msg(PDBUG, "connect to VPE timed out.");
		} else {
			msg(PDBUG|PERROR, "Can't connect to %d@%s",
			  portno, hostname);
		}
		return(-1);
	}
	/* turn off the alarm. */
	alarm(0);
	(void) signal(SIGALRM, SIG_DFL);
	(void) signal(SIGPIPE, SIG_IGN);

#ifdef 0
	/*
	 * send the verify packet with the GLC_IDENT, GLC_VERSION, and the
	 * TPC_VERSION byte.
	 */
	vpkt.req = htons(GLC_VERIFY);
	vpkt.len = htons(2 * sizeof (u_long));
	vpkt.id = htonl(0L);
	vpkt.ident = htonl(GLC_IDENT);
	vpkt.gl_version = htonl(GLC_VERSION);
	vpkt.loc_version = htonl(TPC_VERSION);
	if (write(tp_sock, (char *) &vpkt, sizeof(vpkt)) != sizeof (vpkt)) {
		msg(PDBUG|PERROR, "Write err on verify to ProofEditor sock %d:", tp_sock);
		close_socks(IFC_CLOSE_TP);
		return(-1);
	}

	msg(PDBUG, "Verify packet (%d) sent to target editor.", GLC_VERIFY);

	/* wait for the welcome packet. */
	if (read(tp_sock, (char *)&pkt, sizeof(pkt)) != sizeof (pkt)) {
		msg(PDBUG|PERROR, "Read err on welcome pkt from ProofEditor sock %d", tp_sock);
		close_socks(IFC_CLOSE_TP);
		return(-1);
	}
	pkt.req = ntohs(pkt.req);
	pkt.len = ntohs(pkt.len);
	if (pkt.req == GLC_GOAWAY) {
		if (pkt.len > sizeof (errmsg))
			pkt.len = sizeof (errmsg) - 1;
		if (pkt.len > 0)
			dlen = read(ts_sock, errmsg, pkt.len);
		errmsg[dlen] = '\0';
		msg(PDBUG, "We're told \"%s\"!", errmsg);
		close_socks(IFC_CLOSE_TP);
		return(-1);
	} else if (pkt.req != GLC_WELCOME) {
		msg(PDBUG, "[First packet request code is %d!]", pkt.req);
		close_socks(IFC_CLOSE_TP);
		return(-1);
	}
#endif
	msg(PDBUG, "[successfully connected to Proof Editor at %s]", hostname);
	return(0);
}


/*
 * routine called when the alarm to mark a timed out connection sends SIG_ALRM.
 */
conn_to()
{
	conn_timeout++;
}

/*
 * return a pointer to a buffer that contains an ascii representation of an
 * ethernet address.
 */
static char	ebuf[56];


char *
p_etheraddr(buf, blen)
	u_char	*buf;
	int	blen;
{
	char	*op;

	bzero(ebuf, sizeof(ebuf));
	for (op = ebuf; blen; blen--) {
		sprintf(op, "%d:", *buf++);
		for (; *op; op++)
			;
	}
	*--op = '\0';
	return(ebuf);
}

#endif VORTEX
