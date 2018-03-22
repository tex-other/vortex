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

#include <sys/types.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netdb.h>
#include <stdio.h>
#include <ps_comm.h>
#include <gl_comm.h>
#include "socket.h"

/*
 *  This code is part of the VorTeX project.  This file was
 *  written by Steven Procter for the VorTeX project under the
 *  direction of Prof. Michael A. Harrison of the University
 *  of California at Berkeley.  This code was written by Steven
 *  Procter.
 *
 *  Copyright (c) 1987 Steven James Procter
 *  University of California, Berkeley
 *  procter@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  Steven Procter and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 */

/*  The error number.  */
extern int  errno;

/*  This is externally defined in main.c  */
int     tex_socket;

/*  Make the connection to the source editor, vse.  The hostname and the
 *  socket number to connect to are passed to this function.
 */
ConnectToSource (host, port)
char    *host;
{
    int                     s;
    struct sockaddr_in      sin;
    struct hostent          *gethostbyname ();
    struct hostent          *he;
    struct verify_packet    vp;
    
    if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	perror ("vpe (ConnectToSource): socket");
	return (-1);
    }
    if ((he = gethostbyname (host)) == (struct hostent *) NULL) {
	fprintf (stderr, "vpe (ConnectToSource): host \"%s\":", host);
	perror ("");
	return (-1);
    }
    sin.sin_port = htons (port);
    sin.sin_family = AF_INET;
    bcopy (he->h_addr, &sin.sin_addr, he->h_length);
    if (connect (s, &sin, sizeof (struct sockaddr_in))) {
	perror ("vpe (ConnectToSource): connect");
	return (-1);
    }
    if (TestDebug ("socket")) {
	printf ("vpe: connected to source, socket %d.\n", s);
    }
    /*  Send the source editor an acknowledgement that I exist.
     */
    vp.vp_request = GLC_VERIFY;
    vp.vp_datalen = sizeof (u_long) + 2 * sizeof (u_short);
    vp.vp_commdata = 0;
    vp.vp_ident = GLC_IDENT;
    vp.vp_version = GLC_VERSION;
    vp.vp_lversion = PSC_VERSION;

    if (write (s, &vp, sizeof (struct verify_packet)) != 
	sizeof (struct verify_packet)) {
	perror ("vpe: write, verify packet");
	close (s);
	return (-1);
    }
    return (s);
}

/*  ConnectToTeX:  I have been told that I should connect to the TeX 
 *  formatter.  The port number which I should try to connect on and
 *  the timeout (in milliseconds) are passed, both of them u_longs.
 *  The rest of datalen bytes is the host I should try to connect to.
 */

ConnectToTeX (vse, datalen)
{
    int                 s;
    int                 hostlength;
    int                 TimeOut ();
    char                *host;
    u_long              port;
    u_long              timeout;
    struct hostent      *he;
    struct hostent      *gethostbyname();
    struct itimerval    itv;
    struct sockaddr_in  sin;

    if (datalen <= (sizeof (port) + sizeof (timeout))) {
	fprintf (stderr, 
		 "vpe:  Connect to TeX: bad datalen passed (%d).\n",
		 datalen);
	return (-1);
    }
    if (read (vse, &port, sizeof (port)) != sizeof (port)) {
	perror ("vpe:  ConnectToTeX: couldn't read the port number");
	return (-1);
    }
    if (read (vse, &timeout, sizeof (timeout)) != sizeof (timeout)) {
	perror ("vpe:  ConnectToTeX: couldn't read the timeout");
	return (-1);
    }
    timeout = ntohs (timeout);
    hostlength = datalen - sizeof (port) - sizeof (timeout);
    /*  host is not necessarily null terminated  */
    if ((host = (char *) malloc ((u_long) hostlength + 1)) == NULL) {
	fprintf (stderr, "vpe: ConnectToTeX malloc error: no more core???\n");
	return (-1);
    }
    bzero (host, hostlength + 1);
    if (read (vse, host, hostlength) != hostlength) {
	fprintf (stderr, "vse: ConnectToTeX hostlength=%d:", hostlength);
	perror ("");
	return (-1);
    }
    if (TestDebug ("vse") || TestDebug ("tex") || TestDebug ("socket")) {
	printf ("vpe:  Connecting to TeX port=%d timeout=%d host=%s\n", 
		ntohs (port), timeout, host);
	
    }
    if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	perror ("vpe: ConnectToTeX, socket");
	return (-1);
    }
    sin.sin_port = port;
    sin.sin_family = AF_INET;
    
    if ((he = gethostbyname (host)) == (struct hostent *) NULL) {
	fprintf (stderr, "vpe: gethostbyname host=\"%s\":", host);
	perror ("");
	return (-1);
    }
    bcopy ((char *) he->h_addr, (char *) &sin.sin_addr, he->h_length);
    /*  Set an alarm and timeout  */
    itv.it_value.tv_sec = timeout / 1000;
    itv.it_value.tv_usec = timeout % 1000;
    bzero (&itv.it_interval, sizeof (itv.it_interval));
    signal (SIGALRM, TimeOut);
    setitimer (ITIMER_REAL, &itv);
    if (connect (s, &sin, sizeof (struct sockaddr_in))) {
	perror ("vpe: ConnectToTeX connect");
	return (-1);
    }
    signal (SIGALRM, SIG_IGN);

    tex_socket = s;
    return (0);
}

/*  ListenForTeX: The TeX process has been told to connect to me and 
 *  I should listen for a connection from it.  The arguments are a
 *  port number (u_long) and a timeout (u_long).
 */
ListenForTeX (vse, datalen) 
{
    int                 s;
    int                 ns;
    int                 TimeOut ();
    int                 addr_length;
    u_long              port;
    u_long              timeout;
    struct itimerval    itv;
    struct sockaddr_in  sin;
    struct gl_packet    gl;

    if (datalen != (sizeof (port) + sizeof (timeout))) {
	fprintf (stderr, 
		 "vpe:  ListenForTeX: bad datalen passed (%d).\n",
		 datalen);
	return (-1);
    }
    if (read (vse, &port, sizeof (port)) != sizeof (port)) {
	perror ("vpe:  ListenForTeX: couldn't read the port number");
	return (-1);
    }
    if (read (vse, &timeout, sizeof (timeout)) != sizeof (timeout)) {
	perror ("vpe:  ListenForTeX: couldn't read the timeout");
	return (-1);
    }
    port = ntohl (port);
    timeout = ntohl (timeout);
    if (TestDebug ("vse") || TestDebug ("tex") || TestDebug ("socket")) {
	printf ("vpe:  Listening for TeX port=%d timeout=%d\n", 
		port, timeout);
	
    }
    if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	perror ("vpe: ListenForTeX, socket");
	return (-1);
    }
    sin.sin_port = htons (port);
    sin.sin_family = AF_UNIX;
    sin.sin_addr.s_addr = INADDR_ANY;
    while (bind (s, &sin, sizeof (sin))) {
	if (errno != EADDRINUSE) {
	    perror ("vpe: ListenForTeX bind");
	    return (-1);
	}
	if (TestDebug ("socket") || TestDebug ("tex")) {
	    fprintf (stderr, "vpe: port %d unavailable moving up one.\n",
		     port);
	}
	sin.sin_port = htons (++port);
    }
    
    /*  Tell vortex what port I am listening on.  */
    gl.gl_request = htons (GLC_LISTENING);
    gl.gl_datalen = ntohs (sizeof (u_long));
    gl.gl_data = 0;
    if (write (vse, &gl, sizeof (gl)) != sizeof (gl)) {
	perror ("vpe: ListenForTeX write gl");
	return (-1);
    }
    port = htonl (port);
    if (write (vse, &port, sizeof (port)) != sizeof (port)) {
	perror ("vpe: ListenForTeX write port");
	return (-1);
    }
    if (TestDebug ("socket") || TestDebug ("tex") || TestDebug ("vse")) {
	fprintf (stderr, "vpe: notified vse where I am listening.\n");
    }

    /*  Set an alarm and timeout  */
    itv.it_value.tv_sec = timeout / 1000;
    itv.it_value.tv_usec = timeout % 1000;
    bzero (&itv.it_interval, sizeof (itv.it_interval));
    signal (SIGALRM, TimeOut); 
    if (setitimer (ITIMER_REAL, &itv, (struct itimerval *) NULL)) {
	perror ("vpe: ListenForTeX setitimer");
	return (-1);
    }
    addr_length = sizeof (struct sockaddr_in);
    if (listen (s, 1)) {
	perror ("vpe:  ListenForTeX listen");
	close (s);
	return (-1);
    }
    if ((ns = accept (s, &sin, &addr_length)) < 0) {
	perror ("vpe:  ListenForTeX accept");
	close (s);
	return (-1);
    }
    signal (SIGALRM, SIG_IGN);
    close (s);
    if (ns >= 0) {
	tex_socket = ns;
	if (TestDebug ("tex") || TestDebug ("vse") || TestDebug ("socket")) {
	    fprintf (stderr, "vpe:  Connected to tex socket=%d.\n", ns);
	}
	return (0);
    }
    else {
	tex_socket = -1;
	if (TestDebug ("tex") || TestDebug ("vse") || TestDebug ("socket")) {
	    fprintf (stderr, "vpe:  Connection to tex timed out.\n");
	}
	return (-1);
    }
}

/*  Flush all data pending on stream untill an oob data marker.  */

FlushPendingData (stream)
{
    int     mark;
    char    next;

    while (ioctl (stream, SIOCATMARK, &mark), !mark) {
	read (stream, &next, sizeof (next));
    }
    read (stream, &next, sizeof (next));
    return (next);
}

TimeOut ()
{
    if (TestDebug ("socket") || TestDebug ("tex")) {
	fprintf (stderr, "vpe: timed out.\n");
    }
    return (-1);
}
