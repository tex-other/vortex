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
#include <sys/ioctl.h>
#include <X/Xlib.h>
#include <gl_comm.h>
#include <tp_comm.h>
#include <macros.h>
#include <stdio.h>
#include "document.h"    

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

int     pageorder = 0;
int     texreqstat = 0;
#ifndef MAX
#define MAX(a, b)       ((a) > (b) ? (a) : (b))
#endif MAX

ParseTEXCommand (vse, tex)
{
    int     pageno;
    int     nchars;
    char    *data;
    u_long  document;    /*this not used anywhere?*/
    int     doc = 0;
    u_short datalen;
    u_short request;
    extern  struct Context *Context;
    struct  Page *p;

    ioctl (tex, FIONREAD, &nchars);
    if (read (tex, &request, sizeof (request)) != sizeof (request)) {
	perror ("vpe: read request (tex socket)");
	return (-1);
    }
    request = ntohs (request);
    if (read (tex, &datalen, sizeof (request)) != sizeof (datalen)) {
	perror ("vpe: read datalen (tex socket)");
	return (-1);
    }
    datalen = ntohs (datalen);
    if (read (tex, &pageno, sizeof (pageno)) != sizeof (pageno)) {
	perror ("vpe: read page number (tex socket)");
	return (-1);
    }
    pageno = ntohl (pageno);
    if (TestDebug ("tex")) {
	printf ("vpe: TEX request=%d datalen=%d pageno=%d.\n", 
		request, datalen, pageno);
    }
    switch (request) {
    case GLC_VERIFY:
	if (!VerifyTeX (tex)) {
	    close (tex);
	    return (-1);
	}
	break;

    case GLC_GOAWAY:
	fprintf (stderr, "vpe: abnormal exit condition.  I am exiting.\n");
	return (-1);
	break;
	
    case GLC_WELCOME:
	if (TestDebug ("tex")) {
	    printf ("vpe: Welcomed to VorTeX.\n");
	}
	break;
	
    case GLC_LISTENAT:
	ListenForTeX (tex, datalen);
	break;
	
    case GLC_CONNECT:
	ConnectToTeX (tex, datalen);
	break;
	
    case GLC_FLUSH:
	FlushPendingData (tex);
	break;
	
    case GLC_QUIT:
	if (TestDebug ("tex")) {
	    fprintf (stderr, "vpe:  Received a quit command: I'm gone.\n");
	}
	return (-1);
	break;
	
    case GLC_ABORT:
	fprintf (stderr, "vpe:  Received an abort command, exiting.\n");
	return (-1);
	break;
	
    case GLC_ERROR:
	fprintf (stderr, "vpe:  Received an error message: ");
	fprintf (stderr, "I shouldn't get these.\n");
	if (datalen) {
	    char   *line;

	    if ((line = (char *) malloc ((u_long) datalen)) == NULL) {
		fprintf (stderr, "vpe:  malloc failed in GLC_ERROR.\n");
		return (-1);
	    }
	    if (read (tex, line, datalen) != datalen) {
		fprintf (stderr, "vpe:  read GLC_ERROR %d bytes: ", datalen);
		perror ("");
		break;
	    }
	    fprintf (stderr, "vpe:  Error message is \"%.*s\"", datalen, line);
	    free (line);
	}
	break;
	
    case TPC_PAGEOKAY:
	if (TestDebug ("tex") || TestDebug ("document")) {
	    fprintf(stderr,"vpe: recieved a page okay from tex\n");
	    /* should this mean I move some global page to 
	     * a referencable structure??
	     */
	}
	texreqstat = TPC_PAGEOKAY;
	break;
    case TPC_PAGEBAD:
	if (TestDebug ("tex") || TestDebug ("document")) {
	    fprintf(stderr,"vpe: told I sent a bad page\n");
	}
	texreqstat = TPC_PAGEBAD;
	break;
    case TPC_SENDPAGE:
	if (TestDebug ("tex") || TestDebug ("document")) {
	    printf ("vpe:  received request TPC_SENDHEADER.\n");
	}
	lseek (tex, datalen, 1);
	break;
	
    case TPC_PAGEINFO:
	if (TestDebug ("tex") || TestDebug ("document")) {
	    printf ("vpe:  received request TPC_PAGEINFO.\n");
	}
	
	/*  NOTE that when we install multiple connections to TeX processes
	 *  the document number (currently a 0 in the TeXBuild call) will
	 *  be determined by which TeX process we are communicating with.
	 */
	
/*	for(p = Context[doc].c_page;p != (struct Page *)NULL ;p->p_next){*/
/*	    if(p->p_number == pageno){*/
/*		DocumentFreePage(p,doc);*/
/*	    }*/
/*	}*/
	if (TestDebug ("document-test")) {
	    char  filename[BUFSIZ];
	    char  page[BUFSIZ];
	    int   len;
	    int   totallen = 0;
	    int   pagefd;
	    
	    sprintf (filename, "vpe-test-file.%d", pageorder++);
	    pagefd = open (filename, O_RDWR|O_CREAT|O_TRUNC, 0775);
	    if (pagefd < 0) {
		fprintf (stderr, "Couldn't open %s", filename);
		perror ("");
		LEAVE(1);
	    }
	    while (ioctl (tex, FIONREAD, &len), len > 0) {
		if (read (tex, page, MAX(len, sizeof (page))) != 
		    MAX(len, sizeof (page))) {
		    perror ("read tex page");
		    return (-1);
		}
		if (write (pagefd, page, len) != len) {
		    perror ("write tex page");
		    return (-1);
		}
		totallen += len;
	    }
	    if (len < 0) {
		perror ("ioctl FIONREAD on tex socket");
		LEAVE(1);
	    }
	    fprintf (stderr, "Storing page in file %s (%d bytes).\n", 
		     filename, totallen);
	    if (lseek (pagefd, 0, 0)) {
		perror ("lseek pagefd");
		return (-1);
	    }
	    fprintf (stderr, ">>> pagefd=%d ", pagefd);
	    TeXBuild (pagefd, 0, pageno);
	}
	else {
	    TeXBuild (tex, 0, pageno);
	}
	texreqstat = 0;
	break;
	
    default:
	fprintf (stderr,
	 "vpe:  TeX request %d (tex=%d): This opcode is unknown to me.\n",
		 request, tex);
	return (-1);
	break;
    }
    return (0);
}

VerifyTeX (tex)
{
    u_long  ident;
    u_short gl_version;
    u_short l_version;
    
    GetLong (ident, tex, VerifyTeX);
    GetShort (gl_version, tex, VerifyTeX);
    GetShort (l_version, tex, VerifyTeX);
    if (ident != GLC_IDENT) {
	fprintf (stderr, "vpe:  VerifyTeX ident is wrong.\n");
	return (-1);
    }    
    if (gl_version != GLC_VERSION) {
	fprintf (stderr, "vpe:  VerifyTeX gl_version is wrong.\n");
	return (-1);
    }
    if (l_version != TPC_VERSION) {
	fprintf (stderr, "vpe:  VerifyTeX l_version is wrong.\n");
	return (-1);
    }
    return (0);
}
