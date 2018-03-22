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
#include <X/Xlib.h>
#include <gl_comm.h>
#include <ps_comm.h>
#include <stdio.h>
#include "macros.h"

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


/*  Ok here is where we read and parse commands from vse, the VorTeX
 *  source editor. 
 */
int ss;    

ParseVSECommand (vse, tex)
{
    u_short request;
    u_short datalen;
    int status;
    u_long *ids;
    u_long *vpe_select();
    u_long  xwindow;
    char    *data;
    u_short x,y;
    u_long bid;
    struct hf { u_short req,dl;u_long xwin;} hhf;
    
    GetShort (request, vse, ParseVSECommand);
    GetShort (datalen, vse, ParseVSECommand);
    GetLong (xwindow, vse, ParseVSECommand);
    if (TestDebug ("vse")) {
	printf ("vpe: VSE request=%d datalen=%d window %d.\n", 
		request, datalen, xwindow);
    }

    switch (request) {
    case GLC_VERIFY:
	if (!VerifySource (vse)) {
	    close (vse);
	    return (-1);
	}
	break;

    case GLC_GOAWAY:
	fprintf (stderr, "vpe: abnormal exit condition.  I am exiting.\n");
	data = (char *) malloc (datalen + 1);
	bzero (data, datalen + 1);
	read (vse, data, datalen);
	fprintf (stderr, "vpe: vse says \"%s\"\n", data);
	return (-1);
	break;
	
    case GLC_WELCOME:
	if (TestDebug ("vse")) {
	    printf ("vpe: Welcomed to VorTeX.\n");
	}
	break;
	
    case GLC_LISTENAT:
	ListenForTeX (vse, datalen);
	break;
	
    case GLC_CONNECT:
	ConnectToTeX (vse, datalen);
	break;
	
    case GLC_FLUSH:
	FlushPendingData (vse);
	break;
	
    case GLC_QUIT:
	if (TestDebug ("vse")) {
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
	    if (read (vse, line, datalen) != datalen) {
		fprintf (stderr, "vpe:  read GLC_ERROR %d bytes: ", datalen);
		perror ("");
		break;
	    }
	    fprintf (stderr, "vpe:  Error message is \"%.*s\"", datalen, line);
	    free (line);
	}
	break;
	
	/*  Ok here starts the list of Proof <-> Source specific commands.  */
    case PSC_CREATE:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_CREATE received from vse.\n");
	}
	GetNewWindow (vse, xwindow, datalen);
	break;
	
    case PSC_DESTROY:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_DESTROY received from vse.\n");
	}
	KillWindow (vse, xwindow, datalen);
	break;
	
    case PSC_RESIZE:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_RESIZE received from vse.\n");
	}
	ResizeWindow (vse, xwindow, datalen);
	break;

    case PSC_EXPOSE:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_EXPOSE received from vse.\n");
	}
	UpdateWindow (vse, xwindow, datalen);
	break;
	
    case PSC_BSTART:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_BSTART received from vse.\n");
	}
	break;
	
    case PSC_BEND:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_BEND received from vse (datalen=%d)\n", datalen);
	}
	break;
	
    case PSC_MOVEABS:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_MOVEABS received from vse.\n");
	}
	WindowMoveAbs (vse, xwindow, datalen);
	break;
	
    case PSC_MOVEREL:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_MOVEREL received from vse.\n");
	}
	WindowMoveRel (vse, xwindow, datalen);
	break;
	
#ifdef notdef
    case PSC_SCROLLABS:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_SCROLLABS received from vse.\n");
	}
	lseek (vse, datalen, 1);
	break;
	
    case PSC_SCROLLREL:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_SCROLLREL received from vse.\n");
	}
	lseek (vse, datalen, 1);
	break;
	
    case PSC_NEWVIEW:
	lseek (vse, datalen, 1);  /* Get past the data - I don't want it.  */
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_NEWVIEW:  I shouldn't get these!!!\n");
	    return (-1);
	}
	break;
#endif notdef
	
    case PSC_GOTOABS:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_GOTOABS received from vse.\n");
	}
	WindowGotoAbs (vse, tex, xwindow, datalen);
	ss = 0;
	break;
	
    case PSC_GOTOREL:
	if (TestDebug ("vse") || TestDebug ("window")) {
	    printf ("vpe: PSC_GOTOREL received from vse.\n");
	}
	WindowGotoRel (vse, xwindow, datalen);
	break;
	
    case PSC_LOGICAL:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_LOGICAL received from vse.\n");
	}
	lseek (vse, datalen, 1);
	break;
	
    case PSC_DOCUMENT:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_DOCUMENT received from vse.\n");
	}
	GetDocument (vse, xwindow, datalen);
	break;
	
    case PSC_DOCPAGES:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_DOCPAGES received from vse.\n");
	}
	lseek (vse, datalen, 1);
	break;
	
    case PSC_SELECT:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_SELECT received from vse.\n");
	}
	if((read(vse,&x,sizeof(u_short)) != sizeof(u_short)) ||(read(vse,&y,sizeof(u_short)) != sizeof(u_short))){
	    fprintf(stderr,"vpe: unable to read selection event\n");
	    return(-1);
	}else{
	    fprintf(stderr,"got coords of %d %d\n",x,y);
	}
	ss = 1;
	x = ntohs(x);
	y = ntohs(y);
	fprintf(stderr,"sending coords of %d %d\n",x,y);
	if((ids = vpe_select(vse,tex,xwindow,x,y,0,ss)) == (u_long *)NULL){
	    
	    fprintf(stderr,"vpe: selection failed somehow\n");
	    /*we may have written an error to vse*/
	}else{
	    hhf.req = PSC_SELECTION;
	    hhf.dl = 2*sizeof(u_long);
	    hhf.xwin = xwindow;
	    /*write it back to vse*/
	    if(write(vse,&hhf,sizeof(struct hf)) != sizeof(struct hf)){
		fprintf(stderr,"select failed :writing selection id\n");
		return(-1);
	    }else if(write(vse,&(ids[0]),sizeof(ids[0])) != sizeof(status)){
		fprintf(stderr,"select failed :writing first id\n");
		return(-1);
	    }else if(write(vse,&(ids[1]),sizeof(ids[1])) != sizeof(status)){
		fprintf(stderr,"select failed :writing second id\n");
		return(-1);
	    }
	}

	break;
    case PSC_SELECTMORE:
	if (TestDebug ("vse") || TestDebug ("document")) {
	    printf ("vpe: PSC_SELECT received from vse.\n");
	}
	if((read(vse,&x,sizeof(u_short)) != sizeof(u_short)) ||(read(vse,&y,sizeof(u_short)) != sizeof(u_short))){
	    fprintf(stderr,"vpe: unable to read selection event\n");
	    return(-1);
	}else{
	    fprintf(stderr,"got coords of %d %d\n",x,y);
	}
	x = ntohs(x);
	y = ntohs(y);
	fprintf(stderr,"sending coords of %d %d\n",x,y);
	if((ids = vpe_select(vse,tex,xwindow,x,y,1)) == (u_long *)NULL){
	    fprintf(stderr,"vpe: selection failed somehow\n");
	    /*we may have written an error to vse*/
	}else{
	    hhf.req = PSC_SELECTION;
	    hhf.dl = 2*sizeof(u_long);
	    hhf.xwin = xwindow;
	    /*write it back to vse*/
	    if(write(vse,&hhf,sizeof(struct hf)) != sizeof(struct hf)){
		fprintf(stderr,"select failed :writing selection id\n");
		return(-1);
	    }else if(write(vse,&(ids[0]),sizeof(ids[0])) != sizeof(ids[0])){
		fprintf(stderr,"select failed :writing first id\n");
		return(-1);
	    }else if(write(vse,&(ids[1]),sizeof(ids[1])) != sizeof(ids[1])){
		fprintf(stderr,"select failed :writing second id\n");
		return(-1);
	    }
	}
	break;
    case PSC_POSITION:
	bzero(&bid,sizeof(u_long));
	ss = 1;
	if(read(vse,&bid,sizeof(u_long)) != sizeof(u_long)){
	    fprintf(stderr,"vpe: nbox id read failed\n");
	    return(-1);
	}
	bid = ntohl(bid);
	fprintf(stderr,"vpe: vse send box id of %x\n",bid);
	fprintf(stderr,"vpe: vse send window id of %x\n",xwindow);
	if((status = scrollit(vse,tex,bid,xwindow,ss)) < 0){
	    fprintf(stderr,"vpe: failed to find the nbox\n");
	}

	break;
    default:
	printf ("vpe: Request %d is not implimented.\n", request);
	break;
    }
    return (0);
}

/*  Check the verify packet we are getting from the source editor is correct.
 */

VerifySource (vse)
{
    u_long  id;
    u_short gl_version;
    u_short l_version;
    
    if (TestDebug ("vse")) {
	printf ("vpe: checking the vse version numbers.\n");
	fflush (stdout);
    }
    GetLong(id, vse, VerifySource);
    GetShort(gl_version, vse, VerifySource);
    GetShort(l_version, vse, VerifySource);
    if (id != GLC_IDENT) {
	fprintf (stderr, "vpe: Wrong identifier (%d != %d).\n", id, GLC_IDENT);
	return (-1);
    }
    if (read (vse, &gl_version, sizeof (gl_version)) != sizeof (gl_version)) {
	perror ("vpe: read gl_version (VerifySource)");
	return (-1);
    }
    if (gl_version != GLC_VERSION) {
	fprintf (stderr, 
		 "vse: Global version numbers do not match (%d != %d).\n",
		 gl_version, GLC_VERSION);
	return (-1);
    }
    if (l_version != PSC_VERSION) {
	fprintf (stderr, 
		 "vse: Local version numbers do not match (%d != %d.\n",
		 l_version, PSC_VERSION);
	return (-1);
    }
    if (TestDebug ("vse")) {
	printf ("vpe: Received correct verify packet from vse.\n");
    }
    return (0);
}





