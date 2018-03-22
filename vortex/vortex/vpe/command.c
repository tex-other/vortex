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

#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <X/Xlib.h>
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


/*  Get commands from the source editor and the TeX formatter.  Note that
 *  the source editor catches all of the X events, including key and
 *  expose, etc., so I don't check to see if I have any events pending.
 *
 *  W is the proof window ID, vse is the IPC connection to the source 
 *  editor, and tex is the IPC connection to the TeX processor. 
 *  Note that vse MUST be a good connection, but tex may not be.
 */

ParseCommand (vse, tex)
{
    int     rmask;
    int     rval;
    int     nbytes;
    
    if (vse < 0) {
	fprintf (stderr, "vse: Not connected to the source editor.");
	return (-1);
    }
    rmask = 1 << vse;
    if (tex >= 0) {
	rmask |= 1 << tex;
    }
    if ((rval = select (32, &rmask, (int *) NULL, (int *) NULL, (int *) NULL,
			(struct timeval *) NULL)) < 0) {
	perror ("vpe: select");
	return (-1);
    }
    if (rval > 0) {
	if (rmask & (1 << vse)) {
	    ioctl (vse, FIONREAD, &nbytes);
	    if (!nbytes) {
		fprintf (stderr, "vpe:  Reading an inactive file! Abort.\n");
		LEAVE(1);
	    }
	    if (TestDebug ("vse")) {
		printf ("vpe: received a packet from vse (%d bytes).\n",
			nbytes);
	    }
	    if (ParseVSECommand (vse, tex)) {
		return (-1);
	    }
	}
	if ((tex >= 0) && (rmask & (1 << tex))) {
	    if (TestDebug ("vse")) {
		printf ("vpe: received a packet from TeX.\n");
	    }
	    if (ParseTEXCommand (vse, tex)) {
		return (-1);
	    }
	}
    }
    else {
	fprintf (stderr, "vpe: select timed out, why?\n");
    }
    return (0);
}
