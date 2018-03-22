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
#include <X/Xlib.h>
#include <stdio.h>
#include <tp_comm.h>
#include "document.h"
#include "window.h"

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

/*  Set the document number in the given window.  */
GetDocument (vse, window, datalen)
u_long  window;
u_short datalen;
{
    int     fd;
    int     WindowID;
    int     DocumentID;
    u_long  docnumber;
    Nbox    *doc;
    Nbox    *TeXBuild ();
    
    if (datalen != (sizeof docnumber)) {
	fprintf (stderr, "vpe:  GetDocument wrong datalen (%d != %d).\n",
		 datalen, sizeof (docnumber));
    }
    if (read (vse, &docnumber, sizeof (docnumber)) != sizeof (docnumber)) {
	perror ("vpe:  GetDocument couldn't read docnumber");
	return (-1);
    }
    docnumber = ntohl (docnumber);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  GetDocument unknown window %d\n", window);
	return (-1);
    }
    if (TestDebug ("vse") || TestDebug ("document") || TestDebug ("window")) {
	printf ("vpe: GetDocument window=%d WindowID=%d document=%d\n",
		window, WindowID, docnumber);
    }
    if ((DocumentID = DocumentGetDocument (docnumber)) < 0) {
	DocumentID = DocumentAdd ();
	if (DocumentSetNumber (DocumentID, docnumber)) {
	    fprintf (stderr, "vpe: GetDocument: Couldn't add a document.\n");
	    return (-1);
	}
    }
    if (WindowChangeDocument (WindowID, DocumentID)) {
	fprintf (stderr, "vpe:  WindowChangeDocument failed (wid=%d did=%d)\n",
		 WindowID, docnumber);
	return (-1);
    }
    /*  Now paint the window.  */
    WindowPaint (WindowID);
    return (0);
}

