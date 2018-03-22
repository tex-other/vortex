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

/*  Routines to deal with vpe windows.  */

/*  Given a window, a (x, y) position and an (x, y) widht/height, repaint
 *  the damaged portion of that window.  
 */
PaintWindow (window, x, y, width, height)
Window  window;
{
    int     xoffset;
    int     yoffset;
    int     w_width;
    int     w_height;
    int     wid;
    int     DocumentID;
    Nbox    *DocumentGetPage ();
    Nbox    *page;
    
    if ((wid = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  PaintWindow couldn't find it (xw=%d)\n",
		 window);
	return (-1);
    }
    if (WindowGetOffset (wid, &xoffset, &yoffset)) {
	fprintf (stderr, "vpe:  PaintWindow WindowGetOffset failed.\n");
	xoffset = yoffset = 0;
    }
    if (WindowGetSize (wid, &w_width, &w_height)) {
	fprintf (stderr, "vpe:  PaintWindow WindowGetSize failed.\n");
	return (-1);
    }
    if ((DocumentID = WindowGetDocument (wid)) < 0) {
	fprintf (stderr, 
	     "vpe: PaintWindow: wid=%d Couldn't find associated document.\n",
		 wid);
	return (-1);
    }
    /*  XXX - HACK to test the page software.  */
    /*  If there is a non-null page to paint.  */
    if ((page = DocumentGetPage (DocumentID)) != (Nbox *) NULL) {
	paint_page (window, DocumentID);
    }
    else if (TestDebug ("hack") && DocumentValid (DocumentID)) {
	int    fd;
	Nbox   *doc;
	Nbox   *DocumentGetPage ();
	
	printf ("vpe: loading in the fake document.\n");
	if ((fd = open ("f1.1.out", O_RDONLY, 0777)) < 0) {
	    perror ("vpe: PaintWindow: Couldn't open f1.1.out.\n");
	    return (-1);
	}
	TeXBuild (fd, DocumentID, 0);
	if ((doc = DocumentGetPage (DocumentID)) == (Nbox *) NULL) {
	    fprintf (stderr, "vpe: PaintWindow: lost the document!\n");
	    return (-1);
	}
	paint_page (window, DocumentID);
    }
    if (TestDebug ("window")) {
	fprintf (stderr, "vpe:  PaintWindow %dx%d+%d+%d painted.\n",
		 x, y, width, height);
    }
    return (0);
}


