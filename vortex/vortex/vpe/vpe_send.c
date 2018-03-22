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

#include <stdio.h>
#include <gl_comm.h>
#include <ps_comm.h>

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

/*  Commands which I initiate and send to the source editor.  */

NewView (vse, wid)
{
#ifdef notdef
    int     xpos;
    int     ypos;
    int     pagewidth;
    int     pageheight;
    int     wwidth;
    int     wheight;
    short   data;
    Window  window;
    Window  WindowGetXWindow ();
    
    if ((window = WindowGetXWindow (wid)) < 0) {
	fprintf (stderr, "vpe:  NewView WindowGetWindow (wid=%d) failed.\n",
		 wid);
	return (-1);
    }
    if (WindowGetPosition (wid, &xpos, &ypos)) {
	fprintf (stderr, "vpe:  NewView WindowGetPosition failed.\n");
	return (-1);
    }
    if (WindowGetPageSize (wid, &pagewidth, &pageheight)) {
	fprintf (stderr, "vpe:  NewView WindowGetPageSize failed.\n");
	return (-1);
    }
    if (WindowGetSize (wid, &wwidth, wheight)) {
	fprintf (stderr, "vpe:  NewView WindowGetSize failed.\n");
	return (-1);
    }
    if (TestDebug ("window") || TestDebug ("vse") || TestDebug ("socket")) {
	fprintf (stderr, "vpe:  Sending NewView to vse: ");
	fprintf (stderr, "xpos=%d ypos=%d pagewidth=%d pageheight=%d ",
			  xpos, ypos, pagewidth, pageheight);
	fprintf (stderr, "width=%d height=%d\n", wwidth, wheight);
	return (0);
    }
    data = htons (PSC_NEWVIEW);  /*  request  */
    if (write (vse, &data, sizeof (data)) != sizeof (data)) {
	perror ("vpe:  Write (request) failed");
	return (-1);
    }
    data = htons (6 * sizeof (data));  /* datalen  */
    if (write (vse, &data, sizeof (short)) != sizeof (data)) {
	perror ("vpe:  Write (datalen) failed");
	return (-1);
    }
    window = htonl (window);
    if (write (vse, &window, sizeof (window)) != sizeof (window)) {
	perror ("vpe:  Write (datalen) failed");
	return (-1);
    }
    xpos = ntohs (xpos);
    if (write (vse, &xpos, sizeof (xpos)) != sizeof (xpos)) {
	fprintf (stderr, "vpe:  Write (xpos) failed.\n");
	return (-1);
    }
    ypos = ntohs (ypos);
    if (write (vse, &ypos, sizeof (ypos)) != sizeof (ypos)) {
	fprintf (stderr, "vpe:  Write (ypos) failed.\n");
	return (-1);
    }
    pagewidth = ntohs (pagewidth);
    if (write (vse, &pagewidth, sizeof (pagewidth)) != sizeof (pagewidth)) {
	fprintf (stderr, "vpe:  Write (pagewidth) failed.\n");
	return (-1);
    }
    pageheight = ntohs (pageheight);
    if (write (vse, &pageheight, sizeof (pageheight)) != sizeof (pageheight)) {
	fprintf (stderr, "vpe:  Write (pageheight) failed.\n");
	return (-1);
    }
    wwidth = ntohs (wwidth);
    if (write (vse, &wwidth, sizeof (wwidth)) != sizeof (wwidth)) {
	fprintf (stderr, "vpe:  Write (wwidth) failed.\n");
	return (-1);
    }
    wheight = ntohs (wheight);
    if (write (vse, &wheight, sizeof (wheight)) != sizeof (wheight)) {
	fprintf (stderr, "vpe:  Write (wheight) failed.\n");
	return (-1);
    }
    return (0);
#endif notdef
}
