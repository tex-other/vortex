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
#include <stdio.h>
#include <gl_comm.h>
#include <tp_comm.h>
#include "window.h"
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

/*  The source editor has created a new window for me and is telling
 *  me about it.  
 */

GetNewWindow (vse, window, datalen)
u_long  window;
u_short datalen;
{
    int         WindowID;
    u_short     xoffset;
    u_short     yoffset;
    u_short     width;
    u_short     height;
    u_long      fg_pixel;
    u_long      fg_pixmap;
    u_long      bg_pixel;
    u_long      bg_pixmap;
    u_long      hl_pixel;
    u_long      hl_pixmap;
    
    if (datalen != 4 * sizeof (u_short) + 6 * sizeof (u_long)) {
	fprintf (stderr, 
		 "vse: Wrong amount of data pending for GetNewWindow.\n");
	fprintf (stderr, "vse: %d bytes pending (should be %d).\n", 
		 datalen, 4 * sizeof (u_short) + 6 * sizeof (u_long));
	return (-1);
    }
    GetShort(xoffset, vse, GetNewWindow);
    GetShort(yoffset, vse, GetNewWindow);
    GetShort(width, vse, GetNewWindow);
    GetShort(height, vse, GetNewWindow);
    GetLong(fg_pixel, vse, GetNewWindow);
    GetLong(fg_pixmap, vse, GetNewWindow);
    GetLong(bg_pixel, vse, GetNewWindow);
    GetLong(bg_pixmap, vse, GetNewWindow);
    GetLong(hl_pixel, vse, GetNewWindow);
    GetLong(hl_pixmap, vse, GetNewWindow);
    
    if (TestDebug ("vse") || TestDebug ("window")) {
	printf ("vpe: New window %dx%d+%d+%d.\n", (int) width, (int) height, 
		(int) xoffset, (int) yoffset);
    }
    if ((WindowID = WindowNew (window, width, height, xoffset, yoffset,
			       fg_pixel, fg_pixmap, bg_pixel, bg_pixmap,
			       hl_pixel, hl_pixmap)) < 0) {
	fprintf (stderr, 
		 "vpe:  GetNewWindow: couldn't create the new window.\n");


	return (-1);
    }

    return (0);
}

KillWindow (vse, window, datalen)
u_long  window;
u_short datalen;
{
    int     WindowID;

    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe: KillWindow %d: This window is unknown to me.\n",
		 window);
	return (-1);
    }
    WindowDelete (WindowID);
    if (TestDebug ("vse") || TestDebug ("window")) {
	printf ("vpe: Killed window %d (WindowID %d).\n", window, WindowID);
    }
    return (0);
}

ResizeWindow (vse, window, datalen)
u_long  window;
u_short datalen;
{
    int     WindowID;
    u_short width;
    u_short height;
    u_short xoffset;
    u_short yoffset;
    
    if (datalen != 4 * sizeof (u_short)) {
	fprintf (stderr, "vpe:  ResizeWindow wrong datalen (%d != %d).\n",
		 datalen, 4 * sizeof (u_short));
	lseek (vse, datalen, 1);
	return (-1);
    }
    GetShort(xoffset, vse, ResizeWindow);
    GetShort(yoffset, vse, ResizeWindow);
    GetShort(width, vse, ResizeWindow);
    GetShort(height, vse, ResizeWindow);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, 
		 "vpe: ResizeWindow %d: This window is unknown to me.\n",
		 window);
	return (-1);
    }
    WindowResize (WindowID, width, height, xoffset, yoffset);
    if (TestDebug ("vse") || TestDebug ("window")) {
	printf ("vpe: Resized window %d (WindowID %d) to =%dx%d+%d+%d.\n", 
		window, WindowID, width, height, xoffset, yoffset);
    }
    return (0);
}

UpdateWindow (vse, window, datalen)
u_long  window;
u_short datalen;
{
    int     WindowID;
    int     xoffset;
    int     yoffset;
    short   left;
    short   top;
    u_short width;
    u_short height;

    if (datalen != (2 * sizeof (u_short) + 2 * sizeof (short))) {
	fprintf (stderr, "vpe:  ExposeWindow wrong datalen (%d != %d).\n",
		 2 * sizeof (u_short) + 2 * sizeof (short), datalen);
    }
    GetShort(left, vse, UpdateWindow);
    GetShort(top, vse, UpdateWindow);
    GetShort(width, vse, UpdateWindow);
    GetShort(height, vse, UpdateWindow);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  UpdateWindow no wid for window %d.\n", 
		 window);
	return (-1);
    }
    if (WindowGetOffset (WindowID, &xoffset, &yoffset)) {
	fprintf (stderr, "vpe:  UpdateWindow Couldn't get the offset.\n");
	return (-1);
    }
    left = (short) ntohs (left);
    top = (short) ntohs (top);
    width = ntohs (width);
    height = ntohs (height);
    if (TestDebug ("vse") || TestDebug ("window")) {
	fprintf (stderr, "vpe:  Expose window %d: <%d %d> <%d %d>\n",
		 window, left, top, width, height);
    }
    if (PaintWindow (window, left, top, width, height)) {
	fprintf (stderr, "vpe:  PaintWindow (%d %d) (%d %d) failed.\n",
		 left, top, width, height);
	return (-1);
    }
    return (0);
}

/*  Goto an absolute position in the document.  */

WindowGotoAbs (vse, tex, window, datalen)
u_long  window;
u_short datalen;
{
    u_long  pagenumber;
    int     WindowID;

    GetLong(pagenumber, vse, WindowGotoAbs);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  DocumentGotoAbs couldn't find a WindowID.\n");
	return (-1);
    }
    if (WindowSetPage (WindowID, pagenumber)) {
	fprintf (stderr, "vpe:  WindowGotoAbs couldn't go to page %d.\n",
		 pagenumber);
	return (-1);
    }
    if (DocumentCheckPage (tex, WindowGetDocument (WindowID), pagenumber) < 0) {
	fprintf (stderr, "vpe:  WindowGotoAbs couldn't check for page %d.\n",
		 pagenumber);
	return (-1);
    }
    if (TestDebug ("vse") || TestDebug ("document")) {
	fprintf (stderr, "vpe: WindowGotoAbs pagenumber=%d wid=%d.\n", 
		pagenumber, WindowID);
    }
    return (0);
}

WindowGotoRel (vse, window, datalen)
u_long  window;
u_short datalen;
{
    long    offset;
    int     pagenumber;
    int     WindowID;

    GetLong(offset, vse, WindowGotoRel);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  DocumentGotoAbs couldn't find a WindowID.\n");
	return (-1);
    }
    pagenumber = WindowGetPage (WindowID);
    pagenumber += offset;
    if (WindowSetPage (WindowID, pagenumber)) {
	fprintf (stderr, "vpe:  WindowGotoRel WindowSetPage failed wid=%d.\n",
		 WindowID);
	return (-1);
    }
    if (TestDebug ("hack")) {
	int    xpos;
	int    ypos;
	int    width;
	int    height;
	int    xoffset;
	int    yoffset;
	int    DocumentID;
	short  *document;
	short  *DocumentGetBitmap ();
	
	printf ("vpe: DocumentGotoRel offset=%d pagenumber=%d wid=%d.\n", 
		offset, pagenumber, WindowID);
	WindowGetPosition (WindowID, &xpos, &ypos);
	WindowSetPosition (WindowID, xpos, ypos + 100);
	WindowGetSize (WindowID, &width, &height);
	WindowGetOffset (WindowID, &xoffset, &yoffset);
	DocumentID = WindowGetDocument (WindowID);
	paint_page (window, DocumentID);
    }
    return (0);
}

WindowPaint (WindowID)
{
    int     xpos;
    int     ypos;
    int     width;
    int     height;
    int     xoffset;
    int     yoffset;
    int     DocumentID;
    Window  window;
    Window  WindowGetXWindow ();
    
    WindowGetPosition (WindowID, &xpos, &ypos);
    WindowGetSize (WindowID, &width, &height);
    WindowGetOffset (WindowID, &xoffset, &yoffset);
    DocumentID = WindowGetDocument (WindowID);
    window = WindowGetXWindow (WindowID);
    paint_page (window, DocumentID);
}

WindowMoveAbs (vse, window, datalen)
u_long  window;
u_short datalen;
{
    u_short xpos;
    u_short ypos;
    int     WindowID;
    
    if (datalen != 2 * sizeof (u_short)) {
	fprintf (stderr, "vpe:  WindowMoveAbs wrong datalen (%d != %d).\n",
		 datalen, 2 * sizeof (short));
	return (-1);
    }
    GetShort(xpos, vse, WindowMoveAbs);
    GetShort(ypos, vse, WindowMoveAbs);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  WindowMoveAbs couldn't find window %d.\n",
		 window);
    }
    if (WindowSetPosition (WindowID, xpos, ypos)) {
	fprintf (stderr, "vpe:  WindowMoveAbs WindowSetPosition failed.\n");
	return (-1);
    }
    if (TestDebug ("window") || TestDebug ("vse")) {
	printf ("vpe: WindowMoveAbs to <%d %d>.\n", xpos, ypos);
    }
}

WindowMoveRel (vse, window, datalen)
u_long  window;
u_short datalen;
{
    int     xpos;
    int     ypos;
    short   xdelta;
    short   ydelta;
    int     WindowID;
    
    if (datalen != 2 * sizeof (short)) {
	fprintf (stderr, "vpe:  WindowMoveAbs wrong datalen (%d != %d).\n",
		 datalen, 2 * sizeof (short));
	return (-1);
    }
    GetShort(xdelta, vse, WindowMoveRel);
    GetShort(ydelta, vse, WindowMoveRel);
    if ((WindowID = WindowGetWindow (window)) < 0) {
	fprintf (stderr, "vpe:  WindowMoveRel no window id for window %d.\n",
		 window);
	return (-1);
    }
    if (WindowGetPosition (WindowID, &xpos, &ypos)) {
	fprintf (stderr, "vpe:  WindowMoveRel WindowGetPosition failed.\n");
	return (-1);
    }
    if (WindowSetPosition (WindowID, xpos + xdelta, ypos + ydelta)) {
	fprintf (stderr, "vpe:  WindowMoveRel WindowSetPosition failed.\n");
	return (-1);
    }
    xpos += xdelta;
    ypos += ydelta;
    WindowGetPosition (WindowID, &xdelta, &ydelta);
    WindowPaint (WindowID);
    if (TestDebug ("window") || TestDebug ("vse")) {
	printf ("vpe: WindowMoveRel <%d, %d>+<%d %d>.\n",
		xpos, ypos, xdelta, ydelta);
    }
    return (0);
}
