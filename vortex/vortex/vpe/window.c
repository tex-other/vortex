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

int             WindowLength = 4;
int             LastWindow = 0;
struct window   *window = (struct window *) NULL;
Cursor casual;
Cursor special;

#define Check(id) \
    if (((id) < 0) || ((id) > LastWindow)) { \
	return (-1); \
    }

/*  Get a window descriptor.  This returns an index into the window array.  */
WindowNew (w, width, height, xoffset, yoffset, fgp, fgpm, bgp, bgpm, hlp, hlpm)
u_long  w;
{
    register                wid;
    register struct window  *wp;
    
    if (window == (struct window *) NULL) {
	window = (struct window *) malloc
	    (WindowLength * sizeof (struct window));
	LastWindow = 0;
    }
    for (wid = 0; wid < LastWindow; wid++) {
	if (!window[wid].wi_active) {
	    break;
	}
    }
    if ((wid == LastWindow) && (LastWindow == WindowLength)) {
	struct window  *new;
	
	new = (struct window *) malloc (2 * WindowLength * 
					sizeof (struct window));
	bcopy (window, new, WindowLength * sizeof (struct window));
	if (TestDebug ("window")) {
	    printf ("vpe:  wid=%d allocating LastWindow=%d WL<-%d from %d\n", 
		    wid, LastWindow, WindowLength, 2 * WindowLength);
	    printf ("vpe: window=0x%x new=0x%x &new[wid]=0x%x\n", 
		    window, new, &new[wid]);
	}
	WindowLength *= 2;
	free (window);
	window = new;
	LastWindow++;
    }
    else {
	LastWindow++;
    }
    wp = &window[wid];
    wp->wi_window = w;
    wp->wi_width = width;
    wp->wi_height = height;
    wp->wi_xoffset = xoffset;
    wp->wi_yoffset = yoffset;
    wp->wi_fg_pixel = fgp;
    wp->wi_fg_pixmap = fgpm;
    wp->wi_bg_pixel = bgp;
    wp->wi_bg_pixmap = bgpm;
    wp->wi_hl_pixel = hlp;
    wp->wi_hl_pixmap = hlpm;
    wp->wi_active = 1;
    wp->wi_document = 0;  /* XXX: Initialize to document 0.  */
    bzero (&wp->wi_position, sizeof (struct Position));
    if (TestDebug ("window")) {
	printf ("vpe: WindowNew: wid=%d document=%d: painting.\n", wid, 
		window[wid].wi_document);
    }
    if (PaintWindow (w, xoffset, yoffset, width, height)) {
	fprintf (stderr, "vpe: WindowInit: PaintWindow failed.\n");
    }
    return (wid);
}

/*  Delete window with window ID wid.  */
WindowDelete (wid)
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    if (wid == LastWindow - 1) {
	LastWindow--;
    }
    bzero (&window[wid], sizeof (struct window));
    return (0);
}

/*  Return the X window associated with window ID wid.  If this is not a
 *  valid window, return (Window) 0.  
 */
u_long
WindowName (wid)
{
    if ((wid < 0) || (wid > LastWindow)) {
	return ((Window) -1);
    }
    else {
	return (window[wid].wi_window);
    }
}

/*  Return the number of active windows.  */
WindowCount ()
{
    return (LastWindow);
}

/*  Resize the given window.  */
WindowResize (wid, width, height, xoffset, yoffset)
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    window[wid].wi_width = width;
    window[wid].wi_height = height;
    window[wid].wi_xoffset = xoffset;
    window[wid].wi_yoffset = yoffset;
}

/*  Get the document number associated with WindowID wid.  */
WindowGetDocument (wid)
{
    if ((wid < 0) || (wid > LastWindow)) {
	return (-1);
    }
    if (TestDebug ("window")) {
	printf ("vpe: WindowGetDocument: wid=%d document=%d\n", wid,
		window[wid].wi_document);
    }
    return (window[wid].wi_document);
}

/*  Change the document number associated with window ID wid to document. 
 *  Note that document is one of my document IDs, not the one which vse
 *  passes.  
 */
WindowChangeDocument (wid, document)
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    window[wid].wi_document = document;
    if (TestDebug ("window")) {
	printf ("vpe: WindowChangeDocument: wid=%d did<-%d\n", wid, document);
    }
    return (0);
}

/*  Figure out the window ID for the given window.  */
WindowGetWindow (w)
Window  w;
{
    register i;
    
    for (i = 0; i < LastWindow; i++) {
	if (window[i].wi_window == w) {
	    return (i);
	}
    }
    return (-1);
}

/*  Set the page number and top of page pointer for the given window to the given page.  */
WindowSetPage (wid, pagenumber)
{
    
    if ((wid < 0) || (wid > LastWindow)) {
	fprintf (stderr, "vpe:  WindowSetPage invalid window id %d.\n", wid);
	return (-1);
    }
    
    window[wid].wi_position.p_pagenumber = pagenumber;
    /*this shall be changed to get the pagenode to wid*/
    window[wid].wi_position.p_page 
	= (struct node *)DocGetPgbynu(window[wid].wi_document, pagenumber);
    return (0);
}

/*  Return the current page number for the given document.  */
WindowGetPage (wid)
{
    if ((wid < 0) || (wid > LastWindow)) {
	fprintf (stderr, "vpe:  WindowGetPage invaliud window id %d.\n", wid);
	return (-1);
    }
    return (window[wid].wi_position.p_pagenumber);
}

/*  Set the (x, y) position on the window wid to xpos, ypos and paint.  */
WindowSetPosition (wid, xpos, ypos)
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    if (xpos < 0) {
	xpos = 0;
    }
    if (ypos < 0) {
	ypos = 0;
    }
    window[wid].wi_position.p_x = xpos;
    window[wid].wi_position.p_y = ypos;
    /*  Update the screen.  */
    WindowPaint (wid);
    return (0);
}

/*  Return the (x, y) position on the page of window wid in the pointers
 *  *xp, *yp.
 */
WindowGetPosition (wid, xp, yp)
int     *xp;
int     *yp;
{
    if ((wid < 0) || (wid >= LastWindow)) {
	fprintf (stderr, "vpe: WindowGetPosition wid=%d is invalid.\n", wid);
	return (-1);
    }
    *xp = window[wid].wi_position.p_x;
    *yp = window[wid].wi_position.p_y;
    return (0);
}

/*  Return the (width, height) in bits of the current page.  */
WindowGetPageSize (wid, xp, yp)
int     *xp;
int     *yp;
{
    int                     pagenumber;
    int                     DocumentID;
    int                     length;
    register struct window  *wp;
    
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    DocumentID = window[wid].wi_document;
    return (DocumentGetSize (DocumentID, xp, yp));
}

WindowGetSize (wid, xp, yp)
int     *xp;
int     *yp;
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    *xp = window[wid].wi_width;
    *yp = window[wid].wi_height;
    return (0);
}

/*  Get the (x, y) offset from the upper-left corner of the window.  xp and yp
 *  are pointers, the values are returned in them.
 */
WindowGetOffset (wid, xp, yp)
int     *xp;
int     *yp;
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    *xp = window[wid].wi_xoffset;
    *yp = window[wid].wi_yoffset;
    return (0);
}

/*  Return the X window associated with window wid.  */
Window
WindowGetXWindow (wid)
{
    if ((wid < 0) || (wid >= LastWindow)) {
	return (-1);
    }
    return (window[wid].wi_window);
}

WindowGetFGPixel (wid, fg_pixel, fg_pixmap)
int     *fg_pixel;
Pixmap  *fg_pixmap;
{
    Check(wid);
    if (fg_pixel != (int *) NULL) {
	*fg_pixel = window[wid].wi_fg_pixel;
    }
    if (fg_pixmap != (Pixmap *) NULL) {
	*fg_pixmap = window[wid].wi_fg_pixmap;
    }
    return (0);
}

WindowGetBGPixel (wid, bg_pixel, bg_pixmap)
int     *bg_pixel;
Pixmap  *bg_pixmap;
{
    Check(wid);
    if (bg_pixel != (int *) NULL) {
	*bg_pixel = window[wid].wi_bg_pixel;
    }
    if (bg_pixmap != (Pixmap *) NULL) {
	*bg_pixmap = window[wid].wi_bg_pixmap;
    }
    return (0);
}

WindowGetHLPixel (wid, hl_pixel, hl_pixmap)
int     *hl_pixel;
Pixmap  *hl_pixmap;
{
    Check(wid);
    if (hl_pixel != (int *) NULL) {
	*hl_pixel = window[wid].wi_hl_pixel;
    }
    if (hl_pixmap != (Pixmap *) NULL) {
	*hl_pixmap = window[wid].wi_hl_pixmap;
    }
    return (0);
}

DocumentPaint (did)
{
    int     wid;
    
    for (wid = 0; wid < LastWindow; wid++) {
	if (window[wid].wi_active) {
	    if (window[wid].wi_document == did) {
		WindowPaint (wid);
	    }
	}
    }
}
