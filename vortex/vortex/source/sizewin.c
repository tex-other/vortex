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

/*
 *  RCS Info: $Header: sizewin.c,v 0.1 87/05/01 12:27:12 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the source editor/user interface written
 *  by John Coker for the VorTeX project under the direction of
 *  Prof. Michael A. Harrison of the University of California at
 *  Berkeley.
 *
 *  Copyright (c) 1987 John L. Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  John Coker and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  sizewin.c - internal routine to size and place new X windows
 */
static char _ID[] = "@(#)sizewin.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include <sys/param.h>
#include <sys/time.h>
#include "vse.h"
#include "window.h"

/*
 *  The bitmap files for creating the two cursors we use for rubber
 *  banding.  The ``upper left'' corner cursor is for placing the
 *  window and the ``lower right'' one is for sizing it dynamically.
 */
#include "bitmaps/ul_image"
#include "bitmaps/ul_mask"
#include "bitmaps/lr_image"
#include "bitmaps/lr_mask"

/*
 *  The user can change the font used for printing the size of
 *  the window with the ResizeFont X default.  We open this
 *  X font the first time we need it, and then not thereafter.
 */
extern char	*DEFSIZEFONT;
static FontInfo	*resize_font = NULL;

/*
 *  The events to read when placing and sizing the window.  We
 *  want the action when placing the window to be triggered
 *  only by a down event, but when sizing, the final size can
 *  be an up event (which makes the most sense).
 */
static int	PLACE_EVENTS = ButtonPressed|MouseMoved;
static int	SIZE_EVENTS = ButtonPressed|ButtonReleased|MouseMoved;

/*
 *  If we want the window frame to blink when placing it every
 *  once in a while, even when the mouse is not being moved,
 *  we #define FLASH_IDLE.  The frame flashes every FLASH_RATE
 *  milliseconds even if the mouse is not moved.  The constant
 *  MILLION is just a shorthand for the obvious number.
 */
#define FLASH_IDLE
#define FLASH_RATE	250

#define MILLION		1000000

/*
 *  Rubber band the window outline for sizing purposes.  We take
 *  two geometry specifications, assuming that both of them have
 *  the size specified.  First we draw an outline the size of the
 *  first geometry, allowing the user to move it.  If he clicks
 *  the left button, we make the window the size of the first
 *  geometry specification.  If he clicks the middle button, we
 *  make the window the size of the second specification.  If he
 *  click the right button, we let him rubber-band the window to
 *  the size he wants, in multiples of given dx, dy and not smaller
 *  than minx, miny.  During sizing, we draw the width and height
 *  (in terms of dx, dy) in the upper-left corner of the window
 *  to be created.  Note that we don't create the window, just
 *  get its size and return it in the given rectangle.
 */

size_window(winp, geom1, geom2, rect)
	struct window	*winp;
	char		*geom1, *geom2;
	struct rect	*rect;
{
	extern FontInfo	*fixedfont();
	extern char	*program;
	static Cursor	ul_corner = NULL,
			lr_corner = NULL;
	Window		subw;
	register int	minx, miny;
	int		xpos, ypos, xsize, ysize;
	int		rows, cols, nxs, nys;
	XButtonEvent	event;
	int		grabbed = FALSE, junk;
	int		xs1, ys1, xs2, ys2;
	char		text[64];
	unsigned int	rmask[FDM_SIZE];
	struct timeval	tmout;
	int		usecs, nfds;

	/*
	 *  Make the cursors as necessary.  These definitions should
	 *  come from the #include'd bitmap(1) files above.
	 */
	if (ul_corner == NULL) {
		/* make the ``upper-left corner'' cursor */
		ul_corner = XCreateCursor(ul_image_width, ul_image_height,
					  ul_image_bits, ul_mask_bits,
					  ul_image_x_hot, ul_image_y_hot,
					  BlackPixel, WhitePixel, GXxor);
	}
	if (lr_corner == NULL) {
		/* make the ``lower-right corner'' cursor */
		lr_corner = XCreateCursor(lr_image_width, lr_image_height,
					  lr_image_bits, lr_mask_bits,
					  lr_image_x_hot, lr_image_y_hot,
					  BlackPixel, WhitePixel, GXxor);
	}
	if (ul_corner == NULL || lr_corner == NULL)
		ierror("Can't create ``corner'' cursors for sizing window!");

	/*
	 *  Get the resize font if not already done.  We got this
	 *  name from an X default already and use the standard
	 *  font opener to get and and make sure it's okay.
	 */
	if (resize_font == NULL)
		resize_font = fixedfont(DEFSIZEFONT);

	/*
	 *  Make sure we have two good geometries.  We get the width
	 *  and height as specified by the two geometries into two
	 *  sets of size variables.
	 */
	if (geom1 == NULL || *geom1 != '=' || geom2 == NULL || *geom2 != '=')
		error("Bad geometries specified for default window sizes.");

	/* get window size from default geometry cols/rows */
	xs1 = ys1 = -1;
	XParseGeometry(geom1, &junk, &junk, &xs1, &ys1);
	if (xs1 < 0 || ys1 < 0)
		error("Default geometry size not specified fully!");
	xs1 = GETWIDTH(winp, xs1);
	ys1 = GETHEIGHT(winp, ys1);

	/* get window size from alternate geometry cols/rows */
	xs2 = ys2 = -1;
	XParseGeometry(geom2, &junk, &junk, &xs2, &ys2);
	if (xs2 < 0 || ys2 < 0)
		error("Alternate geometry size not specified fully!");
	xs2 = GETWIDTH(winp, xs2);
	ys2 = GETHEIGHT(winp, ys2);

	/*
	 *  Rubber band the box.  We keep the size in terms of the
	 *  dx and dy variables we were passed.  We also write the
	 *  size of the window as it changes in the upper left corner
	 *  of the image.  We don't have to worry about this because
	 *  the window will repaint over it later.
	 */
#ifndef DONT_GRAB_SERVER
	grabbed = XGrabServer() != 0;
#endif !DONT_GRAB_SERVER

	if (XGrabMouse(RootWindow, ul_corner, PLACE_EVENTS) == 0)
		ierror("Can't grab mouse to place new window!");

	PROTECT();
	minx = MINWIDTH(winp);
	miny = MINHEIGHT(winp);
	xpos = ypos = 0;
	XQueryMouse(RootWindow, &xpos, &ypos, &subw);
	xsize = xs1;
	ysize = ys1;

	/* set up mask and timeout for select */
	usecs = FLASH_RATE * 1000;
	tmout.tv_sec = usecs / MILLION;
	tmout.tv_usec = usecs % MILLION;

	/* rubber band for position now */
	window_box(xpos, ypos, xsize, ysize, NULL);
	for (;;) {
#ifdef FLASH_IDLE
		/* make sure we flash once in a while */
		while (XPending() <= 0) {
			bzero((char *)rmask, sizeof (rmask));
			FDM_SET(rmask, dpyno(Xdisplay));
			nfds = select(NOFILE, rmask, NULL, NULL, &tmout);
			if (nfds <= 0 || FDM_ISSET(rmask, dpyno(Xdisplay))) {
				window_box(xpos, ypos, xsize, ysize, NULL);
				window_box(xpos, ypos, xsize, ysize, NULL);
				XFlush();
			}
		}
#endif FLASH_IDLE

		event.type = 0;
		XNextEvent(&event);
		switch (event.type) {
		case ButtonPressed:	/* handle a mouse click */
			switch (event.detail & 0377) {
			case LeftButton:
				debug(DXIO, "Default geometry selected.");
				window_box(xpos, ypos, xsize, ysize, NULL);
				xsize = xs1;
				ysize = ys1;
				goto done;
			case MiddleButton:
				debug(DXIO, "Need to rubber-band for size.");
				/* need to get size */
				window_box(xpos, ypos, xsize, ysize, NULL);
				goto dosize;
			case RightButton:
				debug(DXIO, "Alternate geometry selected.");
				window_box(xpos, ypos, xsize, ysize, NULL);
				xsize = xs2;
				ysize = ys2;
				goto done;
			}
			break;
		case MouseMoved:	/* need to redraw the box */
			window_box(xpos, ypos, xsize, ysize, NULL);
			xpos = event.x;
			ypos = event.y;
			window_box(xpos, ypos, xsize, ysize, NULL);
			break;
		}
		XFlush();
	}

dosize:	/* rubber band for size now */
	XUngrabMouse();
	if (XGrabMouse(RootWindow, lr_corner, SIZE_EVENTS) == 0)
		ierror("Can't re-grab mouse to size new window!");
	XWarpMouse(RootWindow, xpos + xsize, ypos + ysize);
	XSync(TRUE);

	cols = COLSOFX(winp, xsize);
	rows = ROWSOFY(winp, ysize);
	sprintf(text, " %dx%d ", cols, rows);
	window_box(xpos, ypos, xsize, ysize, text);
	for (;;) {
		event.type = 0;
		XNextEvent(&event);
		switch (event.type) {
		case ButtonPressed:
		case ButtonReleased:
			debug(DXIO, "Done stretching window outline.");
			window_box(xpos, ypos, xsize, ysize, NULL);
			goto done;
		case MouseMoved:
			nxs = event.x - xpos;
			nys = event.y - ypos;
			if (nxs < minx)
				nxs = minx;
			if (nys < miny)
				nys = miny;
			if (COLSOFX(winp, nxs) != cols ||
			    ROWSOFY(winp, nys) != rows) {
				window_box(xpos, ypos, xsize, ysize, NULL);
				cols = COLSOFX(winp, nxs);
				rows = ROWSOFY(winp, nys);
				xsize = GETWIDTH(winp, cols);
				ysize = GETHEIGHT(winp, rows);
				sprintf(text, " %dx%d ", cols, rows);
				window_box(xpos, ypos, xsize, ysize, text);
			}
			break;
		}
	}

done:	/* return this size and position */
	XUngrabMouse();
	if (grabbed)
		XUngrabServer();
	UNPROTECT();

	/* set elements of rectangle and return */
	rect->re_left = xpos;
	rect->re_top = ypos;
	rect->re_width = xsize;
	rect->re_height = ysize;
	debug(DXIO, "Done placing and sizing window, geometry =%dx%d+%d+%d.",
	      xsize, ysize, xpos, ypos);

	return (0);
}

static
window_box(xpos, ypos, xsize, ysize, text)
	char	*text;
{
	extern int	BackPixel;
	unsigned int	onpixel;
	Vertex		vertexes[5];

	/* XOR with all possible bits */
	onpixel = (unsigned int)-1;

	bzero(vertexes, sizeof (vertexes));
	vertexes[0].x = xpos;
	vertexes[0].y = ypos;
	vertexes[1].x = xpos + xsize;
	vertexes[1].y = ypos;
	vertexes[2].x = xpos + xsize;
	vertexes[2].y = ypos + ysize;
	vertexes[3].x = xpos;
	vertexes[3].y = ypos + ysize;
	vertexes[4].x = xpos;
	vertexes[4].y = ypos;
	XDraw(RootWindow, vertexes, NITEMS(vertexes), 1, 1,
	      onpixel, GXxor, AllPlanes);

	if (text != NULL) {
		/* paint the text in the upper-left corner */
		XText(RootWindow, xpos + 1, ypos + 1,
		      text, strlen(text),
		      resize_font->id, BackPixel, onpixel);
	}

	return (0);
}
