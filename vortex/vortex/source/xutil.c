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
 *  RCS Info: $Header: xutil.c,v 0.1 87/05/01 12:34:00 john Locked $
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
 *  xutil.c - internal support routines for X windows
 */
static char _ID[] = "@(#)xutil.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  These are the global default values for the windows.
 *  Most of these specify X values which the editor doesn't
 *  care about.  These are set up by getdefaults() at startup.
 */
extern char	*DEFGEOMETRY, *ALTGEOMETRY, *ICONGEOMETRY;
extern char	*DEFFONTNAME;
extern int	DEFBDWIDTH, DEFINTBORDER;

extern int	ForePixel;
extern int	BackPixel;
extern int	MousePixel;
extern int	StripePixel;
extern int	HiLightPixel;
extern Pixmap	BackPixmap;
extern Pixmap	BorderPixmap;

extern Cursor	BaseCursor;

/*
 *  WINDOW_EVENTS is the mask of all events which we accept on all
 *  our X windows.  SWITCH_EVENTS are a subset of the WINDOW_EVENTS
 *  which cause the current window to change.  These are typically
 *  those which are caused by input or moving into the window.
 *  Note that we won't switch the current window if the current
 *  window has the input focus.  ICONWN_EVENTS are those events
 *  we care about on icon windows.
 */
unsigned int	WINDOW_EVENTS = KeyPressed|ButtonPressed |
			        ExposeRegion|ExposeWindow |
			        EnterWindow | UnmapWindow;
unsigned int	SWITCH_EVENTS = KeyPressed|ButtonPressed|EnterWindow;
unsigned int	ICONWN_EVENTS =	KeyPressed|ButtonPressed |
				ExposeRegion|ExposeWindow;
unsigned int	ICPASS_EVENTS = KeyPressed|ButtonPressed;

#include "bitmaps/vortex_logo"

struct window *
makewindow(geometry, fontname, bufp)
	char		*geometry, *fontname;
	struct buffer	*bufp;
{
	extern FontInfo	*fixedfont();
	extern char	*program;
	extern Pixmap	GrayPixmap;
	static int	nextindex = 0;
	struct window	*win;
	int		left, top, width, height, rows, cols;

	/* zero the window structure so we know we start out fresh */
	win = (struct window *)valloc(sizeof (struct window));
	bzero(win, sizeof (struct window));

	/* set window values from defaults */
	win->wi_fgpixel = ForePixel;
	win->wi_bgpixel = BackPixel;
	win->wi_bgpixmap = BackPixmap;
	win->wi_bdpixmap = BorderPixmap;
	win->wi_stpixel = StripePixel;
	win->wi_hlpixel = HiLightPixel;
	win->wi_bdwidth = DEFBDWIDTH;
	win->wi_border = DEFINTBORDER;

	/* set initial window flags */
	win->wi_type = WIN_BUFFER;
	win->wi_flags = WIN_ACTIVE|WIN_REPAINT|WIN_NEWMODE;

	/* open the font for this window */
	if (fontname == NULL || *fontname == '\0')
		fontname = DEFFONTNAME;
	win->wi_fontinfo = fixedfont(fontname);
	strncpy(win->wi_fontname, fontname, sizeof (win->wi_fontname) - 1);
	win->wi_fontname[sizeof (win->wi_fontname) - 1] = '\0';

	/* parse geometry specification */
	if (geometry == NULL)
		geometry = DEFGEOMETRY;
	left = top = rows = cols = -1;
	parse_geom(geometry, &left, &top, &rows, &cols);

	debug(DWINDOW, "Making new window; geometry %s, fontname \"%s\".",
	      geometry, fontname);

	if (rows < 0 || cols < 0) {
		XCloseFont(win->wi_fontinfo);
		win->wi_fontinfo = NULL;
		error("Given window geometry doesn't specify size.");
	}
	if (left < 0 || top < 0) {
		/* bubber band window to determine size/placement */
		size_window(win, geometry, ALTGEOMETRY, &win->wi_whole);
		win->wi_left = DISPLEFT(win);
		win->wi_top = DISPTOP(win);
		win->wi_width = DISPWIDTH(win);
		win->wi_height = DISPHEIGHT(win);
		win->wi_rows = ROWSOFY(win, win->wi_ysize);
		win->wi_cols = COLSOFX(win, win->wi_xsize);
	} else {
		/* calculate window size from given geometry */
		win->wi_xpos = left;
		win->wi_ypos = top;
		win->wi_xsize = GETWIDTH(win, cols);
		win->wi_ysize = GETHEIGHT(win, rows);
		win->wi_left = DISPLEFT(win);
		win->wi_top = DISPTOP(win);
		win->wi_width = DISPWIDTH(win);
		win->wi_height = DISPHEIGHT(win);
		win->wi_rows = rows;
		win->wi_cols = cols;
	}
	if (win->wi_rows < MINROWS || win->wi_cols < MINCOLS) {
		XCloseFont(win->wi_fontinfo);
		win->wi_fontinfo = NULL;
		error("Specified X window size is too small!");
	}
	debug(DWINDOW,
	      "New window =%dx%d+%d+%d, display =%dx%d+%d+%d, %d by %d.",
	      win->wi_xsize, win->wi_ysize, win->wi_xpos, win->wi_ypos,
	      win->wi_width, win->wi_height, win->wi_left, win->wi_top,
	      win->wi_rows, win->wi_cols);

	PROTECT();
	/* open the window now */
	win->wi_xwindow = XCreateWindow(RootWindow,
					win->wi_xpos, win->wi_ypos,
					win->wi_xsize, win->wi_ysize,
					win->wi_bdwidth, GrayPixmap,
					win->wi_bgpixmap);
	if (win->wi_xwindow == NULL)
		ierror("makewindow: Couldn't create X window!");

	/* make window name from index */
	if (nextindex > 0)
		sprintf(win->wi_windname, "%.16s<%d>", program, nextindex);
	else
		strcpy(win->wi_windname, program);

	/* insert this window into the global list */
	win->wi_next = window_list;
	window_list = win;

	/* send all the stuff to X for this window */
	XStoreName(win->wi_xwindow, win->wi_windname);
	XSetResizeHint(win->wi_xwindow, MINWIDTH(win), MINHEIGHT(win),
		       win->wi_fontinfo->width, win->wi_fontinfo->height);
	XDefineCursor(win->wi_xwindow, BaseCursor);
	win->wi_index = nextindex++;

	/* make the icon window */
	left = top = 0;
	width = vortex_logo_width + 2;
	if (bufp == NULL || bufp->bu_name == NULL)
		height = vortex_logo_height + 2;
	else
		height = vortex_logo_height + iconfont->height + 4;
	XParseGeometry(ICONGEOMETRY, &left, &top, &width, &height);

	win->wi_icon.re_left = win->wi_xpos + left;
	win->wi_icon.re_top = win->wi_ypos + top;
	win->wi_icon.re_width = width;
	win->wi_icon.re_height = height;
	win->wi_iconwin = XCreateWindow(RootWindow,
					win->wi_icon.re_left,
					win->wi_icon.re_top,
					win->wi_icon.re_width,
					win->wi_icon.re_height,
					win->wi_bdwidth, win->wi_bdpixmap,
					win->wi_bgpixmap);
	if (win->wi_iconwin != NULL) {
		XSelectInput(win->wi_iconwin, ICONWN_EVENTS);
		XSetIconWindow(win->wi_xwindow, win->wi_iconwin);
		XSetResizeHint(win->wi_iconwin,
			       win->wi_icon.re_width, win->wi_icon.re_height,
			       1, 1);
	}

	/* make the name stripe buffer */
	debug(DPAINT, "Allocating %d bytes for window %d's initial stripe.",
	      STRIPESIZE(win), win->wi_index);
	win->wi_stripe = (unsigned short *)valloc(STRIPESIZE(win));
	bspace(win->wi_stripe, STRIPELEN(win));
	win->wi_flags |= WIN_NEWMODE|WIN_REPAINT;

	/* switch this window to the given buffer officially */
	if (bufp != NULL)
		switchbuffer(win, bufp);

	XMapWindow(win->wi_xwindow);
	XClear(win->wi_xwindow);
	XFlush();
	XSelectInput(win->wi_xwindow, WINDOW_EVENTS);
	UNPROTECT();

	return (win);
}

resizewin(win, width, height)
	struct window	*win;
{
	struct buffer	*bufp;
	int		cols;

	debug(DPAINT, "Window resized to %d wide by %d high.", width, height);
	if (width < 0 || height < 0)
		return (-1);

	if (width < MINWIDTH(win) || height < MINHEIGHT(win)) {
		/* don't allow this */
		if (width < MINWIDTH(win))
			width = MINWIDTH(win);
		if (height < MINHEIGHT(win))
			height = MINHEIGHT(win);
		XChangeWindow(win->wi_xwindow, width, height);
		message("Can't make the window smaller than this.");
	}

	/* make sure there really was a change */
	if (width != win->wi_xsize || height != win->wi_ysize) {
		/* set ``whole'' size from arguments */
		win->wi_xsize = width;
		win->wi_ysize = height;
		/* set display size based on whole size */
		win->wi_left = DISPLEFT(win);
		win->wi_top = DISPTOP(win);
		win->wi_width = DISPWIDTH(win);
		win->wi_height = DISPHEIGHT(win);
		/* set rows/columns based on display size */
		cols = COLSOFX(win, width);
		if (cols > win->wi_cols && win->wi_stripe != NULL) {
			/* we will re-allocate this later */
			vfree(win->wi_stripe);
			win->wi_stripe = NULL;
		}
		win->wi_cols = cols;
		win->wi_rows = ROWSOFY(win, height);

		/* call the buffer-specific resize function */
		bufp = win->wi_buffer;
		if (bufp != NULL && bufp->bu_resize != NULL)
			(*bufp->bu_resize)(win);
	}

	/* make sure we have an appropriate name stripe */
	if (win->wi_stripe == NULL) {
		debug(DPAINT, "Allocating %d bytes for window %d name stripe.",
		      STRIPESIZE(win), win->wi_index);
		win->wi_stripe = (unsigned short *)valloc(STRIPESIZE(win));
		bspace(win->wi_stripe, STRIPELEN(win));
		win->wi_flags |= WIN_NEWMODE;
	}

	return (0);
}

exposewin(win, left, top, width, height)
	struct window	*win;
{
	struct rect	*intersect();
	struct rect	whole, mode, *res;
	struct buffer	*bufp;

	debug(DPAINT, "Window %d exposed from %d, %d to %d by %d.",
	      win->wi_index, left, top, width, height);

	/* the area given by X */
	whole.re_left = left;
	whole.re_top = top;
	whole.re_width = width;
	whole.re_height = height;

	/* check for intersections with display area */
	res = intersect(&whole, &win->wi_display);
	if (res->re_width > 0 && res->re_height > 0) {
		debug(DPAINT,
		      "Display of window %d exposed from %d, %d to %d, %d.",
		      win->wi_index, res->re_left, res->re_top,
		      res->re_width, res->re_height);

		/* call the buffer specific expose function */
		bufp = win->wi_buffer;
		ASSERT(bufp != NULL);
		if (bufp->bu_expose == NULL)
			win->wi_flags |= WIN_REPAINT;
		else
			(*bufp->bu_expose)(win, res);
	}

	/* check for an intersection with the name stripe */
	mode.re_left = 0;
	mode.re_top = 0;
	mode.re_width = win->wi_xsize;
	mode.re_height = STRIPEHEIGHT(win);
	res = intersect(&whole, &mode);
	if (res->re_width > 0 && res->re_height > 0)
		paintstripe(win);
	XFlush();

	return (0);
}

iconevent(winp, event)
	struct window	*winp;
	XEvent		*event;
{
	struct buffer	*bufp;
	int		change = FALSE;
	int		minht, left, wid, third;
	char		nbuf[100];
	XExposeEvent	*expose;

	ASSERT(winp != NULL);
	debug(DXIO, "Got an X event for window %ds icon.", winp->wi_index);

	bufp = winp->wi_buffer;
	if (bufp == NULL || bufp->bu_name == NULL)
		minht = vortex_logo_height;
	else
		minht = vortex_logo_height + iconfont->height + 1;

	/* handle a resize window event */
	if (event->type == ExposeWindow) {
		expose = (XExposeEvent *)event;
		winp->wi_icon.re_left = expose->x;
		winp->wi_icon.re_top = expose->y;
		winp->wi_icon.re_width = expose->width;
		winp->wi_icon.re_height = expose->height;
	}

	/* make sure the window is large enough */
	if (winp->wi_icon.re_width < vortex_logo_width) {
		winp->wi_icon.re_width = vortex_logo_width;
		change = TRUE;
	}
	if (winp->wi_icon.re_height < vortex_logo_height) {
		winp->wi_icon.re_height = vortex_logo_height;
		change = TRUE;
	}
	if (change) {
		XChangeWindow(winp->wi_iconwin,
			      winp->wi_icon.re_width, winp->wi_icon.re_height);
		/* we will get another expose event later */
		return (1);
	}

	/* clear the window first of all */
	XClear(winp->wi_iconwin);

	/* paint the VorTeX logo at the top in the center */
	left = (winp->wi_icon.re_width - vortex_logo_width) / 2;
	third = (winp->wi_icon.re_height - minht) / 3;
	XBitmapBitsPut(winp->wi_iconwin, left, third,
		       vortex_logo_width, vortex_logo_height, vortex_logo_bits,
		       winp->wi_fgpixel, winp->wi_bgpixel,
		       NULL, GXcopy, AllPlanes);

	/* now paint the buffer name if there is one */
	if (bufp != NULL && bufp->bu_name != NULL) {
		makepstring(bufp->bu_name, nbuf, sizeof (nbuf));
		wid = XStringWidth(nbuf, iconfont, 0, 0);
		left = (winp->wi_icon.re_width - wid) / 2;
		if (left < 0)
			left = 0;
		XText(winp->wi_iconwin, left, vortex_logo_height + 2 * third,
		      nbuf, strlen(nbuf), iconfont->id,
		      winp->wi_fgpixel, winp->wi_bgpixel);
	}

	return (0);
}

FontInfo *
fixedfont(fontname)
	char	*fontname;
{
	FontInfo	*font;

	if ((font = XOpenFont(fontname)) == NULL)
		error("Couldn't open X font %s!", fontname);
	if (!font->fixedwidth)
		error("Font %s isn't of fixed width!", fontname);
	if (font->width < 1 || font->height < 1)
		error("Font %s would have null rasters!", fontname);
	if (font->firstchar > ' ' || font->lastchar < '~')
		error("Font %s doesn't have enough characters!", fontname);

	return (font);
}

parse_geom(geometry, xpos, ypos, rows, cols)
	char	*geometry;
	int	*xpos, *ypos, *rows, *cols;
{
	unsigned int	mask;

	/* parse the geometry string */
	mask = XParseGeometry(geometry, xpos, ypos, cols, rows);

	/* convert the negative position coordinates if necessary */
	if ((mask & XValue) != 0 && (mask & XNegative) != 0) {
		if (*xpos < 0)
			*xpos = Xscr_width + *xpos;
		else
			*xpos = Xscr_width - *xpos;
	}
	if ((mask & YValue) != 0 && (mask & YNegative) != 0) {
		if (*ypos < 0)
			*ypos = Xscr_height + *ypos;
		else
			*ypos = Xscr_height - *ypos;
	}

	return (0);
}

/*
 *  Clear the given segment of the window to spaces, making sure
 *  that all of them are marked as untouched.  This should be about
 *  as fast as this operation can be.  Presumably the routine that
 *  called this knows that the window has been cleared (as with XClear),
 */

bspace(ptr, len)
	register unsigned short	*ptr;
{
	register unsigned short	*end;
	register int		value;

	end = ptr + len;
	value = CHAR_SET(' ', FLAG_NONE);
	while (ptr < end)
		*ptr++ = value;
}

/*
 *  Make sure the given repaint buffer segment is cleared to spaces.
 *  If any segment of it is not a space, we make it a space and mark
 *  it as touched.  This should save repaint time, since it may already
 *  be all spaces.
 */

bclear(ptr, len)
	register unsigned short	*ptr;
{
	register unsigned short	*end;
	register int		value;

	end = ptr + len;
	value = CHAR_SET(' ', CBUF_TOUCHED);
	while (ptr < end) {
		if (CHAR_GET(*ptr) != ' ' || FLAG_GET(*ptr) != FLAG_NONE)
			*ptr = value;
		ptr++;
	}
}

/*
 *  Mark every character in the buffer as touched unless it is a space,
 *  which, presumably, we don't need to paint.
 */

brefresh(ptr, len)
	register unsigned short	*ptr;
{
	register unsigned short	*end;

	end = ptr + len;
	while (ptr < end) {
		if (CHAR_GET(*ptr) == ' ' && FLAG_GET(*ptr) == FLAG_NONE)
			*ptr &= ~CBUF_TOUCHED;
		else
			*ptr |= CBUF_TOUCHED;
		ptr++;
	}
}
