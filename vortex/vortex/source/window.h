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
 *  RCS Info: $Header: window.h,v 0.1 87/04/30 20:56:56 john Locked $
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
 *  window.h - editor window data structures
 */
 
#ifndef _WINDOW_
#define _WINDOW_

#ifndef _XLIB_
#define _XLIB_
#include <X/Xlib.h>
#endif !_XLIB_

extern Display	*Xdisplay;		/* X display handle */
extern char	Xdispname[64];		/* name of X display */
extern int	Xconnected;		/* flag indicating connection */
extern int	Xhavecolor;		/* on a color display */
extern int	Xscr_width;		/* width of X display */
extern int	Xscr_height;		/* height of X display */

struct rect {
	short		re_top;		/* offset from window top */
	short		re_left;	/* offset from window left */
	short		re_width;	/* width of rectangle */
	short		re_height;	/* height of rectangle */
};

/*
 *  This is the basic display structure for window management.
 *  Each window on the screen has one of these in the global
 *  list window_list.  The global current_window points to the
 *  one where key strokes input will go.
 *
 *  Note that we maintain a ``name stripe'' (or ``mode line'') for
 *  all windows which contains some information for the user.  The
 *  position and size of the entire window is given by wi_whole; the
 *  area of the X windows usable for the window's client (usually a
 *  buffer) is given by wi_display.  This area is offset at the top
 *  by the name stripe plus the internal border on all sides.  The
 *  current size in rows and columns is kept for all windows, since
 *  it is necessary for source buffers.
 *
 *  All the X window system information is kept with the window
 *  so that every window could have different foreground, background
 *  and border colors if desired by the user.  Also, the X font
 *  information and name are kept here so that each window could
 *  have a separate font (for source buffers and the name stripe).
 */
struct window {
	unsigned short	wi_type;		/* window display type */
	unsigned short	wi_flags;		/* window state flags */
	unsigned int	wi_index;		/* general window handle */
	Window		wi_xwindow;		/* X window handle */
	char		wi_windname[64];	/* name of X window */
	Window		wi_iconwin;		/* X window of icon */
	FontInfo	*wi_fontinfo;		/* X font information */
	char		wi_fontname[64];	/* name of X font */
	struct rect	wi_whole;		/* window position and size */
	struct rect	wi_display;		/* usable area of window */
	struct rect	wi_icon;		/* icon position and size */
	short		wi_rows, wi_cols;	/* size of window in chars */
	struct buffer	*wi_buffer;		/* attached buffer */
	long		wi_bufpos1,		/* buffer position info. */
			wi_bufpos2,		/* more position info. */
			wi_bufpos3,		/* buffer-specific parameter */
			wi_bufpos4;		/* another buffer parameter */
	unsigned short	*wi_screen;		/* char buffer for text */
	unsigned short	*wi_stripe;		/* buffer for name stripe */
	int		wi_fgpixel;		/* foreground pixel value */
	int		wi_bgpixel;		/* background pixel value */
	int		wi_stpixel;		/* name strip pixel value */
	int		wi_hlpixel;		/* high-light pixel value */
	Pixmap		wi_bgpixmap;		/* background tile value */
	Pixmap		wi_bdpixmap;		/* border tile value */
	short		wi_bdwidth;		/* X window border width */
	short		wi_border;		/* internal border width */
	struct window	*wi_next;		/* next window in list */
};

#define wi_cstart	wi_bufpos1	/* start of text for source buffers */
#define wi_curcol	wi_bufpos2	/* current column of cursor */
#define wi_point	wi_bufpos3	/* window's value of point */
#define wi_mark		wi_bufpos4	/* window's value of mark */

#define wi_xoffset	wi_bufpos1	/* X offset on current page */
#define wi_yoffset	wi_bufpos2	/* Y offset on current page */
#define wi_curpage	wi_bufpos3	/* current page for proof buffers */
#define wi_position	wi_bufpos4	/* horizontal position proof buffers */

/* shorten names of full window size values */
#define wi_xpos		wi_whole.re_left
#define wi_ypos		wi_whole.re_top
#define wi_xsize	wi_whole.re_width
#define wi_ysize	wi_whole.re_height

/* shorten names of display size values */
#define wi_left		wi_display.re_left
#define wi_top		wi_display.re_top
#define wi_width	wi_display.re_width
#define wi_height	wi_display.re_height

#define WIN_BUFFER	1			/* a buffer-visiting window */
#define WIN_CONFIRM	2			/* a confirmer window */
#define WIN_ICON	3			/* an icon of a window */

#define WIN_ACTIVE	(1 << 0)		/* this window is active */
#define WIN_FOCUSED	(1 << 1)		/* window is keyboard focus */
#define WIN_REFRESH	(1 << 2)		/* need to display changes */
#define WIN_REPAINT	(1 << 3)		/* need to repaint window */
#define WIN_NEWMODE	(1 << 4)		/* need to redo name stripe */
#define WIN_MINIBUF	(1 << 5)		/* a minibuffer window */
#define WIN_DORAISE	(1 << 6)		/* raise this window soon */

/*
 *  These are convenience constants, since these values are ``fixed''
 *  for all windows.  Note that the name stripe uses the same font
 *  as the source buffer text and is one pixel taller than the size
 *  of the font (so that the text is bounded on all sides).  The name
 *  stripe is offset from the left edge of the window by the internal
 *  border amount.
 */
#define STRIPEHEIGHT(w)	((w)->wi_fontinfo->height + 1)

/*
 *  This macros convert rows and columns into Y and X pixel
 *  distances.  This stuff is all made into macros and kept
 *  here because of the nuisance of keeing an internal border
 *  and dealing with left and top offsets.  These are only used
 *  by the source editor buffer windows.
 */
#define ROWTOY(w,r)	((r) * (w)->wi_fontinfo->height + (w)->wi_top)
#define COLTOX(w,c)	((c) * (w)->wi_fontinfo->width + (w)->wi_left)
#define YTOROW(w,y)	(((y) - (w)->wi_top) / (w)->wi_fontinfo->height)
#define XTOCOL(w,x)	(((x) - (w)->wi_left) / (w)->wi_fontinfo->width)

/*
 *  Here we DON'T take the wi_left and wi_top offsets into account.
 *  Presumably we're already in the display area of the window.
 */
#define COLOFX(w,x)	((x) / (w)->wi_fontinfo->width)
#define ROWOFY(w,y)	((y) / (w)->wi_fontinfo->height)
#define XOFCOL(w,c)	((c) * (w)->wi_fontinfo->width)
#define YOFROW(w,r)	((r) * (w)->wi_fontinfo->height)

/* get width and height of a font character */
#define FONTWIDTH(w)	((w)->wi_fontinfo->width)
#define FONTHEIGHT(w)	((w)->wi_fontinfo->height)
#define BASELINE(w)	((w)->wi_fontinfo->height - 2)

/* get width and height of a window from font size */
#define GETWIDTH(w,c)	((c) * (w)->wi_fontinfo->width + 2 * (w)->wi_border)
#define GETHEIGHT(w,r)	((r) * (w)->wi_fontinfo->height + \
			 2 * (w)->wi_border + STRIPEHEIGHT(w))

/* construct width and height from components */
#define WHOLEWIDTH(w)	((w)->wi_width + 2 * (w)->wi_border)
#define WHOLEHEIGHT(w)	((w)->wi_height + 2 * (w)->wi_border + STRIPEHEIGHT(w))
#define DISPWIDTH(w)	((w)->wi_xsize - 2 * (w)->wi_border)
#define DISPHEIGHT(w)	((w)->wi_ysize - 2 * (w)->wi_border - STRIPEHEIGHT(w))
#define DISPLEFT(w)	((w)->wi_border)
#define DISPTOP(w)	((w)->wi_border + STRIPEHEIGHT(w))

/* get column count from window sizes */
#define COLSOFX(w,x)	(((x) - (2 * (w)->wi_border)) / FONTWIDTH(w))
#define ROWSOFY(w,y)	(((y) - (STRIPEHEIGHT(w) + 2 * (w)->wi_border)) / \
			 FONTHEIGHT(w))

/* the minimum size of a window */
#define MINCOLS		1
#define MINROWS		1
#define MINWIDTH(f)	GETWIDTH(f, MINCOLS)
#define MINHEIGHT(f)	GETHEIGHT(f, MINROWS)

/*
 *  We make macros to get at places on screen (text) output buffers,
 *  even though the calculations are straight forward, to simplify the
 *  code.  These locate us on the screen array of a window containing
 *  a source buffer.
 */
#define SCREENLEN(w)	((w)->wi_rows * (w)->wi_cols)
#define SCREENSIZE(w)	(SCREENLEN(w) * sizeof (unsigned short))
#define DATALEN(w,h)	((w) * (h))
#define DATASIZE(w,h)	(DATALEN(w, h) * sizeof (unsigned short))
#define STRIPELEN(w)	((w)->wi_cols)
#define STRIPESIZE(w)	(STRIPELEN(w) * sizeof (unsigned short))

#define SCREENAT(w,r,c)	((w)->wi_screen + ((r) * (w)->wi_cols) + (c))
#define STRIPEAT(w,c)	((w)->wi_stripe + (c))
#define DATAAT(d,w,r,c)	((d) + ((r) * (w)) + (c))

/*
 *  We keep the characters of output buffers in a short, the lower
 *  byte containing the printable ASCII character to place there
 *  and the upper byte flags that modify the character.  Most of
 *  these flags change the attrributes of the character painted
 *  there, but the CBUF_TOUCHED bit specifies that the character
 *  needs repainting on the display.
 */
#define CHAR_MASK	(0177)		/* mask for screen characters */
#define CBUF_NORMAL	(0)		/* normal text */
#define CBUF_INVERSE	(1 << 8)	/* inverse video characters */
#define CBUF_UNDERLINE	(1 << 9)	/* underlined characters */
#define CBUF_OUTLINE	(1 << 10)	/* these characters outlined */
#define CBUF_HILIGHT	(1 << 11)	/* this character high-lighted */
#define CBUF_TOUCHED	(1 << 12)	/* this character changed */
#define FLAG_MASK	(CBUF_INVERSE|CBUF_UNDERLINE|CBUF_OUTLINE|CBUF_HILIGHT)

#define CHAR_SET(c,f)		(((c) & CHAR_MASK) | (f))
#define CHAR_GET(s)		((s) & CHAR_MASK)
#define FLAG_SET(s,f)		((s) |= (f))
#define FLAG_GET(s)		((s) & FLAG_MASK)
#define INFO_GET(s)		((s) & (FLAG_MASK|CBUF_TOUCHED))
#define CHAR_DIFF(s,c,f)	(CHAR_GET(s) != (c) || FLAG_GET(s) != (f) ? \
				 ((s) = CHAR_SET((c), (f)|CBUF_TOUCHED)) : 0)
#define FLAG_CLR(s,f)		((s) &= ~(f))

extern FontInfo		*iconfont;		/* for icon text */

extern struct window	*window_list,		/* linked list of windows */
			*current_window;	/* current window pointer */

extern struct window	*makewindow(),		/* create a new window */
			*getwindow();		/* return window from index */

extern struct buffer	*windowbuffer();	/* get window's buffer */

/* macros to traverse window lists */
#define forallwins(p)	for (p = window_list; p != NULL; p = p->wi_next)
#define forallbwins(p)	forallwins(p) if (p->wi_type == WIN_BUFFER)

#endif !_WINDOW_
