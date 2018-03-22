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
 *  RCS Info: $Header: source.c,v 0.1 87/05/01 12:27:34 john Locked $
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
 *  source.c - source buffer management routines
 */
static char _ID[] = "@(#)source.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "window.h"
#include "buffer.h"

extern int		text_paint(),		/* paint a source buffer */
			text_input(),		/* handle an input key code */
			text_resize(),		/* handle a resize event */
			text_expose(),		/* update exposed area */
			text_destroy(),		/* destroy a source buffer */
			text_open(),		/* begin editing a buffer */
			text_close();		/* stop editing a buffer */
extern unsigned char	*text_modeline();	/* format a mode line */

source_init(bufp)
	struct buffer	*bufp;
{
	struct source	*srcp;

	/* paranoia may come in handy some day... */
	ASSERT(bufp->bu_type == BUFF_SOURCE);

	/* all source buffer have same handlers */
	bufp->bu_input = text_input;
	bufp->bu_resize = text_resize;
	bufp->bu_expose = text_expose;
	bufp->bu_paint = text_paint;
	bufp->bu_mline = text_modeline;
	bufp->bu_event = NULL;
	bufp->bu_destroy = text_destroy;
	bufp->bu_open = text_open;
	bufp->bu_close = text_close;

	if (bufp->bu_pdata != NULL) {
		vfree(bufp->bu_pdata);
		bufp->bu_pdata = NULL;
	}
	if (bufp->bu_sdata == NULL) {
		srcp = (struct source *)valloc(sizeof (struct source));
		bufp->bu_sdata = srcp;
	} else {
		/* use the already existing one */
		srcp = bufp->bu_sdata;
	}
	bzero(srcp, sizeof (struct source));

	/* set up some reasonable default values */
	srcp->sb_cflags = CBUF_INVERSE;
	srcp->sb_tabwidth = 8;
	srcp->sb_mtime = time(NULL);
	srcp->sb_mark = -1;

	return (0);
}

text_resize(win)
	struct window	*win;
{
	ASSERT(win != NULL);
	debug(DPAINT, "Display area of source window %d resized to %d by %d.",
	      win->wi_index, win->wi_rows, win->wi_cols);

	if (win->wi_rows <= 0 || win->wi_cols <= 0) {
		ierror("Trying to resize source window %d to %d by %d!",
		       win->wi_index, win->wi_rows, win->wi_cols);
		/* NOTREACHED */
	}

	if (win->wi_screen != NULL) {
		vfree(win->wi_screen);
		win->wi_screen = NULL;
	}

	debug(DPAINT, "Allocating %d bytes for window %d's %d by %d screen.",
	      SCREENSIZE(win), win->wi_index, win->wi_rows, win->wi_cols);
	win->wi_screen = (unsigned short *)valloc(SCREENSIZE(win));
	bspace(win->wi_screen, SCREENLEN(win));
	win->wi_flags |= WIN_REPAINT;

	return (0);
}

text_expose(win, rect)
	struct window	*win;
	struct rect	*rect;
{
	register int	row, bottom, cols;
	register int	first, last;
	unsigned short	*sptr, *send;

	ASSERT(win != NULL);
	debug(DPAINT, "Source window %d exposed from %d, %d to %d, %d.",
	      win->wi_index,
	      rect->re_left, rect->re_top,
	      rect->re_width, rect->re_height);

	if (win->wi_screen == NULL) {
		/* make a new, blank screen and return */
		win->wi_screen = (unsigned short *)valloc(SCREENSIZE(win));
		bspace(win->wi_screen, SCREENLEN(win));
		win->wi_flags |= WIN_REFRESH;
		return (1);
	}

	/* if we're going to repaint anyway, don't expose now */
	if ((win->wi_flags & WIN_REPAINT) != 0)
		return (0);

	/* make sure our arguments are okay */
	if (rect->re_left < 0 || rect->re_top < 0)
		return (-1);

	/* figure rectangle based on the font sizes */
	row = ROWOFY(win, rect->re_top);
	bottom = ROWOFY(win, rect->re_top + rect->re_height);
	if (bottom >= win->wi_rows)
		bottom = win->wi_rows - 1;
	
	first = COLOFX(win, rect->re_left);
	last = COLOFX(win, rect->re_left + rect->re_width);
	if (last >= win->wi_cols)
		last = win->wi_cols - 1;
	cols = last - first + 1;

	/* mark text in area as needing repaint */
	while (row <= bottom) {
		sptr = SCREENAT(win, row, first);
		send = sptr + cols;
		while (sptr < send) {
			if (CHAR_GET(*sptr) == ' ' &&
			    FLAG_GET(*sptr) == CBUF_NORMAL)
				FLAG_CLR(*sptr, CBUF_TOUCHED);
			else
				FLAG_SET(*sptr, CBUF_TOUCHED);
			sptr++;
		}
		row++;
	}

	/* update this area to the X window */
	text_update(win, win->wi_left, win->wi_top,
		    win->wi_screen, win->wi_rows, win->wi_cols);

	return (0);
}

text_destroy(bufp)
	struct buffer	*bufp;
{
	if (bufp->bu_sdata != NULL)
		vfree(bufp->bu_sdata);

	return (0);
}

text_open(winp)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;

	bufp = windowbuffer(winp, BUFF_SOURCE);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* need to make a screen buffer of the correct size */
	text_resize(winp);

	/* set the window's version of point and mark */
	setwindowpoint(winp, srcp->sb_point);
	setwindowmark(winp, srcp->sb_mark);

	return (0);
}

text_close(winp)
	struct window	*winp;
{
	struct buffer	*bufp;
	struct source	*srcp;

	bufp = windowbuffer(winp, BUFF_SOURCE);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* save the window's value of point */
	if (winp->wi_point >= srcp->sb_start &&
	    winp->wi_point <= srcp->sb_length)
		srcp->sb_point = winp->wi_point;

	/* save the window's value of mark */
	if (winp->wi_mark < srcp->sb_start || winp->wi_mark > srcp->sb_length)
		srcp->sb_mark = -1;
	else
		srcp->sb_mark = winp->wi_mark;

	return (0);
}
