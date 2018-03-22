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
 *  RCS Info: $Header: minibuf.c,v 0.1 87/05/01 12:21:16 john Locked $
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
 *  minibuf.c - internal minibuffer handling routines
 */
static char _ID[] = "@(#)minibuf.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include <signal.h>
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"
#include "format.h"

struct window	*minibuf_window = NULL;
int		minibuf_used = FALSE;
int		hold_minibuf = FALSE;

static struct buffer	*previous_buffer = NULL;
static struct window	*previous_window = NULL;

static Window		previous_subwin;
static int		previous_mouseX, previous_mouseY;

struct string *
minb_input(prompt, start)
	struct string	*prompt, *start;
{
	extern struct string	*copy_region();
	struct string		*input;
	struct source		*srcp;

	if (minibuf_used)
		error("Can't use the minibuffer recursively.");

	/* get buffer to actually edit in */
	ASSERT(minibuffer->bu_type == BUFF_SOURCE);
	srcp = minibuffer->bu_sdata;
	ASSERT(srcp != NULL);

	PROTECT();
	/* make sure the minibuffer window is visible */
	open_minbwin();

	/* save old window and buffer as necessary */
	previous_buffer = current_buffer;
	previous_window = current_window;
	XQueryMouse(RootWindow,
		    &previous_mouseX, &previous_mouseY,
		    &previous_subwin);

	/* switch to minibuffer window */
	current_buffer = minibuffer;
	switchwindow(minibuf_window, FALSE, TRUE);
	minibuf_window->wi_flags |= WIN_FOCUSED;
	minibuf_used = TRUE;
	UNPROTECT();

	/* clear out the minibuf and insert string and initial text */
	buffer_erase(minibuffer, TRUE);
	if (prompt != NULL && prompt->st_length > 0) {
		insert_string(minibuffer, prompt);
		srcp->sb_start = srcp->sb_point;
	}
	if (start != NULL && start->st_length > 0)
		insert_string(minibuffer, start);
	minibuf_window->wi_point = srcp->sb_point;

	/* go into internal recursive edit */
	edit_loop();

	PROTECT();
	/* copy out text and clean up minibuf */
	input = copy_region(minibuffer, srcp->sb_start, srcp->sb_length);
	clean_minibuf();
	drop_minbwin();

	/* mark the buffer as changed so that it will repaint */
	minibuffer->bu_flags |= BUFF_CHANGED;
	UNPROTECT();

	return (input);
}

/*
 *  This is ugly, but should be about as fast as we can get.  We
 *  keep the print images of all characters in a table of static'ly
 *  defined strings, so we can get them immediately.
 */
extern char	*pstrings[];

#define	cntl_width	2
#define meta_width	4
#define norm_width	1

#define pntlen(c)	(((c) == '\t' && tab_width > 0) ? tab_width : \
			 ((c) < ' ' || (c) == '\177') ? cntl_width : \
			 (c) > 0177 ? meta_width : norm_width)
#define nonprint(c)	((c) < ' ' || (c) > '~')

#define tab_width	8

minb_print(mesg, mlen, hilight)
	unsigned char	*mesg;
{
	register unsigned short	*sptr, *send;
	register int		col, row;
	int			dest, flags;
	register char		*cp;
	register unsigned char	*mp, *mend;
	struct window		*winp;

	/* get the minibuffer to paint on */
	open_minbwin();
	winp = minibuf_window;

	if (winp->wi_screen == NULL)
		panic("Minibuffer window has no screen to paint on!");
	debug(DPAINT, "Slamming a message onto the minibuffer window...");

	if (hilight)
		flags = CBUF_HILIGHT;
	else
		flags = CBUF_NORMAL;

	/* paint given text on window */
	mp = mesg;
	mend = mesg + mlen;
	for (row = 0; row < winp->wi_rows && mp < mend; row++) {
		sptr = SCREENAT(winp, row, 0);
		send = sptr + winp->wi_cols;
		col = 0;
		while (col + pntlen(*mp) <= winp->wi_cols &&
		       mp < mend && *mp != '\n') {
			if (tab_width > 0 && *mp == '\t') {
				dest = col / tab_width * tab_width + tab_width;
				while (col < dest) {
					CHAR_DIFF(*sptr, ' ', flags);
					sptr++;
					col++;
				}
			} else if (nonprint(*mp)) {
				for (cp = pstrings[*mp]; *cp != '\0'; cp++) {
					CHAR_DIFF(*sptr, *cp, flags);
					sptr++;
					col++;
				}
			} else {
				/* just paint the character */
				CHAR_DIFF(*sptr, *mp, flags);
				sptr++;
				col++;
			}

			/* advance to next text character */
			mp++;
		}

		/* clear rest of line */
		while (sptr < send) {
			CHAR_DIFF(*sptr, ' ', flags);
			sptr++;
		}

		/* advance to next text character */
		if (mp < mend && *mp == '\n')
			mp++;
	}

	/* make sure rest of window is blank */
	while (row < winp->wi_rows) {
		sptr = SCREENAT(winp, row, 0);
		send = sptr + winp->wi_cols;
		while (sptr < send) {
			CHAR_DIFF(*sptr, ' ', flags);
			sptr++;
		}
		row++;
	}

	/* update to X as necessary */
	text_update(winp, winp->wi_left, winp->wi_top,
		    winp->wi_screen, winp->wi_rows, winp->wi_cols);
	XFlush();

	/* don't allow minibuffer to repaint on next update */
	winp->wi_flags &= ~(WIN_REPAINT|WIN_REFRESH);
	minibuffer->bu_flags &= ~BUFF_CHANGED;

	return (0);
}

extern char	*MINBGEOMETRY, *DEFMINBFONT;

static
init_minbwin()
{
	/* make the minibuffer window */
	minibuf_window = makewindow(MINBGEOMETRY, DEFMINBFONT, minibuffer);
	minibuf_window->wi_flags |= WIN_MINIBUF;

	/* reset all the minibuffer attributes */
	previous_window = NULL;
	previous_buffer = NULL;
	clean_minibuf();

	hold_minibuf = TRUE;
	return (0);
}

static int	activated = FALSE;

static
open_minbwin()
{
	/* make sure we have a minibuffer window */
	if (minibuf_window == NULL) {
		init_minbwin();
		activated = FALSE;
	} else if ((minibuf_window->wi_flags & WIN_ACTIVE) == 0) {
		reactivatewin(minibuf_window);
		activated = TRUE;
	} else {
		/* didn't have to activate the window */
		activated = FALSE;
	}

	/* get input going to the minibuffer */
	if (minibuf_window->wi_buffer != minibuffer)
		switchbuffer(minibuf_window, minibuffer);
	minibuf_window->wi_flags |= WIN_REPAINT;

	/* handle X events to paint minibuffer */
	input_event(FALSE, NULL);

	return (0);
}

static
drop_minbwin()
{
	if (activated) {
		deactivatewin(minibuf_window);
		activated = FALSE;
	}

	return (0);
}

clean_minibuf()
{
	if (previous_buffer != NULL) {
		current_buffer = previous_buffer;
		previous_buffer->bu_flags |= BUFF_CHANGED;
		previous_buffer = NULL;
	}

	/* unmark this as focused */
	if (minibuf_window->wi_flags & WIN_FOCUSED)
		minibuf_window->wi_flags &= ~WIN_FOCUSED;

	if (previous_window != NULL) {
		switchwindow(previous_window, FALSE, FALSE);
		if (previous_subwin == previous_window->wi_xwindow) {
			Window	subw;
			int	x, y;

			XQueryMouse(RootWindow, &x, &y, &subw);
			if (subw != previous_window->wi_xwindow)
				warptowindow(previous_window);
		}
		previous_window = NULL;
	}

	/* clean up the minibuffer itself */
	minibuf_used = FALSE;
	buffer_erase(minibuffer, TRUE);
	minibuffer->bu_flags &= ~(BUFF_CHANGED|BUFF_MODIFIED);
	minibuf_window->wi_flags &= ~(WIN_REPAINT|WIN_REFRESH);

	return (0);
}
