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
 *  RCS Info: $Header: paint.c,v 0.1 87/05/01 12:22:46 john Locked $
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
 *  paint.c - internal routines to paint source buffer windows
 */
static char _ID[] = "@(#)paint.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

update_screen()
{
	extern int	hold_minibuf;
	struct window	*win;
	struct buffer	*bufp;

	/* make sure all windows are painted */
	for (win = window_list; win != NULL; win = win->wi_next) {
		if ((win->wi_flags & WIN_ACTIVE) == 0)
			continue;

		switch (win->wi_type) {
		case WIN_BUFFER:
			bufp = win->wi_buffer;
			if (bufp == NULL) {
				deactivatewin(win);
				ierror(
		"Buffer window %d has no buffer to paint; deactivated.",
				       win->wi_index);
			}
			if ((win->wi_flags & WIN_MINIBUF) && hold_minibuf) {
				hold_minibuf = FALSE;
				continue;
			}
	
			/* have any mode line variables changed */
			if ((bufp->bu_flags & BUFF_NEWMODE) == 0)
				checkvars(bufp);
	
			/* paint the buffer now if necessary */
			if (((bufp->bu_flags & BUFF_CHANGED) != 0 ||
			    (win->wi_flags & (WIN_REPAINT|WIN_REFRESH))) &&
			    bufp->bu_paint != NULL) {
				/* call the buffer-specific painter */
				(*bufp->bu_paint)(win);
				win->wi_flags &= ~(WIN_REPAINT|WIN_REFRESH);
			}
	
			/* does mode line need reformatting */
			if ((win->wi_flags & WIN_NEWMODE) != 0 ||
			    (bufp->bu_flags & BUFF_NEWMODE) != 0) {
				modeline(win, bufp);
				win->wi_flags &= ~WIN_NEWMODE;
			}
			break;

		default:
			deactivatewin(win);
			ierror("Unknown type %d for window %d; deactivated.",
			       win->wi_type, win->wi_index);
			break;
		}
	}
	XFlush();

	/* clear changed flag on all buffers to mark update */
	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next)
		bufp->bu_flags &= ~(BUFF_CHANGED|BUFF_NEWMODE);

	return (0);
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

/*
 *  Repaint text of window onto the repaint buffer where it
 *  differs from what is already there.  Thus, we do repaint
 *  optimization as possible for a device which can't insert or
 *  scroll (like an X window).
 */

text_paint(winp)
	struct window	*winp;
{
	register unsigned short	*sptr, *send;
	register int		col, row;
	register struct tblock	*tbp;
	register int		code;
	register unsigned long	*txp, *tend;
	register int		dest;
	struct buffer		*bufp;
	struct source		*srcp;
	int			flags;
	register char		*cp;
	int			tries = 0, tab_width;
	int			paint_cursor, found_point;
	register int		current, pointpos;

	/* get buffer and source data for text of buffer */
	bufp = windowbuffer(winp, BUFF_SOURCE);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	tab_width = srcp->sb_tabwidth;
	flags = srcp->sb_cflags;

	if (winp->wi_screen == NULL) {
		debug(DPAINT, "Source window %d has no screen to paint on!",
		      winp->wi_index);
		return (-1);
	}

	debug(DPAINT, "Painting source buffer window %d, size %d x %d",
	      winp->wi_index, winp->wi_rows, winp->wi_cols);

	/* clear display area to background color on repaint */
	if (winp->wi_flags & WIN_REPAINT) {
		XPixSet(winp->wi_xwindow,
			winp->wi_left, winp->wi_top,
			winp->wi_width, winp->wi_height,
			winp->wi_bgpixel);
		/* mark all non-spaces as touched */
		brefresh(winp->wi_screen, SCREENLEN(winp));
	}

	/* quick check to make sure point isn't before top */
	if (winp->wi_point < winp->wi_cstart)
		centerpoint(winp, winp->wi_rows / 3);

	/* get a valid point for this window */
	if (winp->wi_point < srcp->sb_start)
		winp->wi_point = srcp->sb_start;
	if (winp->wi_point > srcp->sb_length)
		winp->wi_point = srcp->sb_length;

paint:	/* find offset into buffer of window top */
	for (tbp = srcp->sb_text; tbp != NULL; tbp = tbp->tb_next)
		if (winp->wi_cstart < tbp->tb_offset + tbp->tb_length)
			break;

	/* paint buffer text on window */
	paint_cursor = TRUE;
	found_point = FALSE;
	tries++;

	pointpos = winp->wi_point;
	debug(DPAINT, "Window %d's buffer offset is %d, window's point is %d.",
	      winp->wi_index, winp->wi_cstart, pointpos);

	if (tbp != NULL) {
		txp = tbp->tb_text + (winp->wi_cstart - tbp->tb_offset);
		tend = tbp->tb_text + tbp->tb_length;
		code = charof(*txp);
	}
	current = winp->wi_cstart;
	for (row = 0; row < winp->wi_rows && tbp != NULL; row++) {
		sptr = SCREENAT(winp, row, 0);
		send = sptr + winp->wi_cols;
		col = 0;
		while (col + pntlen(code) <= winp->wi_cols &&
		       code != '\n' && tbp != NULL) {
			if (current == srcp->sb_point)
				found_point = TRUE;
			if (tab_width > 0 && code == '\t') {
				dest = col / tab_width * tab_width + tab_width;
				if (paint_cursor && current == pointpos) {
					CHAR_DIFF(*sptr, ' ', flags);
					sptr++;
					col++;
					paint_cursor = FALSE;
				}
				while (col < dest) {
					CHAR_DIFF(*sptr, ' ', CBUF_NORMAL);
					sptr++;
					col++;
				}
			} else if (nonprint(code)) {
				cp = pstrings[code];
				if (paint_cursor && current == pointpos) {
					CHAR_DIFF(*sptr, *cp, flags);
					sptr++;
					col++;
					cp++;
					paint_cursor = FALSE;
				}
				while (*cp != '\0') {
					CHAR_DIFF(*sptr, *cp, CBUF_NORMAL);
					sptr++;
					col++;
					cp++;
				}
			} else {
				if (paint_cursor && current == pointpos) {
					CHAR_DIFF(*sptr, code, flags);
					paint_cursor = FALSE;
				} else {
					/* just paint the character */
					CHAR_DIFF(*sptr, code, CBUF_NORMAL);
				}
				sptr++;
				col++;
			}

			/* advance to next text character */
			if (++txp >= tend) {
				tbp = tbp->tb_next;
				if (tbp == NULL) {
					/* end of bufffer */
					txp = tend = NULL;
				} else {
					tend = tbp->tb_text + tbp->tb_length;
					txp = tbp->tb_text;
				}
			}
			if (txp == NULL)
				code = -1;
			else
				code = charof(*txp);
			current++;
		}

		/* check if cursor should be painted at end of line */
		if (current == pointpos)
			found_point = TRUE;
		if (paint_cursor && sptr < send && current == pointpos) {
			CHAR_DIFF(*sptr, ' ', flags);
			sptr++;
			paint_cursor = FALSE;
		}

		/* clear rest of line */
		while (sptr < send) {
			CHAR_DIFF(*sptr, ' ', CBUF_NORMAL);
			sptr++;
		}

		if (code == '\n' && tbp != NULL) {
			/* advance to next text character */
			if (++txp >= tend) {
				tbp = tbp->tb_next;
				if (tbp == NULL) {
					/* end of the buffer */
					txp = tend = NULL;
				} else {
					tend = tbp->tb_text + tbp->tb_length;
					txp = tbp->tb_text;
				}
			}
			if (txp == NULL)
				code = -1;
			else
				code = charof(*txp);
			current++;
		}
	}

	/* check if cursor should be painted at end of buffer */
	if (current == pointpos && row < winp->wi_rows)
		found_point = TRUE;
	if (paint_cursor && row < winp->wi_rows && current == pointpos) {
		sptr = SCREENAT(winp, row, 0);
		send = sptr + winp->wi_cols;
		CHAR_DIFF(*sptr, ' ', flags);
		sptr++;
		paint_cursor = FALSE;
		/* clear out the rest of the line */
		while (sptr < send) {
			CHAR_DIFF(*sptr, ' ', CBUF_NORMAL);
			sptr++;
		}
		row++;
	}

	/* check if point was in the window */
	if (tries < 2 && !found_point) {
		if (pointpos < winp->wi_cstart)
			centerpoint(winp, winp->wi_rows / 2);
		else
			centerpoint(winp, 2 * winp->wi_rows / 3);
		goto paint;
	}
	if (tries < 3 && !found_point) {
		centerpoint(winp, 1);
		goto paint;
	}

	/* make sure rest of window is blank */
	while (row < winp->wi_rows) {
		sptr = SCREENAT(winp, row, 0);
		send = sptr + winp->wi_cols;
		while (sptr < send) {
			CHAR_DIFF(*sptr, ' ', CBUF_NORMAL);
			sptr++;
		}
		row++;
	}

	/* update to X as necessary */
	text_update(winp, winp->wi_left, winp->wi_top,
		    winp->wi_screen, winp->wi_rows, winp->wi_cols);
	return (0);
}

modeline(winp, bufp)
	struct window	*winp;
	struct buffer	*bufp;
{
	extern char		*gettype(), *psymbol();
	register unsigned char	*lptr, *lend;
	register unsigned short	*sptr, *send;
	int			len, dest;
	register int		col;
	register int		flags;
	register char		*cp;
	int			tab_width = 8;
	unsigned char		fake[100];

	/* make sure we have a name stripe to paint on */
	if (winp->wi_stripe == NULL) {
		debug(DPAINT, "Window %d has no name stripe to paint on!",
		      winp->wi_index);
		return (-1);
	}

	/* get text of mode line to paint */
	if (bufp->bu_mline == NULL) {
		sprintf(fake, "VorTeX %s buffer \"%.50s\"",
			gettype(bufp), psymbol(bufp->bu_name));
		lptr = fake;
		len = strlen(fake);
		if (len > winp->wi_cols)
			len = winp->wi_cols;
	} else {
		len = winp->wi_cols;
		lptr = (*bufp->bu_mline)(winp, &len);
		lend = lptr + len;
	}

	/* either use color or inverse video */
	flags = CBUF_NORMAL;
	if (winp->wi_stpixel == winp->wi_fgpixel)
		flags |= CBUF_INVERSE;

	/* set up screen data pointer and end marker */
	col = 0;
	sptr = STRIPEAT(winp, 0);
	send = sptr + winp->wi_cols;

	while (col + pntlen(*lptr) <= winp->wi_cols && lptr < lend) {
		if (tab_width > 0 && *lptr == '\t') {
			dest = col / tab_width * tab_width + tab_width;
			while (col < dest) {
				CHAR_DIFF(*sptr, ' ', flags);
				sptr++;
				col++;
			}
		} else if (nonprint(*lptr)) {
			for (cp = pstrings[*lptr]; *cp != '\0'; cp++) {
				CHAR_DIFF(*sptr, *cp, flags);
				sptr++;
				col++;
			}
		} else {
			CHAR_DIFF(*sptr, *lptr, flags);
			sptr++;
			col++;
		}
		lptr++;
	}

	/* clear rest of line */
	while (sptr < send) {
		CHAR_DIFF(*sptr, ' ', CBUF_NORMAL);
		sptr++;
	}

	/* update mode line text to X window */
	paintstripe(winp);

	return (0);
}

/*
 *  Mark the characters specified by the given rectangle in the
 *  array of text as touched for repaint.  Presumably we'll call
 *  text_update soon to update the text to the screen.
 */

text_touch(data, rows, cols, left, top, width, height)
	unsigned short	*data;
{
	register int		row, maxrow;
	register unsigned short	*sptr, *send;

	if (left + width > cols)
		width = cols - left;
	maxrow = top + height;
	if (maxrow > rows)
		maxrow = rows;

	for (row = top; row < maxrow; row++) {
		sptr = DATAAT(data, cols, row, left);
		send = sptr + width;
		while (sptr < send) {
			FLAG_SET(*sptr, CBUF_TOUCHED);
			sptr++;
		}
	}

	return (0);
}

/*
 *  Repaint parts of window which have been changed since the last call
 *  to this routine.  We know this by just looking for the CBUF_TOUCHED
 *  bit in the character flags byte.  For efficiency sake, we try to
 *  special case segments of text, by emitting them as a single text call.
 *  We depend on the X font being of fixed width.
 */

text_update(winp, xoff, yoff, screen, rows, cols)
	struct window	*winp;
	unsigned short	*screen;
{
	unsigned short	*sptr, *send;
	unsigned short	*left;
	unsigned char	*tp, tbuf[1024];
	register int	info, row, col;
	Window		xwin;
	int		len, x, y;
	int		fg, bg, txpixel;

	/* get X window to paint on */
	xwin = winp->wi_xwindow;

	/* use the stripe color if this is the name stripe */
	if (screen == winp->wi_stripe)
		txpixel = winp->wi_stpixel;
	else
		txpixel = winp->wi_fgpixel;

	/* repaint text which is marked as touched */
	for (row = 0; row < rows; row++) {
		left = sptr = screen + (row * cols);
		send = sptr + cols;
		while (sptr < send && (*sptr & CBUF_TOUCHED) == 0)
			sptr++;
		while (sptr < send) {
			/* collect the string of ``same state'' text */
			col = sptr - left;
			info = INFO_GET(*sptr);
			tp = tbuf;
			while (sptr < send && INFO_GET(*sptr) == info) {
				*tp++ = CHAR_GET(*sptr);
				FLAG_CLR(*sptr, CBUF_TOUCHED);
				sptr++;
			}
			len = tp - tbuf;

			/* figure out text colors */
			if ((info & CBUF_INVERSE) != 0) {
				if ((info & CBUF_HILIGHT) != 0) {
					fg = winp->wi_bgpixel;
					bg = winp->wi_hlpixel;
				} else {
					fg = winp->wi_bgpixel;
					bg = txpixel;
				}
			} else {
				if ((info & CBUF_HILIGHT) != 0) {
					fg = winp->wi_hlpixel;
					bg = winp->wi_bgpixel;
				} else {
					fg = txpixel;
					bg = winp->wi_bgpixel;
				}
			}

			/* paint text now, using given colors */
			XText(xwin,
			      xoff + XOFCOL(winp, col),
			      yoff + YOFROW(winp, row),
			      tbuf, len,
			      winp->wi_fontinfo->id,
			      fg, bg);

			if ((info & CBUF_UNDERLINE) != 0) {
				/* draw a line under the text */
				y = YOFROW(winp, row) + BASELINE(winp);
				XLine(xwin,
				      xoff + XOFCOL(winp, col), y,
				      xoff + XOFCOL(winp, col + len) - 1, y,
				      1, 1,
				      fg, GXcopy, AllPlanes);
			}

			if ((info & CBUF_OUTLINE) != 0) {
				/* draw an outline around the text */
				y = yoff + YOFROW(winp, row);
				XLine(xwin,
				      xoff + XOFCOL(winp, col), y,
				      xoff + XOFCOL(winp, col + len) - 1, y,
				      1, 1,
				      fg, GXcopy, AllPlanes);
				y = yoff + YOFROW(winp, row + 1) - 1;
				XLine(xwin,
				      xoff + XOFCOL(winp, col), y,
				      xoff + XOFCOL(winp, col + len) - 1, y,
				      1, 1,
				      fg, GXcopy, AllPlanes);
				x = xoff + XOFCOL(winp, col);
				XLine(xwin,
				      x, yoff + YOFROW(winp, row),
				      x, yoff + YOFROW(winp, row + 1) - 1,
				      1, 1,
				      fg, GXcopy, AllPlanes);
				x = xoff + XOFCOL(winp, col + len) - 1;
				XLine(xwin,
				      x, yoff + YOFROW(winp, row),
				      x, yoff + YOFROW(winp, row + 1) - 1,
				      1, 1,
				      fg, GXcopy, AllPlanes);
			}

			/* skip to next touched character */
			while (sptr < send && (*sptr & CBUF_TOUCHED) == 0)
				sptr++;
		}
	}

	return (0);
}

/*
 *  Paint the name stripe text--all of it, no matter how much
 *  really needs repainting.  This is called when some portion
 *  of the name stripe has been exposed.
 */

paintstripe(win)
	struct window	*win;
{
	/* make sure stripe area is a solid color */
	if (Xhavecolor) {
		/* paint stripe area the background color */
		XPixSet(win->wi_xwindow, 0, 0,
			win->wi_xsize, STRIPEHEIGHT(win), win->wi_bgpixel);
	} else {
		/* set stripe area to color when inverted */
		XPixSet(win->wi_xwindow, 0, 0,
			win->wi_xsize, STRIPEHEIGHT(win), win->wi_stpixel);
	}

	text_touch(win->wi_stripe, 1, win->wi_cols,
		   0, 0, win->wi_cols, 1);
	text_update(win, win->wi_left, 0,
		    win->wi_stripe, 1, win->wi_cols);
}

/*
 *  DOCUMENTATION
 *
 *  Name: tab-width
 *  Desc: This variable can be set to a fixnum, which specifies
 *	the distances between tab columns painted on the window.
 *	Whenever a tab character (\sc{ASCII} 9) appears in the file,
 *	it causes the next character to be placed at the next tab
 *	stop as determined by the \sym{tab-width}.
 *
 *	If the variable is not set to a fixnum, the default tab
 *	width of 8 columns is assumed.  By default, this variable
 *	is not set; presumably the user would only set it if he
 *	wanted another treatment of the tab character.
 *
 *	If the tab width is set to zero or less, it means that tabs
 *	should be treated as ordinary control characters and printed
 *	as \lit{^I} (representing \sc{ASCII} control-I).
 *  Side: When this variable is changed, winps showing buffers
 *	affected by the change will repaint, in case there were any
 *	tabs in their text.
 */
MKSTRING(TABWIDTH, "tab-width");

/*
 *  DOCUMENTATION
 *
 *  Name: truncate-long-lines
 *  Desc: This variable controls the handling of long lines of
 *	source text by the editor.  If this variable is set to a
 *	non-nil value, long lines do not wrap around to the next
 *	line in the window, but are truncated.
 *
 *	When a line has been truncated, a dollar sign, \lit{$},
 *	appears in the last column.  When wrapped, a backslash,
 *	\lit{\\}, appears in the last column, and the line continues
 *	on the next line of the winp.
 *
 *	By default, this variable is not set, and so long lines
 *	wrap around instead of being truncated.
 *  Side: When the value changes windows containing buffers affected,
 *	by the change will repaint, in case they were displaying any
 *	long lines.
 */
MKSTRING(TRUNCATE, "truncate-long-lines");

/*
 *  DOCUMENTATION
 *
 *  Name: cursor-inverse
 *  Desc: This variable affects the painting mode of the cursor
 *	indicating point in source editing buffers.  If this
 *	variable is set to a non-nil value, the cursor is drawn
 *	in inverse video.
 *
 *	By default, this variable is globally set to t, but may
 *	be reset globally or set locally to affect all buffers
 *	or only one.
 *  Side: When this variable changes, windows containing affected
 *	buffers will repaint to make this change of the cursor
 *	show.
 *  SeeA: cursor-underline cursor-outline
 */
MKSTRING(CURSOR_INVERSE, "cursor-inverse");

/*
 *  DOCUMENTATION
 *
 *  Name: cursor-underline
 *  Desc: This variable affects the painting mode of the cursor
 *	indicating point in source editing buffers.  If this
 *	variable is set to a non-nil value, the cursor is drawn
 *	as an underscore.
 *
 *	By default, this variable is not set, but may be set
 *	globally or locally to affect all buffers or only one.
 *  Side: When this variable changes, windows containing affected
 *	buffers will repaint to make this change of the cursor
 *	show.
 *  SeeA: cursor-inverse cursor-outline
 */
MKSTRING(CURSOR_UNDERLINE, "cursor-underline");

/*
 *  DOCUMENTATION
 *
 *  Name: cursor-outline
 *  Desc: This variable affects the painting mode of the cursor
 *	indicating point in source editing buffers.  If this
 *	variable is set to a non-nil value, the cursor is drawn
 *	as a hollow rectangle.
 *
 *	By default, this variable is not set, but may be set
 *	globally or locally to affect all buffers or only one.
 *  Side: When this variable changes, windows containing affected
 *	buffers will repaint to make this change of the cursor
 *	show.
 *  SeeA: cursor-inverse cursor-underline
 */
MKSTRING(CURSOR_OUTLINE, "cursor-outline");

static
checkvars(bufp)
	struct buffer	*bufp;
{
	struct source	*srcp;
	struct value	val;
	int		new;

	switch (bufp->bu_type) {
	case BUFF_SOURCE:
		/* get source structure for private values */
		srcp = bufp->bu_sdata;
		ASSERT(srcp != NULL);

		/* check out cursor painting flags */
		new = FLAG_NONE;
		val = get_variable(CURSOR_INVERSE, bufp);
		if (truep(val))
			new |= CBUF_INVERSE;
		val = get_variable(CURSOR_UNDERLINE, bufp);
		if (truep(val))
			new |= CBUF_UNDERLINE;
		val = get_variable(CURSOR_OUTLINE, bufp);
		if (truep(val))
			new |= CBUF_OUTLINE;
		if (srcp->sb_cflags != new) {
			srcp->sb_cflags = new;
			bufp->bu_flags |= BUFF_CHANGED;
		}

		/* check out tab width */
		new = 8;
		val = get_variable(TABWIDTH, bufp);
		if (fixnump(val))
			new = gfixnum(val.vl_data);
		if (srcp->sb_tabwidth != new) {
			srcp->sb_tabwidth = new;
			bufp->bu_flags |= BUFF_CHANGED;
		}

		/* check out line truncation */
		new = FALSE;
		val = get_variable(TRUNCATE, bufp);
		if (!truep(val))
			new = TRUE;
		if (((srcp->sb_sflags & SOURC_TRUNCATE) != 0) != new) {
			if (new)
				srcp->sb_sflags |= SOURC_TRUNCATE;
			else
				srcp->sb_sflags &= ~SOURC_TRUNCATE;
			bufp->bu_flags |= BUFF_CHANGED;
		}
		break;
	}
}
