/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
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
 */

static char copyright_notice[] = "Copyright (c) 1986 - 1991 Regents of the University of California.\nAll rights reserved.";

#ifndef lint
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/scroll.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include "rectangs.h"
#include "sun.h"
#include "fdecls.h"

extern sun_d	*sunp;
extern int	arg_val;

scroll(dir)
	register int	dir;
{
	register int	i,
			how_much = (arg_val == 0) ? 3 : arg_val,
			new_x =	view_r.r_left,
			new_y =	view_r.r_top,
			ww =	view_r.r_width,
			wh =	view_r.r_height;

	for (i = 0; i < RLDUMAX; i++) {
		if ((dir & 1 << i) == 0) {
			continue;
		}
		switch(i) {
		case MV_RIGHT:
			new_x += ww / how_much;
			break;
		case MV_LEFT:
			new_x -= ww / how_much;
			break;
		case MV_UP:
			new_y -= wh / how_much;
			break;
		case MV_DOWN:
			new_y += wh / how_much;
			break;
		}
	}
	return(do_abs_scroll(new_x, new_y, 0));
}

/*
 * DOCUMENTATION
 *
 * Name: scroll-right
 * Desc: This command is the rightward analog of \em{scroll-down}.
 * SeeA: scroll-down
 */
scroll_right()
{
	return(scroll(1 << MV_RIGHT));
}

/*
 * DOCUMENTATION
 *
 * Name: scroll-left
 * Desc: This command is the leftward analog of \em{scroll-down}.
 * SeeA: scroll-down
 */
scroll_left()
{
	return(scroll(1 << MV_LEFT));
}

/*
 * DOCUMENTATION
 *
 * Name: scroll-down
 * Desc: This command scrolls the page image down in the window if there
 *	is any more of the page to display vertically.  The amount scrolled
 *	by default is \pass{$1/3$} of the vertical size of the window; 
 *	the default
 *	can be changed by supplying a numeric argument.  That argument
 *	is taken as the denominator of the fraction of the window to scroll
 *	down.  For example, the keystrokes \lit{10d} will scroll down
 *	by \pass{$1/10$} the vertical size of the window.
 * SeeA: numeric-arg scroll-left scroll-right scroll-up
 */
scroll_down()
{
	return(scroll(1 << MV_DOWN));
}

/*
 * DOCUMENTATION
 *
 * Name: scroll-up
 * Desc: This command is the upward analog of \em{scroll-down}.
 * SeeA: scroll-down
 */
scroll_up()
{
	return(scroll(1 << MV_UP));
}

/*
 * DOCUMENTATION
 *
 * Name: scroll-absolute
 * Call: integer
 * Desc: This command scrolls down vertically down the page by the amount
 *	of the argument.  The argument is a percentage between 0 and 100
 *	of the page to scroll down.
 * SeeA: scroll-down
 */
scroll_abs_ver(argv)
	func_arg	*argv;
{
	int	scale = (*argv).integer;

	if (scale < 0 || scale > 100) {
		msg(PLAIN, "absolute vertical scroll arg %d out of range (must be 0-100).",
		  scale);
		return;
	}
	(void) scroll_ver_abs(scale);
}		
	
/*
 * these two functions map from an absolute scale in the range 0-100 to a
 * h or v to paint the window at.
 */
map_abs_ver(scale)
{
	int	new = pg_image_r.r_height - view_r.r_height;
	return((new * scale) / 100);
}

map_abs_hor(scale)
{
	int	new = pg_image_r.r_width - view_r.r_width;
	return((new * scale) / 100);
}

scroll_ver_abs(scale)
{
	return(do_abs_scroll(0, map_abs_ver(scale), 0));
}

scroll_hor_abs(scale)
{
	return(do_abs_scroll(map_abs_hor(scale), view_r.r_top, 0));
}

do_abs_scroll(new_x, new_y, refresh)
{
	rectang	view;
	int	ww =	view_r.r_width,
		wh =	view_r.r_height,
		iw =	pg_image_r.r_width,
		ih =	pg_image_r.r_height;

	/*
	 * modify the x and y we're handed so we never scroll off the
	 * edges of the page.
	 */
	if (new_x + ww > iw) {
		new_x = iw - ww;
	}
	if (new_x < 0) {
		new_x = 0;
	}
	if (new_y + wh > ih) {
		new_y = ih - wh;
	}
	if (new_y < 0) {
		new_y = 0;
	}

	rect_construct(&view,
	  new_x, new_y,
	  draw_width(), draw_height());
	return(do_scroll(&view, refresh));
}

/*
 * scroll to one or more edges of the page.
 */
edge(dir)
	register int	dir;
{
	register int	i,
			new_x = view_r.r_left,
			new_y = view_r.r_top;
	rectang		*ld_r = &dvi->cur_pg->load_r;

	for (i = 0; i < RLDUMAX; i++) {
		if (!(dir & 1 << i)) {
			continue;
		}
		switch(i) {
		case MV_RIGHT:
			new_x = (arg_val == 0) ? 
			  pg_image_r.r_width - view_r.r_width :
			  phys_image_r.r_left + ld_r->r_left +
			    ld_r->r_width - view_r.r_width;
			break;
		case MV_LEFT:
			new_x = (arg_val == 0) ?
			  0 :
			  phys_image_r.r_left + ld_r->r_left;
			break;
		case MV_DOWN:
			new_y = (arg_val == 0) ?
			  pg_image_r.r_height - view_r.r_height :
			  phys_image_r.r_top + ld_r->r_top +
			    ld_r->r_height - view_r.r_height;
			break;
		case MV_UP:
			new_y = (arg_val == 0) ?
			  0 :
			  phys_image_r.r_top + ld_r->r_top;
			break;
		}
	}
	if (new_x < 0)
		new_x = 0;
	if (new_y < 0)
		new_y = 0;
	return(do_abs_scroll(new_x, new_y, 0));
}

/*
 * DOCUMENTATION
 *
 * Name: top-left
 * Desc: This command positions the \lit{DVI} page so the top left
 *	hand corner is visible.
 *	If a numeric argument is given, the top left corner of the
 *	visible page (without the margins) will be shown.
 * SeeA: top-edge top-right bottom-left
 */
top_left()
{
	return(edge(1 << MV_UP | 1 << MV_LEFT));
}

/*
 * DOCUMENTATION
 *
 * Name: top-right
 * Desc: This command positions the \lit{DVI} page so the top right
 *	hand corner is visible.
 *	If a numeric argument is given, the top right corner of the
 *	visible page (without the margins) will be shown.
 * SeeA: top-edge top-left bottom-right
 */
top_right()
{
	return(edge(1 << MV_UP | 1 << MV_RIGHT));
}

/*
 * DOCUMENTATION
 *
 * Name: top-edge
 * Desc: This command positions the page image so the top edge of the
 *	current \lit{DVI} page is visible.
 *	If a numeric argument is given, the top edge of the
 *	visible page (without the top margin) will be shown.
 * SeeA: top-left top-right bottom-edge
 */
top_edge()
{
	return(edge(1 << MV_UP));
}

/*
 * DOCUMENTATION
 *
 * Name: bottom-left
 * Desc: This command positions the \lit{DVI} page so the bottom left
 *	hand corner is visible.
 *	If a numeric argument is given, the bottom left corner of the
 *	visible page (without the margins) will be shown.
 * SeeA: bottom-edge bottom-right top-left
 */
bottom_left()
{
	return(edge(1 << MV_DOWN | 1 << MV_LEFT));
}

/*
 * DOCUMENTATION
 *
 * Name: bottom-right
 * Desc: This command positions the \lit{DVI} page so the bottom right
 *	hand corner is visible.
 * SeeA: bottom-edge bottom-left top-right
 */
bottom_right()
{
	return(edge(1 << MV_DOWN | 1 << MV_RIGHT));
}

/*
 * DOCUMENTATION
 *
 * Name: bottom-edge
 * Desc: This command positions the page image so the bottom edge of the
 *	current \lit{DVI} page is visible.
 *	If a numeric argument is given, the bottom edge of the
 *	visible page (without the bottom margin) will be shown.
 * SeeA: bottom-left bottom-right top-edge
 */
bottom_edge()
{
	return(edge(1 << MV_DOWN));
}

/*
 * DOCUMENTATION
 *
 * Name: right-edge
 * Desc: This command positions the page image so the right edge of
 *	the current \lit{DVI} page is visible.
 *	If a numeric argument is given, the right edge of the
 *	visible page (without the margin) will be shown.
 *	This command is commonly used when one wants to see the
 *	maximum amount of visible page.
 * SeeA: bottom-right left-edge top-right
 */
right_edge()
{
	return(edge(1 << MV_RIGHT));
}

/*
 * DOCUMENTATION
 *
 * Name: left-edge
 * Desc: This command positions the page image so the left edge of the
 *	page is visible.
 *	If a numeric argument is given, the left edge of the
 *	visible page (without the margin) will be shown.
 *	This command is commonly used when one wants to see the
 *	maximum amount of visible page.
 * SeeA: bottom-left right-edge top-left
 */
left_edge()
{
	return(edge(1 << MV_LEFT));
}

/*
 * given an x and a y, return a pointer to the first character on the
 * line containing it.  X and y here are in page image coordinates, that
 * is, with borders, margins, etc.  If y is not ``contained'' by some
 * line, return the line closest to it, where closest is defined as the
 * first line for y's in the top margin, and the last line for y's in the
 * bottom margin.
 */
/* 
 * all of the routines that look up lines cache the most recent successful
 * return value so the next line and prev line functions can perhaps start
 * from a known point.
 */
static pg	*last_ln_page = 0;
static s_char	*last_ln = 0,
		**last_ln_addr = 0;

s_char *
pt_to_line(page, y)
	pg		*page;
	register	y;
{
	register s_char	**scan = &page->lines[page->line_c - 1],
			**end = page->lines;

	for (; scan != end; scan--) {
		if ((*scan)->y < y) {
			break;
		}
	}
	last_ln_addr = scan;
	last_ln = *scan;
	last_ln_page = page;
	return(*scan);
}

/*
 * given a line, return the next/previous line or null if the given line is
 * the last/first line.
 */
static s_char *
step_line(page, line, forwards)
	pg	*page;
	s_char	*line;
{
	s_char	**cur_ln;

	/* attempt to use the last result if possible */
	if (page != last_ln_page || line != last_ln) {
		(void) pt_to_line(page, line->y);
	}
	cur_ln = last_ln_addr;

	if (forwards) {
		if (cur_ln == &page->lines[page->line_c - 1]) {
			return((s_char *) 0);
		}
		cur_ln++;
	} else {
		if (cur_ln == page->lines) {
			return((s_char *) 0);
		}
		cur_ln--;
	}
	last_ln_addr = cur_ln;
	last_ln = *cur_ln;
	last_ln_page = page;
	return(*cur_ln);
}

/*
 * these are the routines the outside world is meant to call.
 */
/*
 * but in fact, no one ever calls them.  I just didn't get around to
 * implementing scrolling by lines for version 2.0.
 */
s_char *
next_line(page, line)
{
	return(step_line(page, line, 1));
}

s_char *
prev_line(page, line)
{
	return(step_line(page, line, 0));
}

