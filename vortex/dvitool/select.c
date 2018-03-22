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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/select.c,v $  (Berkeley)";
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
extern rectang	rectang_null;

/* find the character closest to the x & y passed in. */
static s_char *
schar_at(in_x, in_y)
{
	register s_char	*scan;
	register int	x,
			y,
			w,
			h,
			badness;
	char_entry	*cp;
	s_char		*best;
	int		found_a_char = 0;

	/* we start with s_buf + 1 to skip the beginning S_END. */
	for (scan = dvi->cur_pg->s_buf + 1; scan->c_type != S_END; scan++) {
		if (scan->c_type != S_SEARCHABLE) {
			continue;
		}
		/*
		 * we need to know if we've scanned at least one
		 * searchable chararacter to return, so mark that here.
		 */
		found_a_char = 1;
		/* compute the rect of this character */
		cp = scan->f_info;
		x = (int) scan->x;
		y = (int) scan->y;
		w = cp->width;
		h = cp->height;
		/* see if this point lies inside this rect */
		if (in_x >= x && in_x < x + w && in_y >= y && in_y < y + h) {
			return(scan);
		}
	}
	
	if (!found_a_char) {
		return((s_char *) 0);
	}
	/*
	 * well, we didn't find an exact match for any of the characters
	 * on the page, so we will find the closest match.
	 */
	badness = 100000;
	for (scan = dvi->cur_pg->s_buf + 1; scan->c_type != S_END; scan++) {
		if (scan->c_type != S_SEARCHABLE) {
			continue;
		}
		x = (int) scan->x + (cp->width / 2);
		y = (int) scan->y + (cp->height / 2);
		/* I reuse w & h here -- think of them as diff_x and diff_y */
		w = x - in_x;
		h = y - in_y;
		if (w < 0)
			w = -w;
		if (h < 0)
			h = -h;
		x = w + (15 * h);
		if (x < badness) {
			best = scan;
			badness = x;
		}
	}
	return(best);
}


struct timeval	last_selected,
		this_selected;

/*
 * DOCUMENTATION
 *
 * Name: select-char
 * Desc: This command makes a single character of the \lit{DVI} file the
 *	current selection.  The character chosen is highlighted by 
 *	displaying it in inverse video.  It may only be executed in
 *	response to an input from the mouse.  To extend the selection
 *	to more than one character, use \em{extend-selection}.
 *	A ``double click'' will select a word (a sequence of contiguous
 *	non-space characters).
 * SeeA: ascii-of-selection extend-selection which-char which-font 
 */

/*
 * the maximum number of micro seconds allowed between clicks to get the
 * surrounding word.
 */
#define	SOON_ENOUGH	(500000)
#define MOVE_LIMIT	(2)

select_char(argv)
	func_arg	*argv;
{
	extern Event	last_event;
	int		do_a_word = 0,
			x,
			y;
	static int	last_x,
			last_y;
	s_char		*start,
			*scan,
			*end;
	/*
	 * last_event always points to the event that caused us to be
	 * called.
	 */
	this_selected = event_time(&last_event);
	x = event_x(&last_event);
	y = event_y(&last_event);

	if (this_selected.tv_sec == last_selected.tv_sec &&
	  this_selected.tv_usec - last_selected.tv_usec < SOON_ENOUGH) {
	  	/*
		 * the user is attempting a double click.  check to make
		 * sure that the cursor hasn't moved very far. last_x is
		 * the x coord of the mouse the last time we were called,
		 * x is the x coord for this call.
		 */
	  	if (last_x - x > -MOVE_LIMIT && last_x - x < MOVE_LIMIT
		 && last_y - y > -MOVE_LIMIT && last_y - y < MOVE_LIMIT) {
			do_a_word++;
		}
	}
	last_selected = this_selected;

	if (dvi->sel_start != (s_char *) 0) {
		(void) null_selection();
	}

	/* get the x and y of this event. */
	last_x = event_x(&last_event);
	last_y = event_y(&last_event);

	win_to_search_coords(&x, &y);

	if ((end = start = schar_at(x, y)) == (s_char *) 0) {
		msg(PLAIN, "no characters on this page.");
		return(-1);
	}
	if (do_a_word) {
		/* scan to the front of the word */
		for (scan = start; scan->c_type == S_SEARCHABLE; scan--)
			;
		start = scan + 1;
		for (scan = end; scan->c_type == S_SEARCHABLE; scan++)
			;
		end = scan - 1;
	}
	dvi->sel_start = start;
	dvi->sel_end = end;
	dvi->sel_pg = dvi->cur_pg;
	return(show_sel(dvi->cur_pg, SHOW_SEL, 0));
}

/*
 * DOCUMENTATION
 *
 * Name: extend-selection
 * Desc: This command extends a previously made selection to include
 *	new characters.  The command \em{select-char} only selects
 *	1 character; thus, the usual method to selection a region
 *	of characters is to first select one boundary character with
 *	\em{select-char} and then to select the region by selecting
 *	the other boundary character with \em{extend-selection}.
 *	It is an error to run this command with no current selection.
 * SeeA: ascii-of-selection erase-selection select-char
 */
add_to_sel()
{
	int	x,
		y;
	s_char	*tmp;

	if (dvi->sel_start == (s_char *) 0) {
		return(0);
	}
	if (dvi->cur_pg != dvi->sel_pg) {
		msg(PLAIN, "can't extend a selection from another page!");
		return(-1);
	}

	x = event_x(&last_event);
	y = event_y(&last_event);

	win_to_search_coords(&x, &y);

	tmp = schar_at(x, y);

	/*
	 * figure out if we should uninvert the old selection before we
	 * invert the current one.  If we don't uninvert the whole thing,
	 * we must uninvert the characters that will be used as the
	 * background for inverting, otherwise we get garbage where the
	 * inverted and regular characters overlap when we invert them.
	 */
	if (tmp > dvi->sel_start && tmp < dvi->sel_end) {
		show_sel(dvi->cur_pg, UNINVERT_SEL, 0);
	} else {
		show_sel(dvi->cur_pg, UNINVERT_SEL, 1);
	}

	/*
	 * make sure that the first character of the selection is before
	 * the last character since code later depends on that relation.
	 */
	if (tmp < dvi->sel_start) {
		dvi->sel_start = tmp;
	} else {
		dvi->sel_end = tmp;
	}

	return(show_sel(dvi->cur_pg, SHOW_SEL, 0));
}

win_to_search_coords(out_x, out_y)
	int	*out_x,
		*out_y;
{
	register int	x = *out_x,
			y = *out_y;
	extern int	i_cursor_xhot,
			i_cursor_yhot;

	/* add the scroll offsets. */
	x += view_r.r_left;
	y += view_r.r_top;

	/* remove the margins. */
	x -= dvi->cur_pg->load_r.r_left;
	y -= dvi->cur_pg->load_r.r_top;

	/* remove the offsets for the borders. */
	x -= phys_image_r.r_left;
	y -= phys_image_r.r_top;

	*out_x = x;
	*out_y = y;
}

/*
 * DOCUMENTATION
 *
 * Name: erase-selection
 * Desc: This command erases the current selection, if any.
 *	It is primarily used to ensure that a search operation
 *	begins at the top of the page.  A search begins
 *	at the end of the current selection, if any, or the top
 *	of the page, so erasing the current selection guarantees
 *	that the search will begin at the top of the current page.
 * SeeA: select-char
 */
erase_selection()
{
	if (dvi->sel_start == (s_char *) 0) {
		msg(PLAIN, "no selection to erase.");
	}
	null_selection();
}

/*
 * get rid of the current selection, if any.
 */

null_selection()
{
	if (dvi->sel_start == (s_char *) 0) {
		return;
	}
		
	if (dvi->sel_pg == dvi->cur_pg) {
		show_sel(dvi->cur_pg, UNINVERT_SEL, 0);
	} else {
		uninv_sel(dvi->sel_pg);
	}

	dvi->sel_start = dvi->sel_end = (s_char *) 0;
	dvi->sel_pg = (pg *) 0;
}


rectang		bound_r;

show_sel(page, op, only_backing)
	pg	*page;
{
	s_char	*scan;
	int	new_pg = 0,
		strict;

	bound_r = rectang_null;

	if (page != dvi->cur_pg) {
		if (build_page_image(page) < 0)
			return(-1);
		show_page_number(page);
		page->time_stamp = time(0);
		new_pg++;
		dvi->cur_pg = page;
		view_changed();
	}

	for (scan = dvi->sel_start; scan <= dvi->sel_end; scan++) {
		highlight(page, scan, op, &bound_r);
	}

	page->sel_r.r_left = bound_r.r_left;
	page->sel_r.r_top = bound_r.r_top;
	page->sel_r.r_width = bound_r.r_width;
	page->sel_r.r_height = bound_r.r_height;

	strict = new_pg || op == SHOW_SEL;
	return(show_pg_rect(page, &bound_r, strict, only_backing));
}

/*
 * show the ascii value of the selection to the user.  We are guaranteed
 * that a selection has already been made.
 */
static char	sia_buf[1024],
		*sia_bp[] = {
			sia_buf,
			(char *) 0
		};

/*
 * DOCUMENTATION
 *
 * Name: ascii-of-selection
 * Desc: This function displays a previously-made selection in ASCII
 *	to make it easier to determine what the correct search string
 *	for the selection would be.
 * SeeA: erase-selection, extend-selection, select-char
 */

sel_in_ascii()
{
	register s_char		*scan;
	register char		*bp = sia_buf,
				*cp;
	char			*dp;

	for (scan = dvi->sel_start; scan <= dvi->sel_end; scan++) {
		if (bp - sia_buf > sizeof(sia_buf) - 5) {
			msg(PLAIN, "the selection is too long to show.  break it into pieces.");
			return(-1);
		}
		if (scan->c_type == S_SPACE || scan->c_type == S_EOL) {
			*bp++ = ' ';
			continue;
		}
		if (scan->c_type == S_SEARCHABLE) {
			cp = p_char(scan->c);
			while (*cp != '\0') {
				*bp++ = *cp++;
			}
			continue;
		}
	}
	*bp = '\0';
	(void) typeout(sia_bp, 1, T_LINES, &dp);
}
