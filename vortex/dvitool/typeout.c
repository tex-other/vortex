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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/typeout.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include <sunwindow/rect.h>

/*
 * this file implements a typeout mechanism to show arbitrary strings in
 * the tool.  We do this by scribbling onto the same display canvas that
 * we use for the dvi image, prompting 0 or more times a la more(1) for a
 * keystroke if we have more input than can be fit on one screen and then
 * fixing up the screen after the typeout is done.
 */

/* the display window will have a box of this width around it. */
#define T_BORDER_WIDTH	(2)
#define T_MARGIN	(3 * (T_BORDER_WIDTH))

static int
	char_x,			/* the width of a single char in pixels */
	char_y,
	chars_wide,		/* the number of chars per line */
	chars_high,		/* the number of lines per screen */
	win_w,			/* the width of the screen in pixels */
	win_h,
	start_x,		/* x coordinate of first string */
	start_y,
	lines,			/* number of lines in display window */
	cols,			/*           columns */
	max_width,		/* longest string is this many chars */
	wrap_count,		/* number of strings wider than the screen */
	*lengths,		/* lengths[k] == strlen(words[k]) */
	*lscan,			/* used to scan the lengths */
	wrote,			/* how many strings we've written */
	rtn_c;			/* the character to return */

static char
	more_msg[] = "--more--",
	**scan;			/* used to scan the list of strings */

static rectang
	view,			/* the total area we will write into. */
	strings;		/* just the area of the strings w/o borders */

/*
 * typeout the strings in words.  return an ascii character to be added
 * to the string if the user gives us an ascii char that isn't a space.
 */

typeout(words, how_many, how_display, rtnmsg)
	char	**words,
		**rtnmsg;
{
	int	k,
		*lenp;
	char	**scan;

	(void) create_drawing_surface();

	char_x = def_font_x();
	char_y = def_font_y();
	win_w = draw_width();
	win_h = draw_height();

	/* we assume we will return no error message. */
	*rtnmsg = (char *) 0;

	/* check for absurd conditions. */
	if (char_x < 2 ||
	  char_y < 2 ||
	  win_w < 2 * T_MARGIN + (sizeof(more_msg) * char_x) ||
	  win_h < 2 * T_MARGIN + (2 * char_y)) {
		*rtnmsg = " [!]";
		return(-1);
	}

	push_cursor(MOUSE_CUR);

	/* compute the width and height of our drawing area in characters. */
	chars_wide = win_w / char_x;
	/* make sure there is room for the border. */
	k = win_w % char_x;
	for (;;) {
		if (k > 2 * T_MARGIN) {
			break;
		}
		k += char_x;
		chars_wide--;
	}

	chars_high = win_h / char_y;
	k = win_h % char_y;
	for (;;) {
		if (k > 2 * T_MARGIN) {
			break;
		}
		k += char_y;
		chars_high--;
	}

	/*
	 * store the string lengths, compute the maximum string length
	 * and record how many strings will have to wrap around.
	 */
	lengths = (int *) alloc(how_many * sizeof(int));
	lenp = lengths;
	wrap_count = 0;
	max_width = 0;
	scan = words;
	/*
	 * it isn't really true that lengths[k] == strlen(words[k]).  if
	 * strlen(words[k]) > the maximum window width, we mark that line
	 * with a -1.
	 */
	for (; *scan != (char *) 0; scan++, lenp++) {
		k = strlen(*scan);
		if (k > max_width) {
			max_width = k;
		}
		if (k > chars_wide) {
			*lenp = -1;
			/* compute how many times this line will wrap. */
			for(k -= chars_wide; k > 0;) {
				wrap_count++;
				k -= chars_wide;
			}
		} else {
			*lenp = k;
		}
	}

	if (how_display == T_LINES || wrap_count > 0) {
		k = typeout_by_lines(words, how_many, wrap_count);
	} else {
		k = typeout_ala_ls(words, how_many);
	}
	free(lengths);
	/*
	 * if the typeout wrote something, show it, then restore the old
	 * image.
	 */

	if (k < 0) {
		/* nothing was written, so no cleanup needed */
		k = 0;
		goto cleanup;
	}
	if (k == 0) {
		show_rect(&view, 0);
		k = wait_for('\0');
		if (k == ' ' || !isascii(k)) {
			k = 0;
		}
	}
	if (dvi->file == (FILE *) 0) {
		clear_rect(&view);
		display_no_file();
	} else {
		show_page(dvi->cur_pg, h_view_start(), 
		  v_view_start(), 1);
	}
cleanup:
	pop_cursor();
	return(k);
}

/*
 * print the strings one per line.
 */
/*
 * return values:
 *	< 0	error, nothing written to screen so no refresh needed.
 *	0	everything is normal.
 *	> 0	user typed a key we want to add to the string.
 */

typeout_by_lines(words, how_many, wrap_count)
	char	**words;
{
 	int	w, h, x, y, written, num_lines, paging;
	char	*cp;

	/*
	 * clear the amount of the screen that we need to display the
	 * strings and draw a box around this area.
	 */
	w = (max_width < chars_wide) ? max_width : chars_wide;
	num_lines = how_many + wrap_count;
	lines = h = (num_lines < chars_high) ? num_lines : chars_high;

	w *= char_x;
	h *= char_y;
	/*
	 * compute where we need to put this rect.  we have w < win_w
	 * and h < win_h.
	 */
	x = (win_w - w) / 2 + h_view_start();
	y = (win_h - h) / 2 + v_view_start();

	/* box_it draws a box around the given dimensions */
	rect_construct(&strings, x, y, w, h);
	rect_construct(&view, 
	  x - T_MARGIN,
	  y - T_MARGIN,
	  w + 2 * T_MARGIN,
	  h + 2 * T_MARGIN);
	clear_rect(&view);
	box_it(x - 2 * T_BORDER_WIDTH, 
	  y - 2 * T_BORDER_WIDTH,
	  w + 4 * T_BORDER_WIDTH,
	  h + 4 * T_BORDER_WIDTH,
	  T_BORDER_WIDTH,
	  BOX_DRAW);

	scan = words;
	lscan = lengths;

	/*
	 * we give the y of the upper left hand corner of the character
	 * to be drawn, not the of the baseline.
	 */

	start_y = y;
	written = wrote = 0;
	paging = lines < num_lines;
	for (; how_many-- > 0; scan++, lscan++) {
		if (*lscan < 0) {
			int	l;
			/* this string would wrap around, so break it. */
			cp = *scan;
			l = strlen(cp);
			for (;;) {
				draw_text(cp, x, y, 
				  (l < chars_wide) ? l : chars_wide);
				written++;
				wrote++;
				y += char_y;
				l -= chars_wide;
				cp += chars_wide;
				if (l < 1) {
					break;
				}
				if (paging && written == lines - 1) {
					if ((rtn_c = wait_for_more(&view,
					   x, y)) != 0) {
						return(rtn_c);
					}
					num_lines -= written;
					paging = lines < num_lines;
					written = 0;
					y = start_y;
				}
			}
		} else {
			/* this string will fit on this line */
			draw_text(*scan, x, y, *lscan);
			written++;
			wrote++;
			y += char_y;
		}
		if (paging && written == lines - 1 && how_many > 2) {
			if ((rtn_c = wait_for_more(&view, x, y)) != 0) {
				return(rtn_c);
			}
			num_lines -= written;
			paging = lines < num_lines;
			written = 0;
			y = start_y;
		}
	}
	return((wrote == 0) ? -1 : 0);
}

/*
 * print the strings (which presumably are filenames or command names or
 * something like that) in columns alphabetically like ls(1) does it.
 */
/*
 * return values:
 *	< 0	error, nothing written to screen so no refresh needed.
 *	0	everything is normal.
 *	> 0	user typed a key we want to add to the string.
 */
/*
 * columns will have at least this many spaces between them.
 */
#define SPACING	(3)

typeout_ala_ls(words, how_many)
	char	**words;
{
	int	w, h, x, y,
		bump_x,		/* total width of a column in pixels */
		per_screen,	/* how many strings we can fit */
		paging;

	/*
	 * handle degenerate cases. We want (2*width) + spacing, not
	 * 2*(width+spacing) because we don't want spacing at the end of
	 * the last column.
	 */
	if ((2 * max_width) + SPACING > chars_wide) {
		return(typeout_by_lines(words, how_many, 0));
	}
	/*
	 * compute the number of columns and lines we will need.
	 */
	cols = chars_wide / (max_width + SPACING);
	if (chars_wide % (max_width + SPACING) > max_width) {
		cols++;
	}
	if (how_many < cols) {
		cols = how_many;
	}

	lines = how_many / cols;
	if (how_many % cols > 0) {
		lines++;
	}
	if (lines > chars_high) {
		lines = chars_high;
	}

	/* build the rect for displaying. */
	w = ((cols - 1) * (max_width + SPACING) + max_width) * char_x;
	h = lines * char_y;
	/*
	 * compute where we need to put this rect.  we have w < win_w
	 * and h < win_h.
	 */

	x = (win_w - w) / 2 + h_view_start();
	y = (win_h - h) / 2 + v_view_start();
	
	/* box_it draws a box around the given dimensions */
	rect_construct(&strings, x, y, w, h);
	rect_construct(&view, 
	  x - T_MARGIN,
	  y - T_MARGIN,
	  w + 2 * T_MARGIN,
	  h + 2 * T_MARGIN);
	clear_rect(&view);
	box_it(x - 2 * T_BORDER_WIDTH, 
	  y - 2 * T_BORDER_WIDTH,
	  w + 4 * T_BORDER_WIDTH,
	  h + 4 * T_BORDER_WIDTH,
	  T_BORDER_WIDTH,
	  BOX_DRAW);

	start_x = x;
	bump_x = (max_width + SPACING) * char_x;
	start_y = y;
	per_screen = cols * lines;
	scan = words;
	lscan = lengths;
	/*
	 * we use h to keep track of the number of strings written in the
	 * columns and w for the number of columns filled up.
	 */
	wrote = h = 0;
	w = 1;
	paging = (how_many > per_screen);
	for (; how_many != 0; --how_many, scan++, lscan++) {
		draw_text(*scan, x, y, *lscan);
		h++;
		wrote++;
		y += char_y;
		if (paging && w >= cols && h >= lines - 1) {
			if ((rtn_c = wait_for_more(&view, start_x, y)) != 0) {
				return(rtn_c);
			}
			x = start_x;
			y = start_y;
			h = 0;
			w = 1;
			paging = (how_many > per_screen);
			continue;
		}
		if (h == ((paging) ? lines - 1 : lines)) {
			x += bump_x;
			y = start_y;
			w++;
			h = 0;
			continue;
		}
	}
	return((wrote == 0) ? -1 : 0);
}


/*
 * print the more string at x, y; wait for some user input; and clear the
 * rect.
 */
wait_for_more(r, x, y)
	rectang	*r;
{
	int	c;

	draw_text(more_msg, x, y, sizeof(more_msg));
	show_rect(r, 0);
	c = wait_for('\0');
	clear_rect(&strings);
	if (isascii(c) && c != ' ') {
		return(c);
	}
	return(0);
}
