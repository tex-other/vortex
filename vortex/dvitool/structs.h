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

/*
 * $Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/structs.h,v 2.15 1993/09/16 02:29:01 munson Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>

typedef struct Switches {
	int	use_existing,
		init_page;
} switches;

typedef struct pixrect pixr;

/*
 * this definition is intentionally (virtually) identical to Sun's.  They
 * typedef short coord and then define r_left & r_top as coord.
 */
typedef struct  Rectangle {
	short	r_left,
		r_top,
		r_width,
		r_height;
} rectang;

typedef union Func_Arg {
	char	*str;
	int	integer;
} func_arg;

typedef struct function {
	char		*name;
	int		(*funcp)();
	int		flags;
	int		(*arg0)();
	char		*arg0_def;
	int		(*ctrl0)();
	int		(*arg1)();
	char		*arg1_def;
	int		(*ctrl1)();
} func;

/*
 * the character info read from the PXL or PK files.
 */
typedef struct Char_Entry {
	unsigned short	width,		/* in pixels */
			height;
	short		x_offset,	/* in pixels */
			y_offset;
	unsigned short	flags;
	union {
		long	file_offset;
		pixr	*pr;
	} address;
	int	tfmw;
} char_entry;

/*
 * the structure for search characters, used by the word-searching
 * routines.
 */
typedef struct search_char {
	u_char	c,
		f;		/* the font this char is set in */
	u_char	c_type;		/* [space,searchable,unsearchable] */
	u_char	flags;
	short	x,		/* of the upper left hand corner */
		y;		/* of the characters' box in pixels */
	char_entry *f_info;	/* including w, h, & *pixrect */
} s_char;
	
/*
 * all of the information for a single page.
 */
typedef struct Page {
	long		offset;		/* fseek offset from byte 0 of file. */
	long		size;		/* size in bytes. */
	short		count[10],	/* the 10 TeX count variables. */
			ppage;		/* physical page number. */
	char		*load_i;	/* the graphic page image. */
	rectang		load_r;		/* the rect for that image. */
	s_char		*s_buf,		/* the search buffer. */
			*s_buf_end,	/* last char in the search buffer. */
					/*
					 * the ``lines'' variables are
					 * created and updated correctly,
					 * but they are not used by any
					 * of the functions.  I just
					 * didn't have time to get around
					 * to scrolling by lines.
					 */
			**lines;	/* the ``lines'' of this page. */
	rectang		sel_r;		/* bounding rect of the selection. */
	int		max_lines,	/* lines is max_lines long. */
			line_c;		/* count of lines in lines. */
	long		time_stamp;	/* creation or use time stamp. */
	struct Page	*next,
			*prev;
} pg;

typedef struct dvi_pages {
	pg	*first,
		*last;
	int	num_cached;		/* count of current cached pages. */
} pgs;

/*
 * all of the pertinent info about the DVI file being previewed.
 * parameters specific to the dvi file (as listed in the dvitype
 * documentation) are listed first.
 */

typedef struct dvi_header {
	long	postamble,	/* all these are found in the postamble. */
		last_page;
	int	num,
		den,
		TeX_mag,	/* the mag value in the dvi file. */
		max_height,
		max_width,
		stack_depth,
		num_pages;
				/* these are not in the postamble. */
	int	user_mag,	/* the mag value the user has requested. */
		mag;		/* the product of TeX_mag and user_mag. */
	FILE	*file;
	char	*fname,
		*cwd;
	int	w_in_pixels,	/* max_width converted to pixels. */
		h_in_pixels,
		l_mar_in_pixels,/* left margin in pixels. */
		t_mar_in_pixels,
		kern_limit,	/* threshold over which h moves are spaces. */
		baseln_limit;	/* same for linebreaks. */
	pgs	*pages;
	pg	*cur_pg;	/* the page we are looking at right now. */
	s_char	*sel_start,	/* the first char of the selection... */
		*sel_end;	/* ...and the last char. */
	pg	*sel_pg;	/* the page the selection is on. */
} dvi_h;

extern dvi_h	*dvi;

/* global tool data */
typedef struct tool_data {
	dvi_h		*dvi;
	int		pid,
			created,
			max_fds,
			in_rc;
	char		*prog_name,
			*home;
	switches	*sw;
} tl_data;

