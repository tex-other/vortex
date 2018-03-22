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
 *  Copyright (c) 1986 - 1991 Regents of the University of California.
 *  All rights reserved.
 */

/*
 * $Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/vars.h,v 2.15 1993/09/16 02:29:01 munson Exp $
 */

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/*
 * this file contains the default values of the variables and the
 * declaration of the variable structure.  To understand better how these
 * #defines operate on the variables themselves, you may want to look at
 * the table in vars.c that equates user-visible variable names to their
 * C names.
 */

/*
 * these are boolean values.  turn them on with 1, off with 0.
 */
#define V_DEF_DRAW_B		(1)
#define V_DEF_SCROLLBARS	(1)
#define V_DEF_SHOW_LD_IMAGE	(0)
#define V_DEF_ANSI_KEYS		(1)
#define V_DEF_ICONIC		(0)

/*
 * these are integer values of varying range.  range checking is provided
 * in the un_def fields of the variable structure.  there is no check to
 * ensure that these defaults fall in those bounds so use some care
 * if you change these.
 */

/* the X and Y hot spot of the default cursor, or the one name below. */
#define V_DEF_CURSOR_XHOT	(9)
#define V_DEF_CURSOR_YHOT	(9)

/* the X and Y placement of the icon. */
#define V_DEF_ICON_X		(1000)
#define V_DEF_ICON_Y		(0)

/* the X and Y placement and the width and height of the window. */
#define V_DEF_WIN_W		(1064)
#define V_DEF_WIN_H		(400)
#define V_DEF_WIN_X		(88)
#define V_DEF_WIN_Y		(200)

/* the number of dvi pages dvitool will cache. */
#define V_DEF_PAGES_CACHED	(3)

/*
 * the ascii character that is the user's abort key.  This is looked for
 * at various critical points in the processing to let the user abort the
 * action he has begun.  It is declared and is set as an integer because
 * I didn't think this one variable justified implementing the type of
 * char.
 */
#define V_DEF_ABORT_CH		(0x7)	/* control-g */

/*
 * these two control how arbitrary horizontal and vertical movements are
 * translated into kerns, spaces and line breaks.  In general, it is
 * probably not a good thing to change these defaults unless you know
 * what you are doing.
 */
#define V_DEF_KERN_THRESHOLD	(150)
#define V_DEF_LBRK_THRESHOLD	(110)

/* the margins (in scaled points)  */
/* 2^16 * 72.27 = 4736286.7 */

#define ONE_INCH_SP 		(4736287)
#define V_DEF_TOP_MAR		ONE_INCH_SP
#define V_DEF_LEFT_MAR		ONE_INCH_SP

/*
 * the width and height of the display page before the borders (if any)
 * are added in.
 */
#define V_DEF_PAGE_WIDTH	(ONE_INCH_SP * 8.5)
#define V_DEF_PAGE_HEIGHT	(ONE_INCH_SP * 11)

/*
 * the width of the border stripe (in pixels) that will be painted at the
 * edge of the page to create the page outline.
 */
#define V_DEF_BORDER_W		(3)

/*
 * the string variables.
 */
#ifndef	V_DEF_FONT_PATH
#	define V_DEF_FONT_PATH	"/usr/local/fonts/pk:/usr/local/fonts/pixel"
#endif	V_DEF_FONT_PATH
#define V_DEF_ICON_FILE		""
#define V_DEF_CURSOR_FILE	""
#define V_DEF_LOG_FILE		""

/*
 * these are used for the bounds of some of the variables.
 * They should change when Sun changes the dimensions of it's screen again.
 */
#define SCR_WIDTH	(3000)
#define SCR_HEIGHT	(2000)

/*
 * these define an enumerated type that I keep around to let the print
 * routine know how to represent a dimen.
 */
#define DIM_IN		(0)
#define DIM_SP		(1)
#define DIM_CM		(2)
#define DIM_PT		(3)

/* the type of the variable goes in the first 2 bits */
#define V_INT		(0)
#define V_STR		(1)
#define V_BOOL		(2)
#define V_DIM		(3)

#define V_ALL_TYPES	(0x3)

/* the rest of the bits are switches */
#define V_NORC		(1 << 2)	/* action procedure not invoked
					 * while reading the .rc file.
					 */
#define V_ODVI		(1 << 3)	/* action procedure not invoked
					 * unless there is an open DVI
					 * file.
					 */
#define V_C_OCTAL	(1 << 4)	/* show int in  C octal */
#define V_C_HEX		(1 << 5)	/*		C hex */
#define V_T_OCTAL	(1 << 6)	/*		TeX octal */
#define V_T_HEX		(1 << 7)	/*		TeX hex */
#define HEX_OCTAL_MASK	(V_C_OCTAL|V_C_HEX|V_T_OCTAL|V_T_HEX)

typedef char	*var;

typedef struct Variable {
	char	*name;
	var	val;
	int	flags,
		(*changed)(),		/* put changed value into action. */
		un_def1,		/* undefined words, used for... */
		un_def2;		/* ...different tasks. */
} variable;

