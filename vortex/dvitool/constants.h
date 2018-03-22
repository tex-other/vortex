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
 * $Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/constants.h,v 2.15 1993/09/16 02:29:01 munson Exp $
 */

/* the ID of dvi files we can process. */
#define DVIFORMAT	(2)

/* the stack size of the DVI interpreter. */
#define STACKSIZE	(50)

/* the assumed dots per inch of the user's screen. */
#ifdef AM
#	define HCONVRESOLUTION (118)
#	define VCONVRESOLUTION (110)
#else
#	define HCONVRESOLUTION (120)
#	define VCONVRESOLUTION HCONVRESOLUTION
#endif AM

/* the maximum length of argument input strings. */
#define ARGBUF_SIZE (100)

/* these two from <sys/param.h> */
#define MAXPATHLEN	(1024)

/* draw operations for selections and messages. */
#define UNINVERT_SEL	(0)
#define SHOW_SEL	(1)
#define CLEAR_SEL	(2)
		
/* draw operations for boxes. */
#define BOX_DRAW	(0)
#define BOX_INVERT	(1)

/* the image cursors. */
#define MAIN_CUR	(0)
#define MOUSE_CUR	(1)
#define HOUR_CUR	(2)

/* the message types. */
#define PLAIN		(0)
#define FATAL		(1 << 0)
#define WAIT		(1 << 1)
#define PERROR		(1 << 2)
#define TITLE		(1 << 3)
#define APPEND		(1 << 4)
#define LITERAL		(1 << 5)
#define OVERWRITE	(1 << 6)

/* the message cursors. */
#define NO_MCUR		(0)
#define INT_MCUR	(1)
#define STR_MCUR	(2)
#define FNAME_MCUR	(3)
#define FUNC_MCUR	(4)
#define FONT_MCUR	(5)
#define VAR_MCUR	(6)
#define REG_MCUR	(7)
#define VHELP_MCUR	(8)
#define FHELP_MCUR	(9)
#define OHELP_MCUR	(10)
#define LSTR_MCUR	(11)
#define MCURS		(12)

/* these are for the command interpreter. */
#define	ODVI		(1 << 0)	/* requires an open dvi file. */
#define NORC		(1 << 1)	/* can't do this in the .rc file */
#define SEL		(1 << 2)	/* must have selected a region */
#define ISBUT		(1 << 3)	/* must run command from mouse */


/* the various types of search characters. */
#define S_SEARCHABLE	(0)
#define S_UNSEARCHABLE	(1)
#define S_SPACE		(2)
#define S_EOL		(3)
#define S_END		(4)

/* and the flags for search characters. */
#define S_INVERTED	(1)

/* the types of TYPEOUT. */
#define T_LINES		(0)	/* one string per line */
#define T_COL_BY_ROW	(1)	/* columnate filenames a la ls(1) */

#define KEEP_PG_CACHED	(1)	/* be sure not to throw this page out. */

/* the different directions to scroll. */
#define MV_RIGHT	(0)
#define MV_LEFT		(1)
#define MV_UP		(2)
#define MV_DOWN		(3)
#define RLDUMAX 	(4)
