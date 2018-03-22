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
 * $Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/font.h,v 2.15 1993/09/16 02:29:01 munson Exp $
 */

/*
 * Definitions for font handling.
 */

#define PXLID		(1001)
#define PXL_CHARS	(128)

/*
 * the size of the pixel buffer.  With a buffer this big we can read a
 * single line of a pixel image that is 2048 pixels wide.
 */
#define PXL_BUF_SIZE	(2048/32)

/*
 * These constants define the state of a bit-map in a loaded font; either
 * it is unloaded (not yet read from the font file), loaded (read from
 * the font file, interpreted into a bitmap and cached in memory) or not
 * found (the bitmap was not read because the font file could not be
 * found).
 */
#define C_LOADED	(1 << 0)
#define NOT_FOUND	(1 << 1)

/*
 * this bit tells us whether this character is to be read from a PXL or a
 * PK format file, and the next tells us whether the character def is in
 * short or extended form (long form is not allowed).
 */
#define C_PK		(1 << 2)
#define C_SHORT		(1 << 3)
/*
 * bits 4-7 of the flag byte are reserved for the dyn_f in PK files.
 */
#define C_DYN_F_MASK	(0xf0)
/*
 * bit 8 tells us when parsing the run-encodings whether to start with
 * black or white pixels.
 */
#define C_START_BLACK	(1 << 8)

/*
 * flags which tell us which type of font file to open.
 */
#define TRY_PXL		(1 << 0)
#define TRY_PK		(1 << 1)


typedef struct Font_Entry {
	int		k,		/* font number as assigned by TeX. */
			s,		/* font space. */
			d,		/* design size from DVI file. */
			font_mag;	/* computed mag from DVI file. */
	char		*name;
	char_entry	*ch;		/* character information. */
	u_int		max_ch;		/* index of highest character. */
	FILE		*fp;
	u_short		flags,
			used;
	struct Font_Entry *next;
} fontp;

