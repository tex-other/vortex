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
 * $Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/keys.h,v 2.15 1993/09/16 02:29:01 munson Exp $
 */

/*
 * define the virtual keys.
 */

#define SHIFT_MOUSE	(1)
#define CONTROL_MOUSE	(2)

#define LEFT_MOUSE	(0)
#define MIDDLE_MOUSE	(4)
#define RIGHT_MOUSE	(8)


#define	FIRST_MOUSE	(128)
#define LAST_MOUSE	(FIRST_MOUSE + 11)
#define FIRST_HSCROLL	(LAST_MOUSE + 1)
#define LAST_HSCROLL	(FIRST_HSCROLL + 11)
#define FIRST_VSCROLL	(LAST_HSCROLL + 1)
#define LAST_VSCROLL	(FIRST_VSCROLL + 11)

/* declare the user's terminal characters */
extern int	erase_ch,
		werase_ch,
		kill_ch,
		intr_ch,
		abort_ch,
		lit_next_ch,
		reprint_ch;
