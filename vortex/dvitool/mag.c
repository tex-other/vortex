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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/mag.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/*
 * come here to change the global magnification at which we are viewing the
 * current file.
 */

#include "structs.h"
#include "constants.h"
#include "mag.h"

#define MAG_0		(1000)
#define MAG_1		(1200)
#define MAG_2		(1440)
#define MAG_3		(1728)
#define MAG_4		(2074)
#define MAG_5		(2488)
#define MAG_MINUS_1	(833)
#define MAG_MINUS_2	(694)
#define MAG_MINUS_3	(579)
#define MAG_MINUS_4	(482)
#define MAG_MINUS_5	(402)

/*
 * the structure that holds the array of steps and mags is guaranteed to
 * have an entry where both step and mag are MAG_SENTINEL in its first
 * and last elements.  Outside routines can declare initialize a pointer
 * to &magstep[1] and cruise around without fear.
 */

mags magsteps[] = {
{ MAG_SENTINEL,	MAG_SENTINEL, 	(char *) 0 },
{ -5,		MAG_MINUS_5,	"step -5" },
{ -4,		MAG_MINUS_4,	"step -4" },
{ -3,		MAG_MINUS_3,	"step -3" },
{ -2,		MAG_MINUS_2,	"step -2" },
{ -1,		MAG_MINUS_1,	"step -1" },
{ 0,		MAG_0,		"step  0" },
{ 1,		MAG_1,		"step  1" },
{ 2,		MAG_2,		"step  2" },
{ 3,		MAG_3,		"step  3" },
{ 4,		MAG_4,		"step  4" },
{ 5,		MAG_5,		"step  5" },
{ MAG_SENTINEL, MAG_SENTINEL,	(char *) 0 },
};

step_from_mag(val, step)
	int	*step;
{
	mags	*mp = &magsteps[1];

	for (; mp->step != MAG_SENTINEL; mp++) {
		if (mp->mag == val) {
			*step = mp->step;
			return(0);
		}
	}
	*step = 0;
	return(-1);
}

/*
 * this routine presumes new_mag is valid, that is, one of the values in
 * the magsteps structure.  nothing catastrophic will happen if it is
 * not, but probably dvitool will have a hard time finding font files.
 */
do_mag(new_mag)
{
	extern int	rereading;
	int		status = 0;

	if (new_mag != dvi->user_mag) {
		dvi->user_mag = new_mag;
		rereading = 1;
		status = reload_this_file();
		rereading = 0;
	}
	return(status);
}

/*
 * DOCUMENTATION
 *
 * Name: magstep-0
 * Desc: This function displays the \lit{DVI} file at its normal size.
 *	It is a no-op unless the file has previous been displayed at
 *	some other magstep.  Note that magnification in \lit{dvitool}
 *	is global, that is, everything including the width of the page
 *	and the margins will be affected by the magnification routines.
 *	There is no analogy to \pass{\TeX's} true points in \lit{dvitool}.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag0()
{
	return(do_mag(MAG_0));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-1
 * Desc: This command magnifies the document to 120 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag1()
{
	return(do_mag(MAG_1));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-2
 * Desc: This command magnifies the document to 144 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag2()
{
	return(do_mag(MAG_2));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-3
 * Desc: This command magnifies the document to 172.8 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag3()
{
	return(do_mag(MAG_3));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-4
 * Desc: This command magnifies the document to 207.4 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag4()
{
	return(do_mag(MAG_4));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-5
 * Desc: This command magnifies the document to 248.8 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag5()
{
	return(do_mag(MAG_5));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-minus-1
 * Desc: This command shrinks the document to 83.3 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag_minus_1()
{
	return(do_mag(MAG_MINUS_1));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-minus-2
 * Desc: This command shrinks the document to 69.4 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag_minus_2()
{
	return(do_mag(MAG_MINUS_2));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-minus-3
 * Desc: This command shrinks the document to 57.9 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag_minus_3()
{
	return(do_mag(MAG_MINUS_3));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-minus-4
 * Desc: This command shrinks the document to 48.2 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag_minus_4()
{
	return(do_mag(MAG_MINUS_4));
}
/*
 * DOCUMENTATION
 *
 * Name: magstep-minus-5
 * Desc: This command shrinks the document to 40.2 percent of its
 *	\em{magstep-0} size.
 * SeeA: magstep-minus-5\pass{$\ldots$}magstep-5
 */
mag_minus_5()
{
	return(do_mag(MAG_MINUS_5));
}

