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
 *  RCS Info: $Header: dobeep.c,v 0.1 87/05/01 11:34:28 john Locked $
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
 *  dobeep.c - X bell control functions
 */
static char _ID[] = "@(#)dobeep.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include "vse.h"
#include "vlisp.h"
#include "window.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: x-bell-volume
 *  Call: (x-bell-volume [ 'fixnum ])
 *  Retu: fixnum
 *  Desc: This function sets the bell volume for X to the volume
 *	specified by its optional argument, if given.  This function
 *	always returns a fixnum indicating the current volume, which
 *	will be the same as the argument if one is given.
 *
 *	The bell volume should be an integer between zero and seven,
 *	with zero being no sound (disable bell) and seven being the
 *	loudest.  If the hardware cannot change the bell volume,
 *	positive values mean ring the bell, and zero disables it.
 *  Side: Errors and aborts, among other occurances, cause the bell
 *	to be rung, so disabling the bell by setting the volume to
 *	zero will cause these signals not to occur.
 *  SeeA: beep
 */
static int	bell_volume = 1;

DEFUN(dovolume, "x-bell-volume", FLAG_NONE, "nBell volume: ")
{
	struct value	arg, ret;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a volume fixnum");
		bell_volume = gfixnum(arg.vl_data);
		if (bell_volume < 0)
			bell_volume = 0;
		if (bell_volume > 7)
			bell_volume = 7;
		XFeepControl(bell_volume);
	}

	/* return the current volume */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, bell_volume);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: beep
 *  Call: (beep [ 'count ])
 *  Retu: t
 *  Desc: This function rings the terminal bell as many times as
 *	specified by the optional argument, or once if no argument
 *	is given.
 *  Xref: ring-bell
 */

DEFUN(dobeep, "beep", FLAG_NONE, NULL)
{
	struct value	arg;
	register int	count = 1;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg))
			BADARGN(1, "a positive fixnum");
		count = gfixnum(arg.vl_data);
		if (count <= 0)
			return (v_nil);
	}

	while (count-- > 0)
		beep(0);
	return (v_t);
}

beep(vol)
{
	static char	CTRLG = '\004';

	if (havedisplay)
		XFeep(vol);
	else
		write(STDOUT, &CTRLG, 1);
}
