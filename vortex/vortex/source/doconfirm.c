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
 *  RCS Info: $Header: doconfirm.c,v 0.1 87/05/01 11:39:56 john Locked $
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
 *  doconfirm.c - confirmer window interface function
 */
static char _ID[] = "@(#)doconfirm.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: confirm
 *  Call: (confirm 'message)
 *  Retu: t or nil
 *  Desc: This function brings up a confirmer window with the given
 *	message.  The window also has three buttons; ``yes'', ``no''
 *	and ``cancel''.  If ``yes'' is pressed, the function returns
 *	t, if ``no'' it returns nil and ``cancel'' aborts the action.
 *
 *	Nothing else may be done until the user deals with the
 *	confirmer window.
 *  SeeA: yes-or-no
 */

DEFUN(doconfirm, "confirm", FUNC_VISUAL, NULL)
{
	struct value	arg;
	char		mesg[1024];

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string message");
	makecstring(gstring(arg.vl_data), mesg, sizeof (mesg));

	if (confirm(mesg))
		return (v_t);
	else
		return (v_nil);
}

#include "bitmaps/question"
#include "bitmaps/question_mask"

confirm(message)
	char	*message;
{
	Window		window;
	XEvent		event;

	return (FALSE);
}
