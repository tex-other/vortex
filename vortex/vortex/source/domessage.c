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
 *  RCS Info: $Header: domessage.c,v 0.1 87/05/01 11:55:27 john Locked $
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
 *  domessage.c - routines and functions to print messages
 */
static char _ID[] = "@(#)domessage.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"
#include "format.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: message
 *  Call: (message 'string)
 *  Retu: nil
 *  Desc: This function prints the string that its single argument
 *	must evaluate to on the minibuffer as a message to the user.
 *	This function always returns nil.
 *  SeeA: error
 */

DEFUN(domessage, "message", FLAG_NONE, NULL)
{
	struct value	arg;
	char		msg[STRBUF];

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string message");
	makepstring(gstring(arg.vl_data), msg, sizeof (msg));

	int_message(msg);
	return (v_nil);
}

/* VARARGS */
message(va_alist)
	va_dcl
{
#include "fmtdecl.h"

#include "fmtcode.h"

	int_message(msgbuf);
	return (0);
}

int_message(mesg)
	char	*mesg;
{
	if (havedisplay && !subprocess) {
		/* paint the message on the minibuffer */
		minb_print(mesg, strlen(mesg), FALSE);
	} else {
		/* print the message to the terminal */
		write(STDOUT, mesg, strlen(mesg));
		write(STDOUT, "\r\n", 2);
	}

	return (0);
}

err_message(mesg)
	char	*mesg;
{
	if (havedisplay && !subprocess) {
		/* paint the message on the minibuffer */
		minb_print(mesg, strlen(mesg), TRUE);
	} else {
		/* print the message to the terminal */
		write(STDOUT, mesg, strlen(mesg));
		write(STDOUT, "\r\n", 2);
	}

	return (0);
}
