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
 *  RCS Info: $Header: prefix.c,v 0.1 87/05/01 12:23:52 john Locked $
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
 *  prefix.c - the various prefix commands (vLisp functions)
 */
static char _ID[] = "@(#)prefix.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  These global variables hold the accumulated global prefix.
 *  If any prefix commands have been called since the last
 *  interactive function invocation, prefix_typed will be
 *  true and global_prefix will contain the integer representing
 *  the prefix.  Prefix sign contains 1 or -1 which toggles
 *  each time the prefix is negated.
 */
static int	generic_typed = FALSE;
static int	prefix_typed = FALSE;
static int	global_prefix = 0;
static int	prefix_sign = 1;

/*
 *  DOCUMENTATION
 *
 *  Name: digit-command
 *  Call: (digit-command)
 *  Retu: nil
 *  Desc: This is the function to which the digit keys are bound to
 *	make prefixes work.  Normally, this command functions just
 *	as \sym{self-insert}, but if a prefix is in progress, the
 *	number the digit represents is ``added'' to the current
 *	prefix.
 *
 *	Actually, the previous value of the global prefix is multipled
 *	by ten and the value of the digit added, which will act as
 *	though the digits typed make up a larger number.
 *  Side: Interactive commands which take the prefix as an argument
 *	will, of course, have that argument changed.
 *  SeeA: meta-digit-command generic-prefix self-insert
 */
extern struct string	*typed_keys;

DEFUN(dodigit, "digit-command", FUNC_PREFIX, "")
{
	struct window	*winp = current_window;
	struct buffer	*bufp;
	int		code;

	/* get the current window's buffer */
	bufp = windowbuffer(winp, BUFF_SOURCE);

	/* get the value of the key strokes typed */
	if (typed_keys == NULL)
		return (v_nil);
	code = typed_keys->st_buffer[typed_keys->st_length - 1];

	if (generic_typed) {
		generic_typed = FALSE;
		global_prefix = 4;
		prefix_typed = TRUE;
	}

	if (prefix_typed) {
		if (code >= '0' && code <= '9')
			global_prefix = (global_prefix * 10) + (code - '0');
	} else {
		/* this is what self-insert does */
		insert_char(bufp, code);
		setwindowpoint(winp, winp->wi_point + 1);
	}
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: meta-digit-command
 *  Call: (meta-digit-command)
 *  Retu: nil
 *  Desc: This is the function to which the meta-digit keys are
 *	bound to make prefixes work.  This command begins a prefix
 *	string, and causes all keys bound to \sym{digit-command} to
 *	behave specially.  The number the digit represents becomes
 *	the current prefix.
 *
 *	Actually, the previous value of the global prefix is multipled
 *	by ten and the value of the digit added, which will act as
 *	though the digits typed make up a larger number.  However, if
 *	the last part of the key sequence is not a digit, it is
 *	assumed to be a minus sign, and negates the current prefix
 *	argument.
 *  Side: Interactive commands which take the prefix as an argument
 *	will, of course, have that argument changed.
 *  SeeA: digit-command generic-prefix self-insert
 */

DEFUN(dometadigit, "meta-digit-command", FUNC_PREFIX, "")
{
	int	code;

	/* get the value of the key strokes typed */
	if (typed_keys == NULL)
		return (v_nil);
	code = typed_keys->st_buffer[typed_keys->st_length - 1];

	prefix_typed = TRUE;
	if (code >= '0' && code <= '9')
		global_prefix = (global_prefix * 10) + (code - '0');
	else if (prefix_sign >= 0)
		prefix_sign = -1;
	else
		prefix_sign = 1;
	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: generic-prefix
 *  Call: (generic-prefix)
 *  Retu: nil
 *  Desc: This function is bound by default to the key C-u (control-U)
 *	as the ``times four'' prefix.  Each time it is typed, it
 *	multiplies the global prefix by four.
 *  SeeA: digit-command meta-digit-command get-global-prefix
 */

DEFUN(dogenprefix, "generic-prefix", FUNC_PREFIX, "")
{
	CHECKAC(0, 0);

	if (generic_typed) {
		/* C-u C-u  ->  ESC 1 6 */
		generic_typed = FALSE;
		global_prefix = 4;
		prefix_typed = TRUE;
	}

	if (prefix_typed)
		global_prefix *= 4;
	else
		generic_typed = TRUE;

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: get-global-prefix
 *  Call: (get-global-prefix)
 *  Retu: fixnum
 *  Desc: This function returns the current value of the interactive
 *	prefix typed by the user.  The prefix returned is a fixnum
 *	or nil if none has been typed.
 *  SeeA: set-global-prefix digit-command generic-prefix
 */

DEFUN(dogetprefix, "get-global-prefix", FLAG_NONE, NULL)
{
	extern struct value	get_prefix();

	CHECKAC(0, 0);
	return get_prefix();
}

/*
 *  DOCUMENTATION
 *
 *  Name: set-global-prefix
 *  Call: (set-global-prefix 'fixnum)
 *  Retu: fixnum
 *  Desc: This function changes the current value of the interactive
 *	prefix to the value given.  The prefix must be a fixnum
 *	or nil to indicate that none was typed.  This value will be
 *	that handed to interactive functions that take the prefix
 *	as one of their arguments.
 *  SeeA: get-global-prefix digit-command generic-prefix
 */

DEFUN(dosetprefix, "set-global-prefix", FLAG_NONE, NULL)
{
	extern struct value	get_prefix();
	struct value		arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_NIL:
		clear_prefix();
		break;
	case LISP_FIXNUM:
		prefix_typed = TRUE;
		prefix_sign = 1;
		global_prefix = gfixnum(arg.vl_data);
		if (global_prefix < 0) {
			prefix_sign = -1;
			global_prefix *= -1;
		}
		break;
	default:
		BADARGN(1, "a fixnum prefix");
	}
	return get_prefix();
}

struct value
get_prefix()
{
	struct value	ret;

	/* make sure some prefix was given */
	if (generic_typed)
		return (v_t);
	else if (!prefix_typed)
		return (v_nil);

	/* return the global prefix as a fixnum */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, prefix_sign * global_prefix);
	return (ret);
}

clear_prefix()
{
	generic_typed = FALSE;
	prefix_typed = FALSE;
	prefix_sign = 1;
	global_prefix = 0;
}
