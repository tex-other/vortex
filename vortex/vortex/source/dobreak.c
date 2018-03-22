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
 *  RCS Info: $Header: dobreak.c,v 0.1 87/05/01 11:35:08 john Locked $
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
 *  dobreak.c - vLisp interface functions to break loop
 */
static char _ID[] = "@(#)dobreak.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "window.h"
#include "channel.h"

/*
 *  DOCUMENTATION
 *
 *  Name: break-loop
 *  Call: (break-loop [ 't ])
 *  Retu: any
 *  Desc: This function causes a break loop to be entered by
 *	the editor.  This allows one to examine the state of
 *	the editor through the normal interactive lisp methods.
 *	If the optional argument is present and evaluates to
 *	non-nil, the break loop is continuable and the function
 *	\sym{break-loop} will return if the user evals \sym{cont}.
 *
 *	There are several ways to exit the break loop.  The
 *	function \sym{reset} returns to the visual editor by
 *	\sym{throw}ing to the \lit{top-level} catch.  Typing
 *	your end-of-file character (usually control-D) does
 *	a \sym{reset} automatically.  Or, \sym{cont} allows
 *	you to return from the break loop with a specified
 *	value.  Of course, \sym{exit} will exit the program.
 *
 *	Break loops may nest, so if you are in a break loop and
 *	an error occurs or another call is made to \sym{break-loop},
 *	another break loop will be entered.  The level of the
 *	break loop is indicated by the prompt.
 * Side: The break loop is run on the terminal which started up
 *	the editor, not in the editor itself and the editor is
 *	disabled during the break loop.  Thus, when using the
 *	break loop, all your usualy \sc{UNIX} line editing
 *	characters and modes are in force, not those of the
 *	editor.
 *  SeeA: reset cont exit
 */
extern struct value	break_loop();

DEFUN(dobreakloop, "break-loop", FLAG_NONE, "")
{
	struct value	arg, ret;
	int		cancont = FALSE;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (truep(arg))
			cancont = TRUE;
	}

	ret = break_loop(cancont, NULL);
	return (ret);
}

/*
 *  The break loop variables.  These have to be global because break
 *  loops can come and go at different levels and other routines
 *  sometimes examine them.  Anyway, their names should indicate
 *  their use.
 */
extern int		break_level;		/* recursive break loops */
extern int		break_return;		/* function cont was called */
extern int		break_cancont;		/* can continue break loop */

extern struct string	*TOPLEVCATCH;		/* reset throws to here */

struct value
break_loop(cancont, message)
	char	*message;
{
	struct value	sexpr, result;
	struct string	sbuf;
	unsigned char	pbuf[SMALLBUF];
	int		scont;

	if (havedisplay) {
		/* explaine and update screen */
		int_message("Entering break loop...");
		update_screen();
	}

	/* initialize this level of the break loop */
	break_level++;
	sprintf(pbuf, "<%d> ", break_level);
	sbuf.st_length = strlen(pbuf);
	sbuf.st_buffer = pbuf;

	/* print a message to the user if not already done */
	if (havedisplay && message != NULL) {
		scpatom(message, cstdout);
		cprinc('\n', cstdout);
	}
	if (cancont)
		scpatom("[Entered continuable break loop]\n", cstdout);
	else
		scpatom("[Entered non-continuable break loop]\n", cstdout);

	/* can we continue? */
	scont = break_cancont;
	break_cancont = cancont;

	/* do a small read/eval/print loop */
	break_return = FALSE;
	for (;;) {
		cpatom(&sbuf, cstdout);
		cflush(cstdout);
		sexpr = cread(cstdin);
		if (eq(sexpr, NOVALUE)) {
			scpatom("(reset)\n", cstdout);
			cflush(cstdout);
			cseek(cstdin, 0L, 0);
			int_throw(TOPLEVCATCH, v_t);
		}
		result = evalsexpr(sexpr);
		if (break_return)
			break;
		cprint(result, cstdout);
		cprinc('\n', cstdout);
	}
	cflush(cstdout);

	ASSERT(break_cancont);

	break_level--;
	break_return = FALSE;
	break_cancont = scont;

	return (result);
}
