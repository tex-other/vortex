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
 *  RCS Info: $Header: lisp.c,v 0.1 87/05/01 12:19:57 john Locked $
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
 *  lisp.c - basic vLisp read-eval-print top level
 */
static char _ID[] = "@(#)lisp.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "catch.h"
#include "channel.h"

/*
 *  The break loop variables.  Here lisp_loop sets break_loop
 *  the lisp_break_loop which handles break loops for the
 *  line oriented lisp interpreter running stand alone.
 */
extern int		break_level;		/* recursive break loops */
extern int		break_return;		/* function cont was called */
extern int		break_cancont;		/* can continue break loop */

MKSTRING(HELLOSTR, "Welcome to VorTeX lisp, version 0.\n");
MKSTRING(PROMPTSTR, "-> ");

/*
 *  The catch tag names.  We need to set them in lisp_loop so that
 *  the internal long_jmp goes to the right place.  Once we enter
 *  lisp_loop, we never can return.  We don't use the abort catch
 *  in the lisp loop, we trap to error on abort.
 */
extern struct string	*TOPLEVCATCH,	/* for ``clean'' throws to top-level */
			*ERRORCATCH;	/* for errors if no break loop */

lisp_loop()
{
	extern int	interrupt();
	struct value	input, evaled, val;
	char		msg[STRBUF];
	jmp_buf		tplbuf, errbuf;

	/* print a startup message */
	cpatom(HELLOSTR, cstdout);

	/* set up top-level catch here */
	switch (setjmp(tplbuf)) {
	case -1:	/* error */
		panic("Set jump failed for top-level catch!");
	case 0:		/* just been set */
		int_catch(TOPLEVCATCH, tplbuf, FLAG_NONE);
		break;
	default:	/* longjmp back to here */
		debug(DTHROW, "Returned from lisp top-level throw.");
		int_message("[return to top-level]");
		goto restart;
	}

	/* set up the error catch here */
	switch (setjmp(errbuf)) {
	case -1:	/* error */
		panic("Set jump failed for error catch!");
	case 0:		/* just been set */
		int_catch(ERRORCATCH, errbuf, FLAG_NONE);
		break;
	default:	/* longjmp back to here */
		debug(DTHROW, "Returned from lisp error throw.");
		val = throw_value(ERRORCATCH);
		if (stringp(val)) {
			makepstring(gstring(val.vl_data), msg, sizeof (msg));
			int_message(msg);
		}
		int_message("[abort to top-level]");
		goto restart;
	}

	/* initialize interrupts to cause a break loop */
	signal(SIGINT, interrupt);

restart:
	/* re-initialize call frame and stacked symbols */
	lisp_restore();

	for (;;) {
		cpatom(PROMPTSTR, cstdout);
		cflush(cstdout);
		input = cread(cstdin);
		if (eq(input, NOVALUE)) {
			scpatom("(exit)\n", cstdout);
			break;
		}
		evaled = evalsexpr(input);
		cprint(evaled, cstdout);
		cprinc('\n', cstdout);
		process_check();
	}
	cflush(cstdout);

	exit(0);
}

lisp_restore()
{
	struct value	stack_goto();
	extern int	errline;
	extern char	*errfile;
	extern int	overflow_ccells,	overflow_symbols,
			overflow_strings,	overflow_buffer,
			overflow_functs;

	/* clean up break loop globals */
	break_level = 0;
	break_return = break_cancont = FALSE;

	/* clean up prog catches and globals */
	clean_progs();

	/* clean up stacked symbol bindings */
	clean_global();

	/* clean up call frame */
	stack_goto(0);

	/* clean up error line/file indicators */
	errline = -1;
	errfile = NULL;

	/* reset mask to no signals blocked */
	SETSIGMASK(0);
}

static
interrupt(sig)
{
	extern struct stack	break_loop();
	extern struct string	*BREAKABORT;

	/* okay to call recursively through interrupts */
	if (sig > 0 && sig < NSIG)
		SIGRELSE(sig);

	/* break if we can, otherwise it's an error */
	if (truevar(BREAKABORT)) {
		int_message("Keyboard interrupt.");
		break_loop(TRUE, "Interrupt.");
	}
	error("Interrupt.");
}
