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
 *  RCS Info: $Header: main.c,v 0.1 87/05/01 12:20:31 john Locked $
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
 *  main.c - main routine for VorTeX source editor
 */
static char _ID[] = "@(#)main.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include "vse.h"
#include "vlisp.h"
#include "catch.h"
#include "channel.h"

int	_curmask = 0;	/* current blocked signal mask */
int	_protcount = 0;	/* level of interrupt protection */

/*
 *  When we're running only with the lisp interpreter interactively
 *  this is the main loop.  We read commands on the input until
 *  EOF occurs, evaluating each s-expression as read.  If our
 *  argument is non-NULL, we write a transcript of all input
 *  and evaluated s-expressions into it as they occur.
 */
char	*program = NULL;	/* program name, will be set if standalone */
char	USAGE[] = "usage: %s [ -l | -v ] [ -f font ] [ -i file ] files [ display ] [ geometry ]\n";

int	havedisplay = FALSE;	/* we're painting on an X display */
int	initialized = FALSE;	/* we're running as the editor */
int	quitting = FALSE;	/* we're in cleanup processesing */
int	subprocess = FALSE;	/* this process is a child of VorTeX */

MKSTRING(TOPLEVCATCH, "top-level");
MKSTRING(ERRORCATCH, "error");
MKSTRING(ABORTCATCH, "abort");

/*
 *  The break loop variables.  Both edit_loop and lisp_loop
 *  are expected to set break_loop to some routine which
 *  handles break loops appropriately.  The routines cont
 *  and reset are defined below and work with any break
 *  loop type.
 */
int		break_level;		/* counts recursive break loops */
int		break_return;		/* function cont was called */
int		break_cancont;		/* can continue break loop */

long	debug_states;			/* current debugging states */

#define MAXFARGS	1000		/* maximum command line files */

int	nosetupfile = FALSE;		/* inhibit startup file reading */

main(argc, argv)
	char	*argv[];
{
	extern char	*rindex(), *getenv();
	char		*ap;
	int		filec;
	char		*filev[MAXFARGS];
	char		*display = NULL,
			*fontname = NULL,
			*geometry = NULL;
	char		*initfile = NULL;
	int		isvisual;

	/*
	 *  Get program name from argv[0].  We'd still like to
	 *  be able to run the lisp interpreter without the
	 *  editor, so if the program name is "vlisp" or we're
	 *  given the -l option, we don't do the X editor.
	 */
	program = rindex(*argv, '/');
	if (program == NULL)
		program = *argv;
	else
		program++;
	if (!strcmp(program, "vlisp"))
		isvisual = FALSE;
	else
		isvisual = TRUE;

	/*
	 *  Do an initial argument processing for the basic program
	 *  and for the window system.  We extract option arguments,
	 *  which we'll handle, and window system arguments.  For
	 *  now we handle the X display and geometry arguments here.
	 *  A display argument is one that contains a colon and a
	 *  geometry argument is one that begins with an equals sign.
	 */
	filec = 0;
	while (--argc > 0 && *++argv != NULL) {
		if (**argv == '-' && strcmp(*argv, "-")) {
			for (ap = ++*argv; *ap != '\0'; ap++)
				switch (*ap) {
				case 'l':	/* lisp interpreter */
					isvisual = FALSE;
					break;
				case 'v':	/* visual editor */
					isvisual = TRUE;
					break;
				case 'i':	/* user's init file */
					if (--argc < 1 || *++argv == NULL) {
						fprintf(stderr,
			"%s: Need a file name argument for the -i option!\n",
							program);
						exit(1);
					}
					initfile = *argv;
					break;
				case 'f':	/* X font name */
					if (--argc < 1 || *++argv == NULL) {
						fprintf(stderr,
			"%s: Need an X font argument for the -f option!\n",
							program);
						exit(1);
					}
					fontname = *argv;
					break;
#ifdef DEBUG
				case 'd':	/* debug state */
					if (--argc < 1 || *++argv == NULL) {
						fprintf(stderr,
			"%s: Need a debug state argument for the -d option!\n",
							program);
						exit(1);
					}
					int_debug(*argv);
					break;
#endif DEBUG
				case 'S':	/* no setup file */
					nosetupfile = TRUE;
					break;
				default:	/* bad option */
					fprintf(stderr, USAGE, program);
					exit(1);
				}
		} else if (isvisual && rindex(*argv, ':') != NULL) {
			if (display != NULL) {
				fprintf(stderr,
		"%s: Multiple X displays specified on the command line!?!\n",
					program);
				exit(1);
			}
			display = *argv;
		} else if (isvisual && **argv == '=') {
			if (geometry != NULL) {
				fprintf(stderr,
		"%s: Multiple X geometries specified on the command line!?!\n",
					program);
				exit(1);
			}
			geometry = *argv;
		} else {
			if (filec >= MAXFARGS) {
				fprintf(stderr,
	"%s: Too many files given on the command line; only first %d used!\n",
					program, MAXFARGS);
				break;
			}
			filev[filec++] = *argv;
		}
	}

#ifdef DEBUG
	if (isvisual) {
		extern char	*XGetDefault();
		char		*val;

		if ((val = XGetDefault(program, "DebugStates")) != NULL) {
			int_debug(val);
			free(val);
		}
	}
#endif DEBUG

	/*
	 *  Initialize everything for the lisp interpreter.
	 *  This needs to be done both for the standalone lisp
	 *  interpreter and the whole editor, including the
	 *  lisp system.  We pass in the partial argument list,
	 *  containing only the non-option and non-window system
	 *  specific arguments, which should leave file names
	 *  to edit.
	 */
	if (!initialized && initvlisp(filec, filev, initfile) != 0) {
		fprintf(stderr, "%s: Can't initialize the lisp interpreter!\n",
			program);
		exit(1);
	}

	/*
	 *  Make the connection to the X server and establish the
	 *  initial window.  This call will fail if any of a number
	 *  of things is wrong, so the error message we print here
	 *  is just a summary, a more specific one will already have
	 *  been printed.
	 */
	if (isvisual && initwindows(display, fontname, geometry) != 0) {
		fprintf(stderr, "%s: Can't initialize X window system!\n",
			program);
		exit(1);
	}

	/*
	 *  Get terminal characters and modes for sub-processes and
	 *  their ptys.  We don't care if this fails, since we have
	 *  a set of default characters/modes anyway.
	 */
	getmodes();

	/*
	 *  Now run the actual read/eval/print loop as appropriate.
	 *  If we're a visual editor, this is modified to reading
	 *  keystrokes and looking up their bindings, and executing
	 *  the corresponding functions as appropriate.  These functions
	 *  should never return; the user should call exit-vortex when
	 *  he is ready to quit (or type EOF if using just the lisp
	 *  interpreter).
	 */
	initialized = TRUE;
	quitting = FALSE;

	if (havedisplay) {
		/* run the visual editor forever */
		edit_loop();
	} else {
		/* run the lisp system until EOF */
		lisp_loop();
	}
	ASSERT(FALSE);

	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: cont
 *  Call: (cont [ 'any ])
 *  Desc: This function is only available inside a break loop.
 *	If the error which called the break loop is continuable
 *	(initiated with \sym{cerror} rather than \sym{error}),
 *	then the argument to \sym{cont} (or nil if no argument
 *	was given) becomes the return value of the call to
 *	\sym{cerror] which generated this error.
 *
 *	If the break loop is not continuable, or one is not in
 *	a break loop, this function will not work, but will
 *	abort with an error message.  The other way to end a
 *	a break loop is \sym{reset}, which aborts up to the
 *	top level.
 *  Side: This function never returns, its side effect is to
 *	pass control back to the \sym{cerror} in such a manner that
 *	it appears that the call to \sym{cerror} returns the
 *	argument to \sym{cont}.
 *  SeeA: reset cerror error
 */

DEFUN(docont, "cont", FLAG_NONE, NULL)
{
	struct value	ret;

	if (break_level <= 0)
		error("Not currently in a break loop!");
	if (!break_cancont)
		error("This break loop is not continuable!");

	CHECKAC(0, 1);
	if (GETACOUNT() > 0)
		ret = EVALARGN(1);
	else
		ret = v_nil;
	break_return = TRUE;
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: reset
 *  Call: (reset)
 *  Desc: This function never returns, its side effect is to
 *	pass control back to the top level.  This is equivalent
 *	to evaluating \lit{(throw 'top-level)}.  All evaluation
 *	is abandoned and control is returned to to the editor.
 *
 *	This function is usually called when in a break loop to
 *	return to the normal lisp read/eval/print loop.
 *  SeeA: cont cerror error
 */

DEFUN(doreset, "reset", FLAG_NONE, NULL)
{
	CHECKAC(0, 0);

	int_throw(TOPLEVCATCH, v_t);
	/* NOTREACHED */
}
