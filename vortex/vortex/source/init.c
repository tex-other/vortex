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
 *  RCS Info: $Header: init.c,v 0.1 87/05/01 12:16:31 john Locked $
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
 *  init.c - overall vLisp and editor initialization routine
 */
static char _ID[] = "@(#)init.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"

static char	VORTEXRC[] = "~/.vortexrc";
static char	VLISPINIT[] = "startup.vl";

static char	ENVNAME[] = "VORTEXLOADPATH";
static char	LOADPATH[] = ".:~/lib/vlisp:/yew2/vortex/vlisp:/usr/local/lib/vortex/vlisp";

/*
 *  DOCUMENTATION
 *
 *  Name: load-path
 *  Desc: This variable should be set to a list which contains
 *	strings which are the directories to search in for a
 *	file to be \sym{load}ed.
 *
 *	At startup, this variable is inititalized from the
 *	environment variable VORTEXLOADPATH, if it exists,
 *	otherwise it has a system dependent default value.
 *  SeeA: load
 */
MKSTRING(LOADPATH_NAME, "load-path");

/*
 *  DOCUMENTATION
 *
 *  Name: program-name
 *  Desc: This variable is set at startup the the name by
 *	which the editor was invoked.  It will be bound globally
 *	to a positive-length string.
 *  Side: Changing this variable has no effect on the system, it
 *	is provided for the user's convenience.
 *  SeeA: program-args
 */
MKSTRING(ARGZERO_NAME, "program-name");

/*
 *  DOCUMENTATION
 *
 *  Name: program-args
 *  Desc: This variable is set at startup to the command line
 *	arguments given to VorTeX the program.  These arguments
 *	are not interpreted by the program itself, but rather
 *	by the vlisp startup code.  Usually this is done by the
 *	system, unknoticed by the user, but he may substitute
 *	his own argument processing.
 *
 *	Each command line argument becomes a string in the
 *	list of all arguments.  The list may be nil if no
 *	arguments were given on the command line.
 *  Side: Setting this variable has effect only if the standard
 *	vlisp startup is called after it has been changed.
 *  SeeA: program-name
 */
MKSTRING(ARGLIST_NAME, "program-args");

static int	strings_done = FALSE;
static int	values_done = FALSE;

initvlisp(argc, argv, rcfile)
	char	*argv[], *rcfile;
{
	extern char	*getenv();
	extern char	*fixpath(), *findlpath();
	struct value	pathtolist();
	extern int	intr_catcher();
	extern char	*program;
	extern int	nosetupfile;
	char		*path;
	struct string	*str;
	struct value	val, alist;

	/* make globally-used standard values, like nil and t */
	if (standard_values() != 0)
		return (-1);

	/*
	 *  These two must come before any calls to any part of
	 *  the lisp interpreter.  These two functions intern the
	 *  strings needed internally in the string symbol storage
	 *  and initialize the builtin values function use--things
	 *  that can't be done at compile time.
	 */
	if (!strings_done) {
		builtin_strings();
		strings_done = TRUE;
	}
	if (!values_done) {
		builtin_values();
		values_done = TRUE;
	}

	/* insert builtin symbols into symbol table */
	if (initsymbols() != 0)
		return (-1);

	/* initialize directory stack and channels list */
	if (initdirs() != 0 || initchannels() != 0)
		return (-1);

	if (initprocess() != 0)
		return (-1);

	if (initbuffers() != 0)
		return (-1);

	/* create program name from argv[0] */
	str = save_string(program, strlen(program));
	val.vl_type = LISP_STRING;
	sstring(val.vl_data, str);
	setglobal(ARGZERO_NAME, val, FLAG_NONE);

	/* create argument list variable */
	alist = v_nil;
	val.vl_type = LISP_STRING;
	while (--argc >= 0) {
		str = save_string(argv[argc], strlen(argv[argc]));
		sstring(val.vl_data, str);
		alist = cons(val, alist);
	}
	setglobal(ARGLIST_NAME, alist, FLAG_NONE);

	/* create the load path variable */
	if ((path = getenv(ENVNAME)) == NULL)
		path = LOADPATH;
	val = pathtolist(path);
	setglobal(LOADPATH_NAME, val, FLAG_NONE);

	if (!nosetupfile) {
		/* read in the system bootstrap lisp code */
		path = findlpath(VLISPINIT, val);
		if (path == NULL)
			error("Can't find initialization file %s!", VLISPINIT);
		else
			loadfile(path, TRUE);

		/* read in the user's .vortexrc if there is one */
		if (rcfile == NULL || *rcfile == '\0') {
			path = fixpath(VORTEXRC);
			if (access(path, F_OK) == 0)
				loadfile(path, TRUE);
		} else {
			/* read in the given rcfile */
			path = fixpath(rcfile);
			loadfile(path, TRUE);
		}
	}

	return (0);
}
