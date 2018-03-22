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
 *  RCS Info: $Header: dointeract.c,v 0.1 87/05/01 11:50:36 john Locked $
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
 *  dointeract.c - vLisp interactive function and internal routines
 */
static char _ID[] = "@(#)dointeract.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: interactive
 *  Call: (interactive [ 'string ... ])
 *  Retu: nil
 *  Desc: This function, when called, does nothing at all, and just
 *	returns nil.  It is used as a declaration in vlisp functions
 *	of they way the function is to be called interactively.  When
 *	a function is created with \sym{defun} or a macro is created
 *	with \sym{defmacro}, and the first expression in the body is
 *	a call to this function, the string arguments are interpreted
 *	as the argument sequence for interactive invocation.
 *
 *	When a function is called interactively (either through being
 *	bound to a key or through \sym{extended-command}), it must
 *	obtain its arguments by prompting in the \sym{minibuf}, if any
 *	are required, instead of through the normal lisp mechanisms.
 *	Many functions cannot be called interactively at all.  A call
 *	to \sym{interactive} must appear as the first command for a
 *	user defined vlisp function to be callable from the keyboard.
 *	The optionsl string arguments to \sym{interactive} specify how
 *	many and how these arguments are to be read.  The arguments given
 *	here must match the definition of the function form, of course.
 *
 *	The form of the arguments in the strings is as follows.  Each
 *	argument specification corresponds to a string argument.
 *	Each argument specification itself is a single character, which
 *	specifies the type of argument, and an optional prompt string,
 *	immediately concatenated to the type character.  Some types of
 *	arguments, such as ``prefix arguments'', don't require input to
 *	be read from the keyboard, so no prompt is necessary.  Most,
 *	however, do require prompts.  In the list of argument type
 *	characters below, those marked with an asterisk enable
 *	completion when being entered, this marked with two asterisks
 *	force the user to enter one of a list of possible items
 *	(exact match completion only).
 *
 *	\tab{l	read as a lisp s-expression
 *	n	must be a fixnum
 *	N	prefix as a number or read as \lit{n}
 *	s	a string (in double quotes)
 *	S	a symbol (from typed text string)
 *	c*	a lisp function name
 *	C**	an existing function name only
 *	I**	a command (interactive function)
 *	v*	a possible lisp variable name
 *	V**	an existing variable only
 *	f*	a possible file name, with ~ expanded
 *	F**	an existing file name only
 *	d**	an existing directory name only
 *	b*	a possible buffer name
 *	B**	an existing buffer name only
 *	k	a key sequence which could be bound is expected
 *	K	the key sequence which invoked this command
 *	p	the prefix argument as a fixnum (no prompt necessary)
 *	P	the prefix argument in ``raw form'' (no prompt)
 *	.	point in current buffer as a fixnum (no prompt)
 *	m	mark in current buffer as a fixnum (no prompt)
 *	r	current region as two fixnums in order (no prompt)
 *	^	beginning of buffer (start position) as fixnum (no prompt)
 *	$	end of bufer (length) as fixnum (no prompt)
 *	w	the current window id (no prompt)
 *	X	X position of locator on window (-1 if not in window)
 *	Y	Y position of locator on window (-1 if not in window)
 *	x	X position of locator, or prompt if not in window
 *	y	Y position of locator, or prompt if not in window}
 *	
 *
 *	The reason there are so many different types of arguments
 *	possible is that in many cases (such as file and buffer names)
 *	completion can be done on the arguments, making them easier for
 *	the user type enter.  Also, by specifying specific lisp types
 *	for arguments, the lisp programmer can have the interpreter
 *	do his argument checking for him.
 *
 *	An example to show how argument prompting works is the
 *	interactive arguments for the command \sym{write-region}:
 *
 *	\lit{(interactive "r" "fWrite to file: ")}
 *
 *	The first argument here form the current region, point
 *	and mark as two fixnums, representing the minimum and maximim
 *	character positions of interest.  The second argument is the
 *	file name to write to, which is prompted for with the string
 *	\lit{"Write to file: "} in the \sym{minibuf} with completion
 *	on file names.
 *  Side: This function is a side effect.  When a function is being
 *	created with \sym{defun}, or a macro with \sym{defmacro}, the
 *	arguments to the optional interactive arguments are processed.
 *	When it is actually called during execution of the function,
 *	\sym{interactive} just returns nil.
 *  SeeA: extended-command minibuf minibuf-complete defun defmacro
 */
static char	POSSIBLE[] = "lnNascCIvVfFdbBkKpP.mr^$wXYxy";

DEFUN(dointeractive, "interactive", FLAG_NONE, "")
{
	return (v_nil);
}


/*
 *  Here is the routine to parse an interactive statement
 *  as given in a vlisp function body.  The argument to this
 *  function should be the whole expression which is the
 *  call to the function interactive.
 */

parse_istmt(expr, func)
	struct value	expr;
	struct function	*func;
{
	extern char	*index();
	struct value	arg;
	struct string	*str;
	register int	argc, count;

	if (!listp(expr))
		return (1);
	debug(DREAD, "Parsing interactive statement %v for %Y...",
	      expr, func->fn_pname);

	/* scan out individual argument specifications */
	count = length(expr) - 1;
	if (count > MAXIARGS)
		error("No more than %d interactive arguments.", MAXIARGS);
	for (argc = 0; argc < count; argc++) {
		arg = nth(argc + 1, expr);
		if (!stringp(arg))
			error("All arguments of interactive must be strings!");
		str = gstring(arg.vl_data);
		if (str->st_length < 1)
			error("Arguments to interactive can't be null!");
		if (index(POSSIBLE, *str->st_buffer) == NULL)
			error("Interactive argument type `%C' unknown!",
			      (int)*str->st_buffer);
		func->fn_atype[argc] = *str->st_buffer;
		if (str->st_length > 1) {
			func->fn_prompt[argc] =
			   save_string(str->st_buffer + 1, str->st_length - 1);
		} else {
			/* no prompt string at all */
			func->fn_prompt[argc] = NULL;
		}
	}
	func->fn_iargc = argc;
	func->fn_flags |= FUNC_INTERACT;

	return (0);
}

/*
 *  Build the relevant fields of a function struct from an
 *  interactive argument specification as given in a builtin
 *  function or as the argument to interactive.  This is
 *  used for builtin functions, the interactive argument
 *  specifications are in one C string, separted by newlines
 *  rather than in separate lisp strings.
 */

parse_istring(istring, func)
	char		*istring;
	struct function	*func;
{
	register int	argc;
	register char	*bp, *ep;

	if (istring == NULL)
		return (1);

	/* scan out individual argument specifications */
	bp = istring;
	for (argc = 0; *bp != '\0' && argc < MAXIARGS; argc++) {
		for (ep = bp; *ep != '\0' && *ep != '\n'; ep++)
			;
		func->fn_atype[argc] = *bp;
		bp++;
		if (*bp != '\0' && *bp != '\n')
			func->fn_prompt[argc] = save_string(bp, ep - bp);
		else
			func->fn_prompt[argc] = NULL;
		if (*ep != '\0')
			ep++;
		bp = ep;
	}
	func->fn_iargc = argc;
	func->fn_flags |= FUNC_INTERACT;

	return (0);
}
