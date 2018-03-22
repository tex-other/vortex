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
 *  RCS Info: $Header: function.h,v 0.1 87/04/30 20:54:02 john Locked $
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
 *  function.h - vLisp function data structures
 */
 
#ifndef _FUNCTION_
#define _FUNCTION_

#include "value.h"

/*
 *  The stack structure is the description of one vlisp call
 *  frame.  The current call frame is the currently executing
 *  vlisp function, even for builtin functions.
 */
struct stack {
	struct value	st_value;	/* pre-evaluated function */
	struct value	st_funct;	/* function value, arg0 */
	unsigned short	st_flags;	/* frame flags */
	short		st_argc;	/* argument count */
	struct value	st_argl;	/* list of arguments */
	struct value	st_return;	/* return value of function */
	struct value	(*st_unwind)();	/* C function to call when unwinding */
};

#define STACK_ISCATCH	(1 << 0)	/* there is a catch at this frame */
#define STACK_UNWIND	(1 << 1)	/* there is a unwind-protect here */

extern struct stack	*stack_ptr;	/* current frame pointer */
extern struct stack	*stack_stack;	/* call frame stack (bottom) */
extern struct stack	*stack_top;	/* call frame stack limit */
#define call_depth	(stack_ptr - stack_stack)

/*
 *  These are the constants that control the call frame stack
 *  management routines.  When we push a frame, we check to make
 *  sure there is room under the current stack_top.  If not, we
 *  add another STACK_GROWTH frames to the frame stack (they are
 *  not freed later), up the STACK_MAXDEPTH, where an error will
 *  occur.
 */
#define STACK_INITIAL	1000		/* initial stack depth allocated */
#define STACK_GROWTH	1000		/* grow stack in these increments */
#define STACK_MAXDEPTH	10000		/* maximum depth of call stack */

/*
 *  The types of arguments that may be specified.  Each of these
 *  argument types is read a certain way when the functions is
 *  called interactively.  When defining a function, one specifies
 *  the interactive calling sequence by argument types letters and
 *  prompts.  The argument type letters are listed below.  For more
 *  information on interactive argument strings, see the documentation
 *  on the function interactive.
 */

#define ARG_NONE		'\0'	/* no arguments at all */
#define ARG_LISPTYPE		'l'	/* read as a lisp value */
#define ARG_NUMBER		'n'	/* argument must be a fixnum */
#define ARG_PREFIXNUM		'N'	/* prefix or read fixnum */
#define ARG_SYMBOL		'S'	/* a symbol (from text string) */
#define ARG_STRING		's'	/* a string (in double quotes) */
#define ARG_FUNCTION		'c'	/* a lisp function name */
#define ARG_EFUNCTION		'C'	/* an existing function name only */
#define ARG_COMMAND		'I'	/* a command (interactive function) */
#define ARG_VARIABLE		'v'	/* a lisp variable name */
#define ARG_EVARIABLE		'V'	/* an existing variable only */
#define ARG_FILENAME		'f'	/* a UNIX file name, with ~ expanded */
#define ARG_EFILENAME		'F'	/* an existing file name only */
#define ARG_DIRECTORY		'd'	/* an existing directory name only */
#define ARG_BUFFER		'b'	/* a possible buffer name */
#define ARG_EBUFFER		'B'	/* an existing buffer name only */
#define ARG_KEYS		'k'	/* a key sequence */
#define ARG_BOUNDTO		'K'	/* key sequence which invoked it */
#define ARG_PREFIX		'p'	/* the command prefix argument */
#define ARG_RAWPREFIX		'P'	/* the command prefix, in raw form */
#define ARG_POINTPOS		'.'	/* the current buffer's point */
#define ARG_MARKPOS		'm'	/* the current buffer's mark */
#define ARG_REGION		'r'	/* the current region in order */
#define ARG_START		'^'	/* start of the buffer */
#define ARG_LENGTH		'$'	/* the length of the buffer */
#define ARG_CURWIN		'w'	/* current window number */
#define ARG_XPOS		'x'	/* reasonable X locator position */
#define ARG_YPOS		'y'	/* reasonable Y locator position */
#define ARG_XRAWPOS		'X'	/* raw X locator position */
#define ARG_YRAWPOS		'Y'	/* raw Y locator position */
#define ARG_DOCUMENT		'D'	/* the latest document read */

/*
 *  The function struct holds the description of a function and
 *  is referred to when a function is called.  If the function is
 *  written in C, fn_funct will be a function pointer to a C routine;
 *  if not, fn_body points to a list that makes up the body of the
 *  function.  The function structs are kept in a table from which
 *  they are allocated as needed.
 */
struct function {
	unsigned short	fn_mark : 1,	/* garbage collection mark */
			fn_free : 1,	/* this function is available */
			fn_flags : 14;	/* various flags (see below) */
	short		fn_disc;	/* function discipline */
	short		fn_argc;	/* argument count (size of fn_alist) */
	short		fn_iargc;	/* number of interactive args */
	struct string	*fn_alist[20];	/* local argument name list */
	struct value	(*fn_funct)();	/* C function to call */
	struct value	fn_body;	/* body of (user) function */
	unsigned char	fn_atype[12];	/* types of interactive args */
	struct string	*fn_prompt[12];	/* prompts for interactive args */
	struct string	*fn_pname;	/* print name of function */
};
#define MAXARGC		20	/* max arguments; size of fn_alist */
#define MAXIARGS	12	/* max interactive args; size of fn_atype */

#define FUNC_BUILTIN	(1 << 0)	/* builtin function */
#define FUNC_INTERACT	(1 << 1)	/* can be called interactively */
#define FUNC_MACRO	(1 << 2)	/* this is a macro function */
#define FUNC_PREFIX	(1 << 3)	/* this is a prefix command */
#define FUNC_IGNORE	(1 << 4)	/* don't put this call on history */
#define FUNC_NOINT	(1 << 5)	/* this can't be interrupted */
#define	FUNC_VISUAL	(1 << 6)	/* function requires visual mode */

#define DISC_LAMBDA	1		/* lambda (expr) discipline */
#define DISC_NLAMBDA	2		/* nlambda (fexpr) discipline */
#define DISC_LEXPR	3		/* lexpr discipline */
#define DISC_MACRO	4		/* macro discipline */

/*
 *  The function call_function interprets any type of function
 *  and sets up the frame for a new function of any type.  Then,
 *  it calls one of the special functions, call_<discipline> if
 *  the function is written in vlisp, or the C function of it's
 *  a builtin.
 */
extern struct value	call_function();
extern struct value	call_lambda(), call_nlambda(),
			call_lexpr(), call_macro();

/*
 *  The make_<discipline> function return a function structure
 *  built given the argumens and body lists.  These are used
 *  internally by defun and the different discipline form
 *  functions.
 */
extern struct function	*make_lambda(), *make_nlambda(),
			*make_lexpr(), *make_macro();

#endif !_FUNCTION_
