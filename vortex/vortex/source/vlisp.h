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
 *  RCS Info: $Header: vlisp.h,v 0.1 87/04/30 20:56:23 john Locked $
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
 *  vlisp.h - vLisp general include file (for functions)
 */
 
#ifndef _VLISP_
#define _VLISP_

/*
 *  These include files are needed by the defined macros below.
 *  They may be explicitly included in the C source files which
 *  also include this file, but all header files are protected
 *  against multiple inclusion.  This file should be included
 *  by all C files which contain definitions of vlisp built-in
 *  symbols.
 */
#include "value.h"
#include "function.h"
#include "symtab.h"

/*
 *  These macros generate C code that defines values and
 *  symbols for built-in objects.  These macros should be the
 *  only programatic means to create new vlisp values or
 *  symbol bindings.  There are several levels of functions
 *  here; the macros DEFUN, DEFSAME and DEFVAR build lisp
 *  values and insert them into the symbol table automatically
 *  at run time.  These macros generate C code and macros for
 *  the exsyms preprocessor, which removes them.
 *
 *  We do this in two passes.  One through just the C preprocessor
 *  to reduce constants to numbers and interpolate include files.
 *  Then, through the exsyms preprocessor, which extracts the
 *  macro entries named above, and comments out the # lines left
 *  in by the first pass.  Then, we go back with the full C
 *  compiler and finish things up.
 *
 *  The macros DEFVALUE and DEFSYMBOL are unknown by either pass
 *  of the C preprocessor, they are passed on to exsyms, which
 *  removes them and does it's own thing.  DEFVALUE signals it
 *  to generate code which initializes the (struct value) it
 *  defines elsewhere.  If one wants to access this value, it
 *  must be explicitly declared as an extern.  DEFSYMBOL takes
 *  string name, symbol flags, and the name of a C symbol which
 *  is an (extern) value struct.  It causes C code to be generated
 *  which will insert a new symbol into the vlisp symbol table.
 *
 *  DEFUN(cname, lname, flags, iargs)
 *  This macro delcares a new vlisp builtin function, named lname
 *  in the symbol table.  The function itself will be static with
 *  the C symbol cname.  The function flags given by the third argument
 *  will be set and iargs describes the interactive argument
 *  specification.
 *
 *  DEFSAME(cname, lname)
 *  Enter a previously defined function into the symbol table under
 *  a new name.  This must follow the DEFUN it is attached to in
 *  the same source file.  Cname is as given to the original DEFUN,
 *  lname is the new symbol name.
 *
 *  DEFVAR(lname, cvalue, flags)
 *  Enter a builtin variable into the symbol table under the name
 *  lname, with value cvalue (which must be the C symbol name of
 *  a value struct) with symbol flags set as in the third argument.
 */
		
#define DEFUN(cname,lname,flags,iargs)					\
	struct value cname();						\
	static struct function bfn_/**/cname = {			\
		0, FALSE, flags|FUNC_BUILTIN,				\
		DISC_NLAMBDA, 1, 0, { NULL },				\
		cname, { 0, 0, 0 },					\
		{ ARG_NONE }, { (struct string *)iargs },		\
		NULL							\
	};								\
	unsigned long bdata_/**/cname = (unsigned long)&bfn_/**/cname;	\
	DEFVALUE(bvl_/**/cname,LISP_FUNC,bdata_/**/cname)		\
	DEFSYMBOL(lname,FLAG_NONE,bvl_/**/cname)			\
	static struct value cname()

#define DEFSAME(cname,lname)						\
	DEFSYMBOL(lname,FLAG_NONE,bvl_/**/cname)			\

#define DEFVAR(lname,cval,flags)					\
	DEFSYMBOL(lname,flags,cval)

/*
 *  These macros create (struct string *) declarations for
 *  initializing global variables at compile time.  MKSTRING
 *  works with a C string (typed with double quotes) and figures
 *  out the length automatically.  MKSTRLEN takes the length
 *  of string string in case the string is NULL or a variable.
 *  Note that MKSTRLEN is expanded in place, but MKSTRING is
 *  initialized at startup in standard lisp storage.  Note
 *  that both these macros must be terminated with a semicolon.
 */
#define MKSTRLEN(n,s,l)	\
	static struct string	bst_/**/n = { 0, 1, (l), \
					      (unsigned char *)(s) }; \
	struct string	*n = &bst_/**/n
#define MKSTRING(n,s)	\
	DEFSTRING(n, sizeof (s) - 1, s) \
	struct string	*n = NULL

/*
 *  Here are macros useful for processing arguments and returning
 *  values that can be used inside a lisp function.  These macros
 *  assume that the current call frame is set up properly, which it
 *  must for the interpreter to be doing things properly.  Note
 *  that these macros, at least RETURN(), must be used by authors
 *  of vlisp builtin functions, or chaos will result.  They also
 *  hide the form of the call frame, so if used consistently, make
 *  this data structure transparent.
 *
 *  Most of these functions just allow one to access the arguments
 *  in a reasonably transparent way with respect to the internal
 *  function calling sequence implementation.  These functions should
 *  be used, instead of directly referencing the call frame values, so
 *  that modifications to this data structure don't break anything
 *  else.
 *
 *  GETFNAME()
 *  This macro expands to a call to the (char *) function pquick()
 *  with the value in st_value to return the original ``name'' by
 *  which the function was called.  This is almost always an atom,
 *  but may also be a list (function call).
 *
 *  GETALIST()
 *  This macro expands to the list of arguments in the nlambda
 *  form required by all builtin functions.  All the arguments
 *  are given in this list, in order, and none are evaluated.
 *  see GETARGN and EVALARGN below for more specific means of
 *  extracting arguments.
 *
 *  GETACOUNT()
 *  This macro expands to the number of argument the function was
 *  called with (the length of the nlambda argument list).
 *
 *  CHECKAC(min, max)
 *  This macro calls a function which makes sure that the number
 *  of arguments given falls in the range min to max.  If not,
 *  an appropriate error messages is printed.  If there is no
 *  maximum number of arguments, max can bi given as -1 (min should
 *  not be less than zero).  Min and max can be the same, in which
 *  case exactly that many arguments must be given.
 *
 *  GETARGN(n)
 *  This macro expands to a function which makes sure that this
 *  numbered arguments exists and returns it, unevaluated.  If
 *  fewer arguments than this exist, an error occurs.
 *
 *  EVALARGN(n)
 *  This macro expands to a function which checks that the
 *  requested argument exists and evaluate it and returns the
 *  result.  If too few arguments exist or the argument fails
 *  evaluation, an error occurs.
 *
 *  BADARGN(n, what)
 *  This macro expands to a function which prints an error about
 *  the nth argument being of the wrong type.  The correct type,
 *  complete with the appropriate article, should be given as
 *  what.  An example is BADARGN(2, "a channel").
 *
 *  GETLOCALS()
 *  This macro expands to the list of local bindings of the current
 *  call frame.  This list should not be modified except through
 *  SETLOCALS or ADDLOCAL, see below.
 *
 *  SETLOCALS(list)
 *  This macro set the list of call frame local bindings to the
 *  linked list of (struct local *) that is its argument.  This
 *  should be used with extreme vcaution, since this is used to
 *  restore outer bindings when a function returns.
 *
 *  ADDLOCAL(local)
 *  This macro expands to a function which adds the given local
 *  binding, a (struct local *), to the end of the local binding
 *  list in the current call frame.
 *
 *  RETURN(value)
 *  THis macro expands to code that sets up the given return value
 *  in the current call frame and returns from the C function.
 *  THis should be the only means of returning from a builtin vlisp
 *  function coded in C.  If an error occurs, error will abort to
 *  somewhere else, otherwise a non-NULL value should be returned.
 */
extern struct value	getntharg(), evalntharg();
extern char		*pquick();

#define GETFNAME()	pquick(stack_ptr->st_value)
#define GETFUNCT()	(stack_ptr->st_funct);
#define GETALIST()	(stack_ptr->st_argl)
#define GETACOUNT()	(stack_ptr->st_argc)
#define CHECKAC(l,m)	checkacount(GETFNAME(), GETACOUNT(), (l), (m))
#define GETARGN(n)	getntharg(GETFNAME(), GETALIST(), (n))
#define EVALARGN(n)	evalntharg(GETFNAME(), GETALIST(), (n))
#define BADARGN(n,s)	badntharg(GETFNAME(), (n), (s))

#endif !_VLISP_
