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
 *  RCS Info: $Header: dogetenv.c,v 0.1 87/05/01 11:49:46 john Locked $
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
 *  dogetenv.c - get and set environment variables
 */
static char _ID[] = "@(#)dogetenv.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: getenv
 *  Call: (getenv 'name)
 *  Retu: any
 *  Desc: This function extracts a variable from the environment
 *	inherited from the parent process and returns it as a
 *	lisp string.  The argument should evaluate to a string or
 *	symbol which will be matched against the environment
 *	variable names.  If the variable doesn't exist, nil is
 *	returned.
 *  SeeA: setenv
 */

DEFUN(dogetenv, "getenv", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	extern char	*getenv();
	char		*env, cname[128];
	struct string	*name, *str;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (stringp(arg))
		name = gstring(arg.vl_data);
	else if (symbolp(arg))
		name = gsymbol(arg.vl_data)->sy_pname;
	else
		BADARGN(1, "a string variable name");

	/* get the environment variable */
	makecstring(name, cname, sizeof (cname));
	if ((env = getenv(cname)) == NULL)
		return (v_nil);
	str = save_string(env, strlen(env));

	/* return the value as a string */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, str);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: setenv
 *  Call: (setenv 'name 'value)
 *  Retu: t
 *  Desc: This function sets the named environment variable to the
 *	string value given.  This environment is inherited by child
 *	processes, but otherwise has no effect.  The first argument
 *	must evaluate to a string or symbol naming the environment
 *	variable and the second to the value to which it should be
 *	set.
 *
 *	If the second argument is given as nil, the specified
 *	environment variable is removed from the environment
 *	altogether.
 *  Side: Future calls to \sym{getenv} will return the changed value
 *	of this variable and child processes will inherit the changed
 *	value, but the environment will not be changed in the parent
 *	\ux{} process.
 *  SeeA: getenv exec
 */

DEFUN(dosetenv, "setenv", FLAG_NONE, NULL)
{
	struct value	arg;
	extern char	*getenv();
	char		cname[128], cvalue[1024];
	struct string	*name, *value;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	if (stringp(arg))
		name = gstring(arg.vl_data);
	else if (symbolp(arg))
		name = gsymbol(arg.vl_data)->sy_pname;
	else
		BADARGN(1, "a string variable name");
	arg = EVALARGN(2);
	if (stringp(arg))
		value = gstring(arg.vl_data);
	else if (nullp(arg))
		value = NULL;
	else
		BADARGN(2, "a string value");

	/* get the environment variable name and value */
	makecstring(name, cname, sizeof (cname));
	if (value == NULL) {
		/* delete this environment variable */
		setenv(cname, NULL);
	} else {
		makecstring(value, cvalue, sizeof (cvalue));
		setenv(cname, cvalue);
	}

	return (v_t);
}

extern char	**environ;		/* global UNIX environment pointer */
static char	**my_environ = NULL;	/* environment storage we allocated */
static int	my_envlen = 0;		/* length of allocated environment */

setenv(name, value)
	char	*name, *value;
{
	return (0);
}
