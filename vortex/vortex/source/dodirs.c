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
 *  RCS Info: $Header: dodirs.c,v 0.1 87/05/01 11:42:42 john Locked $
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
 *  dodirs.c - directory list manipulation functions
 */
static char _ID[] = "@(#)dodirs.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include <sys/param.h>
#include "vse.h"
#include "vlisp.h"
#include "dirs.h"

static struct dirlist	*directories = NULL;
char			currentdir[MAXPATHLEN+1];

struct dirlist	*change(), *dalloc();
struct value	vdirs();

initdirs()
{
	extern char	*getwd();

	/* get the full pathname of the current directory */
	if (getwd(currentdir) == NULL)
		strcpy(currentdir, ".");

	/* make the head of the list the current directory */
	directories = dalloc(currentdir);

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: dirs
 *  Call: (dirs)
 *  Retu: list
 *  Desc: This function returns a list of the current directory
 *	stack.  The \sym{car} is the current directory.  This list
 *	always contains at least one entry; \sym{pushd} makes the
 *	stack deeper, \sym{popd} makes it shallower, and \sym{cd}
 *	just changes the top element.
 *  SeeA: cd pushd popd
 */

DEFUN(dodirs, "dirs", FLAG_NONE, NULL)
{
	CHECKAC(0, 0);
	return vdirs();
}

/*
 *  DOCUMENTATION
 *
 *  Name: pushd
 *  Call: (pushd [ 'string ])
 *  Retu: list
 *  Desc: This function pushes another directory onto the stack
 *	or exchanges the top two elements of the directory stack.
 *	If an argument is present, it must evaluate to a string
 *	which contains the name of a valid directory to change to.
 *	This directory is then inserted into the directory stack
 *	and the current working directory is changed to there.
 *	If no argumens are present, the top two elements of the
 *	directory stack are exchanged, switching the current
 *	working directory to new top element.
 *  SeeA: cd popd dirs
 */

DEFUN(dopushd, "pushd", FLAG_NONE, "dPush to: ")
{
	struct dirlist	*first, *second, *rest, *dp, *last;
	struct value	arg;
	register int	number, count;
	struct string	*str;
	char		dname[MAXPATHLEN], *dir;

	CHECKAC(0, 1);
	if (GETACOUNT() == 0) {
		ASSERT(directories != NULL);
		if (directories->di_next == NULL)
			error("No other directory to swap with current.");
		second = directories->di_next;
		rest = second->di_next;
		first = change(second->di_path);
		first->di_next = directories;
		directories->di_next = rest;
		directories = first;
		dfree(second);
	} else {
		arg = EVALARGN(1);
		dir = NULL;
		switch (arg.vl_type) {
		case LISP_SYMBOL:
			str = gsymbol(arg.vl_data)->sy_pname;
			makecstring(str, dname, sizeof (dname));
			if (dname[0] == '+' && isdigit(dname[1]))
				number = atoi(dname + 1);
			else
				dir = dname;
			break;
		case LISP_STRING:
			str = gstring(arg.vl_data);
			makecstring(str, dname, sizeof (dname));
			dir = dname;
			break;
		case LISP_FIXNUM:
			number = gfixnum(arg.vl_data);
			break;
		default:
			BADARGN(1, "a directory name");
		}
		if (dir == NULL) {
			count = 0;
			for (dp = directories; dp != NULL; dp = dp->di_next)
				count++;
			if (number <= 0 || number >= count)
				error("That one doesn't exist on the stack.");
			dp = directories;
			for (count = 0; count < number; count++) {
				last = dp;
				dp = dp->di_next;
			}
			first = change(dp->di_path);
			last->di_next = dp->di_next;
			first->di_next = directories;
			directories = first;
			dfree(dp);
		} else {
			first = change(fixpath(dir));
			second = directories;
			directories = first;
			first->di_next = second;
		}
	}
	
	return vdirs();
}

/*
 *  DOCUMENTATION
 *
 *  Name: popd
 *  Call: (popd [ 'number ])
 *  Retu: list
 *  Desc: This function pops an element of the directory stack,
 *	changing the current working directory to the new top
 *	of the stack.  The optional argument specifies how far into
 *	the stack to pop, in this case only the specified element
 *	is removed.  The current directory stack is returned as
 *	a list, the \sym{car} of which is the current directory.
 *  SeeA: cd pushd dirs
 */

DEFUN(dopopd, "popd", FLAG_NONE, "")
{
	struct value	arg;
	struct dirlist	*first, *second, *last, *new, *dp;
	register int	number, count;
	struct string	*str;
	char		buf[SMALLBUF];

	CHECKAC(0, 1);
	if (GETACOUNT() == 0) {
		ASSERT(directories != NULL);
		if (directories->di_next == NULL)
			error("Directory stack has only one entry.");
		first = directories;
		second = first->di_next;
		new = change(second->di_path);
		new->di_next = second->di_next;
		directories = new;
		dfree(first);
		dfree(second);
	} else {
		arg = EVALARGN(1);
		switch (arg.vl_type) {
		case LISP_FIXNUM:
			number = gfixnum(arg.vl_data);
			break;
		case LISP_SYMBOL:
			str = gsymbol(arg.vl_data)->sy_pname;
			makecstring(str, buf, sizeof (buf));
			if (buf[0] != '+' || !isdigit(buf[1]))
				error("a directory index");
			number = atoi(buf + 1);
			break;
		default:
			BADARGN(1, "a directory index");
		}
		count = 0;
		for (dp = directories; dp != NULL; dp = dp->di_next)
			count++;
		if (number <= 0 || number >= count)
			error("That directory does not exist on the stack.");
		dp = directories;
		for (count = 0; count < number; count++) {
			last = dp;
			dp = dp->di_next;
		}
		last->di_next = dp->di_next;
		dfree(dp);
	}
	
	return vdirs();
}

/*
 *  DOCUMENTATION
 *
 *  Name: cd
 *  Call: (cd 'string)
 *  Retu: list
 *  Desc: This function changes the current working directory of
 *	the editor to the directory specified by its argument.
 *	This argument must evaluate to a string which is a
 *	valid directory name.  The function returns a list which
 *	is the directory stack, the \sym{car} of which is the
 *	current directory.  Actually, \sym{cd} just replaces the
 *	top of the directory stack and changes there.
 *  SeeA: pushd popd dirs
 */

DEFUN(dochdir, "cd", FLAG_NONE, "dChange to: ")
{
	extern char	*getwd();
	extern char	*gethomedir(), *fixpath();
	struct value	arg;
	struct string	*str;
	char		path[MAXPATHLEN];
	struct dirlist	*new;

	CHECKAC(0, 1);
	if (GETACOUNT() == 0) {
		/* just change to home directory */
		strcpy(path, "~");
	} else {
		arg = EVALARGN(1);
		switch (arg.vl_type) {
		case LISP_SYMBOL:
			str = gsymbol(arg.vl_data)->sy_pname;
			makecstring(str, path, sizeof (path));
			break;
		case LISP_STRING:
			str = gstring(arg.vl_data);
			makecstring(str, path, sizeof (path));
			break;
		default:
			BADARGN(1, "a directory name");
		}
	}

	/* replace top element of list with new dir */
	new = change(fixpath(path));
	if (directories != NULL) {
		new->di_next = directories->di_next;
		dfree(directories);
	}
	directories = new;

	return vdirs();
}

static struct dirlist *
change(path)
	char	*path;
{
	/* change to the asked-for directory */
	if (chdir(path) != 0)
		perror("Can't chdir to %s", path);

	if (getwd(currentdir) == NULL) {
		/* some horrible error; shouldn't happen */
		strcpy(currentdir, ".");
	}
	return dalloc(currentdir);
}

static struct value
vdirs()
{
	struct value	head;
	struct ccell	*elt, *last;
	struct dirlist	*dirp;
	register int	len;

	if (directories == NULL)
		return (v_nil);

	/* make up the list */
	head.vl_type = LISP_CONS;
	last = NULL;
	for (dirp = directories; dirp != NULL; dirp = dirp->di_next) {
		elt = save_ccell();
		if (last == NULL)
			slist(head.vl_data, elt);
		else
			slist(last->cc_cdr, elt);
		elt->cc_tcar = LISP_STRING;
		len = strlen(dirp->di_path);
		sstring(elt->cc_car, save_string(dirp->di_path, len));
		elt->cc_tcdr = LISP_CONS;
		last = elt;
	}
	last->cc_tcdr = LISP_NIL;
	slist(last->cc_cdr, NULL);

	return (head);
}

static struct dirlist *
dalloc(path)
	char	*path;
{
	struct dirlist	*dirp;

	dirp = (struct dirlist *)valloc(sizeof (struct dirlist));
	strncpy(dirp->di_path, path, sizeof (dirp->di_path));
	dirp->di_path[sizeof (dirp->di_path) - 1] = '\0';
	dirp->di_next = NULL;

	return (dirp);
}

static
dfree(dir)
	struct dirlist	*dir;
{
	ASSERT(dir != NULL);
	vfree(dir);
}
