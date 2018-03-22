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
 *  RCS Info: $Header: doload.c,v 0.1 87/05/01 11:53:17 john Locked $
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
 *  doload.c - load vLisp code from UNIX files
 */
static char _ID[] = "@(#)doload.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/param.h>
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "channel.h"

extern int	errline;
extern char	*errfile;

/*
 *  DOCUMENTATION
 *
 *  Name: load
 *  Call: (load 'file [ silent ])
 *  Retu: t
 *  Desc: This function reads the lisp s-expressions from the
 *	file given as its first argument and evaluates them one by
 *	one.  The first argument to \sym{load} must evaluate to a
 *	string which is considered a \sc{UNIX} file name to open and
 *	read from.  The function always returns t.  If the optional
 *	second is present, and non-nil, it supresses the message
 *	anouncing that the file is being loaded.
 *
 *	If some error occurs while a file is being read,
 *	the error message is preceeded with the filename and
 *	line number, and the interpreter aborts back to the
 *	top level.
 *
 *	The full filename may be specified or the typical
 *	\lit{.vl} extension may be omitted.  When the
 *	file is not specified as a full path (the name contains
 *	no slashes), \sym{load} will search through the directories
 *	listed in the \sym{load-path} for the file, both with and
 *	without the \lit{.vl} extension.
 *  Xref: source
 *  SeeA: fopen read eval load-path
 */
extern struct string	*LOADPATH_NAME;

DEFUN(doload, "load", FLAG_NONE, "FLoad vlisp file: ")
{
	extern char	*findlpath(), *index();
	struct value	arg, pvar;
	struct string	*str;
	char		*path, fbuf[STRBUF], alt[STRBUF+3];
	int		silent = FALSE;

	/* make sure we get a good list of arguments */
	CHECKAC(1, 2);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			silent = TRUE;
	}

	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_SYMBOL:
		str = gsymbol(arg.vl_data)->sy_pname;
		break;
	case LISP_STRING:
		str = gstring(arg.vl_data);
		break;
	default:
		BADARGN(1, "a string file name");
	}
	makecstring(str, fbuf, sizeof (fbuf));
	if (*fbuf == '~' || index(fbuf, '/') != NULL) {
		/* full path name has been specified */
		path = fixpath(fbuf);
	} else {
		pvar = get_variable(LOADPATH_NAME, NULL);
		if (dtprp(pvar)) {
			path = findlpath(fbuf, pvar);
			if (path == NULL) {
				/* try adding a .vl extension */
				strcpy(alt, fbuf);
				strcat(alt, ".vl");
				path = findlpath(alt, pvar);
			}
			if (path == NULL)
				error("Can't find %s in load-path.", fbuf);
		} else {
			/* no path, just use the file name */
			path = fbuf;
			if (access(path, F_OK) != 0) {
				/* try adding a .vl extension */
				strcpy(alt, fbuf);
				strcat(alt, ".vl");
				path = alt;
			}
			if (access(path, F_OK) != 0) {
				/* file not in current directory */
				error("Can't find %s (no load path).", fbuf);
			}
		}
	}
	loadfile(path, silent);

	return (v_t);
}

static struct channel	*load_channel;	/* channel opened for load */

static struct value
load_cleanup()
{
	if (load_channel != NULL) {
		cclose(load_channel);
		load_channel = NULL;
	}

	return (v_nil);
}

loadfile(file, silent)
	char	*file;
{
	extern char	*alloca();
	extern char	*program;
	struct value	expr, result;
	struct channel	*chan;
	char		*ofile;
	int		oline;

	if ((chan = fcopen(file, CHAN_READ|CHAN_LOCK_SH)) == NULL)
		perror("Can't open load file %s", file);

	/* save current file and line indication */
	ofile = errfile;
	oline = errline;
	/* now starting in this file */
	errfile = alloca(strlen(file) + 1);
	strcpy(errfile, file);
	errline = 1;

	/* set unwind-protect function */
	uwprotect(load_cleanup);
	load_channel = chan;

	/* now we start loading this file */
	if (!silent)
		message("Loading %s...", file);

	/* read in each line and eval it */
	do {
		/* read in next s-expression */
		expr = cread(chan);
		if (eq(expr, NOVALUE))
			break;
		/* evaluate this s-expression */
		result = evalsexpr(expr);
	} while (!eq(expr, NOVALUE));

	/* we're done loading this file */
	if (!silent)
		message("Loading %s...done", file);
	errfile = ofile;
	errline = oline;

	return (0);
}

char *
findlpath(file, lval)
	char		*file;
	struct value	lval;
{
	extern char	*nthname();
	register int	i, count, len;
	struct value	dir;
	struct string	*str;
	char		*path, pbuf[STRBUF];

	if (!dtprp(lval))
		return (NULL);

	count = length(lval);
	for (i = 0; i < count; i++) {
		dir = nth(i, lval);
		switch (dir.vl_type) {
		case LISP_STRING:
			str = gstring(dir.vl_data);
			break;
		case LISP_SYMBOL:
			str = gsymbol(dir.vl_data)->sy_pname;
			break;
		default:
			error("The %s element of the path isn't a string!",
			    nthname(i + 1));
			break;
		}
		makecstring(str, pbuf, sizeof (pbuf) - 1);
		len = strlen(pbuf);
		pbuf[len++] = '/';
		strncpy(pbuf + len, file, sizeof (pbuf) - len - 1);
		pbuf[sizeof (pbuf) - 1] = '\0';
		path = fixpath(pbuf);
		if (access(path, F_OK) == 0)
			return (path);
	}

	/* not found in load path */
	return (NULL);
}
