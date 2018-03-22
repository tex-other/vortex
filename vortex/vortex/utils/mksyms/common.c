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
 *  RCS Info: $Header: common.c,v 0.1 87/02/22 01:17:23 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the utility program mksyms which
 *  builds the standard symbols for the source editor of VorTeX,
 *  written by John Coker under Prof. Michael A. Harrison of
 *  the University of California at Berkeley.
 *
 *  common.c - common routines for storage and errors
 */
 
#include "syms.h"

extern char	*program;

char *
alloc(nbytes)
{
	char	*ptr, *malloc();

	if ((ptr = malloc(nbytes)) == NULL) {
		fprintf(stderr, "%s: No more memory, too many entries!\n",
		    program);
		exit(1);
	}
	bzero(ptr, nbytes);
	return (ptr);
}

char *
strsave(str)
	char	*str;
{
	char	*ptr, *malloc();

	if ((ptr = malloc(strlen(str) + 1)) == NULL) {
		fprintf(stderr, "%s: No more memory, too many strings!\n",
		    program);
		exit(1);
	}
	strcpy(ptr, str);
	return (ptr);
}

char	*errfile = NULL;
int	errline = -1;

error(format, args)
	char	*format;
{
	if (errfile != NULL && errline > 0) {
		/* precede message with filename and line number */
		fprintf(stderr, "\"%s\", line %d: ", errfile, errline);
	}
	fprintf(stderr, "(%s) ", program);
	_doprnt(format, &args, stderr);
	putc('\n', stderr);
}
