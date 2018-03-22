/* 
 * Copyright (c) 1986 The Regents of the University of California.
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
 *  RCS Info: $Header$
 *
 *  VorTeX -- Visually ORiented TeX
 *
 *  Peehong Chen, John Coker, Jeff McCarrell, Steve Procter
 *  for Prof. Michael Harrison of the Computer Science Division
 *  University of California, Berkeley
 *
 *  mkdoc: prepare documentation from lisp and C source files
 *
 *  util.c - general utlilty routines
 */

#include <stdio.h>

extern char	*program;

char	*errfile;
int	errline;

error(format, args)
	char	*format;
{
	if (errfile == NULL)
		fprintf(stderr, "%s: ", program);
	else
		fprintf(stderr,
		    "%s: \"%s\", line %d: ", program, errfile, errline);

	_doprnt(format, &args, stderr);
	putc('\n', stderr);
}

char *
alloc(nbytes)
{
	char	*ptr, *malloc();

	if ((ptr = malloc(nbytes)) == NULL) {
		fputs("malloc: No more memory; too may docstrs!\n", stderr);
		exit(1);
	}
	return (ptr);
}

char *
strsave(str)
	char	*str;
{
	char	*ptr, *malloc();

	if ((ptr = malloc(strlen(str) + 1)) == NULL) {
		fputs("malloc: No more memory; strings too long!\n", stderr);
		exit(1);
	}
	strcpy(ptr, str);
	return (ptr);
}
