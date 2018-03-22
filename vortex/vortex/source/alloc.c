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
 *  RCS Info: $Header: alloc.c,v 0.1 87/05/01 11:23:03 john Locked $
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
 *  alloc.c - general heap storage allocation
 */
static char _ID[] = "@(#)alloc.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"

/*
 *  Some versions of malloc blow it if given a value of less
 *  than 8 bytes to allocate.  Anyway, we check to make sure
 *  we never ask for less than this.  It should be greater
 *  than zero, just in case.
 */
#define MINALLOC	8

char *
valloc(nbytes)
{
	extern char	*malloc();
	char		*ptr;

	if (nbytes < 1)
		ierror("valloc: Called to allocate only %d bytes!", nbytes);

	if (nbytes < MINALLOC)
		nbytes = MINALLOC;
	if ((ptr = malloc(nbytes)) == NULL)
		panic("Out of general heap memory!");
	return (ptr);
}

char *
strsave(str)
	char	*str;
{
	char		*ptr;
	int		nbytes;

	if (str == NULL)
		ierror("strsave: Called with a null string!");

	nbytes = strlen(str) + 1;
	if (nbytes < MINALLOC)
		nbytes = MINALLOC;
	ptr = valloc(nbytes);
	strcpy(ptr, str);
	return (ptr);
}

vfree(addr)
	char	*addr;
{
	free(addr);
}
