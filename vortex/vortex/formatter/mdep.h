/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifndef MDEP_
#define MDEP_

/*
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents.
 *
 *  This file is part of the VorTeX proof editor 
 *  written by Jeffrey W. McCarrell for the VorTeX project
 *  under the direction of Prof. Michael A. Harrison
 *  of the University of California at Berkeley.
 *
 *  Copyright (c) 1987 by Jeffrey W. McCarrell
 *  and The Regents of the University of California.
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  jwm@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 */

/*
 * All(?) of the machine dependent stuff should be here.  In particular, we
 * define some of the low level things needed for select.
 */

#include <sys/errno.h>
#include <sys/param.h>
#include <sys/time.h>

extern int	errno;

#define BPI		(NBBY*NBPW)	/* bits per int */
/* the ceiling of a / b. */
#define CDIV(a,b)	(((a)+((b)-1))/(b))

/*
 * setup the macros to test and set bits in the masks for select.
 */
/* the size of the array of ints needed for read, write, exception masks. */
#define FDM_SIZE	CDIV(NOFILE,BPI)
/* twiddle bit number b in mask a. */
#define FDM_SET(a,b)	((a[((b)/BPI)]) |= (1<<((b)%BPI)))
#define FDM_CLR(a,b)	((a[((b)/BPI)]) &= (~(1<<((b)%BPI))))
#define FDM_ISSET(a,b)	((a[((b)/BPI)]) & (1<<((b)%BPI)))
#define FDM_ISCLR(a,b)	(((a[((b)/BPI)]) & (1<<((b)%BPI))) == 0)



#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	roundup(x, y)	((((x)+((y)-1))/(y))*(y))

#endif
