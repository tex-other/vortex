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
 *  RCS Info: $Header: catch.h,v 0.1 87/04/30 20:50:42 john Locked $
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
 *  catch.h - data structures for catch and throw
 */
 
#ifndef _CATCH_
#define _CATCH_

#include <setjmp.h>
#include "function.h"

/*
 *  Throws/catches implement non-local gotos for lisp, and
 *  are the way we implement popping back to top-level on
 *  errors.  Two catches are set up initially, ``top-level''
 *  and ``error''.  These all go to the editor top-level,
 *  but the user can re-catch somewhere else later
 *  on.  This is a dangerous thing, because it will mean that
 *  the user's own code takes over for these functions.
 */

struct catch {
	struct string	*ca_tag;	/* user tag for this catch */
	unsigned short	ca_flags;	/* special purpose flags */
	jmp_buf		ca_jmpbuf;	/* internal data for setjmp */
	struct value	ca_value;	/* lisp value to return */
	long		ca_frame;	/* call frame associated with catch */
	struct catch	*ca_stack;	/* catches stacked with this tag */
	struct catch	*ca_next;	/* next catch in list */
};

#define CATCH_PUSHED	(1 << 0)	/* there is a pushed catch here */
#define CATCH_DELETE	(1 << 1)	/* delete catch instead of setting */
#define CATCH_DESTROY	(1 << 2)	/* destroy all catches with this tag */
#define CATCH_LOCAL	(1 << 3)	/* not a permanent catch */

extern int		int_catch();	/* internal catch function */
extern int		int_throw();	/* internal throw function */
extern struct value	throw_value();	/* return value from throw */
extern struct catch	*get_catch();	/* find catch for given tag */

#define JMPBUFSIZE	(sizeof (jmp_buf))
#define LONGJMPARG	1

extern struct catch	*catch_list;	/* list of active catches */

#endif !_CATCH_
