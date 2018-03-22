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
 *  RCS Info: $Header: symtab.h,v 0.1 87/04/30 20:55:21 john Locked $
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
 *  symtab.h - vLisp symbol table data structures
 */
 
#ifndef _SYMTAB_
#define _SYMTAB_

#include "value.h"

/*
 *  The symbol storage structure.  This struct is used to
 *  store the symbols, both those set by the program itself
 *  and by the user through vlisp.  Any lisp object can
 *  be bound to a symbol, then one of these structures is
 *  created.
 *
 *  Initially, the list of standard symbols is bound and
 *  inserted into the list.  Next, environment variables are
 *  imported as symbols.  Those with imbedded colons (:)
 *  become lists, other become strings.  These new symbols
 *  have the STAB_EXPORT bit set so that they are exported to
 *  the environment (for child processes).
 *
 *  If the symbol has the attribute STAB_LOCAL set, we always
 *  set it locally in a buffer, if possible.  The flag STAB_PERM
 *  means the symbol must exist, it can't be remob'ed.
 *
 *  Users may arbitrarily create symbols and implicitly push
 *  or pop in the individual symbol binding stack.  Other
 *  routines are expected to know when indivvidual symbols
 *  should be pushed or popped.  The user may create these
 *  explicitly by defining local variables with let, or
 *  implicitly by naming function arguments that have the
 *  same name as global symbols.
 */
struct symtab {
	struct string	*st_name;	/* name of variable */
	unsigned int	st_flags;	/* flags on variable */
	struct value	st_value;	/* value (s-expression) */
	struct symtab	*st_stack;	/* linked list of variable stack */
	struct symtab	*st_next;	/* next variable in list */
};

#define STAB_PERM	(1 << 0)	/* this symbol can't be removed */
#define STAB_LOCAL	(1 << 1)	/* this should be made local */
#define STAB_STACKED	(1 << 2)	/* this is a stacked value */

extern int		sym_tabsize;	/* number of entries in hash table */
extern struct symtab	*sym_table[];	/* the global symbol hash table */

extern struct symtab	*getglobal(),	/* get global symbol name */
			*getlocal();	/* get local symbol name */
extern struct value	get_variable();	/* find the variable if set */

#define STAB_HASHSIZE	1009		/* size of symbol hash table */

#endif !_SYMTAB_
