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
 *  RCS Info: $Header: symtab.c,v 0.1 87/05/01 12:29:24 john Locked $
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
 *  symtab.c - internal symbol table management routines
 */
static char _ID[] = "@(#)symtab.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "symtab.h"
#include "function.h"
#include "buffer.h"

int	symtab_changed = FALSE;

/*
 *  This is the symbol hash table.  Symbol names are hashed into
 *  this table for their bound value.  Each symbol has a stack
 *  of actual values, the top of which is the current binding.
 *  Thus, dynamic bindings are implemented by pusing values on
 *  the stack for a given symbol.  Note that both functions and
 *  variables live in this table, so there is no difference
 *  between a functions and a variable binding.
 */
struct symtab	*sym_table[STAB_HASHSIZE];
int		sym_tabsize = NITEMS(sym_table);

/*
 *  We allocate symtab stuctures from a pool we keep.  They are
 *  gotten initially from valloc(), but not freed again, we
 *  keep reusing them to speed things up.
 */
struct symtab	*alloc_sym();

/*
 *  We intern t and nil here for sure, marking them as permanent
 *  symbols, they can't be deleted (although they could be changed).
 *  By the time we get here, v_nil and v_t have already been set
 *  up to the the right values.
 */
MKSTRING(T_NAME, "t");
MKSTRING(NIL_NAME, "nil");

static int	done_symbols = FALSE;

initsymbols()
{
	if (!done_symbols) {
		/* insert t into symbol table as a special case */
		setglobal(T_NAME, v_t, STAB_PERM);
		setglobal(NIL_NAME, v_nil, STAB_PERM);

		/* insert the builtin symbols into the symbol table */
		builtin_symbols();

		done_symbols = TRUE;
	}

	return (0);
}

setglobal(name, value, flags)
	struct string	*name;
	struct value	value;
{
	struct symtab	*next, **lstptr;
	register int	ind;

	if (name == NULL || eq(value, NOVALUE)) {
		ierror("setglobal(%x, %x, %x): Bad arguments!",
		    name, value, flags);
	}

	/* try to find this variable in list */
	ind = hashname(name);
	lstptr = &sym_table[ind];
	for (next = *lstptr; next != NULL; next = next->st_next)
		if (sequal(next->st_name, name))
			break;

	/* now set it, blocking interrupts during this */
	PROTECT();
	if (next == NULL) {
		/* make up the new symbol, insert it */
		next = alloc_sym(name);
		next->st_next = *lstptr;
		*lstptr = next;
	}
	next->st_value = value;
	next->st_flags |= flags;
	symtab_changed = TRUE;
	UNPROTECT();

	return (0);
}

setlocal(name, value, flags, slstptr)
	struct string	*name;
	struct value	value;
	struct symtab	**slstptr;
{
	struct symtab	*new, *next, *last;

	if (name == NULL || eq(value, NOVALUE) || slstptr == NULL) {
		ierror("setlocal(%x, %x, %x, %x): Bad arguments!",
		    name, value, flags, slstptr);
		return (-1);
	}

	last = NULL;
	for (next = *slstptr; next != NULL; next = next->st_next) {
		if (sequal(next->st_name, name))
			break;
		last = next;
	}

	/* now set the symbol, blocking interrupts */
	PROTECT();
	if (next != NULL) {
		/* reset the value, keep all else the same */
		next->st_flags |= flags;
		next->st_value = value;
		new = next;
	} else {
		/* make up new variable structure */
		new = alloc_sym(name);
		new->st_flags = flags;
		new->st_value = value;

		/* insert new symbol value into the list */
		if (last != NULL)
			last->st_next = new;
		else
			*slstptr = new;
		new->st_next = next;
	}
	UNPROTECT();

	return (0);
}

pushglobal(name, value, flags)
	struct string	*name;
	struct value	value;
{
	struct symtab	*next, *new, *last;
	int		ind;
	struct symtab	**lstptr;

	if (name == NULL || eq(value, NOVALUE)) {
		ierror("pushglobal(%x, %x): Bad arguments!", name, value);
		return (-1);
	}

	/* search for the symbol in the table */
	ind = hashname(name);
	lstptr = &sym_table[ind];
	last = NULL;
	for (next = *lstptr; next != NULL; next = next->st_next) {
		if (sequal(next->st_name, name))
			break;
		last = next;
	}

	/* make up the new symtab struct to set */
	PROTECT();
	new = alloc_sym(name);
	new->st_value = value;
	new->st_flags = flags;

	if (next != NULL) {
		new->st_stack = next;
		new->st_flags |= next->st_flags | STAB_STACKED;
		new->st_next = next->st_next;
		if (last == NULL)
			*lstptr = new;
		else
			last->st_next = new;
	} else {
		new->st_stack = NULL;
		new->st_flags |= STAB_STACKED;
		new->st_next = *lstptr;
		*lstptr = new;
	}
	symtab_changed = TRUE;
	UNPROTECT();

	return (0);
}

popglobal(name)
	struct string	*name;
{
	struct symtab	*next, *last, *new, **lstptr;

	if (name == NULL)
		ierror("popglobal: Null name argument to pop!");

	last = NULL;
	lstptr = &sym_table[hashname(name)];
	for (next = *lstptr; next != NULL; next = next->st_next) {
		if (sequal(next->st_name, name))
			break;
		last = next;
	}
	if (next == NULL)
		return (1);

	/* pop symbol, blocking interrupts */
	PROTECT();
	if (next->st_stack == NULL) {
		/* just unlink the whole variable entry */
		if (last == NULL)
			*lstptr = next->st_next;
		else
			last->st_next = next->st_next;
	} else {
		/* pop one variable off the entry stack */
		new = next->st_stack;
		new->st_next = next->st_next;
		if (last == NULL)
			*lstptr = new;
		else
			last->st_next = new;
	}
	free_sym(next);
	symtab_changed = TRUE;
	UNPROTECT();

	return (0);
}

remglobal(name)
	struct string	*name;
{
	struct symtab	*next, *last;
	int		ind;
	struct symtab	**lstptr;

	if (name == NULL)
		ierror("remglobal(%x): Bad name argument!", name);

	/* search for the symbol in the table */
	ind = hashname(name);
	lstptr = &sym_table[ind];
	last = NULL;
	for (next = *lstptr; next != NULL; next = next->st_next) {
		if (sequal(next->st_name, name))
			break;
		last = next;
	}
	if (next == NULL)
		return (1);
	if ((next->st_flags & STAB_PERM) != 0) {
		error("Can't delete global symbol %Y, it's permanent!",
		      next->st_name);
		/* NOTREACHED */
	}

	PROTECT();
	if (last == NULL)
		*lstptr = next->st_next;
	else
		last->st_next = next->st_next;
	while (next != NULL) {
		free_sym(next);
		next = next->st_stack;
	}
	symtab_changed = TRUE;
	UNPROTECT();

	return (0);
}

remlocal(name, slistp)
	struct string	*name;
	struct symtab	**slistp;
{
	struct symtab	*next, *last;

	if (slistp == NULL)
		ierror("remlocal(%x, %x): Bad arguments!", name, slistp);

	if (name == NULL) {
		/* remove all symbols in local list */
		PROTECT();
		for (next = *slistp; next != NULL; next = next->st_next) {
			for (last = next->st_stack; last != NULL;
			     last = last->st_stack)
				free_sym(last);
			free_sym(next);
		}
		*slistp = NULL;
		UNPROTECT();
	} else {
		/* search for the symbol in local list */
		last = NULL;
		for (next = *slistp; next != NULL; next = next->st_next) {
			if (sequal(next->st_name, name))
				break;
			last = next;
		}
		if (next == NULL)
			return (1);
		if ((next->st_flags & STAB_PERM) != 0) {
			error("Can't delete local symbol %Y, it's permanent!",
			      next->st_name);
			/* NOTREACHED */
		}
	
		PROTECT();
		if (last == NULL)
			*slistp = next->st_next;
		else
			last->st_next = next->st_next;
		while (next != NULL) {
			free_sym(next);
			next = next->st_stack;
		}
		UNPROTECT();
	}

	return (0);
}

clean_global()
{
	struct symtab	**lstptr, *last, *next, *end;
	register int	ind;

	PROTECT();
	for (ind = 0; ind < sym_tabsize; ind++) {
		last = NULL;
		lstptr = &sym_table[ind];
		for (next = *lstptr; next != NULL; next = next->st_next) {
			if ((next->st_flags & STAB_STACKED) != 0) {
				for (end = next; end->st_stack != NULL;
				     end = end->st_stack)
					free_sym(end);
				if (end == NULL) {
					if (last == NULL)
						*lstptr = next->st_next;
					else
						last->st_next = next->st_next;
					free_sym(next);
					next = last;
				} else {
					if (last == NULL)
						*lstptr = end;
					else
						last->st_next = end;
					next = end;
				}
				next->st_flags &= ~STAB_STACKED;
			}
			last = next;
		}
	}
	symtab_changed = TRUE;
	UNPROTECT();

	return (0);
}

struct symtab *
getglobal(name)
	struct string	*name;
{
	struct symtab	*next, **lstptr;
	int		ind;

	ind = hashname(name);
	lstptr = &sym_table[ind];
	for (next = *lstptr; next != NULL; next = next->st_next)
		if (sequal(next->st_name, name))
			return (next);
	return (NULL);
}

struct symtab *
getlocal(name, slstptr)
	struct string	*name;
	struct symtab	*slstptr;
{
	struct symtab	*next;

	for (next = slstptr; next != NULL; next = next->st_next)
		if (sequal(next->st_name, name))
			return (next);
	return (NULL);
}

struct value
get_variable(name, bufp)
	struct string	*name;
	struct buffer	*bufp;
{
	struct symtab	*sym = NULL;

	if (bufp != NULL && bufp->bu_locals != NULL)
		sym = getlocal(name, bufp->bu_locals);
	if (sym == NULL)
		sym = getglobal(name);
	if (sym != NULL)
		return (sym->st_value);
	else
		return (NOVALUE);
}

truevar(name)
	struct string	*name;
{
	struct value	val;

	val = get_variable(name, current_buffer);
	if (truep(val))
		return (TRUE);
	else
		return (FALSE);
}

static int
hashname(name)
	struct string	*name;
{
	register int	count, pos, inx;
	unsigned char	*ptr;

	count = 0;
	pos = name->st_length + 2;
	ptr = name->st_buffer;
	for (inx = 0; inx < name->st_length; inx++)
		count += *ptr++ * pos--;

	return (count % sym_tabsize);
}

/*
 *  These routines allocate and free symtab's from a private
 *  list of entries.  Instead of handing used symbols back to
 *  free(3), we keep them in a list for later use.  This
 *  saves time, since much allocation and deallocation of
 *  symbols is done every time a function is called.
 */
static struct symtab	*avail_symbols = NULL;

static struct symtab *
alloc_sym(name)
	struct string	*name;
{
	struct symtab	*sym;

	if (avail_symbols == NULL) {
		/* get completely new storage */
		sym = (struct symtab *)valloc(sizeof (struct symtab));
	} else {
		/* grab the top one off the saved list */
		sym = avail_symbols;
		avail_symbols = sym->st_next;
	}

	/* save the assigned name and return it */
	sym->st_name = name;
	sym->st_flags = FLAG_NONE;
	sym->st_value = NOVALUE;
	sym->st_stack = sym->st_next = NULL;
	return (sym);
}

static int
free_sym(sym)
	struct symtab	*sym;
{
	sym->st_name = NULL;
	sym->st_next = avail_symbols;
	avail_symbols = sym;

	return (0);
}
