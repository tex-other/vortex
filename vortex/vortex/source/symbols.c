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
 *  RCS Info: $Header: symbols.c,v 0.1 87/05/01 12:28:32 john Locked $
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
 *  symbols.c - internal symbol storage and handling routines
 */
static char _ID[] = "@(#)symbols.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "value.h"

/*
 *  These routines manage the storage of symbol structs for the
 *  vlisp symbol values.  We maintain a table of symbols at
 *  any one time and allocate from that table.  Multiple values
 *  may point to the same symbol, when garbage collection time
 *  is at hand, things will be resolved.  Save_symbol takes a
 *  string struct and sees if that symbol already exists in the
 *  table.  If so, that symbol is returned, otherwise a new one
 *  is created with the given print name.
 *
 *  We use a straight-forward hash into the array of symbols to
 *  speed up the lookup of symbols from their names.  The search
 *  starts from the hash point and wraps around the end back to
 *  the beginning and on to the start point again.  For this reason
 *  the size of the table should be prime.
 */
int		aagrow_symbols = 0;	/* automatically grow symbols */
static int	minfree_symbols = 0;	/* actual amount that must be free */
static int	curfree_symbols = 0;	/* actual amount that is free */
int		softlim_symbols = 10;	/* percentage that must be free */
int		cursize_symbols = 0;	/* number allocated */
int		overflow_symbols = 0;	/* have already used too many */

static struct symbol	*symbol_table = NULL, *symbol_tabend;
static int		symbol_initsize = 1213;
static int		symbol_haveholes = FALSE;

struct symbol *
save_symbol(name)
	struct string	*name;
{
	register struct symbol	*sym;
	struct symbol		*start;

	if (name == NULL)
		ierror("save_symbol(0x%x): Bad argument!");

	if (name->st_length < 1 || name->st_buffer == NULL)
		ierror("Trying to make a zero-length symbol!");

	if (symbol_table == NULL) {
		/* allocate initial symbol storage */
		grow_symbols(symbol_initsize);
		start = symbol_table;
	} else if (symbol_haveholes) {
		/* search for an eq symbol in the entire table */
		start = symbol_table + hashsymbol(name);
		for (sym = start; sym < symbol_tabend; sym++)
			if (sym->sy_pname != NULL &&
			    sequal(sym->sy_pname, name))
				return (sym);
		for (sym = symbol_table; sym < start; sym++)
			if (sym->sy_pname != NULL &&
			    sequal(sym->sy_pname, name))
				return (sym);
		/* no exact match found in symbol list */
	} else {
		/* only search until next free symbol */
		start = symbol_table + hashsymbol(name);
		for (sym = start;
		     sym < symbol_tabend && sym->sy_pname != NULL; sym++)
			if (sequal(sym->sy_pname, name))
				return (sym);
		for (sym = symbol_table;
		     sym < start && sym->sy_pname != NULL; sym++)
			if (sequal(sym->sy_pname, name))
				return (sym);
		/* no exact match found in symbol list */
	}

	/* need to make a new symbol struct */
	start = symbol_table + hashsymbol(name);
	for (sym = start; sym < symbol_tabend; sym++)
		if (sym->sy_pname == NULL)
			goto found;
	for (sym = symbol_table; sym < start; sym++)
		if (sym->sy_pname == NULL)
			goto found;
	/* this really shouldn't happen */
	debug(DALLOC, "Symbol size: %d, curfree: %d, minfree: %d.",
	      cursize_symbols, curfree_symbols, minfree_symbols);
	error("Help!  Ran completely out of symbol storage!");

found:	if (!overflow_symbols && curfree_symbols < minfree_symbols) {
		if (aagrow_symbols > 0) {
			/* extend the value symbol storage */
			grow_symbols(aagrow_symbols);
		} else {
			overflow_symbols++;
			cerror(
		    "Too little symbol storage left (%d used, %d free)",
			       cursize_symbols - curfree_symbols,
			       curfree_symbols);
		}
	}

	/* found a free symbol to return */
	sym->sy_pname = name;
	sym->sy_plist = v_nil;
	curfree_symbols--;
	return (sym);
}

#define ATOM_HARDLIM	10		/* must always have this many free */

grow_symbols(incr)
{
	struct symbol	*new, *sym;

	if (incr > 0) {
		new = (struct symbol *)
		    valloc((cursize_symbols + incr) * sizeof (struct symbol));
		if (symbol_table != NULL) {
			bcopy(symbol_table, new,
			    cursize_symbols * sizeof (struct symbol));
			vfree(symbol_table);
		}
		sym = new + cursize_symbols;
		symbol_table = new;
		cursize_symbols += incr;
		curfree_symbols += incr;
		symbol_tabend = symbol_table + cursize_symbols;
		while (sym < symbol_tabend) {
			sym->sy_pname = NULL;
			sym++;
		}

		overflow_symbols = 0;
	}

	/* update minimum free amount variable */
	minfree_symbols = ROUND((double)cursize_symbols *
			      ((double)softlim_symbols / 100.0));
	if (minfree_symbols > cursize_symbols - ATOM_HARDLIM)
		minfree_symbols = cursize_symbols - ATOM_HARDLIM;

	return (cursize_symbols);
}

static int
hashsymbol(name)
	struct string	*name;
{
	register int	count, pos, inx;
	unsigned char	*ptr;

	count = 0;
	pos = name->st_length + 2;
	ptr = name->st_buffer;
	for (inx = 0; inx < name->st_length; inx++)
		count += *ptr++ * pos--;

	return (count % cursize_symbols);
}
