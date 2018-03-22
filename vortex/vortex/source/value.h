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
 *  RCS Info: $Header: value.h,v 0.1 87/04/30 20:55:49 john Locked $
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
 *  value.h - vLisp value data structures
 */
 
#ifndef _VALUE_
#define _VALUE_

/*
 *  The value structure is the basic unit of all the data structures,
 *  If the value here is a LISP_CONS, it points to a cons cell, which
 *  contains a car and cdr value.  Otherwise, the value is contained
 *  in the vl_data field.  This basic value is passed around all the
 *  time (but never explicitly stored) and used as the overall type
 *  for lisp.  Since this is a one-word (four byte) value, it can be
 *  passed on the call stack and assigned directly everywhere.
 */
struct value {
	unsigned int	vl_zero : 1,	/* not used here, for cons cell */
			vl_type : 3,	/* type of data here */
			vl_data : 28;	/* data portion of value */
};

/*
 *  This is the storage for a single symbol, containing all the
 *  pertinent fields.  Here we store the property list, and the
 *  print name.  The property list is just a lisp list containing
 *  alternating symbols and values.  This list may also be nil,
 *  of course.
 */
struct symbol {
	struct value	sy_plist;	/* property list value */
	struct string	*sy_pname;	/* print name of symbol */
};

/*
 *  This is the ``string'' type storage structure.  We manage
 *  strings as a single long array of characters, with these
 *  structures sitting on top of that storage.  These store
 *  and retrieve the actual string values.  We keep not only
 *  the actual string, but also the length so that strings may
 *  contain the nul character (ASCII 0).
 */
struct string {
	unsigned char	st_free,	/* this string is available */
			st_perm;	/* can't be collected */
	unsigned short	st_length;	/* length (in bytes) of string */
	unsigned char	*st_buffer;	/* point to characters */
};

/*
 *  The function struct, what is pointed to by the data portion
 *  of a value when it is of LISP_FUNC, is defined in "function.h"
 *  since it uses several auxilary data structures and constants.
 *  Just be aware that a (struct function) exists and a pointer
 *  to that structure is kept when a value is of that type.  Unlike
 *  most lisps, vlisp functions are values in and of themselves,
 *  they are not just a field of an symbol.  Thus, symbol are bound
 *  to function values (using set) rather than having a special
 *  means of defining them (i.e., putd).
 */

/*
 *  This is the ``cons cell'' in tradition lisp terminology.  It
 *  contains two spaces for data, a car and a cdr.  Either can
 *  point to a symbol structure, a string, or contain a fixnum or
 *  point to a function structure.  We keep type information for
 *  both the car and the cdr so that we can tell types without
 *  dereferencing the pointers (since they are not always pointers).
 *  Any part of a list (any value of LISP_CONS) is actually one of
 *  these cons cells.
 */
struct ccell {
	unsigned int	cc_mark : 1,	/* garbage collection bit */
			cc_tcar : 3,	/* car data type */
			cc_car : 28;	/* car data */
	unsigned int	cc_free : 1,	/* cell not used, available */
			cc_tcdr : 3,	/* cdr data type */
			cc_cdr : 28;	/* cdr data */
};

/*
 *  An array is like a fixed length list, with faster access time,
 *  expecially for long lists.  A block of vlisp values is allocated
 *  for the array and so any single value can be accessed in one
 *  try (no list to traverse).
 */
struct array {
	unsigned char	ar_free,	/* array header available */
			ar_perm;	/* can't be collected */
	unsigned short	ar_length;	/* length of array */
	struct value	*ar_array;	/* the actual array */
};

/*
 *  Below are some magic numbers that are used for storage of
 *  fixnums in values.  Since we only have 28 bits to store the
 *  integer, instead of the normal 32 that the machine has, we
 *  need to make sure that the sign bit gets properly transfered
 *  when we store or retrieve a fixnum value.
 */
#define FIXNUM_MASK	((1 << 27) - 1)	/* mask of number w/o sign bit */
#define FIXNUM_SIGN	(1 << 27)	/* mask of just the sign bit */
#define FOURBYTE_SIGN	(1 << 31)	/* mask sign bit for integers */
#define SIGNBIT_SHIFT	(31 - 27)	/* displacement of sign bit */
#define FIXNUM_NEGBITS	(-1 & ~FIXNUM_MASK)
#define MAX_FIXNUM	(1 << 27 - 1)	/* maximum (positive) fixnum */
#define MIN_FIXNUM	(-(1 << 28 -1))	/* minimum (negative) fixnum */

/*
 *  Below are defined the macros which extract information of the
 *  different types from a data field of a cons cell.  All the g<type>(c)
 *  macros extract the value, and the s<type>(c,v) macros set the given
 *  value into the cons cell data given.  The two things of interest here
 *  are that pointers are stored without the two lowest bits (which are
 *  assumed to be zero, since things are all stored on word boundaries)
 *  and that fixnums are stored instead of a pointer to them.
 */
#define gfixnum(c)	((int)(((c) & FIXNUM_SIGN) ? \
			 (c) | FIXNUM_NEGBITS : (c)))
#define gsymbol(c)	((struct symbol *)(c))
#define gstring(c)	((struct string *)(c))
#define gfunct(c)	((struct function *)(c))
#define glist(c)	((struct ccell *)(c))
#define garray(c)	((struct array *)(c))
#define sfixnum(c,v)	((c) = (unsigned long)((v) & FIXNUM_MASK) | \
			 (((v) & FOURBYTE_SIGN) >> SIGNBIT_SHIFT))
#define ssymbol(c,v)	((c) = (unsigned long)(v))
#define sstring(c,v)	((c) = (unsigned long)(v))
#define sfunct(c,v)	((c) = (unsigned long)(v))
#define slist(c,v)	((c) = (unsigned long)(v))
#define sarray(c,v)	((c) = (unsigned long)(v))

/*
 *  Below are the types of a vlisp value.  Type zero is just
 *  for error (like ``symbol not found'') return values.  The
 *  others all are valid types.  The type nil means that there
 *  is valid value, nil is always the same.  A fixnum stores
 *  the integer in the data portion of the value, in the normal
 *  twos complement notation.  Other types store pointers to
 *  specialized structures in the data portion of the value.
 */
#define LISP_NONE	0		/* non-type for errors */
#define LISP_NIL	1		/* special type for nil */
#define LISP_FIXNUM	2		/* integer (in word) */
#define LISP_SYMBOL	3		/* a symbol (struct symbol *) */
#define LISP_STRING	4		/* string (struct string *) */
#define LISP_FUNC	5		/* a function (struct function *) */
#define LISP_CONS	6		/* a cons cell (struct ccell *) */
#define LISP_ARRAY	7		/* an array (struct array *) */

/*
 *  Macros to examine the type of lisp values.  These mimic the names
 *  at the lisp level, but operate on the C (struct value).  Note that
 *  these take the structure, not the pointer.  Several of these allow
 *  different cases.  For example, dtprp only is true of the value is
 *  a cons cell, but listp is true for both nil and a cons cell.  The
 *  last macro, truep, is for determining of the value is non-nil
 *  (and not invalid, which shouldn't happen).
 */
#define invalidp(v)	((v).vl_type == LISP_NONE)
#define nullp(v)	((v).vl_type == LISP_NIL)
#define fixnump(v)	((v).vl_type == LISP_FIXNUM)
#define symbolp(v)	((v).vl_type == LISP_SYMBOL)
#define stringp(v)	((v).vl_type == LISP_STRING)
#define funcp(v)	((v).vl_type == LISP_FUNC)
#define dtprp(v)	((v).vl_type == LISP_CONS)
#define arrayp(v)	((v).vl_type == LISP_ARRAY)
#define atomp(v)	((v).vl_type != LISP_CONS && (v).vl_type != LISP_ARRAY)
#define listp(v)	((v).vl_type == LISP_NIL || (v).vl_type == LISP_CONS)
#define truep(v)	((v).vl_type != LISP_NIL && (v).vl_type != LISP_NONE)

/*
 *  These functions manage storage of secondary objects which are
 *  referenced by values.  Atoms, cons cells, strings and functions
 *  are kept in separate tables to allow intelligent garbage collection
 *  and to save space.  The make_ functions return new storage for the
 *  given types, but don't fill it in.  They should only be used if
 *  one is sure nothing should be eq to the new value being created.
 *  The save_ functions also return storage, but they take a value of
 *  the type to be created, and if an equivalent value already exists
 *  in the table, that is returned instead of the handed value, otherwise
 *  the passed value is copied into safe storage and a pointer to it
 *  is returned.
 */
extern struct symbol	*save_symbol();		/* struct string *pname; */
extern struct ccell	*save_ccell();		/* */
extern struct string	*save_string();		/* char *buf; int length; */
extern struct function	*save_funct();		/* struct function *func; */
extern struct array	*save_array();		/* int length; */
extern struct tblock	*save_tblock();		/* */

/*
 *  These functions access symbols stored in the standard symbol
 *  table.  Get_local() looks for the bound name in the given local
 *  variable list.  Get_global() only searches the global symbol
 *  table.  Get_symbol() searches both.  All these function take
 *  a (struct string *) argument which is the print name of the
 *  binding to search for.  If the given name is bound, the bound
 *  value is returned, otherwiese NOVALUE is returned.
 */
extern struct value	get_local(), get_global(), get_symbol();

/*
 *  These functions perform common operations on lists.  Car and
 *  cdr do the obvious things, length counts the number of elements
 *  in the list.  Nth returns the nth element in the given list
 *  or nil if it isn't that long and nthcdr returns the result of
 *  n cdrs of the argument list.  Copy copies the list into another
 *  of completely different cons cells and returns the new list.
 *  It is an error for any of these functions to be given a non-list
 *  argument.  Index retrieves the nth element of the given array
 *  and returns that value; store stores the corresponding array
 *  value.
 */
extern struct value	car(), cdr();
extern struct value	cons(), makelist(), append();
extern int		length();
extern struct value	nth(), nthcdr();
extern struct value	copy();
extern struct value	aindex();
extern int		store();

/*
 *  Several equality tests.  Equal determnes if the two vlisp
 *  values (struct value)s would print the same.  Eq, a macro,
 *  checks if they are stored in the same place.  Sequal and
 *  fequal do equal for (string struct *)s and (struct function *)s.
 */
extern int	equal();
#define eq(a,b)	((a).vl_type == (b).vl_type && (a).vl_data == (b).vl_data)
extern int	sequal(), fequal();

/*
 *  This is the basic lisp evaluator.  It takes as its argument
 *  a (struct value), evaluates it, and returns the result of that
 *  evaluation, or NULL if there was an error.  Most builtin
 *  functions evaluate their arguments, and the must use this
 *  function to do so.  Call_function is the means for directly
 *  calling a vlisp function.
 */
extern struct value	evalsexpr();
extern struct value	call_function();

/*
 *  These function manipulate property lists of symbols, which are
 *  just vlisp lists referenced by each symbol.  Putprop takes a
 *  symbol structure, a string name, and a value and makes the
 *  named property have value in the given symbol.  Getprop takes
 *  an symbol struct and a string name and returns a the property
 *  value for that name.  Remprop takes an symbol structure and
 *  a property name and removes the property, if it exists, from
 *  the plist for that symbol.
 */
extern int		putprop();
extern struct value	getprop();
extern int		remprop();

/*
 *  Here are some values used so commonly that we just like
 *  to have their values around to copy on demand.  Most of
 *  these are obvious by their names, the value NOVALUE is
 *  special, it is just an all-zero word, returned for ``not
 *  found'' types of errors.
 */
extern struct value	v_t, v_nil, v_zero, v_one, v_four, v_neg, v_null;
extern struct value	NOVALUE;

#endif !_VALUE_
