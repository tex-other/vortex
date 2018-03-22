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
 *  RCS Info: $Header: syms.h,v 0.1 87/02/22 01:18:45 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the utility program mksyms which
 *  builds the standard symbols for the source editor of VorTeX,
 *  written by John Coker under Prof. Michael A. Harrison of
 *  the University of California at Berkeley.
 *
 *  (c) 1987  John Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *
 *  All rights reserved by the author.  See the copyright
 *  notice distributed with this software for the complete
 *  description of the conditions under which this software
 *  is made available.
 *
 *  syms.h - include file for symbol data base programs
 */
 
#include <stdio.h>
#include <ctype.h>
#include <ndbm.h>
#include <sys/file.h>
#include "value.h"
#include "symtab.h"

/*
 *  Symbol entries from the database are read in internally
 *  and multiple references collapsed.  This way, we can later
 *  regenerate the database without these multiple references
 *  and we can generate an optimal C source file to declare
 *  and initialize the symbols.
 */
struct entry {
#ifdef FILENAMES
	char		en_sfile[64];	/* file name of source */
#endif FILENAMES
	short		en_what;	/* what type of entry */
	short		en_externed;	/* already delclared it extern */
	char		en_name[64];	/* type dependent (C or Lisp) name */
	char		en_flags[36];	/* flags, type or length field */
	char		en_value[128];	/* type dependent value */
	struct entry	*en_next;	/* next in list */
};

#define ENT_VALUE	1	/* initialze a (struct value) */
#define ENT_SYMBOL	2	/* setup a builtin symbol */
#define ENT_STRING	3	/* initialize a predefined string */

/*
 *  These two variables should reflect the position in the
 *  current input file where reading is being done.  This
 *  way, error() can print informative error messages.
 *  These variables need to be updates also by the C pre-
 *  processor ``# line file'' directives if they exist.
 */
extern char	*errfile;
extern int	errline;

/*
 *  The mode to open the dbm database when it is created.
 *  This is actually anded with the umack(2) of the current
 *  user, hopefully though the database will turn out to
 *  be group-writeable.
 */
#define DBMODE	0664
