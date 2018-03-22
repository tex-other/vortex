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
 *  RCS Info: $Header: docstr.h,v 0.1 87/04/30 20:52:13 john Locked $
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
 *  docstr.h - document string data structures
 */
 
#ifndef _DOCSTR_
#define _DOCSTR_

/*
 *  This is the struct which holds information used by the
 *  info (or help) facilities.  This is general purpose,
 *  help info in accessed by name, which returns a structure
 *  containing the fields of information as given by the
 *  author in source comments.  This programmer documentation
 *  on the builtin symbols is extracted from the source files
 *  (both C and vlisp) and built into a database by the mkdoc
 *  program.
 */
struct docstr {
	struct string	*ds_name;	/* name accessed by */
	struct string	*ds_call;	/* calling sequence */
	struct string	*ds_retu;	/* return value type */
	struct string	*ds_desc;	/* general description */
	struct string	*ds_side;	/* side effects */
	struct string	*ds_errs;	/* possible errors */
	struct string	*ds_xref[10];	/* cross references */
	struct string	*ds_seea[10];	/* see also references */
	struct docstr	*ds_next;	/* next in list */
};
#define MAXXREF		10		/* size of ds_xref array */
#define MAXSEEA		10		/* size of ds_seea array */

extern struct docstr	*get_exact(), *get_regexp(), *next_regexp();

/*
 *  We do hashing on documentation strings.  We do an alphabetic
 *  has on the name, for fast lookup when searching out an exact
 *  match.  When looking for a regular expression match, we have
 *  to search the whole table linearly anyway.
 *
 *  When we load a DOCSTR file, we add to this hash table the
 *  entries in that file and add a reference to that file to the
 *  file table.  When we want to retrieve a documentation string,
 *  we find it in the hash table, then read the information from
 *  the file indexed by that entry, de_offset bytes from the beginning
 *  of the file and de_length bytes long.
 */
struct docent {
	struct string	*de_key;	/* documentation key string */
	short		de_file;	/* entry in DOCSTR file table */
	short		de_length;	/* length of entry in file */
	long		de_offset;	/* offset into file of start */
	struct docent	*de_next;	/* next entry in hash chain */
};

struct docfile {
	long		df_entries;	/* number of entries found */
	char		df_path[1024];	/* full pathname of file */
	long		df_mtime;	/* modified time of file */
	long		df_size;	/* length of file (in bytes) */
};

#define DOCENT_TABSIZE		27	/* size of entry hash table */
#define DOCFILE_TABSIZE		25	/* size of docstr file table */

#endif !_DOCSTR_
