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
 *  RCS Info: $Header: docstr.c,v 0.1 87/05/01 11:41:58 john Locked $
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
 *  docstr.c - internal document string routines
 */
static char _ID[] = "@(#)docstr.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include "vse.h"
#include "vlisp.h"
#include "docstr.h"

/*
 *  We do hashing on documentation strings.  We do an alphabetic
 *  hash on the name, for fast lookup when searching out an exact
 *  match.  When looking for a regular expression match, we have
 *  to search the whole table linearly anyway.
 */
static struct docent	*doc_table[DOCENT_TABSIZE];
static int		doc_tabsize = NITEMS(doc_table);

/*
 *  There are a fixed number of possible documentation string
 *  files that can be loaded, DOCFILE_TABSIZE.  This includes
 *  reading the same file in twice.  However, there should
 *  probably be no more than one docstr file anyway (the
 *  standard one and possily one documenting the user's
 *  own functions).
 */
static struct docfile	file_table[DOCFILE_TABSIZE];
static int		file_tabsize = NITEMS(file_table);

/*
 *  This function tries to retrieve a (struct docstr) for the
 *  name given.  This name must match a key exactly for it to
 *  be found.  A (struct docstr) is built up from the information
 *  in the file associated with the key in the memory hash table
 *  and this docstr is returned.  The docstr points to static
 *  storage, multiple calls to get_exact or get_regexp will
 *  overwrite the data.
 */
extern struct docstr	*docstr_find();

struct docstr *
get_exact(name)
	struct string	*name;
{
	struct docent	*ent;
	int		index;

	if (name == NULL || name->st_length < 1) {
		ierror("get_exact: Null name argument!");
		return (NULL);
	}

	/* search for the matching entry */
	index = hashindex(name);
	for (ent = doc_table[index]; ent != NULL; ent = ent->de_next)
		if (sequal(ent->de_key, name))
			break;

	/* return the entry we found, or NULL */
	if (ent != NULL && sequal(ent->de_key, name))
		return docstr_find(ent);
	else
		return (NULL);
}

/*
 *  This function finds all the entries in the documentation
 *  string hash table that match the regular expression pattern
 *  given here.  Since this may be more than one, it only returns
 *  the first match directly, next_regexp() returns the next match,
 *  resuming searching from where the last one was found.
 *
 *  The static pointers, get_lchain and get_lentry point to the
 *  hash chain and hash entry where the last match was found.
 *  These variables are used to locate the place to continue
 *  searching when next_regexp is called.  Note that next_regep
 *  doesn't call re_comp again, since it assumes that was just
 *  done by get_regexp, so it doesn't need to know the pattern.
 *
 *  Once we fond a pattern match, get_exact, get_regexp and next_regexp
 *  call docstr_find to read and build up the (struct docstr) to
 *  return.  Docstr_find builds the docstr from static storage,
 *  so multiple calls will overwrite previous data.
 */
static struct docent	**get_lchain, *get_lentry;

struct docstr *
get_regexp(pat)
	char	*pat;
{
	extern char	*re_comp();
	char		*err;
	char		cpat[SMALLBUF];

	if (pat == NULL || *pat == '\0') {
		ierror("get_regexp(0x%x): Bad pattern argument!", pat);
		return (NULL);
	}

	/* compile the regular expression pattern */
	makecstring(pat, cpat, sizeof (cpat));
	if ((err = re_comp(cpat)) != NULL) {
		error("Bad regular expression: %s.", err);
		get_lchain = NULL;
		return (NULL);
	}

	get_lchain = doc_table;
	get_lentry = *get_lchain;

	return next_regexp();
}

struct docstr *
next_regexp()
{
	struct docent	*ret;
	char		nbuf[SMALLBUF];

	if (get_lchain == NULL || get_lchain >= doc_table + doc_tabsize)
		error("Not set up for next regular expression match!");

	/* search out the next pattern match */
	if (get_lentry == NULL)
		get_lchain++;
	while (get_lchain < doc_table + doc_tabsize) {
		if (get_lentry == NULL)
			get_lentry = *get_lchain;
		while (get_lentry != NULL) {
			makecstring(get_lentry->de_key, nbuf, sizeof (nbuf));
			if (re_exec(nbuf) == 1) {
				ret = get_lentry;
				get_lentry = get_lentry->de_next;
				return docstr_find(ret);
			}
			get_lentry = get_lentry->de_next;
		}
		get_lchain++;
	}

	/* not another entry found */
	get_lchain = NULL;
	return (NULL);
}

/*
 *  DOCUMENTATION
 *
 *  Name: docstr-last-match
 *  Desc: This global variable holds the last value returned
 *	by \sym{docstr-find-match} or \sym{docstr-find-regexp}
 *	and \sym{docstr-next-regexp (as well as internal accesses
 *	which use those functions, like \sym{apropos} and \sym{info}).
 *
 *	The variable will be set to a list with six elements.
 *	The first five elements will be strings and the last will
 *	be a list of strings (or possibly a nil list).  Each string
 *	represents a field of the documentation string format that
 *	documents functions and variables of vlisp.  The fields
 *	are represented symbolically as:
 *
 *	\tab{NAME	string name of entry
 *	CALL	string calling sequence (if function)
 *	RETU	string return value type (if function)
 *	DESC	string description of function/variable
 *	SIDE	string side effects (if applicable)
 *	SEEA	list of strings which are see also references}
 *
 *	All fields except \lit{NAME} and \lit{DESC} are optional,
 *	and if they don't occur in the documentation, they will be
 *	represented as empty strings in the list.  Except for the
 *	\lit{SEEA} list, which will be nil if there are no ``see also''
 *	references.
 *  Side: This variable will change whenever a document string
 *	is accessed internally.  This may be through some builtin
 *	function such as \sym{apropos} or through user access such
 *	as \sym{docstr-find-match}.
 *  SeeA: docstr-find-match docstr-find-regexp
 */
MKSTRING(DOCSTRVAR_NAME, "docstr-last-match");

/*
 *  This function does the actual work of reading in a document
 *  string matching the given entry.  We return a pointer to
 *  static storage, which will be overwritten by subsequent
 *  calls to docstr_find.
 *
 *  This function also sets the value of the global variable
 *  docstr-last-match to a list containing the strings as
 *  described above under the documentation for docstr-last-match.
 */
static struct docstr	docstr_buf;

#define SEPCHAR		'\005'

static struct docstr *
docstr_find(ent)
	struct docent	*ent;
{
	struct docfile	*dfil;
	FILE		*filep;
	struct stat	stbuf;
	char		lbuf[BIGBUF];
	register char	*bp, *ep;
	register int	i, len, in;
	struct docstr	*docp = &docstr_buf;
	struct string	*null;
	struct value	list, seea, sval;

	dfil = &file_table[ent->de_file];
	if (stat(dfil->df_path, &stbuf) < 0 ||
	    (filep = fopen(dfil->df_path, "r")) == NULL)
		perror("Can't read docstr file %s anymore", dfil->df_path);
	if (dfil->df_size != stbuf.st_size) {
		fclose(filep);
		error("The docstr file %s changed size!", dfil->df_path);
	}
	if (dfil->df_mtime < stbuf.st_mtime)
		message("warning: Docstr file %s modified since read.",
		    dfil->df_path);
	fseek(filep, ent->de_offset, 0);

	/* zero the structure just to be safe */
	bzero(docp, sizeof (struct docstr));

	/* check for end-of-file (or error) */
	if (getc(filep) != '@' || (in = getc(filep)) == '@')
		goto badone;

	/* read in the name line */
	if ((len = scan_docline(filep, lbuf+1, sizeof (lbuf) - 1)) < 0)
		goto badone;
	*lbuf = in;
	docp->ds_name = save_string(lbuf, len + 1);

	/* read in calling sequence line */
	if ((len = scan_docline(filep, lbuf, sizeof (lbuf))) > 0)
		docp->ds_call = save_string(lbuf, len);

	/* read in return value line */
	if ((len = scan_docline(filep, lbuf, sizeof (lbuf))) > 0)
		docp->ds_retu = save_string(lbuf, len);

	/* read in the description line */
	if ((len = scan_docline(filep, lbuf, sizeof (lbuf))) < 1)
		goto badone;
	docp->ds_desc = save_string(lbuf, len);

	/* read in side effects line */
	if ((len = scan_docline(filep, lbuf, sizeof (lbuf))) > 0)
		docp->ds_side = save_string(lbuf, len);

	/* read in cross reference array */
	if ((len = scan_docline(filep, lbuf, sizeof (lbuf))) > 0) {
		lbuf[len] = '\0';
		bp = ep = lbuf;
		i = 0;
		while (*bp != '\0' && i < MAXXREF) {
			while (*ep != SEPCHAR && *ep != '\0')
				ep++;
			docp->ds_xref[i] = save_string(bp, ep - bp);
			if (*ep != '\0')
				ep++;
			bp = ep;
			i++;
		}
	}

	/* read in see also array */
	if ((len = scan_docline(filep, lbuf, sizeof (lbuf))) > 0) {
		lbuf[len] = '\0';
		bp = ep = lbuf;
		i = 0;
		while (*bp != '\0' && i < MAXSEEA) {
			while (*ep != SEPCHAR && *ep != '\0')
				ep++;
			docp->ds_seea[i] = save_string(bp, ep - bp);
			if (*ep != '\0')
				ep++;
			bp = ep;
			i++;
		}
	}
	fclose(filep);

	/*
	 *  Set up the global variable docstr-last-match.
	 *  We work backward through the list of fields (all of
	 *  the except the cross references) and cons them
	 *  into the list we'll eventually set to the global
	 *  variable.
	 */
	list = v_nil;
	sval.vl_type = LISP_STRING;
	null = save_string(NULL, 0);

	/* the ``see also'' references */
	seea = v_nil;
	if (docp->ds_seea[0] != NULL) {
		for (i = MAXSEEA - 1; i >= 0 && docp->ds_seea[i] == NULL; i--)
			;
		while (i >= 0) {
			sstring(sval.vl_data, docp->ds_seea[i]);
			seea = cons(sval, seea);
			i--;
		}
	}
	list = cons(seea, list);

	/* the side effects string */
	if (docp->ds_side == NULL)
		sstring(sval.vl_data, null);
	else
		sstring(sval.vl_data, docp->ds_side);
	list = cons(sval, list);

	/* the description string */
	if (docp->ds_desc == NULL)
		sstring(sval.vl_data, null);
	else
		sstring(sval.vl_data, docp->ds_desc);
	list = cons(sval, list);

	/* the return value string */
	if (docp->ds_retu == NULL)
		sstring(sval.vl_data, null);
	else
		sstring(sval.vl_data, docp->ds_retu);
	list = cons(sval, list);

	/* the call sequence string */
	if (docp->ds_call == NULL)
		sstring(sval.vl_data, null);
	else
		sstring(sval.vl_data, docp->ds_call);
	list = cons(sval, list);

	/* the name string */
	if (docp->ds_name == NULL)
		sstring(sval.vl_data, null);
	else
		sstring(sval.vl_data, docp->ds_name);
	list = cons(sval, list);

	/* set the variable */
	setglobal(DOCSTRVAR_NAME, list, FLAG_NONE);

	return (docp);

badone:	fclose(filep);
	error("Bad format in docstr file %s now!", dfil->df_path);
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: load-docstr-file
 *  Call: (load-docstr-file 'file [ 'silent ])
 *  Retu: fixnum
 *  Desc: This function loads in the list of documentation strings
 *	from the filename that is its argument.  This file must be
 *	readable, and must have been created by the \em{mkdoc} utility
 *	(all this is done automatically for properly set-up systems).
 *	It returns the number of documentation strings found.  It is
 *	an error for the file not to be readable or to be of the
 *	wrong format.  If the optional second argument is present and
 *	non-nil, no messagees are printed while the file is being loaded.
 *
 *	Documentation string descriptions and related information can
 *	be printed out as help information with the \sym{apropos} command
 *	and others which provide on-line help.  This information can
 *	also be used to prepare a typeset manual.  For more information
 *	on the preparation of documentation string database files, see
 *	the \em{mkdoc} program that comes with VorTeX.
 *
 *	Documentation strings are actually more complicated than just
 *	strings.  They have elements that can be separately accessed
 *	for various functions as well a description string which tells
 *	what the symbol is.  Every documentation string has a name
 *	and a description, the name is used to access the description
 *	(and other optional information) in a hash table separate
 *	separate from that of the lisp symbol table.  Since descriptions
 *	are kept separately from symbols, so one may have document
 *	string names that are a superset of actual symbols at any one
 *	time.
 *  Side: Documentation strings are not kept in memory; a table of
 *	references to positions in the given file is built.  This
 *	means that the file must remain readable and not be changed
 *	once it has been loaded.
 *  Xref: docstr documentation
 *  SeeA: apropos
 */

static FILE	*doc_filep = NULL;
static int	old_errline;
static char	*old_errfile;

extern int	errline;
extern char	*errfile;

static struct value
docstr_cleanup()
{
	if (doc_filep != NULL) {
		fclose(doc_filep);
		doc_filep = NULL;
	}
	errline = old_errline;
	errfile = old_errfile;
	return (v_zero);
}

DEFUN(doloaddoc, "load-docstr-file", FLAG_NONE, "FDOCSTR filename: ")
{
	struct value	docstr_cleanup();
	struct value	arg, ret;
	char		*path, pbuf[STRBUF];
	char		lbuf[BIGBUF];
	register int	count, start, current;
	register FILE	*filep;
	struct docent	*dent, *xref;
	struct docfile	*dfil;
	struct stat	stbuf;
	struct string	*name;
	register char	*bp, *ep;
	register int	len, in;
	int		silent = FALSE;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a file name string");
	name = gstring(arg.vl_data);
	makecstring(name, pbuf, sizeof (pbuf));
	path = fixpath(pbuf);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (truep(arg))
			silent = TRUE;
	}

	/* check to make sue we can handle another file */
	for (dfil = file_table; dfil < file_table + file_tabsize; dfil++)
		if (dfil->df_entries <= 0)
			break;
	if (dfil >= file_table + file_tabsize)
		error("Too many docstr files loaded already!");

	if (stat(path, &stbuf) < 0 || (filep = fopen(path, "r")) == NULL)
		perror("Can't open docstr file %s", path);

	/* set unwind-protect function */
	uwprotect(docstr_cleanup);
	doc_filep = filep;

	if (fgets(lbuf, sizeof (lbuf), filep) == NULL)
		error("Reputed docstr file %s is empty!", path);
	if (strncmp(lbuf, "DOCSTR", 6) != 0 ||
	    fgets(lbuf, sizeof (lbuf), filep) == NULL ||
	    strncmp(lbuf, "@@", 2) != 0) {
		/* bad format preface */
		error("File %s doesn't seem to contain docstrs!", path);
	}

	if (!silent)
		message("Loading docstrs from %s...", path);

	/* make up entry in document file table */
	strncpy(dfil->df_path, path, sizeof (dfil->df_path) - 1);
	path[sizeof (dfil->df_path) - 1] = '\0';
	dfil->df_entries = 0;
	dfil->df_mtime = stbuf.st_mtime;
	dfil->df_size = stbuf.st_size;

	count = 0;
	start = ftell(filep);
	while (!feof(filep)) {
		/* scan name of next entry */
		if (getc(filep) != '@' || (in = getc(filep)) == '@')
			break;
		*lbuf = in;
		len = scan_docline(filep, lbuf+1, sizeof (lbuf) - 1);
		if (len < 0)
			break;
		name = save_string(lbuf, len + 1);

		/* ignore calling sequence */
		scan_docline(filep, NULL, 0);
		/* ignore return value */
		scan_docline(filep, NULL, 0);
		/* ignore description */
		scan_docline(filep, NULL, 0);
		/* ignore side efects */
		scan_docline(filep, NULL, 0);
		/* save cross references */
		scan_docline(filep, lbuf, sizeof (lbuf));
		/* ignore see also references */
		scan_docline(filep, NULL, 0);

		/* finished reading the entry */
		count++;
		current = ftell(filep);

		/* convert docstr to primary entry */
		dent = (struct docent *)valloc(sizeof (struct docent));
		dent->de_key = name;
		dent->de_file = (dfil - file_table);
		dent->de_length = current - start;
		dent->de_offset = start;
		insert_entry(dent, FALSE);
	
		/* convert cross references if there are any */
		bp = ep = lbuf;
		while (*bp != '\0') {
			while (*ep != SEPCHAR && *ep != '\0')
				ep++;

			/* insert this cross reference */
			xref = (struct docent *)valloc(sizeof (struct docent));
			bcopy(dent, xref, sizeof (struct docent));
			xref->de_key = save_string(bp, ep - bp);
			insert_entry(dent, TRUE);

			/* advance to next element in list */
			if (*ep != '\0')
				ep++;
			bp = ep;
		}

		/* on to next entry ... */
		start = current;
	}
	dfil->df_entries = count;

	if (!silent)
		message("Loading docstrs from %s...done (found %d).",
		    path, count);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: docstr-find-match
 *  Call: (docstr-find-match 'topic)
 *  Retu: list
 *  Desc: This function searches out the documentation stored
 *	under the given topic (which must be an exact match).
 *	The argument must evaluate to a string or a symbol.
 *	If the topic is found, t is returned, otherwise this
 *	function returns nil.
 *  Side: The value of the documentation is put into the global
 *	variable \sym{docstr-last-match} as described under
 *	the documentation for that variable.
 *  SeeA: load-docstr-file docstr-find-regexp docstr-last-match
 */

DEFUN(dofindmatch, "docstr-find-match", FLAG_NONE, NULL)
{
	struct value	arg;
	struct string	*name;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_STRING:
		name = gstring(arg.vl_data);
		break;
	case LISP_SYMBOL:
		name = gsymbol(arg.vl_data)->sy_pname;
		break;
	default:
		BADARGN(1, "a string topic");
	}

	if (get_exact(name) == NULL)
		return (v_nil);
	else
		return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: docstr-find-regexp
 *  Call: (docstr-find-regexp 'pattern)
 *  Retu: t or nil
 *  Desc: This function searches out all documentation whose
 *	name is matched by the regular expression pattern.
 *	The argument must evaluate to a string or a symbol.
 *	If one or more matches are found, the function returns
 *	t, otherwise it returns nil.  Since there is likely to
 *	be more than one match for the pattern, the user should
 *	call \sym{docstr-next-regexp} after the call to
 *	\sym{docstr-find-regexp} until it returns nil.
 *  Side: The value of the documentation is put into the global
 *	variable \sym{docstr-last-match} as described under
 *	the documentation for that variable.
 *  SeeA: load-docstr-file docstr-next-regexp docstr-last-match
 */

DEFUN(dofindregexp, "docstr-find-regexp", FLAG_NONE, NULL)
{
	struct value	arg;
	struct string	*pat;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_STRING:
		pat = gstring(arg.vl_data);
		break;
	case LISP_SYMBOL:
		pat = gsymbol(arg.vl_data)->sy_pname;
		break;
	default:
		BADARGN(1, "a string pattern");
	}

	if (get_regexp(pat) == NULL)
		return (v_nil);
	else
		return (v_t);
}

/*
 *  DOCUMENTATION
 *
 *  Name: docstr-next-regexp
 *  Call: (docstr-next-regexp)
 *  Retu: t or nil
 *  Desc: This function finds the next match to the pattern
 *	most previously given to \sym{docstr-find-regexp}.
 *	If another match is found, t is returned, otherwise this
 *	function returns nil.
 *  Side: The value of the documentation is put into the global
 *	variable \sym{docstr-last-match} as described under
 *	the documentation for that variable.
 *  SeeA: load-docstr-file docstr-find-regexp docstr-last-match
 */

DEFUN(donextregexp, "docstr-next-regexp", FLAG_NONE, NULL)
{
	CHECKAC(0, 0);

	if (next_regexp() == NULL)
		return (v_nil);
	else
		return (v_t);
}

static int
scan_docline(filep, lbuf, size)
	register FILE	*filep;
	char		*lbuf;
	register int	size;
{
	register int	len, in;
	register char	*cp;

	cp = lbuf;
	len = 0;
	while (len < size) {
		in = getc(filep);
		if (in == '\\')
			in = getc(filep);
		else if (in == '\n')
			break;
		*cp++ = in;
		len++;
	}

	/* flush rest of line if necessary */
	while (in != '\n') {
		if (in == '\\')
			in = getc(filep);
		in = getc(filep);
	}

	return (len);
}

static int
insert_entry(new, noclobber)
	struct docent	*new;
{
	struct docent	*next, *last, **begin;
	struct string	*key;

	last = NULL;
	key = new->de_key;
	begin = &doc_table[hashindex(key)];
	for (next = *begin; next != NULL; next = next->de_next) {
		if (sequal(next->de_key, key))
			break;
		last = next;
	}
	/* we try to save the old key */
	if (next != NULL && !strcmp(next->de_key, key)) {
		if (noclobber)
			return (1);
		/* unlink it, and free the storage */
		free(next);
		next = next->de_next;
	}
	if (last == NULL)
		*begin = new;
	else
		last->de_next = new;
	new->de_next = next;

	return (0);
}

/*
 *  This hash algorithm is trivial.  We simply have a hash
 *  category for each letter of the alphabet and one for words
 *  that begin with non-letters.  This is to facilitate
 *  alphabetization.
 */

static int
hashindex(str)
	struct string	*str;
{
	register int	index, first;

	first = *str->st_buffer;
	if (islower(first))
		index = first - 'a' + 1;
	else if (isupper(first))
		index = first - 'A' + 1;
	else
		index = 0;

	return (index);
}
