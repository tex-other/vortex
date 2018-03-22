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
 *  RCS Info: $Header: dostat.c,v 0.1 87/05/01 12:03:16 john Locked $
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
 *  dostat.c - return information about UNIX files
 */
static char _ID[] = "@(#)dostat.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/param.h>
#include <sys/stat.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: stat
 *  Call: (stat 'filename)
 *  Retu: symbol
 *  Desc: This function returns the \sc{UNIX} filesystem information
 *	for the named file, broken down into fields similar to the
 *	C \em{stat(2)} system call.  The argument must evaluate to
 *	a string which contains the path of an existing file (or a
 *	directory).  The function actualy returns a symbol created by
 *	\sym{gensym} whose property list contains a property name/value
 *	pair field for each field of the stat information as described
 *	below.
 *
 *	\tab{inode	fixnum which is the inode number on filesystem
 *	type	type of file/directory, see below
 *	mode	fixnum of mode bits (see \sym{chmod})
 *	uid	fixnum id of owning user
 *	gid	fixnum id of owning group
 *	size	fixnum size of the file in bytes
 *	atime	last access time (in \sym{time} format)
 *	mtime	last modified time (in \sym{time} format)
 *	ctime	last inode change time (in \sym{time} format)}
 *
 *	All of the fields above are the same as those described in
 *	\em{stat(2)}.  The field type, which doesn't appear in
 *	the normal \em{stat(2)} struct, is a symbol with a
 *	print name which is one of:
 *
 *	\tab{f	a regular file
 *	d	a directory
 *	c	a character special file (usually a terminal port)
 *	b	a block special file (a \sc{UNIX} filesystem)
 *	l	a symbolic link (information is for object of link)
 *	s	a socket (for \sc{UNIX} domain)}
 *
 *	The typical way to access one of these fields is illustrated
 *	by the code fragment below which defines a function to return
 *	the size (in bytes) of a file.
 *
 *	\tab{\lit{(defun file-size (fname)}
 *	\lit{  (let ((sret nil))}
 *	\lit{    (setq sret (stat fname))}
 *	\lit{    (getprop sret 'size)))}}
 *
 *	The most interesting thing here is that we're using the
 *	property list feature to implement aggregate data structures.
 *	In fact, the \lit{atime}, \lit{mtime} and \lit{ctime} fields
 *	are similar aggregate data structures implemented with property
 *	lists, see \sym{time}.
 *
 *	There is an additional field if the file being examined
 *	is a symbolic link, \lit{link}.  The value of the \lit{link}
 *	property is the pathname that the system will reference
 *	when one tries to reference the symbolic link.  This link
 *	property is \em{only} created when the object in question
 *	is a symbolic link (created with the \em{symlink(2)} system
 *	call).  This call uses the \em{lstat} system call rather
 *	than plain \em{stat}, to facilitate this.
 *  Side: A new symbol is created with a substantial property list
 *	which requires some space to store.
 *  SeeA: fopen getprop getplist time gensym
 */
MKSTRING(STATLEADER, "%stat");

DEFUN(dostat, "stat", FLAG_NONE, NULL)
{
	struct string	*sgensym();
	struct value	vtime();
	struct value	arg, ret, plist;
	struct value	pname, value;
	struct symbol	*sym;
	struct stat	stbuf;
	char		pbuf[MAXPATHLEN], *path;
	char		lbuf[MAXPATHLEN-1];
	int		len;

	CHECKAC(1, 1);
	/* get file name argument */
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");
	makecstring(gstring(arg.vl_data), pbuf, sizeof (pbuf));
	path = fixpath(pbuf);

	/* stat the file, gte info */
	if (lstat(path, &stbuf) < 0)
		perror("Can't stat %s", path);

	/*
	 *  Make up property list from fields.  It would be
	 *  really nice if there was some way to do this in
	 *  some more intelligent way, but I can't think of
	 *  one that's easier than doing it like this...
	 */
	plist = v_nil;

	if ((stbuf.st_mode & S_IFMT) == S_IFLNK) {
		sym = save_symbol(save_string("link", 4));
		pname.vl_type = LISP_SYMBOL;
		ssymbol(pname.vl_data, sym);
		value.vl_type = LISP_STRING;
		if (readlink(path, lbuf, sizeof (lbuf) - 1) < 0)
			perror("Can't readlink of %s", path);
		lbuf[sizeof (lbuf) - 1] = '\0';
		len = strlen(lbuf);
		sstring(value.vl_data, save_string(lbuf, len));
		plist = cons(pname, cons(value, plist));
	}

	/* inode change time */
	sym = save_symbol(save_string("ctime", 5));
	pname.vl_type = LISP_SYMBOL;
	ssymbol(pname.vl_data, sym);
	value = vtime(stbuf.st_ctime);
	plist = cons(pname, cons(value, plist));

	/* file modified time */
	sym = save_symbol(save_string("mtime", 5));
	ssymbol(pname.vl_data, sym);
	value = vtime(stbuf.st_mtime);
	plist = cons(pname, cons(value, plist));

	/* file access time */
	sym = save_symbol(save_string("atime", 5));
	ssymbol(pname.vl_data, sym);
	value = vtime(stbuf.st_atime);
	plist = cons(pname, cons(value, plist));

	/* file size (in bytes) */
	sym = save_symbol(save_string("size", 4));
	ssymbol(pname.vl_data, sym);
	value.vl_type = LISP_FIXNUM;
	sfixnum(value.vl_data, stbuf.st_size);
	plist = cons(pname, cons(value, plist));

	/* gid of owning group, a fixnum */
	sym = save_symbol(save_string("gid", 3));
	ssymbol(pname.vl_data, sym);
	value.vl_type = LISP_FIXNUM;
	sfixnum(value.vl_data, stbuf.st_uid);
	plist = cons(pname, cons(value, plist));

	/* uid of owner, a fixnum */
	sym = save_symbol(save_string("uid", 3));
	ssymbol(pname.vl_data, sym);
	value.vl_type = LISP_FIXNUM;
	sfixnum(value.vl_data, stbuf.st_uid);
	plist = cons(pname, cons(value, plist));

	/* mode of file, permissions */
	sym = save_symbol(save_string("mode", 4));
	ssymbol(pname.vl_data, sym);
	value.vl_type = LISP_FIXNUM;
	sfixnum(value.vl_data, stbuf.st_mode & 07777);
	plist = cons(pname, cons(value, plist));

	/* type of file, a symbol */
	sym = save_symbol(save_string("type", 4));
	ssymbol(pname.vl_data, sym);
	value.vl_type = LISP_SYMBOL;
	switch (stbuf.st_mode & S_IFMT) {
	case S_IFDIR:
		sym = save_symbol(save_string("d", 1));
		break;
	case S_IFCHR:
		sym = save_symbol(save_string("c", 1));
		break;
	case S_IFBLK:
		sym = save_symbol(save_string("b", 1));
		break;
	case S_IFLNK:
		sym = save_symbol(save_string("l", 1));
		break;
	case S_IFSOCK:
		sym = save_symbol(save_string("s", 1));
		break;
	default:
		sym = save_symbol(save_string("f", 1));
		break;
	}
	ssymbol(value.vl_data, sym);
	plist = cons(pname, cons(value, plist));

	/* file inode number */
	sym = save_symbol(save_string("inode", 5));
	ssymbol(pname.vl_data, sym);
	value.vl_type = LISP_FIXNUM;
	sfixnum(value.vl_data, stbuf.st_ino);
	plist = cons(pname, cons(value, plist));

	/* make up symbol struct and return the new symbol */
	sym = save_symbol(sgensym(STATLEADER));
	sym->sy_plist = plist;
	ret.vl_type = LISP_SYMBOL;
	ssymbol(ret.vl_data, sym);
	return (ret);
}
