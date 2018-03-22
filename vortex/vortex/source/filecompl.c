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
 *  RCS Info: $Header: filecompl.c,v 0.1 87/05/01 12:11:06 john Locked $
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
 *  filecompl.c - UNIX file completion list routines
 */
static char _ID[] = "@(#)filecompl.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  For each directory we're interested in we keep one of these
 *  structures.  Each of these contains the last modified time of
 *  the directory and the time the list was made so that we can
 *  check to see if the list is out of date.
 *
 *  These three constants control the maximum sizes of the lists
 *  and total number of characters for file and directory names.
 *  The MAXFILES and MAXDIRS parameters control the maximum length
 *  of the file and directory name lists and the CHARBUF limits
 *  the total number of characters all file and directory names
 *  may occupy.
 *
 *  We allocated up to MAXCOMPL of these structures into the list
 *  dircomp_list before was start reusing the oldest entries in
 *  the list (determined from the dc_rtime).
 */
#define MAXFILES	1000
#define MAXDIRS		100
#define CHARBUF		10240
#define MAXCOMPL	10

struct dircomp {
	char		dc_path[MAXPATHLEN+1];	/* pathname of directory */
	long		dc_mtime;		/* last mtime of directory */
	char		*dc_files[MAXFILES+1];	/* the list of files */
	char		*dc_dirs[MAXDIRS+1];	/* the list of directories */
	char		dc_cbuf[CHARBUF];	/* buffer of characters */
};

struct dircomp	*dircomp_list[MAXCOMPL] = NULL;
static int	dircomp_count = 0;

char **
match_files(partial)
	char	*partial;
{
	extern char	*rindex();
	struct dircomp	*buildcomp();
	static char	*matches[MAXFILES+1];
	extern char	*fixpath();
	char		pbuf[MAXPATHLEN+1];
	struct dircomp	*dcmp;
	char		*dir, *file;
	register char	**str;
	register int	m, len;

	/* make sure we have a valid directory and perhaps file */
	if (partial == NULL || *partial == '\0') {
		dir = ".";
		file = NULL;
	} else {
		strncpy(pbuf, partial, sizeof (pbuf));
		pbuf[sizeof (pbuf) - 1] = '\0';

		/* find the directory and file portions */
		file = rindex(pbuf, '/');
		if (file == NULL) {
			/* no directory specified, use "." */
			dir = ".";
			file = pbuf;
		} else if (file == dir) {
			/* we've got "/file", use "/" */
			dir = "/";
			file++;
		} else {
			/* terminate the directory name */
			dir = pbuf;
			*file++ = '\0';
		}
	}

	/* get or build the directory list */
	if ((dcmp = buildcomp(dir)) == NULL)
		return (NULL);
	if (file == NULL || *file == '\0')
		return (dcmp->dc_files);

	/* find the matches to the given file name */
	for (str = dcmp->dc_files; *str != NULL && **str < *file; str++)
		;
	len = strlen(file);
	m = 0;
	for ( ; *str != NULL && **str == *file && m < MAXFILES; str++) {
		if (!strncmp(*str, file, len))
			matches[m++] = *str;
	}
	matches[m] = NULL;
	return (matches);
}

char **
match_dirs(partial)
	char	*partial;
{
	struct dircomp	*buildcomp();
	extern char	*fixpath();
	static char	*matches[MAXDIRS+1];
	char		pbuf[MAXPATHLEN+1];
	struct dircomp	*dcmp;
	char		*dir, *file;
	register char	**str;
	register int	m, len;

	/* make sure we have a valid directory and perhaps file */
	if (partial == NULL || *partial == '\0') {
		dir = ".";
		file = NULL;
	} else {
		strncpy(pbuf, partial, sizeof (pbuf));
		pbuf[sizeof (pbuf) - 1] = '\0';

		/* find the directory and file portions */
		file = rindex(pbuf, '/');
		if (file == NULL) {
			/* no directory specified, use "." */
			dir = ".";
			file = pbuf;
		} else if (file == dir) {
			/* we've got "/file", use "/" */
			dir = "/";
			file++;
		} else {
			/* terminate the directory name */
			dir = pbuf;
			*file++ = '\0';
		}
	}

	/* get or build the directory list */
	if ((dcmp = buildcomp(dir)) == NULL)
		return (NULL);
	if (file == NULL || *file == '\0')
		return (dcmp->dc_files);

	/* find the matches to the given file name */
	for (str = dcmp->dc_dirs; *str != NULL && **str < *file; str++)
		;
	len = strlen(file);
	m = 0;
	for ( ; *str != NULL && **str == *file && m < MAXDIRS; str++) {
		if (!strncmp(*str, file, len))
			matches[m++] = *str;
	}
	matches[m] = NULL;
	return (matches);
}

compl_files(partial, result, maxlen)
	char	*partial, *result;
{
	extern char	*fixpath();
	extern char	**match_files();
	char		**matches;
	char		bigbuf[2048];
	char		pbuf[MAXPATHLEN+1];
	int		status;
	char		*dir, *file;

	ASSERT(result != NULL && maxlen > 1);

	/* make sure we have a valid directory and perhaps file */
	if (partial == NULL || *partial == '\0') {
		dir = ".";
		file = NULL;
	} else {
		strncpy(pbuf, partial, sizeof (pbuf));
		pbuf[sizeof (pbuf) - 1] = '\0';

		/* find the directory and file portions */
		file = rindex(pbuf, '/');
		if (file == NULL) {
			/* no directory specified, use "." */
			dir = ".";
			file = pbuf;
		} else if (file == dir) {
			/* we've got "/file", use "/" */
			dir = "/";
			file++;
		} else {
			/* terminate the directory name */
			dir = pbuf;
			*file++ = '\0';
		}
	}

	matches = match_files(partial);
	if (matches == NULL || *matches == NULL)
		return (COMPL_NOMATCH);

	*bigbuf = '\0';
	status = compl_expand(file, matches, bigbuf);
	if (status == COMPL_NOMATCH || status == COMPL_NOTUNIQ) {
		/* no completion could have been done */
		*result = '\0';
	} else if (*bigbuf == '\0') {
		/* no completion was done */
		*result = '\0';
	} else {
		strncpy(result, bigbuf, maxlen - 1);
		bigbuf[maxlen - 1] = '\0';
	}
	return (status);
}

compl_dirs(partial, result, maxlen)
	char	*partial, *result;
{
	extern char	*fixpath();
	extern char	**match_dirs();
	char		**matches;
	char		bigbuf[2048];
	char		pbuf[MAXPATHLEN+1];
	int		status;
	char		*dir, *file;

	ASSERT(result != NULL && maxlen > 1);

	/* make sure we have a valid directory and perhaps file */
	if (partial == NULL || *partial == '\0') {
		dir = ".";
		file = NULL;
	} else {
		strncpy(pbuf, partial, sizeof (pbuf));
		pbuf[sizeof (pbuf) - 1] = '\0';

		/* find the directory and file portions */
		file = rindex(pbuf, '/');
		if (file == NULL) {
			/* no directory specified, use "." */
			dir = ".";
			file = pbuf;
		} else if (file == dir) {
			/* we've got "/file", use "/" */
			dir = "/";
			file++;
		} else {
			/* terminate the directory name */
			dir = pbuf;
			*file++ = '\0';
		}
	}

	matches = match_dirs(partial);
	if (matches == NULL || *matches == NULL)
		return (COMPL_NOMATCH);

	*bigbuf = '\0';
	status = compl_expand(file, matches, bigbuf);
	if (status == COMPL_NOMATCH || status == COMPL_NOTUNIQ) {
		/* no completion could have been done */
		*result = '\0';
	} else if (*bigbuf == '\0') {
		/* no completion was done */
		*result = '\0';
	} else {
		strncpy(result, bigbuf, maxlen - 1);
		bigbuf[maxlen - 1] = '\0';
	}
	return (status);
}

static int
compare(sp1, sp2)
	char	**sp1, **sp2;
{
	register char	*p1, *p2;

	ASSERT(sp1 != NULL && (p1 = *sp1) != NULL);
	ASSERT(sp1 != NULL && (p2 = *sp2) != NULL);

	while (*p1 != '\0' && *p2 != '\0' && *p1 == *p2)
		p1++, p2++;
	return (*p1 - *p2);
}

static struct dircomp *
buildcomp(dir)
	char	*dir;
{
	extern char	*alloca();
	DIR		*dirp;
	struct direct	*direct;
	struct dircomp	*dcmp, *old;
	struct stat	stbuf;
	register char	*sbuf, *send;
	register char	*path, *rest;
	register int	d, f, len;

	/*
	 *  See if we can find this directory already.
	 *  We check to make sure we can still stat the directory
	 *  (otherwise it must have been removed) and that it hasn't
	 *  changed (the mtime is not more recent than the saved
	 *  mtime in our record).
	 */
	path = fixpath(dir);
	for (d = 0; d < dircomp_count; d++) {
		dcmp = dircomp_list[d];
		if (!strcmp(dcmp->dc_path, path)) {
			/* make sure the list isn't out-of-date */
			if (stat(path, &stbuf) < 0) {
				*dcmp->dc_path = '\0';
				return (NULL);
			}
			if (stbuf.st_mtime > dcmp->dc_mtime) {
				*dcmp->dc_path = '\0';
				dcmp = NULL;
				break;
			}
			return (dcmp);
		}
	}
	if (d >= dircomp_count)
		dcmp = NULL;

	/*
	 *  We know that we have to find a free element.  First
	 *  of all, we look for an element that is already free.
	 *  If that doesn't get results, we make a new element
	 *  unless we already have MAXCOMPLY elements, in which
	 *  case we dispose of the oldest one and use its place.
	 */
	if (dcmp == NULL) {
		/* try to find a free element in the list */
		for (d = 0; d < dircomp_count; d++) {
			dcmp = dircomp_list[d];
			if (*dcmp->dc_path == '\0')
				break;
		}
		if (d >= dircomp_count)
			dcmp = NULL;
	}
	if (dcmp == NULL && dircomp_count < MAXCOMPL) {
		/* make a new entry on the list */
		dircomp_list[dircomp_count] = (struct dircomp *)
		    valloc(sizeof (struct dircomp));
		dcmp = dircomp_list[dircomp_count];
		dircomp_count++;
	}
	if (dcmp == NULL) {
		/* find oldest existing element in list */
		old = NULL;
		for (d = 0; d < dircomp_count; d++) {
			dcmp = dircomp_list[d];
			if (old == NULL || dcmp->dc_mtime < old->dc_mtime)
				old = dcmp;
		}
		dcmp = old;
	}
	ASSERT(dcmp != NULL);

	/*
	 *  Now read all the elements of the directory into lists
	 *  of all the elements (the files) and only the directories.
	 *  All this is done with the storage in the (struct dircomp)
	 *  and if we run out of space, we won't get a complete list.
	 */
	bzero(dcmp, sizeof (dcmp));
	debug(DINPUT, "Building completions list for \"%s\"...", path);

	if (stat(path, &stbuf) < 0 || (dirp = opendir(path)) == NULL) {
		message("(can't open directory \"%s\" for completion)", path);
		return (NULL);
	}

	PROTECT();
	strncpy(dcmp->dc_path, path, sizeof (dcmp->dc_path) - 1);
	dcmp->dc_mtime = time(NULL);

	/* set up string buffer pointers */
	sbuf = dcmp->dc_cbuf;
	send = sbuf + sizeof (dcmp->dc_cbuf);

	/* set up the path name for stat(2) */
	d = strlen(dcmp->dc_path);
	path = alloca(d + MAXPATHLEN + 2);
	strcpy(path, dcmp->dc_path);
	rest = path + d;
	*rest++ = '/';

	d = f = 0;
	for (direct = readdir(dirp); direct != NULL; direct = readdir(dirp)) {
		/* see what kind of file this is */
		len = direct->d_namlen;
		strncpy(rest, direct->d_name, len);
		rest[len] = '\0';
		if (stat(path, &stbuf) < 0)
			continue;

		if (f < MAXFILES) {
			/* insert this into the list of files */
			if (sbuf + len + 1 >= send)
				break;
			dcmp->dc_files[f] = sbuf;
			strncpy(sbuf, direct->d_name, len);
			sbuf += len;
			*sbuf++ = '\0';
			f++;
		}

		if (d < MAXDIRS && (stbuf.st_mode & S_IFMT) == S_IFDIR) {
			/* insert this into the list of directories */
			if (sbuf + len + 1 >= send)
				break;
			dcmp->dc_dirs[d] = sbuf;
			strncpy(sbuf, direct->d_name, len);
			sbuf += len;
			*sbuf++ = '\0';
			d++;
		}
	}
	dcmp->dc_files[f] = NULL;
	dcmp->dc_dirs[d] = NULL;
	closedir(dirp);

	if (f > 0)
		qsort(dcmp->dc_files, f, sizeof (char **), compare);
	if (d > 0)
		qsort(dcmp->dc_dirs, d, sizeof (char **), compare);
	UNPROTECT();

	return (dcmp);
}
