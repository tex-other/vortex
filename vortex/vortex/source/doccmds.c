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
 *  RCS Info: $Header: doccmds.c,v 0.1 87/05/01 11:36:46 john Locked $
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
 *  doccmds.c - editor document functions
 */
static char _ID[] = "@(#)doccmds.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "gl_comm.h"
#include "ts_comm.h"
#include "document.h"

struct document	*document_list = NULL;		/* list of active documents */
struct docfile	doc_files[MAXDFILES];		/* table of document files */

struct document *
getdocument(file)
{
	struct document	*docp;
	register int	f;

	/* search for this file ID in the list of documents */
	for (docp = document_list; docp != NULL; docp = docp->dc_next) {
		if (docp->dc_rootID == file)
			return (docp);
		for (f = 0; f < docp->dc_filecnt; f++) {
			if (docp->dc_files[f] == file)
				return(docp);
		}
	}

	/* no such document ID found */
	return (NULL);
}

#ifdef DEBUG
/*
 *  DOCUMENTATION
 *
 *  Name: fake-document
 *  Call: (fake-document)
 *  Retu: fixnum
 *  Desc: This function creates a new document internal the source
 *	editor without calling the formatter process at all.  This
 *	document will be invalid for anything except testing.
 *  SeeA: dvi-document make-document
 */

DEFUN(dofakedoc, "fake-document", FLAG_NONE, "")
{
	struct document	*docp;
	struct value	ret;

	/* no arguments at all */
	CHECKAC(0, 0);

	PROTECT();
	/* make up the document structure */
	docp = (struct document *)valloc(sizeof (struct document));
	docp->dc_rootID = 0;
	docp->dc_filecnt = 0;
	docp->dc_next = document_list;
	document_list = docp;
	UNPROTECT();

	message("Fake document %d is now current document.", docp->dc_rootID);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, docp->dc_rootID);
	return (ret);
}
#endif DEBUG

/*
 *  DOCUMENTATION
 *
 *  Name: make-document
 *  Call: (make-document 'filename)
 *  Retu: fixnum
 *  Desc: This function creates a new document whose master file
 *	is that specified by the given \TeX{} source file.  If the
 *	file is not being visited in any buffer, it is visited.
 *	Buffers which are visiting \TeX{} source files become special
 *	in several ways--changes to them need to be communicated
 *	to the remote TeX formatter and the user is not allowed to
 *	kill the buffer until the document is closed.
 *
 *	The contents of this master file buffer will be scanned by
 *	the formatter and processed as \TeX{} source code.  Note that
 *	changes to the source buffer are sent to the formatter
 *	program when (or before) the buffer is written or when a
 *	\sym{format-document} command is invoked.
 *
 *	This new document becomes the ``current document.''  That is,
 *	all future document commands will implicitly work on this
 *	document until the user changes the current document with
 *	\sym{switch-document} or closes it (in which case, the most
 *	recent other document because the current document).
 *  Side: If a formatter is not running when this function is
 *	called, one is started.  \sym{make-document} implicitly calls
 *	\sym{start-formatter} in this case.  See the documentation on
 *	the latter function for more information.
 *
 *	The contents of the buffer are sent immediately.  If other
 *	files are required (via a \lit{\\input} statement), they
 *	will be sent as requested by the formatter.
 *  SeeA: start-formatter format-document proof-document switch-document
 */

DEFUN(domakedoc, "make-document", FLAG_NONE, "FDocument master file: ")
{
	extern char	*getenv();
	extern int	format_socket;
	struct buffer	*findfile();
	struct value	arg, ret;
	struct document	*docp;
	struct docfile	*fp;
	struct buffer	*master;
	struct string	*file;
	int		fileID;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");

	/* we can only have one document for now */
	if (current_document != NULL) {
		closedoc(current_document->dc_rootID);
		message("(closed existing current document)");
		current_document = NULL;
	}

	/* start the formatter if necessary */
	if (format_socket <= 0 && startformat(NULL) != 0)
		error("Failed to start formatter; can't make a document!");

	/* find the filename in a buffer */
	file = gstring(arg.vl_data);
	master = findfile(file);
	ASSERT(master != NULL);

	PROTECT();
	/* allocate the document structure */
	docp = (struct document *)valloc(sizeof (struct document));
	bzero((char *)docp, sizeof (struct document));

	/* for now only one document is allowed */
	bzero((char *)doc_files, sizeof (doc_files));
	fileID = documentfile(docp, master);
	ASSERT(fileID >= 0);

	/* read in master file--mark this one as the root (ID 0) */
	docp->dc_rootID = fileID;
	docp->dc_next = current_document;
	current_document = docp;

	/* tell the formatter about it */
	sendformat(TSC_FORMAT, fileID, file->st_length, file->st_buffer);
	UNPROTECT();

	message("(new document; master file is \"%S\")", file);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, docp->dc_rootID);
	return (ret);
}

documentfile(docp, bufp)
	struct document	*docp;
	struct buffer	*bufp;
{
	struct docfile	*fp, *fend;
	int		fID;

	if (docp == NULL && (docp = current_document) == NULL)
		ierror("No current document; how can there be any files?!?");

	/* see if we've already got this buffer */
	fend = doc_files + MAXDFILES;
	for (fp = doc_files+1; fp < fend; fp++)
		if (fp->df_refcnt > 0 && fp->df_buffer == bufp)
			break;

	if (fp >= fend) {
		/* look for an empty entry */
		for (fp = doc_files+1; fp < fend; fp++)
			if (fp->df_refcnt <= 0)
				break;
		if (fp >= fend)
			return (-1);

		/* set this up as a new file */
		fp->df_refcnt = 1;
		if (bufp->bu_name == NULL) {
			/* this shouldn't happen! */
			*fp->df_name, '\0';
		} else {
			/* use the name from the given buffer */
			makecstring(bufp->bu_name,
				    fp->df_name, sizeof (fp->df_name));
		}
		fp->df_buffer = bufp;

		/* this buffer now has a file reference */
		fID = fp - doc_files;
		buffer_fileID(bufp, fID);

		/* add one more file to this document */
		docp->dc_files[docp->dc_filecnt++] = fID;
		docp->dc_filecnt++;

		/* don't allow user to kill this buffer */
		bufp->bu_flags |= BUFF_TEXFILE;

		debug(DITEX, "Buffer %S became new TeX file ID %d.",
		      bufp->bu_name, fID);
	} else {
		/* we already have it, just bump reference count */
		fp->df_refcnt++;
		fID = fp - doc_files;
	}

	return (fID);
}

struct buffer *
docfilebuf(fileID)
{
	struct docfile	*fp;

	if (fileID < 0 || fileID >= MAXDFILES)
		return (NULL);
	fp = &doc_files[fileID];
	if (fp->df_refcnt < 1 || fp->df_buffer == NULL) {
		debug(DITEX, "Inactive document file ID %d requested!",fileID);
		return (NULL);
	}

	/* return the associated buffer */
	return (fp->df_buffer);
}

/*
 *  DOCUMENTATION
 *
 *  Name: current-document
 *  Call: (current-document)
 *  Retu: fixnum
 *  Desc: This function returns the ID of the current document (which
 *	is a fixnum).  If no document is currently active (there is
 *	no current document), it returns nil.
 *  SeeA: make-document
 */

DEFUN(docurrentdoc, "current-document", FLAG_NONE, NULL)
{
	struct value	ret;

	CHECKAC(0, 0);
	if (current_document == NULL)
		return (v_nil);

	/* return the document ID as a fixnum */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, current_document->dc_rootID);
	return (ret);
}

#ifdef notyet
/*
 *  DOCUMENTATION
 *
 *  Name: switch-document
 *  Call: (switch-document 'document)
 *  Retu: document
 *  Desc: This function changes the document resident as the current
 *	document.  The current document is used implicitly by the
 *	commands which operate on documents.  Documents are creatd
 *	by \sym{make-document} and destroyed by \sym{close-document}.
 *  SeeA: make-document format-document proof-document close-document
 */

DEFUN(doswitchdoc, "switch-document", FLAG_NONE, "nNew document ID: ")
{
	struct value	arg, ret;
	int		docid;
	struct document	*last, *next;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a document number");
	docid = gfixnum(arg.vl_data);

	last = NULL;
	for (next = document_list; next != NULL; next = next->dc_next) {
		if (next->dc_rootID == docid)
			break;
		last = next;
	}
	if (next == NULL)
		error("Sorry, that's not a valid document ID!");

	/* switch the document order around if we need to */
	if (last != NULL) {
		PROTECT();
		/* delete new current document from list */
		last->dc_next = next->dc_next;
		/* insert it at the head of the list */
		next->dc_next = document_list;
		document_list = next;
		UNPROTECT();
	}

	/* return the newly current document ID */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, next->dc_rootID);
	return (ret);
}
#endif notyet

/*
 *  DOCUMENTATION
 *
 *  Name: close-document
 *  Call: (close-document 'document)
 *  Retu: t or nil
 *  Desc: This function removes the given document from the system,
 *	freeing up its resources for later reuse.
 *
 *	If the document doesn't exist, the function returns nil, if
 *	it actually has to close a document, \sym{close-document}
 *	returns t.
 *  SeeA: make-document start-formatter
 */

DEFUN(doclosedoc, "close-document", FLAG_NONE, "")
{
	struct value	arg;

	CHECKAC(0, 0);

	if (current_document != NULL)
		closedoc(current_document->dc_rootID);

	return (v_t);
}

closedoc(root)
{
	struct document	*next, *last;
	register int	f;
	struct docfile	*file;

	last = NULL;
	for (next = document_list; next != NULL; next = next->dc_next) {
		if (next->dc_rootID == root)
			break;
		last = next;
	}
	if (next == NULL)
		return (-1);

	PROTECT();
	/* remove the document from the list */
	if (last == NULL)
		document_list = next->dc_next;
	else
		last->dc_next = next->dc_next;
	if (next == current_document)
		current_document = document_list;

	/* for now, we only have one document, so don't bother */
	current_document = NULL;
	quitformat(0);

#ifdef notyet
	/* clean up unused document file resources */
	for (f = 0; f < next->dc_filecnt; f++) {
		file = &doc_files[next->dc_files[f]];
		if (--file->df_refcnt < 1 && file->df_buffer != NULL) {
			file->df_buffer->bu_flags &= ~BUFF_TEXFILE;
			file->df_buffer = NULL;
		}
	}

	/* inform the formatter of the closed files and document*/
	if (format_socket > 0) {
		for (f = 0; f < next->dc_filecnt; f++)
			sendformat(TSC_CLOSEFILE, next->dc_files[f], 0, NULL);
		sendformat(TSC_CLOSEDOC, next->dc_rootID, 0, NULL);
	}

	/* lose this storage */
	vfree(next);
	UNPROTECT();
#endif notyet

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: format-document
 *  Call: (format-document)
 *  Retu: nil
 *  Desc: This function sends a message to the incremental \TeX{}
 *	process to begin reformatting the current document.  There
 *	must be a current document previously opened with
 *	\sym{make-document}.
 *  SeeA: make-document close-document
 */

DEFUN(doformatdoc, "format-document", FLAG_NONE, "")
{
	struct docfile	*fp;

	if (current_document == NULL)
		error("No current document!");

	/* get the root file entry */
	fp = &doc_files[current_document->dc_rootID];
	if (fp->df_refcnt < 1 || *fp->df_name == '\0')
		ierror("No root file for current document!");

	/* make sure all changes are send to formatter */
	flush_inserts();
	flush_deletes();

	/* tell the formatter about it */
	sendformat(TSC_FORMAT, current_document->dc_rootID,
		   strlen(fp->df_name), fp->df_name);
	message("Formatting document--master file \"%s\"...", fp->df_name);

	return (v_nil);
}
