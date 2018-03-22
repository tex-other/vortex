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
 *  RCS Info: $Header: document.h,v 0.1 87/04/30 20:52:34 john Locked $
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
 *  document.h - VorTeX document data structures
 */
 
#ifndef _DOCUMENT_
#define _DOCUMENT_

/*
 *  There is a (small) limit on the number of active files between
 *  all TeX documents.  This is currently 128, since the file ID
 *  must be described in seven bits.  This makes the obvious data
 *  structure for storing the active file table a fixed array.  Each
 *  file (really a buffer) has a name and a buffer pointer and a
 *  reference count.  Each document has a table of file IDs (indices
 *  into the file table) for all of the files it uses.
 */
#define MAXDFILES	128

struct docfile {
	int		df_refcnt;	/* how many times used */
	char		df_name[1025];	/* string file name */
	struct buffer	*df_buffer;	/* buffer pointer */
};

/*
 *  Each document has a unique document ID which is used to identify
 *  it (useful when there is more than one currently active document).
 *  Note that we keep these documents as a linked list to allow an
 *  arbitrary number of documents, although practicaly only one or two,
 *  maybe three, will work).  The TeX formatter may not actually
 *  implement more than one document at once, which is reasonable.
 *
 *  Note that the first document in the linked list is the current
 *  document.  Switching documents rearranges the linked list, which
 *  means that the list remains sorted by most recently used document.
 */
#define current_document document_list

struct document {
	short		dc_rootID;	/* root file ID (document ID) */
	short		dc_filecnt;	/* number of files in document */
	unsigned char	dc_files[128];	/* indecies into file table */
	struct document	*dc_next;	/* next document in list */
};
#define dc_master	dc_files[0]	/* first file is master file */

extern struct document	*document_list;

extern struct docfile	doc_files[MAXDFILES];

extern struct document	*getdocument();
extern struct buffer	*docfilebuf();

#define ANYFILE		0

#endif !_DOCUMENT_
