/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the VorTeX project written by Pehong Chen, John
 *  Coker, Steve Procter and Ikuo Minakata for the VorTeX project under
 *  Prof. Michael A. Harrison of the University of California at Berkeley.
 *
 *  (c) 1987  Pehong Chen, John Coker, Steve Procter and Ikuo Minakata.
 *  University of California, Berkeley
 *  vortex-designers@renoir.Berkeley.EDU
 *
 *  All rights reserved by the authors.  See the copyright notice
 *  distributed with this software for the complete description of
 *  the conditions under which it is made available.
 *
 *  ts_comm.h - TeX/source editor request specification
 */
 
#ifndef _TSCOMM_
#define _TSCOMM_
/*
 *  TeX and source editor communications.
 *
 *  Packet format:
 *
 *	u_short	request		request code as defined below
 *	u_short	datalen		length of rest of packet
 *	u_long	id		file ID
 *	<datalen bytes>		rest of packet; zero or more bytes
 *
 *  In the request definitions below, long is four bytes, short
 *  is two bytes, char is one byte and an array is zero or more
 *  of those objects as specified by the datalen field in the
 *  packet header in bytes.
 *
 *  TSC_VERSION should be bumped each and every time this file is
 *  changed.  This is to insure that the programs will not try to
 *  communicate with different protocol specifications.
 */
#define TSC_VERSION		14

/*
 *  TSC_FORMAT
 *	string	filename	root TeX file name
 *
 *  S -> T
 *  Format document rooted at master file fid.
 */
#define TSC_FORMAT		(GLC_LASTREQ + 1)

/*
 *  TSC_CLOSEDOC
 *
 *  S <-> T:  close a document.
 *  Invalidate the document and free all its resources.
 */
#define TSC_CLOSEDOC		(TSC_FORMAT + 1)

/*
 *  TSC_OPENFILE
 *	u_long	data[]		contents of TeX file
 *
 *  S -> T:  open a source file (fid given in packet header) with
 *  contents in data array.  The same file, if already exists, gets trashed.
 *
 *  Each entry in the data array is a u_long:
 *
 *	        18             7       7
 *	------------------------------------
 *	|   ID per letter  |  fid  | ascii |
 *	------------------------------------
 */
#define TSC_OPENFILE		(TSC_CLOSEDOC + 1)

/*
 *  TSC_CLOSEFILE
 *
 *  S <-> T:  close a file (fid given in packet header).
 *  Invalidate the file ID specified and free all resources
 *  used by the specified file.
 */
#define TSC_CLOSEFILE		(TSC_OPENFILE + 1)

/*
 *  These constants define insertion/deletion points for the commands
 *  below so that they may be more flexible (and perhaps easier for the
 *  formatter/proof editor to generate).
 */
#define IRS_CUR			0
#define IRS_BOF			1
#define IRS_EOF			2

/*
 *  TSC_INSERT
 *	u_long	flag		IRS_CUR, current point as reference
 *				IRS_BOF, BOF as reference
 *				IRS_EOF, EOF as reference
 *	long	offset		offset to reference point, +forward, -backward
 *	u_long	count		number of chars to insert
 *	u_long	data[]
 *
 *  S -> T:  insert data in front of insertion point in file 
 *  (fid given in packet header).
 *  The insertion point is determined by stepping offset nodes relative
 *  to the reference point, which is determined by flag.
 *
 */
#define TSC_INSERT		(TSC_CLOSEFILE + 1)

/*
 *  TSC_DELETE
 *	u_long	flag		IRS_CUR, current point as reference
 *				IRS_BOF, BOF as reference
 *				IRS_EOF, EOF as reference
 *	u_long	offset		nonnegative offset to reference point
 *	u_long	count		number of chars to delete
 *
 *  S -> T:  delete count characters starting from deletion point in file 
 *  (fid given in packet header).
 *  The deletion point is determined by steping offset nodes relative
 *  to the reference point, which is determined by flag.
 *
 */
#define TSC_DELETE		(TSC_INSERT + 1)

/*
 *  TSC_TEXINPUT
 *	string	filename	file name as known to \input
 *
 *  T -> S:  ask for a file.
 *  Sent when the file is not found in the formatter's IR.
 */
#define TSC_TEXINPUT		(TSC_DELETE + 1)

/*
 *  TSC_TEXOUTPUT
 *	u_long	filelen		length of file name
 *	string	filename	output filename string
 *	string	content		file's contents
 *
 *  T -> S:  write for a file to the source editor's disk space.
 */
#define TSC_TEXOUTPUT		(TSC_TEXINPUT + 1)

/*
 *  TSC_TEXMESSAGE
 *	string	msg		the message
 *
 *  T -> S:  report message from the formatter.
 *  Sent when TeX is verbose.
 */
#define TSC_TEXMESSAGE		(TSC_TEXOUTPUT + 1)

/*
 *  TSC_TEXERROR
 *	u_long	lineno		line number of error
 *
 *  T -> S:  report an TeX error in LINENO of FID.
 *  An error message precedes this by TEXMESSAGE.
 */
#define TSC_TEXERROR		(TSC_TEXMESSAGE + 1)

#define TS_TGT2SRC		0  /* given target pos, return source pos */
#define TS_SRC2TGT		1  /* given source pos, return target pos */

/*
 *  TSC_EXECUTE
 *	<args>			arguments to the primitive
 *
 *  S -> T:  execute primitive id (given in header)
 */
#define TSC_EXECUTE		(TSC_TEXERROR + 1)

/*
 *  TSC_RETURN
 *
 *  T -> S:  return result of executing primitive id (given in header)
 *	long	code		return code
 *	<more data>		depending on each primitive.
 */
#define TSC_RETURN		(TSC_EXECUTE + 1)


#define TSC_LASTREQ		TSC_RETURN

#endif !_TSCOMM_
