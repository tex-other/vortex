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
 *  tp_comm.h - TeX/proof editor request specification
 */
 
#ifndef _TPCOMM_
#define _TPCOMM_
/*
 *  TeX and proof editor communications.
 *
 *  Packet format:
 *
 *	u_short	request		request code as defined below
 *	u_short	datalen		length of rest of packet
 *	u_long	pageno		page number (physical)
 *	<datalen bytes>		rest of packet; zero or more bytes
 *
 *  In the request definitions below, long is four bytes, short
 *  is two bytes, char is one byte and an array is zero or more
 *  of those objects as specified by the datalen field in the
 *  packet header in bytes.
 *
 *  Note the two constants below.  TPC_VERSION should be bumped
 *  each and every time this file is changed.  This is to insure
 *  that the programs will not try to communicate with different
 *  protocol specifications.
 */

#define TPC_VERSION		15


/*
 *  TPC_SENDPAGE
 *
 *  T <- P  for page information
 *  Send page information to the proof editor to preview that page.
 */

#define TPC_SENDPAGE		(GLC_LASTREQ + 1)


/*
 *  TPC_PAGEINFO
 *	long		globalmag	global mag (same for every page)
 *	long		count[10]	ten count registers
 *	short		fc		number of fonts used
 *	_Font		hdr		actual font header	---|
 *	char		name[hdr->ln]	actual font name	   |
 *	...		...					   |---> fc
 *	_Font		hdr		actual font header	   |
 *	char		name[hdr->ln]	actual font name	---|
 *	<data stream>			flattened page tree
 *
 *  T -> P  send page information
 *  This starts a stream of page information, which consists of a header,
 *  a stream of positioning commands plus structure and char boxes.
 *  Positioning commands define where the next box in the stream
 *  is to be placed relative to the current box, and is a u_short.
 *  There may not be a right child to the PageBox, nor may there be
 *  children to the CharBoxes.
 *  The data stream (flattened page tree) looks like the following:
 *	<box_type>	(1 byte)
 *	[BOX]		(5 words)
 *	<box_type>	(1 byte)
 *	[BOX]		(5 words)
 *	......
 *	<box_type>	(1 byte)
 *	[BOX]		(5 words)
 *	<EOP>		(1 byte)
 */

#define TPC_PAGEINFO		(TPC_SENDPAGE + 1)


/*
 *  TPC_PAGEOKAY
 *
 *  T -> P  inform the target editor that the page requested is still valid
 *  in its work space.
 *
 */

#define TPC_PAGEOKAY		(TPC_PAGEINFO + 1)


/*
 *  TPC_PAGEBAD
 *
 *  T -> P  inform the target editor that the page requested does not exist.
 *
 */

#define TPC_PAGEBAD		(TPC_PAGEOKAY + 1)


/*
 *  Structure for font specification.
 */

typedef struct _font {
	long		size;			/* font at size */
	short		fid;			/* font identifier. */
	short		ln;			/* font name length. */
} _Font;


#define TP_PAGE			80	/* 'P': begnning of a page box */
#define TP_PAR			81	/* 'Q': a paragraph box */
#define TP_WORD			87	/* 'W': a word box */
#define TP_CHAR			67	/* 'C': a character box */
#define TP_RULE			82	/* 'R': a character box */
#define TP_SPECIAL		83	/* 'S': a special box */
#define TP_EOP			69	/* 'E': begnning of a page box */

typedef struct _tbox {			/* terminal char box in trnasmission */
	long		id;		/* box id */
	long		ch;		/* char in ascii */
	long		ft;		/* font number */
	long		xb;		/* baseline x coordinate */
	long		yb;		/* baseline y coordinate */
} _Tbox;

typedef struct _nbox {			/* nonterminal box in trnasmission*/
	long		id;		/* box id */
	long		xc;		/* upperleft cornor x coordinate */
	long		yc;		/* upperleft cornor x coordinate */
	long		wd;		/* horizontal width */
	long		ht;		/* vertical height */
} _Nbox;

#define TPC_LASTREQ	TPC_PAGEBAD

#endif !_TPCOMM_
