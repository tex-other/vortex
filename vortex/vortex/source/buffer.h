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
 *  RCS Info: $Header: buffer.h,v 0.1 87/04/30 20:50:27 john Locked $
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
 *  buffer.h - editor buffer data structures
 */
 
#ifndef _BUFFER_
#define _BUFFER_

#include "value.h"

/*
 *  The buffer structure.  One of these exists for each buffer in the
 *  editor and anything that can get window real estate must operate
 *  through this buffer paradigm.  To make this work we have function
 *  pointers for the type dependent manipulations.  Also, there are
 *  pointers for type dependent data, since this will be different in
 *  the different editors.
 */
struct buffer {
	struct string	*bu_name;	/* buffer name (normal handle) */
	unsigned char	bu_type;	/* type of buffer */
	unsigned char	bu_locker;	/* who locked this buffer */
	unsigned short	bu_flags;	/* buffer status flags */
	unsigned short	bu_mflags;	/* mode line painting flags */
	unsigned short	bu_events;	/* X events to forward */
	struct symtab	*bu_locals;	/* local buffer variables */
	struct value	bu_keymap;	/* keymap local to buffer */
	struct source	*bu_sdata;	/* source editor local data */
	struct proof	*bu_pdata;	/* proof editor local data */
	int		(*bu_input)();	/* input handler function */
	int		(*bu_paint)();	/* repaint function */
	int		(*bu_resize)();	/* window resize function */
	int		(*bu_expose)();	/* window exposure function */
	unsigned char	*(*bu_mline)();	/* mode line formatter */
	int		(*bu_event)();	/* routine to accept X events */
	int		(*bu_destroy)();/* buffer destroy hook */
	int		(*bu_open)();	/* when newly viewed in a window */
	int		(*bu_close)();	/* no longer viewed in a window */
	struct buffer	*bu_next;	/* next buffer in list */
};
#define BUFF_ANY	0		/* any/bad buffer type */
#define BUFF_SOURCE	1		/* source editor buffer */
#define BUFF_PROOF	2		/* proof editor buffer */
#define BUFF_GRAPHIC	3		/* graphics editor buffer */
#define BUFF_TABLE	4		/* table editor buffer */
#define BUFF_MATH	5		/* math editor buffer */

#define BUFF_MODIFIED	(1 << 0)	/* buffer changed since write */
#define BUFF_INPUT	(1 << 1)	/* buffer feeds process input */
#define BUFF_OUTPUT	(1 << 2)	/* buffer takes process output */
#define BUFF_PRECIOUS	(1 << 3)	/* can't kill this buffer */
#define BUFF_KILLOK	(1 << 4)	/* okay to kill existing buffer */
#define BUFF_HOLD	(1 << 5)	/* some process using this buffer */
#define BUFF_READONLY	(1 << 6)	/* this buffer is read-only */
#define BUFF_CHANGED	(1 << 7)	/* buffer changed since update */
#define BUFF_NEWMODE	(1 << 8)	/* need to generate a new mode line */
#define BUFF_MINIBUF	(1 << 9)	/* this is a minibuffer */
#define BUFF_TEXFILE	(1 << 10)	/* this file buffer was \input */

#define LOCK_NOLOCK	0		/* buffer isn't locked */
#define LOCK_SOURCE	1		/* source editor locked it */
#define LOCK_USER	2		/* user locked the buffer */
#define LOCK_ITEX	3		/* TeX formatter locked it */
#define LOCK_PROOF	4		/* proof editor locked it */

/*
 *  Each of these text blocks contains the information to store at
 *  most TBLOCKSIZE characters in a source buffer.  Storage is organized
 *  in this way to limit the number of linked list pointers per byte
 *  stored in a source buffer.  We garbage collect these text blocks
 *  so we need that information as well as the information to find a
 *  given character ID quickly in a buffer.
 */
#define TBLOCKSIZE	512		/* elements in tb_text */

struct tblock {
	unsigned int	tb_mark : 1,	/* mark bit for garbage collection */
			tb_free : 1,	/* free for allocation */
			tb_flags : 6,	/* special flags, possibly */
			tb_length : 24;	/* number of characters used */
	unsigned long	tb_offset;	/* block offset in buffer */
	long		tb_lowID;	/* lowest char ID value */
	long		tb_highID;	/* hightest char ID value */
	struct tblock	*tb_next;	/* next tblock in buffer */
	struct tblock	*tb_prev;	/* previous tblock in buffer */
	unsigned long	tb_text[TBLOCKSIZE];
};

/*
 *  Each character is stored in a long word.  The lower seven bits of
 *  the word give the ASCII character (meaning that source buffers can't
 *  edit meta characters).  The next seven bits is an index into a
 *  dispatch table of file/document references used when this file is
 *  created.  The upper eighteen bits form a number unique to this
 *  ASCII code across all documents and files in the current invocation.
 *  The macros below retrieve and form these char ID values.
 */
extern long	next_charIDs[];

#define newcharID(c)	(++next_charIDs[(c) & 0177])

#define charof(i)	((unsigned)(i) & 0177)
#define fileof(i)	((unsigned)((i) >> 7) & 0177)
#define codeof(i)	((unsigned)(i) >> 14)

#define charID(a,f,c)	(((unsigned)(c) << 14) | \
			 ((unsigned)(f) << 7) | \
			 ((unsigned)(a) & 0177))

/*
 *  The data specific to source editor buffers.  When a buffer is
 *  created by the source editor, its type will be BUFF_SOURCE and
 *  bu_sdata will point to one of these structures, properly filled
 *  in.  It stores the stuff for keeping the list of tblocks (above)
 *  that makes up the text of the buffer and information for painting
 *  and editing it.
 */
struct source {
	unsigned short	sb_sflags;	/* source buffer flags */
	unsigned short	sb_cflags;	/* cursor painting flags */
	unsigned long	sb_fileID;	/* file/document reference */
	short		sb_tabwidth;	/* tab width in columns */
	short		sp_ptpos;	/* point position as percentage */
	struct string	*sb_file;	/* filename being edited */
	struct tblock	*sb_text;	/* text of buffer */
	long		sb_start;	/* starting offset in buffer */
	long		sb_point;	/* point in buffer */
	long		sb_mark;	/* mark in buffer */
	unsigned long	sb_length;	/* total length of buffer */
	unsigned long	sb_mtime;	/* last modified time */
};

#define SOURC_TRUNCATE	(1 << 0)	/* truncate long lines */
#define SOURC_MINBUSED	(1 << 1)	/* this minibuf is in use */

/*
 *  The data specific to proof editor buffers.  When a buffer is
 *  created by the proof editor, its type will be BUFF_PROFF and
 *  bu_data will point to one of these structures, properly filled
 *  out.
 *
 *  The canonical handle for proof buffers is the X window ID as
 *  a long integer.  This means the proof windows have a one-to-one
 *  correspondence with proof ``buffers''.  Real editing does not
 *  take place in the proof windows as it does in source windows,
 *  so this works properly.
 */
struct proof {
	unsigned short	pb_pflags;	/* proof buffer flags */
	unsigned long	pb_document;	/* the document being displayed */
	short		pb_pages;	/* number of physical pages */
	short		pb_curpage;	/* current page in document */
	short		pb_Xtotal;	/* total height of page */
	short		pb_Xvisible;	/* height of visible area */
	short		pb_Xoffset;	/* left offset of visible area */
	short		pb_Ytotal;	/* total width of page */
	short		pb_Yvisible;	/* width of visible area */
	short		pb_Yoffset;	/* top offset of visible area */
};

#define PRF_OPENED	(1 << 0)	/* have opened this window */

extern struct buffer	*buffer_list;	/* list of all buffers */
extern struct buffer	*minibuffer;	/* the minibuffer buffer */
extern struct buffer	*scratchbuf;	/* the ``other'' buffer */
extern struct buffer	*current_buffer;/* the current buffer */

extern struct buffer	*buffer_create(), *buffer_get();

/* macros for traversing buffer list */
#define forallbufs(p)	for (p = buffer_list; p != NULL; p = p->bu_next)
#define forallsbufs(p)	forallbufs(p) if (p->bu_type == BUFF_SOURCE)
#define forallpbufs(p)	forallbufs(p) if (p->bu_type == BUFF_PROOF)

#endif !_BUFFER_
