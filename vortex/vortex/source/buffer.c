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
 *  RCS Info: $Header: buffer.c,v 0.1 87/05/01 11:25:26 john Locked $
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
 *  buffer.c - internal buffer management routines
 */
static char _ID[] = "@(#)buffer.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

struct buffer	*buffer_list, *current_buffer;
struct buffer	*scratchbuf, *minibuffer;
static int	buflist_changed = FALSE;

/*
 *  DOCUMENTATION
 *
 *  Name: *minibuffer*
 *  Desc: This buffer is always present (it cannot be killed) and is
 *	used as the input window for commands which need to prompt for
 *	arguments.  In emacs parlance, this buffer is called the
 *	\em{minibuffer}.
 *
 *	The minibuffer is not normally used directly by the user and
 *	should be managed transparently by the \VorTeX{} system.
 *	Of course, all the normal source buffer edting commands work
 *	in the minibuffer as they do in other source editor buffers.
 *
 *	The minibuffer has its own window, called the \em{minibuffer window}
 *	which pops up when needed and may disappear afterwards.  See
 *	below and \sym{auto-raise-windows} and \sym{deactivate-window}.
 *  Side: If this window is deactivated with \sym{deactivate-window},
 *	it will be automatically reactivated when it is needed and then
 *	deactivated when the input is finished.
 *
 *	If \sym{auto-raise-windows} is set, which it is by default for
 *	the minibuffer, the minibuffer window will raise when it is
 *	needed.
 *  SeeA: minibuf-input minibuf-print deactivate-window auto-raise-windows
 */
MKSTRING(MINIBUF_NAME, "*minibuffer*");

/*
 *  DOCUMENTATION
 *
 *  Name: *scratch*
 *  Desc: This buffer is always present as the ``other buffer''.
 *	Whenever a buffer is needed and no other is available,
 *	the \em{scratch buffer} is used.
 *  SeeA: *minibuffer* switch-to-buffer
 */
MKSTRING(SCRATCH_NAME, "*scratch*");

/*
 *  DOCUMENTATION
 *
 *  Name: mode-line-format
 *  Desc: This variable is local to each buffer and specifies the
 *	format used to display the mode line at the top of each
 *	window in the ``name stripe.''  All normal editor windows
 *	have name stripes and mode lines to display in them.
 *
 *	The format of the mode line is specified in a string with
 *	escapes similar to those of the \sym{format} command or
 *	the \UNIX{} \em{printf(3)} function.  All characters in
 *	the string are copied out verbatim except the percent sign,
 *	which indicates an escape.  The format of an escape is the
 *	percent sign, an optional number specifying the maximum
 *	width of that field in the output, and a character specifying
 *	what is to be represented by that field.  The eascapes and
 *	what they cause to be displayed are listed below:
 *
 *	\tab{\lit{%b}	the buffer name as a symbol
 *	\lit{%*}	\lit{*} if modified, \lit{%} if read-only, or \lit{-}
 *	\lit{%f}	full path name of the file, or \lit{none} if no file
 *	\lit{%F}	last componenet of the path name (the actual file name)
 *	\lit{%~}	the path name of the file relative to the user's home
 *	\lit{%[}	recursive edit level, one \lit{[} per level
 *	\lit{%]}	recursive edit level, one \lit{]} per level
 *	\lit{%w}	the buffer's window number
 *	\lit{%m}	the local mode string as text
 *	\lit{%M}	the global mode string as text
 *	\lit{%u}	the user local mode string as text
 *	\lit{%U}	the user global mode string as text
 *	\lit{%s}	size of buffer in characters
 *	\lit{%p}	position in buffer, as percentage
 *	\lit{% }	fill out line with spaces
 *	\lit{%-}	fill out line with dashes
 *	\lit{%=}	fill out line with equal signs
 *	\lit{%_}	fill out line with underscores
 *	\lit{%%}	a single percent sign}
 *
 *	Note that \lit{% }, \lit{%-}, \lit{=} and \lit{%_} fill up the
 *	remaining space with a specific character.  If multiple such
 *	``space filling'' escapes are used, they divide the available
 *	space between them.  Thus, a \lit{% } at each end of the mode string
 *	would cause it to be centered on the display (with the entire line
 *	highlighted).
 *
 *	The \lit{%m} and \lit{%M} escapes show a text representation of
 *	two variables, \sym{mode-string} and \sym{global-mode-string}.
 *	The first is local to each buffer and the second is global
 *	to all buffers.
 *  Side: Changing this variable doesn't change the mode line on
 *	the screen until it is redisplayed (the buffer is painted
 *	completely) or some value on the mode line changes.
 *  Xref: default-mode-line-format
 *  SeeA: set global-mode-string mode-string
 */
MKSTRING(MODEFMT_NAME, "mode-line-format");

MKSTRING(DEFMODEFMTVAL, "VorTeX Buffer %b%* \"%f\"  %[[%w:%m]%] %p  %M% ");
MKSTRING(DEFMODESTRVAL, "fundamental");

MKSTRING(MINBMODEFMTVAL, "VorTeX Input  %[[%w:%m]%] %p  %M% ");
MKSTRING(MINBMODESTRVAL, "minibuffer");

extern struct string	*LOCALMODE_NAME, *GLOBALMODE_NAME;

initbuffers()
{
	struct value	val;

	/* set up global default mode line defaults */
	val.vl_type = LISP_STRING;
	sstring(val.vl_data, DEFMODEFMTVAL);
	setglobal(MODEFMT_NAME, val, STAB_LOCAL);
	setglobal(GLOBALMODE_NAME, v_null, FLAG_NONE);
	val.vl_type = LISP_STRING;
	sstring(val.vl_data, DEFMODESTRVAL);
	setglobal(LOCALMODE_NAME, val, STAB_LOCAL);

	/* make minibuffer as source buffer */
	minibuffer = buffer_create(MINIBUF_NAME, BUFF_SOURCE,
				   BUFF_MINIBUF|BUFF_PRECIOUS);

	/* make minibuffer local bindings */
	val.vl_type = LISP_STRING;
	sstring(val.vl_data, MINBMODEFMTVAL);
	setlocal(MODEFMT_NAME, val, FLAG_NONE, &minibuffer->bu_locals);
	val.vl_type = LISP_STRING;
	sstring(val.vl_data, MINBMODESTRVAL);
	setlocal(LOCALMODE_NAME, val, FLAG_NONE, &minibuffer->bu_locals);

	/* make scratch buffer as first/current buffer */
	scratchbuf = buffer_create(SCRATCH_NAME, BUFF_SOURCE, BUFF_PRECIOUS);

	/* make the list up */
	buffer_list = scratchbuf;
	scratchbuf->bu_next = minibuffer;
	current_buffer = scratchbuf;

	return (0);
}

struct buffer *
buffer_get(bname, strict)
	struct string	*bname;
{
	struct buffer	*bufp;

	if (bname == NULL || bname->st_length < 1)
		error("Null buffer name to get!");

	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
		if (sequal(bufp->bu_name, bname))
			return (bufp);
	}
	if (strict)
		error("No buffer named %Y exists currently!", bname);
	return (NULL);
}

struct buffer *
buffer_create(bname, type, flags)
	struct string	*bname;
{
	struct buffer	*bufp, *last, *next;

	/* just check for a bad name */
	if (bname == NULL || bname->st_length < 1)
		ierror("Null buffer name to create!");

	last = NULL;
	for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
		if (sequal(bufp->bu_name, bname))
			break;
		last = bufp;
	}
	if (bufp == NULL) {
		/* need to make a new buffer */
		bufp = (struct buffer *)valloc(sizeof (struct buffer));
		bufp->bu_name = bname;
		bufp->bu_sdata = NULL;
		bufp->bu_pdata = NULL;
		debug(DBUFFER, "Created new buffer structure for %Y.", bname);
		buflist_changed = TRUE;
	} else if ((flags & BUFF_KILLOK) == 0 &&
		   (bufp->bu_flags & BUFF_MODIFIED) != 0 &&
		    !yes_or_no("Buffer %Y has been modified, clobber it? ",
			       bufp->bu_name)) {
		debug(DBUFFER, "Not taking over existing buffer %Y.", bname);
		return (NULL);
	} else {
		/* remove buffer from the buffer list */
		if (last == NULL)
			buffer_list = bufp->bu_next;
		else
			last->bu_next = bufp->bu_next;
		debug(DBUFFER, "Took over existing buffer for %Y.", bname);
	}

	PROTECT();
	/* zero handlers just in case */
	bufp->bu_input = NULL;
	bufp->bu_resize = NULL;
	bufp->bu_expose = NULL;
	bufp->bu_paint = NULL;
	bufp->bu_mline = NULL;
	bufp->bu_event = NULL;
	bufp->bu_destroy = NULL;
	bufp->bu_open = NULL;
	bufp->bu_close = NULL;

	/* zero out buffer attributes */
	bufp->bu_type = type;
	bufp->bu_flags = (flags & ~BUFF_KILLOK) | BUFF_CHANGED;
	bufp->bu_keymap = v_nil;
	bufp->bu_locals = NULL;
	bufp->bu_mflags = FLAG_NONE;
	switch (type) {
	case BUFF_SOURCE:
		source_init(bufp);
		break;
	case BUFF_PROOF:
		proof_init(bufp);
		break;
	}

	if (bufp->bu_flags & BUFF_MINIBUF) {
		/* add this buffer to the end of the list */
		last = NULL;
		for (next = buffer_list; next != NULL; next = next->bu_next)
			last = next;
		if (last == NULL)
			buffer_list = bufp;
		else
			last->bu_next = bufp;
		bufp->bu_next = NULL;
	} else {
		/* link this buffer into the head of the list */
		bufp->bu_next = buffer_list;
		buffer_list = bufp;
	}
	UNPROTECT();

	return (bufp);
}

buffer_remove(bufp, force)
	struct buffer	*bufp;
{
	struct buffer	*last, *next;
	struct window	*win;

	if (!force && (bufp->bu_flags & BUFF_PRECIOUS) != 0)
		error("Can't remove buffer %Y, it's precious!", bufp->bu_name);
	if (!force &&(bufp->bu_flags & BUFF_TEXFILE) != 0)
		error("Can't remove buffer %Y, it's a TeX input source!",
		      bufp->bu_name);
	if (!force && (bufp->bu_flags & BUFF_MODIFIED) != 0) {
		if (!yes_or_no("Buffer %Y has been modified, remove anyway? ",
			       bufp->bu_name))
			return (1);
	}

	/* find this buffer in the list */
	last = NULL;
	for (next = buffer_list; next != NULL; next = next->bu_next) {
		if (next == bufp)
			break;
		last = next;
	}

	/* remove this buffer from the list */
	PROTECT();
	if (next != NULL) {
		next = next->bu_next;
		if (last == NULL)
			buffer_list = next;
		else
			last->bu_next = next;
	}

	/* make sure no windows are displaying this buffer */
	forallbwins(win) {
		if (win->wi_buffer == bufp)
			switchbuffer(win, buffer_list);
	}

	/* make sure this wasn't the current buffer */
	if (current_buffer == bufp)
		current_buffer = current_window->wi_buffer;

	/* release storage used by the buffer */
	if (bufp->bu_destroy != NULL)
		(*bufp->bu_destroy)(bufp);
	vfree(bufp);

	buflist_changed = TRUE;
	UNPROTECT();

	return (0);
}

buffer_erase(bufp, force)
	struct buffer	*bufp;
{
	struct source	*srcp;

	if (bufp->bu_type != BUFF_SOURCE)
		return (0);

	srcp = bufp->bu_sdata;
	if (!force && (bufp->bu_flags & BUFF_READONLY) != 0)
		error("Can't erase %Y; buffer is read-only.", bufp->bu_name);

	if (!force && srcp->sb_length > 0 &&
	    (bufp->bu_flags & BUFF_MODIFIED) != 0 &&
	    !yes_or_no("Buffer %Y has been modified, erase it anyway? ",
		       bufp->bu_name))
		return (1);

	PROTECT();
	if (srcp->sb_length > 0)
		bufp->bu_flags |= BUFF_MODIFIED|BUFF_CHANGED|BUFF_NEWMODE;
	srcp->sb_text = NULL;
	srcp->sb_start = srcp->sb_length = 0;
	srcp->sb_point = 0;
	srcp->sb_mark = -1;
	srcp->sb_mtime = time(NULL);
	UNPROTECT();

	return (0);
}

switchbuffer(winp, bufp)
	struct window	*winp;
	struct buffer	*bufp;
{
	struct buffer	*obuf;

	ASSERT(bufp != NULL);
	if (winp->wi_type != WIN_BUFFER)
		error("Window %d isn't a buffer window!", winp->wi_index);
	if (winp->wi_buffer == bufp)
		return (1);

	/* call the old buffer's close function */
	if ((obuf = winp->wi_buffer) != NULL && obuf->bu_close != NULL)
		(*obuf->bu_close)(winp);

	/* switch the buffer now */
	winp->wi_buffer = bufp;
	winp->wi_flags |= WIN_REPAINT|WIN_NEWMODE;

	if (bufp->bu_type == BUFF_SOURCE) {
		/* inherit point from the new buffer */
		setwindowpoint(winp, getpoint(bufp));
		/* no mark set in this window */
		setwindowmark(winp, -1);
	}

	/* call the new buffer's open function */
	if (bufp->bu_open != NULL)
		(*bufp->bu_open)(winp);

	return (0);
}

struct buffer *
windowbuffer(winp, type)
	struct window	*winp;
{
	extern char	*gettype();
	struct buffer	*bufp, fake;

	ASSERT(winp != NULL);
	if (winp->wi_type != WIN_BUFFER || (bufp = winp->wi_buffer) == NULL) {
		error("Window %d isn't a buffer-visiting window!",
		      winp->wi_index);
	}
	if (type != BUFF_ANY && bufp->bu_type != type) {
		fake.bu_type = type;
		error("Window %d isn't visiting a %s buffer!",
		      winp->wi_index, gettype(&fake));
	}
	if (bufp->bu_type == BUFF_SOURCE) {
		/* inherit point and mark from the window */
		setpoint(bufp, winp->wi_point);
		setmark(bufp, winp->wi_mark);
	}
	return (bufp);
}

/*
 *  Make a sorted list of all the current buffers and return it for
 *  completion purposes.  We make one optimization here, we don't
 *  rebuild the list until the buffer list changes somehow.  This
 *  means that the above routines must set buflist_changed when they
 *  change the buffer list.
 */
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

/*
 *  For the list of buffer names, we keep a table of strings and
 *  a buffer of character storage for list storage.  This means
 *  that if there are more than MAXSTRINGS buffers and/or their
 *  names add up to more than MAXSTRLEN characters (terminating
 *  '\0' characters are also stored), not all the buffer names
 *  will appear in the list.
 */
#define MAXSTRINGS	1000
#define MAXSTRLEN	10240

char **
match_buffers(partial)
	char	*partial;
{
	static char	*strings[MAXSTRINGS + 1];
	static char	strbuf[MAXSTRLEN];
	static char	*matches[MAXSTRINGS+1];
	register int	s, slen;
	struct buffer	*bufp;
	struct string	*name;
	register char	*sbuf, *send;
	register char	**str;
	register int	m, len;

	/* make sure the list isn't out-of-date */
	if (strings[0] != NULL && buflist_changed)
		strings[0] = NULL;
	buflist_changed = FALSE;

	if (strings[0] == NULL) {
		PROTECT();
		/* zero out the string table for safety */
		bzero(strings, sizeof (strings));

		/* set up string buffer pointers */
		sbuf = strbuf;
		send = strbuf + sizeof (strbuf);

		/* make up the new list of buffer names */
		s = 0;
		for (bufp = buffer_list; bufp != NULL; bufp = bufp->bu_next) {
			if (s >= MAXSTRINGS)
				break;
			name = bufp->bu_name;
			if (name != NULL && name->st_length > 0) {
				slen = name->st_length + 1;
				if (sbuf + slen >= send)
					break;
				strings[s] = sbuf;
				sbuf += slen;
				makecstring(name, strings[s], slen);
				s++;
			}
		}
		if (s > 0)
			qsort(strings, s, sizeof (char **), compare);
		UNPROTECT();
	}

	if (*strings == NULL || partial == NULL || *partial == '\0')
		return (strings);

	/* find the matches to the given buffer name */
	for (str = strings; *str != NULL && **str < *partial; str++)
		;
	len = strlen(partial);
	m = 0;
	for ( ; *str != NULL && **str == *partial && m < MAXSTRINGS; str++) {
		if (!strncmp(*str, partial, len))
			matches[m++] = *str;
	}
	matches[m] = NULL;
	return (matches);
}

compl_buffers(partial, result, maxlen)
	char	*partial, *result;
{
	extern char	**match_buffers();
	char		**matches;
	char		bigbuf[2048];
	int		status;

	ASSERT(result != NULL && maxlen > 1);

	matches = match_buffers(partial);
	if (matches == NULL || *matches == NULL)
		return (COMPL_NOMATCH);

	*bigbuf = '\0';
	status = compl_expand(partial, matches, bigbuf);
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

buffer_fileID(bufp, fID)
	struct buffer	*bufp;
	register int	fID;
{
	register struct tblock	*tbp;
	struct source		*srcp;
	register unsigned long	*txp, *tend;
	register int		cID, c;

	/* make sure this is a source editor buffer */
	if (bufp->bu_type != BUFF_SOURCE)
		error("Non-source buffer to send to formatter!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* set this file ID in the source buffer struct */
	srcp->sb_fileID = fID;

	/* traverse buffer setting the given file ID in each character */
	for (tbp = srcp->sb_text; tbp != NULL; tbp = tbp->tb_next) {
		tend = tbp->tb_text + tbp->tb_length;
		for (txp = tbp->tb_text; txp < tend; txp++) {
			c = charof(*txp);
			cID = codeof(*txp);
			*txp = charID(c, fID, cID);
		}
	}

	return (0);
}
