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
 *  RCS Info: $Header: minbcmds.c,v 0.1 87/05/01 12:20:56 john Locked $
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
 *  minbcmds.c - special minibuffer commands (vLisp functions)
 */
static char _ID[] = "@(#)minbcmds.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include <signal.h>
#include <varargs.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"
#include "format.h"

extern struct window	*minibuf_window;

static struct symbol	*yes_or_no_name = NULL;
MKSTRING(YES_OR_NO_FUNC, "yes-or-no");

/* VARARGS */
yes_or_no(va_alist)
	va_dcl
{
#include "fmtdecl.h"
	struct string	*prompt;
	struct value	arg0, func, arg1, ret;

#include "fmtcode.h"

	/* turn it into a prompt */
	prompt = save_string(msgbuf, msglen);

	/* make the symbol if it hasn't already been done */
	if (yes_or_no_name == NULL)
		yes_or_no_name = save_symbol(YES_OR_NO_FUNC);

	/* call the function with this message */
	arg0.vl_type = LISP_SYMBOL;
	ssymbol(arg0.vl_data, yes_or_no_name);
	func = evalsexpr(arg0);
	if (!funcp(func)) {
		error("Symbol %Y isn't bound to a function anymore!",
		      yes_or_no_name);
	}
	arg1.vl_type = LISP_STRING;
	sstring(arg1.vl_data, prompt);
	ret = call_function(func, arg0, arg1);

	/* return the C value of this function */
	if (nullp(ret))
		return (FALSE);
	else
		return (TRUE);
}

/*
 *  DOCUMENTATION
 *
 *  Name: yes-or-no
 *  Call: (yes-or-no 'question)
 *  Retu: t or nil
 *  Desc: This predicate function prints the text to which the
 *	argument must evaluate in the minibuffer and waits for
 *	the user to type either ``yes'' or ``no''.  Then, t is
 *	returned if the user typed ``yes'' and nil if he typed
 *	``no.''
 *
 *	This function forces the user to type either ``yes'' or
 *	``no'' (and return).  It does not allow defaults or any
 *	abbreviations.  It is meant (and used internally) for
 *	confirmation of actions that could have serious consequences.
 *  Side: If this function is re-defined, the editor itself will
 *	use the user's redefined function instead of this default
 *	one.
 *  SeeA: *minibuffer* minibuf-input
 */
MKSTRING(AGAINMSG, "Please answer ``yes'' or ``no'': ");
MKSTRING(YES_STR, "yes");
MKSTRING(NO_STR, "no");

DEFUN(doyesorno, "yes-or-no", FLAG_NONE, NULL)
{
	struct string	*minb_input();
	struct value	arg;
	struct string	*prompt, *badmsg, *ans;

	/* get the argument message */
	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a question string");
	prompt = gstring(arg.vl_data);
	if (prompt->st_length < 1)
		prompt = AGAINMSG;

	badmsg = NULL;
	for (;;) {
		/* read the answer */
		ans = minb_input(prompt, NULL);

		/* check if it says anything */
		if (sequal(*ans->st_buffer, YES_STR))
			return (v_t);
		if (sequal(*ans->st_buffer, NO_STR))
			return (v_nil);

		/* bad answer, try again */
		if (badmsg == NULL)
			badmsg = AGAINMSG;
		prompt = badmsg;
		beep(0);
	}
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: minibuf-print
 *  Call: (minibuf-print 'string)
 *  Retu: nil
 *  Desc: This function paints the given string onto the first line
 *	of the minibuffer window without affecting the contents of
 *	the minibuffer.  The true appearance of the minibuffer will
 *	appear as soon as it needs repainting, as when its contents
 *	changes.
 *  Side: If the minibuffer is not visible or deactivated, it will
 *	will be made visible and reactivated.
 *  SeeA: *minibuffer* minibuf-input
 */

DEFUN(dominbprint, "minibuf-print", FUNC_VISUAL, NULL)
{
	struct value	arg;
	struct string	*str;
	char		text[1024];

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string message");
	str = gstring(arg.vl_data);

	makepstring(str, text, sizeof (text));
	minb_print(text, strlen(text), FALSE);

	return (v_nil);
}

/*
 *  DOCUMENTATION
 *
 *  Name: minibuf-input
 *  Call: (minibuf-input 'prompt [ 'start ])
 *  Retu: any
 *  Desc: This function reads on vlisp expression through the
 *	minibuffer, prompting with the string to which the
 *	first argument must evaluate.  The second argument,
 *	if present, must also evaluate to a string, which is
 *	the default text.  This default text may be edited
 *	and any or all of it after editing will be returned
 *	when the user types return.  The text after editing
 *	(usually just insertions) is returned as a string.
 *
 *	When editing in the minibuffer, all the normal
 *	editing commands work as expected, except that
 *	C-m and C-j are bound to \sym{minibuf-return} and
 *	return from the minibuffer edit rather than just
 *	inserting themselves.
 *  Side: If the minibuffer is not visible or deactivated, will
 *	will be made visible and reactivated.
 *  Xref: read-minibuf
 *  SeeA: *minibuffer* minibuf-complete minibuf-return
 */

DEFUN(dominbinput, "minibuf-input", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct string	*prompt, *start, *input;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a prompt string");
	prompt = gstring(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "an initial text string");
		start = gstring(arg.vl_data);
	} else {
		/* no initial text */
		start = NULL;
	}

	input = minb_input(prompt, start);
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, input);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: minibuf-complete
 *  Call: (minibuf-complete)
 *  Retu: t or nil
 *  Desc: This function is normally bound to the space key
 *	only in the minibuffer when completion can be done
 *	on the item being read.  In this case, the when
 *	\sym{minibuf-complete} is called, it tries to fill
 *	in more of the current input token for the user.
 *
 *	If any completion was possible, this function will
 *	return t, else it returns nil.
 *
 *	This function should only be used in the minibuffer,
 *	it has no real meaning in any other.  If the current
 *	input has no completion, \sym{minibuf-complete} just
 *	acts like \sym{self-insert}.  If \sym{minibuf-complete}
 *	is being called while not in the minibuffer, an error
 *	occurs.
 *  SeeA: *minibuffer* minibuf-return self-insert minibuf-input
 */
extern struct string	*typed_keys;
extern int		minibuf_used;

extern int	(*type_complete)();

#define MAXCOMPL	2048	/* completion can be no longer than this */

DEFUN(dominbcomplete, "minibuf-complete", FLAG_NONE, "")
{
	extern char	*alloca();
	struct string	*copy_region();
	struct string	*text, add;
	int		status, tlen;
	char		*partial, *result;
	struct source	*srcp;

	if (current_buffer != minibuffer || !minibuf_used)
		return (v_nil);
	if (type_complete == NULL) {
		/* not to complete, call self-insert instead */
		if (typed_keys == NULL || typed_keys->st_length < 1) {
			/* nothing to insert! */
			return (v_nil);
		} else {
			insert_char(minibuffer, *typed_keys->st_buffer);
			minibuf_window->wi_point++;
			return (v_t);
		}
	}

	/* get the minibuffer source information */
	srcp = minibuffer->bu_sdata;
	ASSERT(srcp != NULL);

	/* copy the text out and perform the completion */
	tlen = srcp->sb_length - srcp->sb_start;
	result = alloca(MAXCOMPL);
	if (tlen <= 0) {
		/* there is no partial string */
		partial = "";
	} else {
		tlen++;
		partial = alloca(tlen);
		text = copy_region(minibuffer,
				   srcp->sb_start, srcp->sb_length);
		makecstring(text, partial, tlen);
	}
	debug(DINPUT, "Trying to complete on \"%s\"...", partial);
	status = (*type_complete)(partial, result, MAXCOMPL);

	/* handle the result of completion */
	if (status < 0) {
		/* this shouldn't happen */
		ierror("Completion routine returned negative status!");
	} if (status == COMPL_NOMATCH) {
		/* no matches at all for completion */
		beep(0);
		return (v_nil);
	} else if (status == COMPL_NOTUNIQ) {
		/* not unique enough for completion */
		beep(0);
		return (v_nil);
	} else {
		/* did some completion here */
		add.st_buffer = (unsigned char *)result;
		add.st_length = strlen(result);
		append_string(minibuffer, &add);
		/* move point to the end of the buffer */
		srcp->sb_point = srcp->sb_length;
		minibuf_window->wi_point = srcp->sb_point;
		return (v_t);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: minibuf-help
 *  Call: (minibuf-help)
 *  Retu: t or nil
 *  Desc: This function is normally bound to the question mark
 *	only in the minibuffer when completion can be done
 *	on the item being read.  In this case, when \sym{minibuf-help}
 *	is called, it lists all the possible completions of the text
 *	typed so far in the buffer \lit{*completions*}.
 *
 *	If any completion was possible, this function will
 *	return the number of possible matches, else it will
 *	return nil.
 *
 *	This function should only be used in the minibuffer,
 *	it has no real meaning in any other.  If the current
 *	input has no completion, \sym{minibuf-help} just
 *	acts like \sym{self-insert}.  If \sym{minibuf-help}
 *	is being called while not in the minibuffer, an error
 *	occurs.
 *  SeeA: *minibuffer* minibuf-complete self-insert minibuf-input
 */
MKSTRING(HELPBUFFER_NAME, "*completions*");

static int	compl_window;

extern char	**(*type_matches)();

#define HELPCOLS	80
#define HELPROWS	24

DEFUN(dominbhelp, "minibuf-help", FLAG_NONE, "")
{
	extern char		**compl_match();
	extern struct string	*copy_region();
	extern struct window	*minibuf_window;
	struct value		ret;
	struct string		*text;
	struct buffer		*out;
	struct window		*win;
	char			*partial, **matches;
	register char		**str;
	register int		count, cols, nper, rows;
	register int		len, maxlen, n, tlen;
	struct source		*srcp;
	char			geom[100];

	if (current_buffer != minibuffer || !minibuf_used)
		return (v_nil);
	if (type_matches == NULL) {
		/* not to complete, call self-insert instead */
		if (typed_keys == NULL || typed_keys->st_length < 1) {
			/* nothing to insert! */
			return (v_nil);
		} else {
			insert_char(minibuffer, *typed_keys->st_buffer);
			minibuf_window->wi_point++;
			return (v_t);
		}
	}

	/* get the minibuffer source information */
	srcp = minibuffer->bu_sdata;
	ASSERT(srcp != NULL);

	/* copy the text out and find the matches */
	tlen = srcp->sb_length - srcp->sb_start;
	if (tlen <= 0) {
		/* all the completions are possible matches */
		partial = NULL;
		debug(DINPUT, "Getting all possible completion matches.");
	} else {
		tlen++;
		partial = alloca(tlen);
		text = copy_region(minibuffer,
				   srcp->sb_start, srcp->sb_length);
		makecstring(text, partial, tlen);
		debug(DINPUT, "Looking for all matches to \"%s\"...", partial);
	}
	matches = (*type_matches)(partial);
	if (matches == NULL || *matches == NULL)
		return (v_nil);

	PROTECT();
	/* get the output buffer and window */
	out = buffer_create(HELPBUFFER_NAME, BUFF_SOURCE, BUFF_KILLOK);
	ASSERT(out != NULL);

	/* count number of possible completions and maximum length */
	maxlen = 0;
	for (str = matches; *str != NULL; str++) {
		len = strlen(*str);
		if (len > maxlen)
			maxlen = len;
	}
	count = str - matches;

	/* figure out columns and items per column */
	cols = HELPCOLS / (maxlen + 1);
	if (cols < 1)
		cols = 1;
	if (count % cols == 0)
		nper = count / cols;
	else
		nper = (count / cols) + 1;

	/* write the information into the buffer */
	str = matches;
	maxlen++;
	for (n = 0; n < count; n++) {
		if ((n > 0 && (n % cols) == cols - 1) || n >= count - 1)
			bappend(out, "%s\n", *str);
		else
			bappend(out, "%-*s", maxlen, *str);
		str++;
	}
	out->bu_flags = BUFF_READONLY|BUFF_CHANGED|BUFF_NEWMODE;

	ASSERT(minibuf_window != NULL);
	rows = nper + 1;
	if (rows > HELPROWS)
		rows = HELPROWS;
	sprintf(geom, "=%dx%d+%d+%d",
		HELPCOLS, rows,
		minibuf_window->wi_xpos,
		minibuf_window->wi_ypos - GETHEIGHT(minibuf_window, rows) - 5);

	/* clean up any existing window and make new one */
	win = makewindow(geom, NULL, out);
	clean_complwin();
	compl_window = win->wi_index;
	UNPROTECT();

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, count);
	return (ret);
}

clean_complwin()
{
	struct window	*win;

	if (compl_window > 0) {
		for (win = window_list; win != NULL; win = win->wi_next)
			if (win->wi_index == compl_window)
				break;
		if (win != NULL)
			killwindow(win, TRUE);
	}
	compl_window = -1;
}

/*
 *  DOCUMENTATION
 *
 *  Name: minibuf-return
 *  Call: (minibuf-return)
 *  Retu: nil
 *  Desc: This function is bound to the return key just for
 *	the minibuffer.  It is called to terminate the special
 *	edit in the minibuffer that is called when input is
 *	to be read from the terminal.
 *
 *	This function should only be used in the minibuffer;
 *	it has no real meaning in any other.  If \sym{minibuf-return}
 *	is being called while not in the minibuffer, an error
 *	will occur.
 *  Side: If completion is being done on the input, and an
 *	exact match is required, \sym{minibuf-return} will check
 *	to make sure that a valid entry is about to be entered.
 *  SeeA: *minibuffer* minibuf-complete minibuf-input
 */
extern int	exactmatch;

DEFUN(dominbreturn, "minibuf-return", FLAG_NONE, "")
{
	extern char	*alloca();
	struct string	*copy_region();
	extern int	edit_return;
	struct string	*text, add;
	int		status, tlen;
	char		*partial, *result;
	struct source	*srcp;

	if (current_buffer != minibuffer || !minibuf_used)
		return (v_nil);

	/* if we aren't doing completion, just return */
	if (type_complete == NULL || !exactmatch) {
		edit_return = TRUE;
		clean_complwin();
		return (v_nil);
	}

	/* get the minibuffer source information */
	srcp = minibuffer->bu_sdata;
	ASSERT(srcp != NULL);

	/* copy the text out and perform the completion */
	tlen = srcp->sb_length - srcp->sb_start;
	result = alloca(MAXCOMPL);
	*result = '\0';
	if (tlen <= 0) {
		/* there is no partial string */
		partial = "";
	} else {
		tlen++;
		partial = alloca(tlen);
		text = copy_region(minibuffer,
				   srcp->sb_start, srcp->sb_length);
		makecstring(text, partial, tlen);
	}
	status = (*type_complete)(partial, result, MAXCOMPL);

	/* handle the result of completion */
	if (result != NULL && *result != '\0') {
		/* did some completion here */
		add.st_buffer = (unsigned char *)result;
		add.st_length = strlen(result);
		append_string(minibuffer, &add);
		/* move point to the end of the buffer */
		srcp->sb_point = srcp->sb_length;
		minibuf_window->wi_point = srcp->sb_point;
	}

	/* return from the recursive edit if we can */
	if (exactmatch && status != COMPL_ONEMATCH && status != COMPL_GOTONE) {
		beep(0);
		return (v_nil);
	} else {
		edit_return = TRUE;
		clean_complwin();
		return (v_t);
	}
}
