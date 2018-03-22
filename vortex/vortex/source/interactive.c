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
 *  RCS Info: $Header: interactive.c,v 0.1 87/05/01 12:18:48 john Locked $
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
 *  interactive.c - routines to call vLisp functions interactively
 */
static char _ID[] = "@(#)interactive.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "symtab.h"
#include "function.h"
#include "window.h"
#include "document.h"

MKSTRING(DEFPROMPT, ": ");

struct value
call_interactive(bound, keys)
	struct value	bound;
	struct string	*keys;
{
	extern char		*nthname();
	extern struct value	sread();
	extern struct string	*keys_input();
	extern struct value	sym_input(),
				cmd_input(),
				buff_input(),
				file_input(),
				dir_input();
	extern struct value	global_keymap;
	extern int		mouse_Xpos, mouse_Ypos, mouse_window;
	struct value		get_prefix();
	struct string		*minb_input();
	char			nbuf[SMALLBUF];
	struct value		fval, alist, arg, val;
	struct function		*func;
	struct string		*prompt, *input;
	register int		argn;
	struct window		*winp = current_window;
	struct buffer		*bufp = NULL;
	int			min, max;

	if (!funcp(bound)) {
		fval = evalsexpr(bound);
		if (!funcp(fval)) {
			debug(0, "Trying to call %v interactively!", fval);
			if (keys != NULL && keys->st_length > 0)
				error("Binding of %K isn't a function!", keys);
			else
				error("Command %v isn't a function!", bound);
		}
	} else {
		/* it already is a function */
		fval = bound;
	}

	/* bound is certainly a function */
	func = gfunct(fval.vl_data);
	ASSERT(func != NULL);
	if ((func->fn_flags & FUNC_INTERACT) == 0)
		error("Sorry, function %v isn't interactive!", bound);

	/* get the current window/buffer */
	if (winp->wi_type == WIN_BUFFER) {
		bufp = winp->wi_buffer;
		ASSERT(bufp != NULL);
	}

	/* make up argument list for this function */
	alist = v_nil;
	for (argn = 0; argn < func->fn_iargc; argn++) {
		/* get the argument prompt (if any) */
		prompt = func->fn_prompt[argn];
		if (prompt == NULL)
			prompt = DEFPROMPT;

		/* get the argument as appropriate */
		switch (func->fn_atype[argn]) {
		case ARG_LISPTYPE:	/* read as lisp reader */
			input = minb_input(prompt, NULL);
			arg = sread(input);
			break;
		case ARG_NUMBER:	/* argument must be a fixnum */
			input = minb_input(prompt, NULL);
			makecstring(input, nbuf, sizeof (nbuf));
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, atoi(nbuf));
			break;
		case ARG_SYMBOL:	/* a symbol (text string) */
		case ARG_FUNCTION:	/* a lisp function name */
		case ARG_VARIABLE:	/* a lisp variable name */
			arg = sym_input(prompt, FALSE);
			break;
		case ARG_EFUNCTION:	/* an existing function name only */
		case ARG_EVARIABLE:	/* an existing variable only */
			arg = sym_input(prompt, TRUE);
			break;
		case ARG_COMMAND:	/* a command (interactive function) */
			arg = cmd_input(prompt, TRUE);
			break;
		case ARG_STRING:	/* a string (in double quotes) */
			input = minb_input(prompt, NULL);
			arg.vl_type = LISP_STRING;
			sstring(arg.vl_data, input);
			break;
		case ARG_FILENAME:	/* a possible UNIX file */
			arg = file_input(prompt, FALSE);
			break;
		case ARG_EFILENAME:	/* an existing file name only */
			arg = file_input(prompt, TRUE);
			break;
		case ARG_DIRECTORY:	/* an existing directory name only */
			arg = dir_input(prompt, TRUE);
			break;
		case ARG_BUFFER:	/* a possible buffer name */
			arg = buff_input(prompt, FALSE);
			break;
		case ARG_EBUFFER:	/* an existing buffer name only */
			arg = buff_input(prompt, TRUE);
			break;
		case ARG_KEYS:		/* a key sequence */
			input = keys_input(prompt);
			arg.vl_type = LISP_STRING;
			sstring(arg.vl_data, input);
			break;
		case ARG_BOUNDTO:	/* key sequence which invoked it */
			if (keys == NULL) {
				error("%v wasn't called as a key binding!",
				      bound);
			}
			arg.vl_type = LISP_STRING;
			sstring(arg.vl_data, keys);
			break;
		case ARG_RAWPREFIX:	/* the command prefix in raw form */
			arg = get_prefix();
			break;
		case ARG_PREFIX:	/* the command prefix argument */
			arg = get_prefix();
			if (eq(arg, v_nil))
				arg = v_one;
			else if (eq(arg, v_t))
				arg = v_four;
			break;
		case ARG_PREFIXNUM:	/* prefix argument or number */
			arg = get_prefix();
			if (eq(arg, v_t)) {
				/* this was a C-u or something */
				arg = v_four;
			} else if (eq(arg, v_nil)) {
				input = minb_input(prompt, NULL);
				makecstring(input, nbuf, sizeof (nbuf));
				arg.vl_type = LISP_FIXNUM;
				sfixnum(arg.vl_data, atoi(nbuf));
			}
			break;
		case ARG_POINTPOS:	/* the window buffer's point */
			arg.vl_type = LISP_FIXNUM;
			if (bufp == NULL)
				error("Window isn't a buffer--no point!");
			else if (bufp->bu_type == BUFF_SOURCE)
				sfixnum(arg.vl_data, winp->wi_point);
			else if (bufp->bu_type == BUFF_PROOF)
				sfixnum(arg.vl_data, winp->wi_curpage);
			else
				sfixnum(arg.vl_data, 0);
			break;
		case ARG_MARKPOS:	/* the window buffer's mark */
			arg.vl_type = LISP_FIXNUM;
			if (bufp == NULL) {
				error("Window isn't a buffer--no mark!");
			} else if (bufp->bu_type == BUFF_SOURCE) {
				if ((max = winp->wi_mark) < 0)
					error("No mark set in this window!");
				sfixnum(arg.vl_data, winp->wi_mark);
			} else if (bufp->bu_type == BUFF_PROOF) {
				if ((max = winp->wi_mark) < 0)
					error("No position in this window!");
				sfixnum(arg.vl_data, winp->wi_position);
			} else {
				sfixnum(arg.vl_data, 0);
			}
			break;
		case ARG_REGION:	/* the current region in order */
			if (bufp == NULL || bufp->bu_type != BUFF_SOURCE)
				error("Window isn't a source buffer--no region!");
			min = winp->wi_point;
			if ((max = winp->wi_mark) < 0)
				error("No mark set in this window!");
			if (min > max) {
				int	tmp;

				tmp = min;
				min = max;
				max = tmp;
			}
			/* have two arguments to do, so... */
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, min);
			alist = append(alist, arg);
			/* the second one can wait */
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, max);
			break;
		case ARG_START:	/* start position in buffer */
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, getstart(bufp));
			break;
		case ARG_LENGTH:	/* end of buffer */
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, getlength(bufp));
			break;
		case ARG_CURWIN:	/* current window number */
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, current_window->wi_index);
			break;
		case ARG_XRAWPOS:	/* raw X position */
			arg.vl_type = LISP_FIXNUM;
			if (mouse_window != current_window->wi_index)
				sfixnum(arg.vl_data, -1);
			else
				sfixnum(arg.vl_data, mouse_Xpos);
			break;
		case ARG_YRAWPOS:	/* raw Y position */
			arg.vl_type = LISP_FIXNUM;
			if (mouse_window != current_window->wi_index)
				sfixnum(arg.vl_data, -1);
			else
				sfixnum(arg.vl_data, mouse_Ypos);
			break;
		case ARG_XPOS:		/* reasonable X position */
			if (mouse_window != current_window->wi_index) {
				/* prompt for the number */
				input = minb_input(prompt, NULL);
				makecstring(input, nbuf, sizeof (nbuf));
				arg.vl_type = LISP_FIXNUM;
				sfixnum(arg.vl_data, atoi(nbuf));
			} else {
				arg.vl_type = LISP_FIXNUM;
				sfixnum(arg.vl_data, mouse_Xpos);
			}
			break;
		case ARG_YPOS:		/* reasonable Y position */
			if (mouse_window != current_window->wi_index) {
				/* prompt for the number */
				input = minb_input(prompt, NULL);
				makecstring(input, nbuf, sizeof (nbuf));
				arg.vl_type = LISP_FIXNUM;
				sfixnum(arg.vl_data, atoi(nbuf));
			} else {
				arg.vl_type = LISP_FIXNUM;
				sfixnum(arg.vl_data, mouse_Ypos);
			}
			break;
		case ARG_DOCUMENT:	/* the current document */
			if (current_document == NULL)
				error("There is no current document!");
			arg.vl_type = LISP_FIXNUM;
			sfixnum(arg.vl_data, current_document->dc_rootID);
			break;
		default:	/* unknown argument type */
			error("Bad type %d of %s interactive argument!",
			    func->fn_atype[argn], nthname(argn));
			/* NOTREACHED */
		}

		/* append this argument to the list */
		alist = append(alist, arg);
	}

	val = call_function(bound, fval, alist);

	/* clean up afterwards */
	if ((func->fn_flags & FUNC_PREFIX) == 0)
		clear_prefix();

	return (val);
}

/*
 *  The routines below all use completion facilities, but must
 *  do things a little strangely to make the completion work
 *  properly in the different cases.
 *
 *  There is a type-specific function to return the list of
 *  possible completion matches in the variable type_matches.
 *  And a completion function to operate on them stored in
 *  type_complete.  If the prompt should not return without
 *  find an exact match, exactmatch will be set.
 */
int	(*type_complete)() = NULL;	/* complete for type */
char	**(*type_matches)() = NULL;	/* matches for type */
int	exactmatch = FALSE;		/* need an exact match */

static struct value
sym_input(prompt, exact)
	struct string	*prompt;
{
	extern char	**match_symbols();
	extern int	compl_symbols();
	struct string	*input;
	char		**(*omatch)();
	int		(*ocompl)();
	int		oematch;
	struct value	ret;

	/* save old completion state and set new */
	omatch = type_matches;
	type_matches = match_symbols;
	ocompl = type_complete;
	type_complete = compl_symbols;
	oematch = exactmatch;
	exactmatch = exact;

	/* prompt for the symbol name in the minibuffer */
	input = minb_input(prompt, NULL);

	/* restore old completion state */
	type_matches = omatch;
	type_complete = ocompl;
	exactmatch = oematch;

	/* if a null string is read, return nil */
	if (input == NULL || input->st_length < 1)
		return (v_nil);

	/* otherwise, return the buffer name as a string */
	ret.vl_type = LISP_SYMBOL;
	ssymbol(ret.vl_data, save_symbol(input));
	return (ret);
}

static struct value
cmd_input(prompt, exact)
	struct string	*prompt;
{
	return sym_input(prompt, exact);
}

static struct value
buff_input(prompt, exact)
	struct string	*prompt;
{
	extern char	**match_buffers();
	extern int	compl_buffers();
	struct string	*input;
	char		**(*omatch)();
	int		(*ocompl)();
	int		oematch;
	struct value	ret;

	/* save old completion state and set new */
	omatch = type_matches;
	type_matches = match_buffers;
	ocompl = type_complete;
	type_complete = compl_buffers;
	oematch = exactmatch;
	exactmatch = exact;

	/* prompt for the buffer name in the minibuffer */
	input = minb_input(prompt, NULL);

	/* restore old completion state */
	type_matches = omatch;
	type_complete = ocompl;
	exactmatch = oematch;

	/* if a null string is read, return nil */
	if (input == NULL || input->st_length < 1)
		return (v_nil);

	/* otherwise, return the buffer name as a string */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, input);
	return (ret);
}

static struct value
file_input(prompt, exact)
	struct string	*prompt;
{
	extern char	**match_files();
	extern int	compl_files();
	struct string	*input;
	char		**(*omatch)();
	int		(*ocompl)();
	int		oematch;
	struct value	ret;
	char		cwd[1025];
	register char	*cp;
	struct string	*dir;

	/* save old completion state and set new */
	omatch = type_matches;
	type_matches = match_files;
	ocompl = type_complete;
	type_complete = compl_files;
	oematch = exactmatch;
	exactmatch = exact;

	/* get the current directory */
	if (getwd(cwd) == NULL || *cwd == '\0')
		strcpy(cwd, ".");
	else
		strcpy(cwd, fromhome(cwd));

	/* make sure it ends with a slash */
	for (cp = cwd; *cp != '\0'; cp++)
		;
	if (cp[-1] != '/') {
		*cp++ = '/';
		*cp = '\0';
	}
	dir = save_string(cwd, cp - cwd);

	/* prompt for the directory name in the minibuffer */
	input = minb_input(prompt, dir);

	/* restore old completion state */
	type_matches = omatch;
	type_complete = ocompl;
	exactmatch = oematch;

	/* if a null string is read, return nil */
	if (input == NULL || input->st_length < 1)
		return (v_nil);

	/* otherwise, return the file name as a string */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, input);
	return (ret);
}

static struct value
dir_input(prompt, exact)
	struct string	*prompt;
{
	extern char	**match_dirs();
	extern int	compl_dirs();
	struct string	*input;
	char		**(*omatch)();
	int		(*ocompl)();
	int		oematch;
	struct value	ret;
	char		cwd[1025];
	register char	*cp;
	struct string	*dir;

	/* save old completion state and set new */
	omatch = type_matches;
	type_matches = match_dirs;
	ocompl = type_complete;
	type_complete = compl_dirs;
	oematch = exactmatch;
	exactmatch = exact;

	/* get the current directory */
	if (getwd(cwd) == NULL || *cwd == '\0')
		strcpy(cwd, ".");

	/* make sure it ends with a slash */
	for (cp = cwd; *cp != '\0'; cp++)
		;
	if (*--cp != '/') {
		*cp++ = '/';
		*cp = '\0';
	}
	dir = save_string(cwd, cp - cwd);

	/* prompt for the directory name in the minibuffer */
	input = minb_input(prompt, dir);

	/* restore old completion state */
	type_matches = omatch;
	type_complete = ocompl;
	exactmatch = oematch;

	/* if a null string is read, return nil */
	if (input == NULL || input->st_length < 1)
		return (v_nil);

	/* otherwise, return the directory name as a string */
	ret.vl_type = LISP_STRING;
	sstring(ret.vl_data, input);
	return (ret);
}

static struct string *
keys_input(prompt)
	struct string	*prompt;
{
	extern struct value	get_binding();
	extern char		*pkeycode();
	static struct string	keys;
	unsigned char		kbuf[1024];
	int			code;
	struct value		bound;
	char			image[2048];
	register char		*cp, *ip;

	PROTECT();
	/* copy the prompt onto the image string */
	makecstring(prompt, image, sizeof (image) / 2);
	for (cp = image; *cp != '\0'; cp++)
		;
	minb_print(image, cp - image, FALSE);
	XFlush();

	/* read input key strokes until one that can be bound */
	keys.st_buffer = kbuf;
	keys.st_length = 0;
	do {
		/* handle the next input character */
		input_event(TRUE, &code);
		/* we've read next key into code */
		debug(DINPUT, "Read input key stroke `%k' code %o.",
		      code, code);

		/* add it to the sequence in the minibuffer */
		for (ip = pkeycode(code); *ip != '\0'; ip++)
			*cp++ = *ip;
		*cp++ = ' ';
		minb_print(image, cp - image, FALSE);
		XFlush();

		/* add the key code to the string */
		if (keys.st_length >= sizeof (kbuf)) {
			error("Key sequence too long (maximum %d)!",
			      sizeof (kbuf));
		}
		keys.st_buffer[keys.st_length++] = code & 0377;
		bound = get_binding(&keys, global_keymap);
	} while (stringp(bound) || listp(bound));
	UNPROTECT();

	return save_string(keys.st_buffer, keys.st_length);
}
