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
 *  RCS Info: $Header: edit.c,v 0.1 87/05/01 12:10:05 john Locked $
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
 *  edit.c - editor top-level routines
 */
static char _ID[] = "@(#)edit.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "catch.h"
#include "channel.h"
#include "process.h"
#include "window.h"

/*
 *  The catch tag names.  We need to set them in edit_loop so that
 *  the internal long_jmp goes to the right place.  Once we enter
 *  edit_loop, we never can return.
 */
extern struct string	*TOPLEVCATCH,	/* for ``clean'' throws to top-level */
			*ERRORCATCH,	/* for errors if no break loop */
			*ABORTCATCH;	/* for ^G in the visual editor */

/*
 *  Global variables used internally for referencing actions.
 *  The typed_keys string contains the last sequence typed as
 *  an interactive command.  The typed_count variable holds
 *  a count of the bound key sequences that have been typed.
 *  So, the maximum number of key strokes in one binding is
 *  the size of KEYBUF.
 */
static unsigned char	KEYBUF[1024];
MKSTRLEN(typed_keys, KEYBUF, 0);
long		typed_count = 1;

/*
 *  We call edit_loop recursively to do recursive edits.  On
 *  the later calls, the function is expected to return when
 *  the variable edit_return is set TRUE.  However, we only
 *  do so if edit_recurse is greater than zero.
 */
int		edit_return = FALSE;
int		edit_recurse = 0;

static jmp_buf	tplbuf, errbuf, abtbuf;

/*
 *  We keep the proof and formatter sockets here so that we can
 *  select on them in the editor loop, if they are valid file
 *  descriptors.
 */
extern int	proof_socket;
extern int	format_socket;

/*
 *  Requests may have been read and bufferd by the system due to
 *  blocking waits for a specific query return.  In this case,
 *  checking for pending input is not sufficient, so we signal the
 *  case with these global variables.
 */
extern int	format_buffered;
extern int	proof_buffered;

/*
 *  Every so often we check the pending X input queue for abort
 *  keys read so we can acheive the function of the abort key.
 *  This needs to be done fairly frequently, although we never
 *  allow aborts during PROTECT()ed code.  Here we set ABORT_CHECK
 *  to 500,000 usecs, half a second.
 */
#define ABORT_CHECK	500000

edit_loop()
{
	extern int		interrupt(), ttyoutput(), scanabort();
	static int		donesetup = FALSE;
	unsigned char		msg[STRBUF];
	struct value		val;
	unsigned long		rmask[FDM_SIZE];
	struct timeval		ktmout, ptmout, ctmout;
	struct buffer		*bufp;
	int			nfds, code;

	/* check for temporary recursive edits */
	if (donesetup) {
		edit_recurse++;
		edit_return = FALSE;
		goto doedit;
	}

	/* set up top-level catch here */
	switch (setjmp(tplbuf)) {
	case -1:	/* error */
		panic("Set jump failed for top-level catch!");
	case 0:		/* just been set */
		int_catch(TOPLEVCATCH, tplbuf, FLAG_NONE);
		break;
	default:	/* longjmp back to here */
		debug(DTHROW, "Returned from editor top-level throw.");
		int_message("[return to top-level]");
		goto restart;
	}

	/* set up the abort catch here */
	switch (setjmp(abtbuf)) {
	case -1:	/* error */
		panic("Set jump failed for abort catch!");
	case 0:		/* just been set */
		int_catch(ABORTCATCH, abtbuf, FLAG_NONE);
		break;
	default:	/* longjmp back to here */
		debug(DTHROW, "Returned from editor abort throw.");
		val = throw_value(ABORTCATCH);
		if (!nullp(val)) {
			beep(0);
			err_message("Aborted!");
		}
		goto restart;
	}

	/* set up the error catch here */
	switch (setjmp(errbuf)) {
	case -1:	/* error */
		panic("Set jump failed for error catch!");
	case 0:		/* just been set */
		int_catch(ERRORCATCH, errbuf, FLAG_NONE);
		break;
	default:	/* longjmp back to here */
		debug(DTHROW, "Returned from editor error throw.");
		val = throw_value(ERRORCATCH);
		beep(0);
		if (stringp(val)) {
			makepstring(gstring(val.vl_data), msg, sizeof (msg));
			err_message(msg);
		}
		goto restart;
	}

	/* set up dangerous signals */
	signal(SIGINT, interrupt);
	signal(SIGHUP, interrupt);
	signal(SIGTERM, interrupt);
	signal(SIGPIPE, SIG_IGN);
	signal(SIGTTOU, ttyoutput);
	signal(SIGALRM, scanabort);

	/* done with setting up the editor */
	donesetup = TRUE;
	goto doedit;

restart:
	/* re-initialize call frame and stacked symbols */
	lisp_restore();
	edit_restore();

doedit:
	/* start editing with the current buffer */
	update_screen();

	/* make sure typed_keys string is empty */
	typed_keys->st_length = 0;

	/* set keyboard timeout for 1 second */
	ktmout.tv_usec = 0;
	ktmout.tv_sec = 1;
	/* set process timeout as a poll */
	ptmout.tv_usec = 0;
	ptmout.tv_sec = 0;
	/* set communications timeout as a poll */
	ctmout.tv_usec = 0;
	ctmout.tv_sec = 0;

	while (edit_recurse < 1 || !edit_return) {
		if (XPending() <= 0) {
			/* wait for something to come in */
			bzero(rmask, sizeof (rmask));
			FDM_SET(rmask, dpyno(Xdisplay));
			nfds = select(NOFILE, rmask, NULL, NULL, &ktmout);
		}

		/* handle all pending input characters */
		while (input_event(FALSE, &code) > 0) {
			/* figure out current buffer */
			bufp = current_window->wi_buffer;
			if (bufp->bu_input != NULL)
				(*bufp->bu_input)(bufp, code & 0377);
		}

		/* check for processes which have changed state */
		bcopy(process_mask, rmask, sizeof (rmask));
		nfds = select(NOFILE, rmask, NULL, NULL, &ptmout);
		if (nfds > 0) {
			/* handle process output */
			process_check();
		}

		/* check for action from the formatter process */
		if (format_socket > 0) {
			/* check if something has come in */
			bzero(rmask, sizeof (rmask));
			FDM_SET(rmask, format_socket);
			nfds = select(NOFILE, rmask, NULL, NULL, &ctmout);
			if (format_buffered || nfds > 0)
				recvformat();
		}

		/* check for action from the proof editor */
		if (proof_socket > 0) {
			/* check if something has come in */
			bzero(rmask, sizeof (rmask));
			FDM_SET(rmask, proof_socket);
			nfds = select(NOFILE, rmask, NULL, NULL, &ctmout);
			if (proof_buffered || nfds > 0)
				recvproof();
		}

		/* repaint the screen as necessary */
		update_screen();
	}

	/* we only get here when returing from a recursive edit */
	edit_recurse--;
	edit_return = FALSE;

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: echo-prefix-sequence
 *  Desc: This variable enables the printing of partial key
 *	sequences when typed interactively to the editor.
 *	If this variable is set non-nil, a message is printed
 *	showing the current prefix being typed as a long key
 *	sequence is entered.
 *  SeeA: global-set-key
 */
MKSTRING(ECHOPREFIX_NAME, "echo-prefix-sequence");

text_input(bufp, code)
	struct buffer	*bufp;
	unsigned int	code;
{
	extern struct value	get_binding();
	extern struct value	call_interactive();
	extern struct value	global_keymap;
	struct value		bound;

	/* append it to the current key sequence */
	if (typed_keys->st_length >= sizeof (KEYBUF))
		error("Key sequence too long (maximum %d)!", sizeof (KEYBUF));
	typed_keys->st_buffer[typed_keys->st_length] = code;
	typed_keys->st_length++;

	/* check the key binding */
	bound = get_binding(typed_keys, bufp->bu_keymap);
	if (nullp(bound))
		bound = get_binding(typed_keys, global_keymap);

	/* figure out what this is bound to */
	if (nullp(bound)) {
		debug(DINPUT, "Typed key sequence `%K' unbound.", typed_keys);

		/* this key is unbound */
		beep(0);
		clear_prefix();

		/* increment the interactive command counter */
		typed_count++;
		/* zero the collected key sequence */
		typed_keys->st_length = 0;
	} else if (arrayp(bound) || dtprp(bound)) {
		debug(DINPUT, "Typed key sequence `%K' is a keymap.",
		      typed_keys);

		/* this is a prefix binding */
		if (truevar(ECHOPREFIX_NAME)) {
			/* echo this prefix so far */
			message("%K-", typed_keys);
		}
	} else {
		debug(DINPUT, "Typed key sequence `%K' bound to %v.",
		      typed_keys, bound);

		/* the key is bound; call that value */
		call_interactive(bound, typed_keys);

		/* increment the interactive command counter */
		typed_count++;
		/* zero the collected key sequence */
		typed_keys->st_length = 0;
	}

	return (0);
}

edit_restore()
{
	extern char	**(*type_matches)();
	extern int	(*type_complete)();
	extern int	exactmatch;

	edit_return = FALSE;
	edit_recurse = 0;

	/* throw away pending input and partial key stroke */
	flush_input();
	typed_keys->st_length = 0;

	/* disable completion now */
	type_matches = NULL;
	type_complete = NULL;
	exactmatch = FALSE;
	clean_complwin();

	/* unmark minibuffer as being used */
	clean_minibuf();

	/* trash accumulated prefix */
	clear_prefix();
	UNPROTECT();
}

/*
 *  DOCUMENTATION
 *
 *  Name: break-on-interrupt
 *  Desc: This variable controls the action taken when an interrupt
 *	is recieved by the editor.  If it is set to a non-nil value,
 *	it causes a break loop to be entered when an interrupt is
 *	received by the editor.
 *
 *	The break loop will be continuable, and if the user does
 *	continue (with \sym{cont}), the usual interrupt handling
 *	will continue.  If \sym{abort-on-interrupt} is set, an
 *	abort will be generated, otherwise the editor will save
 *	buffers, kill processes and exit.
 *  SeeA: break-loop break-on-abort abort-on-interrupt
 */
MKSTRING(BREAKONINT_NAME, "break-on-interrupt");

/*
 *  DOCUMENTATION
 *
 *  Name: abort-on-interrupt
 *  Desc: This variable controls the action taken when an interrupt
 *	is recieved by the editor.  If it is set to a non-nil value,
 *	it causes an abort (as though C-g had been typed) to occur.
 *	Otherwise, modified file-visiting buffers are saved, processes
 *	are killed and the editor exits.
 *  SeeA: abort-function break-on-abort break-on-interrupt
 */
MKSTRING(ABORTONINT_NAME, "abort-on-interrupt");

/* ARGSUSED */
static
interrupt(sig)
{
	extern struct value	break_loop();

	/* this must be an external interrupt */
	if (truevar(BREAKONINT_NAME)) {
		/* enter break loop */
		break_loop(TRUE, "Interrupt.");
		/* can return from break loop */
	}
	if (truevar(ABORTONINT_NAME)) {
		/* generate an abort throw */
		int_abort();
	}

	/* clean up buffers and processes and exit */
	int_exit(TRUE);
}

/* ARGSUSED */
static
ttyoutput(sig)
{
	if (havedisplay && !quitting)
		int_message("Some output was sent to our parent tty.");
	signal(SIGTTOU, SIG_IGN);
}
