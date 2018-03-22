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
 *  RCS Info: $Header: vse.h,v 0.1 87/04/30 20:56:40 john Locked $
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
 *  vse.h - VorTeX source editor general include file
 */
 
#ifndef _VSE_
#define _VSE_

/*
 *  Here are constants particular to the lisp interpreter,
 *  but used throughout the code.  These are here because
 *  they aren't really specific enough to go into any other
 *  include file.
 */
#define SMALLBUF	128	/* size of a small character buffer */
#define STRBUF		1024	/* size of a standard character buffer */
#define BIGBUF		10240	/* size of a large character buffer */

#define FLAG_NONE	0	/* no flags given, for all flag types */

/*
 *  Here are the types of various utility functions that
 *  are used throughout.  Note that the valloc() mentioned
 *  here uses the malloc(3) stuff.
 */
extern char	*valloc(), *alloca(), *strsave();
extern int	vfree();

/*
 *  These function convert a vlisp string into a C character
 *  array as a null terminated string.  Since vlisp strings
 *  may be any length and may contain NUL (ASCII 0) characters,
 *  we need to do several things.  Nulls in the string given
 *  are just removed from the output string, which is always
 *  null terminated within the size given.  Makepstring also
 *  makes non-printing characters visible as ^C escapes for
 *  the character control-C, and null appears as ^@.
 */
extern int	makecstring();
extern int	makepstring();

/*
 *  Basename() returns the final component of a pathname, which
 *  is the actual file name.  Fixpath() takes a pathname and
 *  ``cleans it up'', removing and resolving tildes (home directory
 *  references) and "." and ".." directories in the path.  The
 *  gethomedir() function returns the home directory of the current
 *  user; first the environment variable HOME is checked, then the
 *  home directory as given in the passwd(5) file.
 */
extern char	*basename(), *fromhome(), *fixpath(), *gethomedir();

/*
 *  Here are some useful values if we're not going to
 *  include some standard system files.  These may or may
 *  not be in different system files, it's always better
 *  to include these .h files after all the system .h files.
 */
#undef TRUE
#define TRUE	1
#undef FALSE
#define FALSE	0

#undef NULL
#define NULL	0

/*
 *  Select(2) file descriptor mask macros by Jeffrey McCarrell.
 *  FDM_SIZE is used to declare the bit mask arrays.
 */
#define BPI		(NBBY * NBPW)	/* bits per int */

#define FDM_SIZE	CDIV(NOFILE, BPI)

#define FDM_SET(a,b)	((a[((b)/BPI)]) |= (1<<((b)%BPI)))
#define FDM_CLR(a,b)	((a[((b)/BPI)]) &= (~(1<<((b)%BPI))))
#define FDM_ISSET(a,b)	((a[((b)/BPI)]) & (1<<((b)%BPI)))
#define FDM_ISCLR(a,b)	(((a[((b)/BPI)]) & (1<<((b)%BPI))) == 0)

/*
 *  Constants for states of completion.  Completion routines
 *  return one of these for each of the five possible results
 *  of a completion attempt.
 */
#define COMPL_NOMATCH	0	/* no matches at all */
#define COMPL_ONEMATCH	1	/* one complete, unique match */
#define COMPL_GOTONE	2	/* one match, but not unique */
#define COMPL_SOME	3	/* some done, but not all */
#define COMPL_NOTUNIQ	4	/* not unique; no completion */

/*
 *  Macros of general utility.  These also may be in some of
 *  the system files, but we can't always depend on them.
 */
#undef CONTROL
#define CONTROL(c)	('c' & 037)

#define ROUND(f)	((int)((f) + 0.5))
#define PLURAL(d)	((d) == 1 ? "" : "s")
#define NITEMS(a)	(sizeof (a) / sizeof (*(a)))

#undef ABS
#define ABS(d)		((d) < 0 ? -(d) : (d))

#undef CDIV
#define CDIV(a,b)	(((a) + ((b) - 1)) / (b))

/*
 *  These macros set and unset signal blocking in a way that
 *  optimizes the actual number of system calls needed.  The
 *  integer _curmask always contains the current signal mask
 *  so we can check if we actually need to change anything
 *  before calling the sigsetmask() system call.
 */
extern int	_curmask, _protcount;

#define SIGMASK(s)	(1 << ((s) - 1))

#define ISBLOCKED(s)	((_curmask & SIGMASK(s)) ? 1 : 0)

#define SIGBLOCK(s)	(ISBLOCKED(s) ? 0 : \
			    _curmask |= SIGMASK(s), sigsetmask(_curmask))
#define SIGRELSE(s)	(ISBLOCKED(s) ? \
			    _curmask &= ~SIGMASK(s), sigsetmask(_curmask) : 0)

#define GETSIGMASK()	(_curmask)
#define SETSIGMASK(m)	(((m) != _curmask) ? sigsetmask(_curmask = (m)) : 0)

/*
 *  Now that we've set up the nice signal-changing paradigm, we
 *  break it.  PROTECT and UNPROTECT are to bracket segments of
 *  code which are non-interruptable.  However, since we ignore
 *  the signall-setting macros above, they cannot be used during
 *  the duration of a PROTECT(), or things may get strange.
 */
#define PROTMASK	(SIGMASK(SIGINT)|SIGMASK(SIGALRM)|SIGMASK(SIGCHLD))

#define PROTECT()	((_protcount <= 0) ? \
			 (sigblock(PROTMASK), (_protcount = 1)) : \
			 _protcount++)
#define UNPROTECT()	((--_protcount <= 0) ? \
			 (sigsetmask(_curmask), 0) : \
			 _protcount)

/*
 *  If we're running the visual editor, havedisplay is set non-zero
 *  one we've sucessfully gotten an X display connection.  Various things
 *  examine this flag to determine behaviour.  If we're done with the
 *  initialization, then initialized will be set true also.  We don't want
 *  to generate errors during cleanup, so quitting will be set once we're
 *  committed to doing so.  If this process is a child, we set subprocess
 *  so that we can avoid various badnesses.
 */
extern int	havedisplay, initialized, quitting, subprocess;

/*
 *  This macro is a safety net for things that can't really happen,
 *  but we check for them just in case.  The argument to ASSERT should
 *  be a C conditional expression, if the expresion is false, we
 *  generate an internal error.  The actual strings message is kept
 *  elsewhere so it doesn't get duplicated throughout the C program.
 */
extern char	ASSERT_MSG[];
#define ASSERT(c)	((c) ? 1 : ierror(ASSERT_MSG, __FILE__, __LINE__))

/*
 *  Timeouts for the IPC connections.  The timeouts are all specified
 *  in milliseconds (1000ths of a second).  We never use that precision,
 *  but this gives us an easy way to change timeouts slightly.  The
 *  proof and formatter timeouts can be deleted, in which case the
 *  connection attempts will never time out (a bad idea).
 */
#define PROOF_TIMEOUT	15000		/* proof/source connection */
#define FORMAT_TIMEOUT	15000		/* formatter/source connection */

#define LISTEN_TIMEOUT	20000		/* GLC_LISTENAT timeout */
#define CONNECT_TIMEOUT	10000		/* GLC_CONNECT timeout */

#define MAX_PORT_TRIES	100		/* maximum ports to try binding */

/*
 *  Debugging flags.  We keep a word of flags which represents the
 *  debugging state, which is the global debug_state.  The lisp
 *  routine debug sets and clears flags in this word.  These flags
 *  are used in two ways.  In deciding what to print, through the
 *  C routine debug(), which prints the given message to stderr if
 *  the given debug state is enabled.  Also, the macro debugging()
 *  is defined below for in-line if statements in C.
 */
extern long		debug_states;	/* debug states word */

#define DALWAYS		0		/* always debugging this */
#define DALLOC		(1 << 0)	/* storage allocation */
#define DPAINT		(1 << 1)	/* window painting */
#define DXIO		(1 << 2)	/* X events and I/O */
#define DCALL		(1 << 3)	/* function calls */
#define DSTACK		(1 << 4)	/* call stack pushes/pops */
#define DPROC		(1 << 5)	/* UNIX process handling */
#define DREAD		(1 << 6)	/* lisp reader */
#define DINPUT		(1 << 7)	/* keyboard input */
#define DTHROW		(1 << 8)	/* catch/throw calls */
#define DBUFFER		(1 << 9)	/* buffer processing */
#define DWINDOW		(1 << 10)	/* window manipulations */
#define DCONN		(1 << 11)	/* IPC connections */
#define DITEX		(1 << 12)	/* iTeX connection */
#define DPROOF		(1 << 13)	/* proof connection */
#define DALL		(DALLOC|DPAINT|DXIO|DCALL|DSTACK|DPROC|\
			 DREAD|DINPUT|DTHROW|DBUFFER|DWINDOW|\
			 DCONN|DITEX|DPROOF)

#define debugging(w)	((w) == 0 || (debug_states & (w)) != 0)

#endif !_VSE_
