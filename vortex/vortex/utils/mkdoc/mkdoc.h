/* 
 * Copyright (c) 1986 The Regents of the University of California.
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
 *  VorTeX -- Visually ORiented TeX
 *
 *  Peehong Chen, John Coker, Jeff McCarrell, Steve Procter
 *  for Prof. Michael Harrison of the Computer Science Division
 *  University of California, Berkeley
 *
 *  mkdoc: prepare documentation from lisp and C source files
 *
 *  mkdoc.h - general include file
 */

#include <stdio.h>
#include <ctype.h>
#include "docstr.h"

#define isblank(c)	((c) == ' ' || (c) == '\t')

/*
 *  These constants are used throughout the program.  They're
 *  defined here for easy inclusion in all the source files.
 */
#define TRUE	1
#define FALSE	0
#define STRBUF	1024
#define BIGBUF	8192

/*
 *  These variables are flags for options.  Each option has
 *  a corresponding flag, and some of these flags are a result
 *  of these options and other considerations.
 */
extern int	csource;
extern int	asciiout;
extern char	*header;
extern char	*trailer;

/*
 *  These are the current file/line indicators for error
 *  message printing.  They need to be carefully update by
 *  all routines that read source code so that informative
 *  error messages can be produced.
 */
extern char	*errfile;
extern int	errline;

/*
 *  These macros provide transparent input (with line counting)
 *  and long pushback (sizeof (_unputb)).  The macro start()
 *  initializes the pushback buffer and should be called before
 *  the first input.  These macros assume that the file pointer
 *  for input will be called filep where these macros are called.
 */
#define input()		((_unputp > _unputb) ? *--_unputp : \
			 ((_ch = getc(filep)) == '\n' || _ch == '\r') ? \
			 errline++, '\n' : _ch)
#define unput(c)	(*_unputp++ = (c) & 0177)
#define start()		(_unputp = _unputb)
extern char	_unputb[512], *_unputp;
extern int	_ch;

/*
 *  These data structures describe what to do with formatting
 *  commands for both ASCII and TeX output.  Each formatting
 *  command is given a separate function when formatting for
 *  ASCII and TeX output.  The (struct fmtcmd) describes what
 *  to do with one formatting command, and what strings (if any)
 *  to pre- and append to the text enclosed by that command.
 *
 *  Since these formatting commands may be nested, we maintain
 *  a stack of current ``environments'', with the top of the
 *  stack being the current environment.  When a ``\cmd{'' the
 *  state appropriate to that command is pushd and the current
 *  action changes to the appropriate one for that command.
 *  When an unescaped ``}'' is seen, the current state is
 *  popped and the previous action becomes in force once again.
 *
 *  Note that the characters which are special to the formatter
 *  (`/', `{' and `}') can be escaped with a backslash (which
 *  is thrown away) to prevent them from looking like format
 *  commands.  The only time when a ``\cmd{'' will not be
 *  recognized is inside a VERBATIM action.  Thus, there cannot
 *  be any sub-environments of a VERBATIM.
 *
 *  Also, a character translation table can be specified which
 *  gives mappings of single characters into text strings.  This
 *  is used to translate newlines for ASCII formatting and the
 *  special characters of TeX for typeset formatting.  Each
 *  translation specifies the character that is to be mapped,
 *  when that mapping is appropriate, and the string to map it
 *  to, which may be empty.
 */
#define COPYTEXT	(1 << 0)	/* copy out a stream of text */
#define COPYWORD	(1 << 1)	/* copy out a single word */
#define VERBATIM	(1 << 2)	/* copy out verbatim, no escapes */
#define TABULAR		(1 << 3)	/* create TeX commands for a table */
#define DELETE		(1 << 4)	/* ignore subject text eompletely */
#define SPECIAL1	(1 << 5)	/* output specific mode one */
#define SPECIAL2	(1 << 6)	/* output specific mode two */
#define SPECIAL3	(1 << 7)	/* output specific mode three */
#define ALLACTS		(COPYTEXT|COPYWORD|VERBATIM|TABULAR|\
			 DELETE|SPECIAL1|SPECIAL2|SPECIAL3)
#define EXCEPT(a)	(ALLACTS & ~(a))

struct fmtcmd {
	char	*fc_name;	/* name of markup command */
	char	*fc_pre;	/* string prepended */
	char	*fc_post;	/* string appended */
	int	fc_act;		/* actionto take with arg */
};

struct xlate {
	char	xl_char;	/* character to look for */
	int	xl_acts;	/* actions when done */
	char	*xl_repl;	/* replacement text */
};

#define MAXSTACK	100	/* maximum depth of command stack */
