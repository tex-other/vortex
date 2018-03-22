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
 *  format.c - output independent generic markup formatting
 */

#include "mkdoc.h"

/*
 *  This function does the output-independent generic markup
 *  formatting.  We're called with a list of format commands
 *  and how to process them and a list of output character
 *  translations.  We then scan the given string and output
 *  the characters as requested to the file pointer given.
 *  If fc_curcol is greater than or equal to zero, we do
 *  line filling also.  Our caller may want to set it to a
 *  positive value when we're called in case there is already
 *  something on the first line of the output.
 */
struct fmtcmd	*fc_stack[MAXSTACK];
static int	fc_index = -1;
#define TABWIDTH	8
#define PARINDENT	5

struct xlate	*fc_xlates;	/* current translation list */
static int	fc_action;	/* current action state */

int	fc_maxlength = 75;	/* maximum length of lines */
int	fc_curcol = -1;		/* current column on line */

#define DOTRANS	1	/* okay to do output translations */
#define NOTRANS	0	/* don't do output translations */

#define outchr(c,t)	fc_putchr((c), (t), filep)
#define outstr(s,t)	fc_putstr((s), (t), filep)

unsigned int	ESCAPEC = '\\';
unsigned int	BEGINENVC = '{';
unsigned int	ENDENVC = '}';

markup_format(docstr, cmdlist, xltlist, init, filep)
	char		*docstr;
	struct fmtcmd	*cmdlist;
	struct xlate	*xltlist;
	register FILE	*filep;
{
	char		token[5], *tend = token + sizeof (token) - 1;
	register char	*cp, *tp;
	struct fmtcmd	*fcp;
	char		*ptr, *last;
	register int	len, scount, lcount;

	/* initialize command stack and initial action */
	fc_index = 0;
	for (fcp = cmdlist; fcp->fc_name != NULL; fcp++)
		;
	fc_stack[fc_index] = fcp;
	fc_action = fc_stack[fc_index]->fc_act = init;

	/* initialize global translation list from argument */
	fc_xlates = xltlist;

	cp = docstr;
	while (*cp != '\0') {
		if (fc_action != VERBATIM &&
		    *cp == ESCAPEC && *(cp+1) == ESCAPEC) {
			/* escaped backslash; skipt first and print second */
			cp++;
			outchr(ESCAPEC, DOTRANS);
		} else if (fc_action != VERBATIM && *cp == ESCAPEC) {
			tp = token;
			ptr = cp++;
			while (isalpha(*cp) && tp < tend)
				*tp++ = *cp++;
			*tp = '\0';
			if (*token == '\0' || *cp != BEGINENVC) {
				/* this isn't a markup command after all */
				outchr(ESCAPEC, DOTRANS);
				outstr(token, DOTRANS);
				continue;
			}
			cp++;

			/* search for this markup command */
			tp = token;
			for (fcp = cmdlist; fcp->fc_name != NULL; fcp++)
				if (!strcmp(fcp->fc_name, tp))
					break;
			if (fcp->fc_name == NULL) {
				error(
			    "Unknown markup command \"%s\"; ignored.", tp);
				/* left at correct command, the null one */
			}
			if (++fc_index >= MAXSTACK) {
				error("Markup command stack overflow!");
				return (-1);
			}
			fc_stack[fc_index] = fcp;
			fc_action = fcp->fc_act;

			/* copy out prepend string and rejoin loop at top */
			if (fcp->fc_pre != NULL && *fcp->fc_pre != '\0')
				outstr(fcp->fc_pre, NOTRANS);

			/* reposition pointer one before word begin */
			while (isspace(*cp))
				cp++;
			cp--;
		} else if (*cp == ESCAPEC && *(cp+1) == ENDENVC) {
			/* skip backslash, output this character */
			cp++;
			outchr(ENDENVC, DOTRANS);
		} else if (fc_index > 0 && *cp == ENDENVC) {
			/* finish off previous action state */
			ptr = fc_stack[fc_index]->fc_post;
			if (ptr != NULL && *ptr != '\0')
				outstr(ptr, NOTRANS);
			/* pop this action state off */
			fc_index--;
		fc_action = fc_stack[fc_index]->fc_act;
		} else if (fc_curcol > 0 &&
			   fc_action != TABULAR && isspace(*cp)) {
			/* skip past spaces, couting newlines */
			lcount = 0;
			while (isspace(*cp)) {
				if (*cp == '\n')
					lcount++;
				cp++;
			}
			if (lcount > 1) {
				/* this is a paragraph break */
				outstr("\n\n", DOTRANS);
				continue;
			}

			/* ptr is at first charcter of next word */
			ptr = last = cp;
			/* skip any existing begin-environments */
			while (*cp == ESCAPEC) {
				for (++cp; isalpha(*cp); cp++)
					;
				if (*cp == BEGINENVC) {
					for (++cp; isspace(*cp); cp++)
						;
				} else {
					cp = last;
					break;
				}
				last = cp;
			}

			/* count length of next word to make sure it fits */
			for (len = 0; *cp != '\0' && !isspace(*cp); len++)
				cp++;

			if (fc_curcol + len > fc_maxlength) {
				/* line too long, break it here */
				outchr('\n', DOTRANS);
			} else {
				/* still okay, output a single space */
				outchr(' ', DOTRANS);
			}

			/* back to one before the first character of word */
			cp = ptr - 1;
		} else if (fc_action != DELETE &&
			   (isspace(*cp) || isgraph(*cp))) {
			/* just spit out the character */
			outchr(*cp, DOTRANS);
		}
		cp++;
	}

	/* close all existing open environments */
	while (fc_index > 0) {
		ptr = fc_stack[fc_index]->fc_post;
		if (ptr != NULL && *ptr != '\0')
			outstr(ptr, NOTRANS);
		fc_index--;
	}

	return (0);
}

static
fc_putchr(chr, dotrans, filep)
	FILE	*filep;
{
	register struct xlate	*xlp;
	register char		*cp;

	/* see if we have a translation for it */
	if (dotrans) {
		for (xlp = fc_xlates; xlp->xl_char != '\0'; xlp++)
			if (xlp->xl_char == chr &&
			    (xlp->xl_acts & fc_action) != 0)
				break;
		if (xlp->xl_char != chr || (xlp->xl_acts & fc_action) == 0)
			xlp = NULL;
	} else {
		/* didn't find a translation, of course */
		xlp = NULL;
	}

	/* output the translation or the character */
	if (xlp == NULL) {
		putc(chr, filep);
		if (fc_curcol >= 0) {
			if (chr == '\t')
				fc_curcol += TABWIDTH - (fc_curcol % TABWIDTH);
			else if (chr == '\n')
				fc_curcol = 0;
			else
				fc_curcol++;
		}
	} else if (xlp->xl_repl != NULL && *xlp->xl_repl != '\0') {
		for (cp = xlp->xl_repl; *cp != '\0'; cp++) {
			putc(*cp, filep);
			if (fc_curcol >= 0) {
				if (*cp == '\t')
					fc_curcol += TABWIDTH -
					    (fc_curcol % TABWIDTH);
				else if (*cp == '\n')
					fc_curcol = 0;
				else
					fc_curcol++;
			}
		}
	}
}

static
fc_putstr(str, dotrans, filep)
	char	*str;
	FILE	*filep;
{
	register char	*cp;

	if (dotrans) {
		for (cp = str; *cp != '\0'; cp++)
			fc_putchr(*cp, DOTRANS, filep);
	} else if (fc_curcol >= 0) {
		for (cp = str; *cp != '\0'; cp++) {
			putc(*cp, filep);
			if (*cp == '\t')
				fc_curcol += TABWIDTH -
				    (fc_curcol % TABWIDTH);
			else if (*cp == '\n')
				fc_curcol = 0;
			else
				fc_curcol++;
		}
	} else {
		for (cp = str; *cp != '\0'; cp++)
			putc(*cp, filep);
	}
}
