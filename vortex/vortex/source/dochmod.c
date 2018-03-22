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
 *  RCS Info: $Header: dochmod.c,v 0.1 87/05/01 11:37:20 john Locked $
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
 *  dochmod.c - file mode manipulation function
 */
static char _ID[] = "@(#)dochmod.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: chmod
 *  Call: (chmod 'mode 'file)
 *  Retu: fixnum
 *  Desc: This function sets the file protection bits (access protection)
 *	for the given \sc{UNIX} file name to the modes specified in
 *	the mode argument.  This mode should evaluate to a
 *	fixnum, which directly specifies the mode, or to a string
 *	which contains a symbolic mode specification as in the
 *	\em{chmod(1)} program.  The file must be a string which is
 *	the \sc{UNIX} pathanme of an existing file.  The mode before
 *	the change is returned.
 *
 *	These protection bits specify who can access a given file.
 *	users trying to access the file fall into three categories:
 *	User, Group and Other.  The means of access also fall into
 *	three categories: Read, Write and eXecute.  Note the capitalized
 *	letters, these are used to specify the status of the user
 *	trying to access a file and the mode of access.  The \em{user}
 *	is the owner of the file, the \em{group} contains all other
 *	users that are members of the group to which a file belongs
 *	(see \em{groups(1)}) and \em{other} are simply all other
 *	users.
 *
 *	The owner of a file may specify how each of these three categories
 *	of users may acess a file.  He can grant any or all of \em{read},
 *	\em{write} or \em{execute} permission to each of the three
 *	groups.  These modes are normally specified as a three digit
 *	octal number, with each digit specifying a bit mask for the
 *	access modes granted.  Read is assigned bit \lit{04}, write
 *	\lit{02} and execute \lit{01}.  Thus, the mode \lit{0777}
 *	specifies all modes for all users.
 *
 *	Using this interface, one may also specify the mode in a
 *	symbolic format.  An example symbolic name would be
 *	\lit{u=rw,g=r,o=}, which specifies that the owner has read
 *	and write permission, the group has read permission, and others
 *	have no permissions at all.  To specify the mode \lit{0777},
 *	one could also use the symbolic mode \lit{a=rwx}.  The symbolic
 *	form has an important advantage over the numeric specification.
 *	One can make changes relative to the current mode.  Thus, if
 *	a file is already of mode \lit{0664}, one can remove the group
 *	write permission with a mode of \lit{g-w}, without disturbing
 *	the other modes.
 *
 *	You've seen several examples above of the symbolic mode
 *	format.  It is a string composed of one or more mode specifications
 *	separated by commas.  Each mode specification contains three
 *	parts, \em{who}, \em{how} and \em{what}.  The \em{who} must
 *	be either one or more of \lit{u} (for User, owner
 *	of the file), \lit{g} (for Group), or \lit{o} (for Other) or
 *	\lit{a} (for All).  Then comes the \em{how}, which must be
 *	one of \lit{=} (for ``set to''), \lit{+} (for ``add to'') or
 *	\lit{-} (for ``remove from'').  The last part, the \em{what},
 *	may be empty, but this is only meaningful for a \em{how} of
 *	\lit{=}.  The \em{what} specifies the modes in question,
 *	one or more of \lit{r} (for Read access), \lit{w} (for Write)
 *	or \lit{x} (for eXecute).  There is also a special \em{what},
 *	\lit{s}, which stands for set user/group id on execution.
 *  SeeA: umask fopen
 */

DEFUN(dochmod, "chmod", FLAG_NONE, "sChange mode: \nFOf file: ")
{
	struct value	arg1, arg2, ret;
	register int	mode, old;
	struct stat	stbuf;
	struct string	*str;
	char		mbuf[SMALLBUF], sbuf[MAXPATHLEN];
	char		*path;

	CHECKAC(2, 2);

	arg2 = EVALARGN(2);
	if (!stringp(arg2))
		BADARGN(2, "a string file name");
	str = gstring(arg2.vl_data);
	makecstring(str, sbuf, sizeof (sbuf));
	path = fixpath(sbuf);
	if (stat(path, &stbuf) < 0)
		perror("Can't access file %s", path);
	old = stbuf.st_mode & 06777;

	arg1 = EVALARGN(1);
	switch (arg1.vl_type) {
	case LISP_FIXNUM:
		mode = gfixnum(arg1.vl_data);
		if (mode < 0 || mode > 06777)
			error("0%o is not a file protection mode.", mode);
		break;
	case LISP_STRING:
		str = gstring(arg1.vl_data);
		makecstring(str, mbuf, sizeof (mbuf));
		mode = modestring(mbuf, old);
		break;
	default:
		BADARGN(1, "a mode description");
	}
	if (mode != old && chmod(path, mode) < 0)
		perror("Can't change mode of %s", path);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, old);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: umask
 *  Call: (umask [ 'mode ])
 *  Retu: fixnum
 *  Desc: This function sets the process \em{umask(2)} according
 *	to the mode argument.  This mode should evaluate to a
 *	fixnum, which directly specifies the mode, or to a string
 *	which contains a symbolic mode specification is in the
 *	\em{chmod(1)} program.  If the mode argument is not given,
 *	just the current umask is returned.  Otherwise, the current
 *	umask is changed according to the argument and the \em{old}
 *	umask is returned.
 *
 *	The umask helps determine with what protection modes files
 *	are opened and is inherited by children automatically.
 *	When the umask is set directly as a fixnum, it represents
 *	the binary complement of the protection bits to be set
 *	when a file is opened.  For example, if the current umask
 *	is \lit{027} (symbolic: \lit{u=rwx,g=rx,o=}) and one opens
 *	a file specifying mode \lit{0666} (symbolic: \lit{ugo=rw}),
 *	the default for \sym{fopen}, the file's protection mode will
 *	actually be \lit{0640} (symbolic: \lit{u=rw,g=r,o=}).
 *
 *	You've seen several examples above of the symbolic mode
 *	format.  It is a string composed of one or more mode specifications
 *	separated by commas.  Each mode specification contains three
 *	parts, \em{who}, \em{how} and \em{what}.  The \em{who} must
 *	be either one or more of \lit{u} (for User, owner
 *	of the file), \lit{g} (for Group), or \lit{o} (for Other) or
 *	\lit{a} (for All).  Then comes the \em{how}, which must be
 *	one of \lit{=} (for ``set to''), \lit{+} (for ``add to'') or
 *	\lit{-} (for ``remove from'').  The last part, the \em{what},
 *	may be empty, but this is only meaningful for a \em{how} of
 *	\lit{=}.  The \em{what} specifies the modes in question,
 *	one or more of \lit{r} (for Read access), \lit{w} (for Write)
 *	or \lit{x} (for eXecute).
 *
 *	The umask value, when specified in symbolic format, is changed
 *	relative to the existing value.  This, if one wants to make
 *	sure that the group always has write access to files created
 *	without chaning anything else, one may specify the mode as
 *	\lit{"g+w"}.
 *  Side: This umask will be inherited by child processes.
 *  SeeA: chmod exec process
 */

DEFUN(doumask, "umask", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	register int	mask, old;
	struct string	*str;
	char		mbuf[SMALLBUF];

	CHECKAC(0, 1);

	/* get the current umask */
	umask(old = umask(0));

	/* set the umask if we have an argument */
	if (GETACOUNT() == 1) {
		arg = EVALARGN(1);
		switch (arg.vl_type) {
		case LISP_FIXNUM:
			mask = gfixnum(arg.vl_data);
			if (mask < 0 || mask > 0777)
				error("0%o is not a protection mask.", mask);
			break;
		case LISP_STRING:
			str = gstring(arg.vl_data);
			makecstring(str, mbuf, sizeof (mbuf));
			mask = ~modestring(mbuf, ~old & 0777) & 0777;
			break;
		default:
			BADARGN(1, "a protection mode");
		}
		umask(mask);
	}

	/* return old umask value */
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, old);
	return (ret);
}

/*
 *  These are the letters that make up the umask specification
 *  syntax.  Other than commas, these are the only characters
 *  that may appear in the expression.  This should be self-
 *  explanatory with an vague understanding of how the chmod(1)
 *  mode specifications work.  Of course, we actually return the
 *  complemement of the mode, because it's for umask(2).
 */
#define iswho(c)	((c) == 'u' || (c) == 'g' || (c) == 'o' || (c) == 'a')
#define isop(c)		((c) == '+' || (c) == '-' || (c) == '=')
#define ismode(c)	((c) == 'r' || (c) == 'w' || (c) == 'x' || (c) == 's')

static int
modestring(string, start)
	char	*string;
{
	char		who[100], mode[100], op;
	register char	*sp, *wp, *mp;
	register int	shift, bit, add;
	register int	mbits = start & 06777;

	/*
	 *  This gets a bit ugly, we expect to see expressions
	 *  of the form [augo]+[+-=][rwxs]+ separated by commas.
	 *  the first part is `who' is affected, the second is
	 *  the `operation' and the third is the modes to be set.
	 */
	for (sp = string; isspace(*sp) || *sp == ','; sp++)
		;
	while (*sp != '\0') {
		/*
		 *  Collect the three parts of the expression,
		 *  this expects there are no spaces imbedded
		 *  in the expressions.  All three parts must
		 *  always be specified except in the case of
		 *  the `equal' operation, for which the modes
		 *  may be left out, meaning `none', I guess.
		 */
		for (wp = who; iswho(*sp); *wp++ = *sp++)
			;
		*wp = '\0';
		op = *sp++;
		for (mp = mode; ismode(*sp); *mp++ = *sp++)
			;
		*mp = '\0';
		if (!isop(op) || (*mode == '\0' && op != '='))
			goto badform;

		/*
		 *  Now perform the requisite operation on the elements
		 *  in who and the elements in mode.  For each who
		 *  and for each mode, perform the operation.  Here we
		 *  treat the equals op like plus, except we zero perms
		 *  first, giving the proper action.  We have to kludge
		 *  for the 'a' who, which means all.  We set shift to
		 *  -1 and check for this when making up the actual
		 *  bit pattern to work with.
		 */
		for (wp = who; *wp != '\0'; wp++) {
			switch (*wp) {
			case 'u':
				shift = 6;
				if (op == '=')
					mbits &= 0077;
				break;
			case 'g':
				shift = 3;
				if (op == '=')
					mbits &= 0707;
				break;
			case 'o':
				shift = 0;
				if (op == '=')
					mbits &= 0770;
				break;
			case 'a':
				shift = -1;
				if (op == '=')
					mbits = 0000;
				break;
			default:
				goto badform;
			}
			for (mp = mode; *mp != '\0'; mp++) {
				switch (*mp) {
				case 'r':
					bit = 04;
					break;
				case 'w':
					bit = 02;
					break;
				case 'x':
					bit = 01;
					break;
				case 's':
					if (*wp == 'u')
						bit = 04;
					else if (*wp == 'g')
						bit = 02;
					else
						goto badform;
					shift = 9;
					break;
				default:
					goto badform;
				}
				if (shift < 0)
					add = (bit << 6) | (bit << 3) | bit;
				else
					add = bit << shift;
				if (op == '-')
					mbits &= ~add;
				else
					mbits |= add;
			}
		}

		/*
		 *  Skip over to next non-separator character,
		 *  this actually means white space may surround,
		 *  or even replace, the comma.  We hope this
		 *  will not be used, as the argument to chmod(1)
		 *  does not allow it, although this generalization
		 *  is easy enough to do here.
		 */
		while (isspace(*sp) || *sp == ',')
			sp++;
	}
	return (mbits);

badform:
	error("Bad symbolic mode, form is ``[augo]+[+-=][rwx]+''.");
	/* NOTREACHED */
}
