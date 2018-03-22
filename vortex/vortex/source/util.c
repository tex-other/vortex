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
 *  RCS Info: $Header: util.c,v 0.1 87/05/01 12:31:44 john Locked $
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
 *  util.c - general utility routines
 */
static char _ID[] = "@(#)util.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <varargs.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "symtab.h"
#include "format.h"

char *
basename(path)
	char	*path;
{
	register char	*cp;

	for (cp = path; *cp != '\0'; cp++)
		;
	for (--cp; cp > path && *cp != '/'; cp--)
		;
	if (*cp == '/')
		return (cp + 1);
	else
		return (cp);
}

/*
 *  DOCUMENTATION
 *
 *  Name: HOME
 *  Desc: This variable should be set to the full pathname of
 *	the current user's home directory.  At startup it will
 *	be imported from the envoronment, and it may be changed
 *	later by the user to change the way the tilde character
 *	expands in path names.
 *  Side: If this variable is not set to a string pathname, it
 *	will be reset without notice to the user's home directory
 *	as gotten from the \em{passwd(8)} file when some part of
 *	VorTeX needs to get the user's home directory.
 *  SeeA: cd fopen
 */
MKSTRING(HOME_NAME, "HOME");

char *
gethomedir()
{
	struct value	home;
	char		*hdir = NULL;
	struct passwd	*pwptr;
	static char	dbuf[MAXPATHLEN];
	struct string	*str;
	int		hlen;

	home = get_variable(HOME_NAME, NULL);
	if (eq(home, NOVALUE)) {
		switch (home.vl_type) {
		case LISP_STRING:
			str = gstring(home.vl_data);
			break;
		case LISP_SYMBOL:
			str = gsymbol(home.vl_data)->sy_pname;
			break;
		default:
			goto bad;
		}
		makecstring(str, dbuf, sizeof (dbuf));
		hdir = dbuf;
	}
bad:	if (hdir == NULL && (pwptr = getpwuid(getuid())) != NULL) {
		hdir = pwptr->pw_dir;
		hlen = strlen(pwptr->pw_dir);
		if (hdir == NULL || hlen <= 0) {
			/* can't get home directory from passwd file */
			hdir = NULL;
		} else {
			/* set proper name as variable */
			home.vl_type = LISP_STRING;
			sstring(home.vl_data, save_string(hdir, hlen));
			setglobal(HOME_NAME, home, STAB_PERM);
			/* copy directory name into static storage */
			strncpy(dbuf, hdir, sizeof (dbuf));
			dbuf[sizeof (dbuf) - 1] = '\0';
			hdir = dbuf;
		}
	}

	/* we've copied it into static space already */
	return (hdir);
}

char *
fromhome(path)
	char	*path;
{
	static char	new[MAXPATHLEN];
	int		len;
	char		*hdir;

	if ((hdir = gethomedir()) == NULL || *hdir == '\0')
		return (path);

	len = strlen(hdir);
	if (len > 1 && !strncmp(hdir, path, len))
		sprintf(new, "~%s", path + len);
	else
		strcpy(new, path);
	return (new);
}

char *
fixpath(path)
	char	*path;
{
	char		old[MAXPATHLEN*2];
	static char	new[MAXPATHLEN*2];
	register char	*op, *np;
	struct passwd	*pwptr;
	char		*hdir;

	if (*path == '~') {
		strcpy(old, path + 1);
		for (op = old; *op != '/' && *op != '\0'; op++)
			;
		*op = '\0';
		if (*old == '\0') {
			/* the current user's home directory */
			hdir = gethomedir();
		} else {
			if ((pwptr = getpwnam(old)) == NULL)
				hdir = "";
			else
				hdir = pwptr->pw_dir;
		}
		if (hdir == NULL)
			hdir = "";
		sprintf(old, "%s/%s", hdir, path + (op - old) + 2);
	} else if (*path != '/') {
		if (getwd(old) == NULL)
			strcpy(old, ".");
		strcat(old, "/");
		strcat(old, path);
	} else {
		/* already a full pathname */
		strcpy(old, path);
	}

	op = old;
	np = new;
	while (*op != '\0') {
		switch (*op) {
		case '/':	/* only want one here */
			while (op[1] == '/')
				op++;
			if (!strncmp(op, "/./", 3)) {
				/* skip redundant "./" */
				op += 2;
			} else if (!strncmp(op, "/../", 4)) {
				for (--np; np > new && *np != '/'; np--)
					;
				op += 3;
			}
			break;
		}
		*np++ = *op++;
	}
	/* delete trailing slashes and terminate path */
	while (np - 1 > new && np[-1] == '/')
		np--;
	*np = '\0';

	return (new);
}

char *
pchar(code)
	unsigned int	code;
{
	static char	buf[10];
	int		ch;

	if (code < ' ') {
		ch = code + '@';
		if (isupper(ch))
			ch = tolower(ch);
		sprintf(buf, "\\^%c", ch);
	} else if (code == '\177') {
		/* ascii DEL, 127 */
		strcpy(buf, "\\^?");
	} else if (code > '\177') {
		sprintf(buf, "\\%03o", code & 0377);
	} else {
		buf[0] = code;
		buf[1] = '\0';
	}
	return (buf);
}

uncode(string)
	char	*string;
{
	int	len, code;

	len = strlen(string);
	if (len > 1 && *string == '\\') {
		switch (string[1]) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* a numeric escape sequence */
			if (sscanf(string, "\\%o", &code) != 1 ||
			    code < 0 || code > 255) {
				error(
		"Illegal character escape, must in octal and less than 0200.");
				return (-1);
			}
			return (code);
		case '^':
			/* introduce a control code */
			if (len < 3) {
				error(
			    "Incomplete control character specification.");
				return (-1);
			}
			if (string[2] == '?')
				return ('\177');
			else
				return (string[2] & 037);
		case 'e':
			return ('\033');
		case 'n':
			return ('\n');
		case 'r':
			return ('\r');
		case 't':
			return ('\t');
		case 'b':
			return ('\b');
		case 'f':
			return ('\f');
		default:
			return (string[1]);
		}
	} else {
		/* just a regular character */
		return (*string);
	}
	/* NOTREACHED */
}

makecstring(str, buf, size)
	struct string	*str;
	unsigned char	buf[];
{
	register unsigned char	*fp, *tp, *fend, *tend;

	ASSERT(buf != NULL);
	if (str == NULL) {
		*buf = '\0';
		return (0);
	}

	tp = buf;
	tend = buf + size - 1;
	fp = str->st_buffer;
	fend = str->st_buffer + str->st_length;
	while (fp < fend && tp < tend) {
		if (*fp != '\0')
			*tp++ = *fp;
		fp++;
	}
	*tp = '\0';

	return (tp - buf);
}

makepstring(str, buf, size)
	struct string	*str;
	unsigned char	buf[];
{
	register unsigned char	*fp, *tp, *fend, *tend;

	ASSERT(buf != NULL);
	if (str == NULL) {
		*buf = '\0';
		return (0);
	}

	tp = buf;
	tend = buf + size - 2;
	fp = str->st_buffer;
	fend = str->st_buffer + str->st_length;
	while (fp < fend && tp < tend) {
		if (*fp > '~') {
			*tp++ = '^';
			*tp++ = '?';
			fp++;
		} else if (*fp < ' ' && *fp != '\t') {
			*tp++ = '^';
			*tp++ = *fp++ + '@';
		} else {
			/* just a normal printable character */
			*tp++ = *fp++;
		}
	}
	*tp = '\0';

	return (tp - buf);
}

char *
pfixnum(i)
{
	static char	buf[SMALLBUF];

	sprintf(buf, "%d", i);
	return (buf);
}

char *
pflonum(d)
	double	d;
{
	static char	buf[SMALLBUF];
	register char	*cp;

	sprintf(buf, "%lf", d);
	for (cp = buf; *cp != '\0'; cp++)
		;
	for (--cp; cp > buf && *cp == '0'; cp--)
		;
	if (*cp == '.')
		cp++;
	*++cp = '\0';
	return (buf);
}

static char	*nth_strings[] = {
	NULL, "first", "second", "third", "fourth", "fifth",
	"sixth", "seventh", "eighth", "ninth", "tenth",
	"11th", "12th", "13th"
};
static int	nth_count = NITEMS(nth_strings);

char *
nthname(n)
{
	static char	nthbuf[SMALLBUF];

	if (n > 0 && n < nth_count) {
		/* we have the special name */
		return (nth_strings[n]);
	} else {
		/* make up the name */
		sprintf(nthbuf, "%d", n);
		if (n % 10 == 1)
			strcat(nthbuf, "st");
		else if (n % 10 == 2)
			strcat(nthbuf, "nd");
		else if (n % 10 == 3)
			strcat(nthbuf, "rd");
		else
			strcat(nthbuf, "th");
		return (nthbuf);
	}
}

static char	*num_strings[] = {
	"zero", "one", "two", "three", "four", "five",
	"six", "seven", "eight", "nine", "ten"
};
static int	num_count = NITEMS(num_strings);

char *
numname(number)
{
	static char	numbuf[SMALLBUF];

	if (number >= 0 && number < num_count) {
		/* we have the special name */
		return (num_strings[number]);
	} else {
		sprintf(numbuf, "%d", number);
		return (numbuf);
	}
}

char *
findpath(name, path, ext)
	char	*name, *path, *ext;
{
	extern char	*fixpath();
	static char	full[MAXPATHLEN];
	register char	*bp, *ep, *file;

	if (name == NULL || path == NULL || name == NULL)
		return (NULL);

	bp = path;
	while (*bp != '\0') {
		for (ep = bp; *ep != '\0' && *ep != ':'; ep++)
			;
		strncpy(full, bp, ep - bp);
		strcpy(full + (ep - bp), "/");
		strcat(full, name);
		file = fixpath(full);
		if (access(file, F_OK) == 0)
			return (file);
		if (*ep == '\0')
			break;
		bp = ep + 1;
	}

	return (NULL);
}

struct value
pathtolist(path)
	char	*path;
{
	register char	*bp, *ep;
	struct string	*str;
	struct value	lval, sval;

	bp = path;
	lval = v_nil;
	while (*bp != '\0') {
		/* find end of the next path */
		for (ep = bp; *ep != '\0' && *ep != ':'; ep++)
			;
		/* add this to the list */
		str = save_string(bp, ep - bp);
		sval.vl_type = LISP_STRING;
		sstring(sval.vl_data, str);
		lval = append(lval, sval);
		if (*ep == '\0')
			break;
		bp = ep + 1;
	}
	return (lval);
}

#define MILLION		1000000

ualarm(usecs)
	unsigned int	usecs;
{
	struct itimerval	new, old;

	/* set the alarm for given time */
	new.it_interval.tv_sec = new.it_value.tv_sec = usecs / MILLION;
	new.it_interval.tv_usec = new.it_value.tv_usec = usecs % MILLION;
	if (setitimer(ITIMER_REAL, &new, &old) < 0)
		return (-1);
	else
		return (0);
}

static FILE	*debugfp = stdout;

/* VARARGS */
debug(va_alist)
	va_dcl
{
#include "fmtdecl.h"
	int	what;

	STARTVA();
	what = GETVARG(int);
	if (what != 0 && (what & debug_states) == 0) {
		ENDVA();
		return;
	}

#include "fmtcode.h"

	fprintf(debugfp, "DEBUG: %s\n", msgbuf);
	fflush(debugfp);
}

char	*badchar = "<bad>";
char	*pstrings[256] = {
	"^@", "^A", "^B", "^C", "^D", "^E", "^F", "^G", "^H", "^I", "^J", "^K",
	"^L", "^M", "^N", "^O", "^P", "^Q", "^R", "^S", "^T", "^U", "^V", "^W",
	"^X", "^Y", "^Z", "^[", "^\\", "^]", "^~", "^_", " ", "!", "\"", "#",
	"$",  "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1",
	"2",  "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
	"@",  "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N",  "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j",  "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~", "^?", "\200", "\201", "\202",
	"\203", "\204", "\205", "\206", "\207", "\210", "\211", "\212", "\213",
	"\214", "\215", "\216", "\217", "\220", "\221", "\222", "\223", "\224",
	"\225", "\226", "\227", "\230", "\231", "\232", "\233", "\234", "\235",
	"\236", "\237", "\240", "\241", "\242", "\243", "\244", "\245", "\246",
	"\247", "\250", "\251", "\252", "\253", "\254", "\255", "\256", "\257",
	"\260", "\261", "\262", "\263", "\264", "\265", "\266", "\267", "\270",
	"\271", "\272", "\273", "\274", "\275", "\276", "\277", "\300", "\301",
	"\302", "\303", "\304", "\305", "\306", "\307", "\310", "\311", "\312",
	"\313", "\314", "\315", "\316", "\317", "\320", "\321", "\322", "\323",
	"\324", "\325", "\326", "\327", "\330", "\331", "\332", "\333", "\334",
	"\335", "\336", "\337", "\340", "\341", "\342", "\343", "\344", "\345",
	"\346", "\347", "\350", "\351", "\352", "\353", "\354", "\355", "\356",
	"\357", "\360", "\361", "\362", "\363", "\364", "\365", "\366", "\367",
	"\370", "\371", "\372", "\373", "\374", "\375", "\376", "\377"
};
