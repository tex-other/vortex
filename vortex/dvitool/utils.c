/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
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
 */

static char copyright_notice[] = "Copyright (c) 1986 - 1991 Regents of the University of California.\nAll rights reserved.";

#ifndef lint
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/utils.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "fdecls.h"
#include <errno.h>
extern int	errno;

/*
 * convert the string at src according to the radix.  return a pointer to
 * the character that terminated the conversion, or (char *) 0 on error.
 * set errno in that case.  ensure that the converted int is in the range
 * lower <= val <= upper.  This routine was taken from the public domain;
 * it was originally written by Richard A. O'Keefe.
 */

char *
str2int(src, radix, lower, upper, val)
	char	*src;
	int	*val;
{
	char	*cp = src,
		c;
	int	digit,
		sign = 1,
		collector = 0;

	for (; *cp == ' ' || *cp == '\t'; cp++)
		;

	if (*cp == '-') {
		sign = -1;
	}

	for (; (c = *cp) != '\0'; cp++) {
		if (radix == 16) {
			if (c >= '0' && c <= '9') {
				digit = c - '0';
			} else if ((c|32) >= 'a' && (c|32) <= 'f') {
				digit = (c|32) - 'a' + 10;
			} else {
				break;
			}
		} else if (radix == 8) {
			if (c >= '0' && c <= '7') {
				digit = c - '0';
			} else {
				break;
			}
		} else if (radix == 10) {
			if (c >= '0' && c <= '9') {
				digit = c - '0';
			} else {
				break;
			}
		}
		collector = radix * collector + digit;
	}
	collector *= sign;
	if (collector < lower || collector > upper) {
		errno = ERANGE;
		return((char *) 0);
	}
	*val = collector;
	return(cp);
}
			

/*
 * Are any of the characters in the two strings the same?
 */

any_of(s1, s2)
	register char	*s1,
			*s2;
{
	register char	c;
	char		*index();

	while (c = *s1++)
		if (index(s2, c) != (char *) 0)
			return(1);
	return(0);
}
      
/*
 * is s2 a substr of s1?  return the position or -1.
 */
str_index(s1, s2)
	char	*s1,
		*s2;
{
	char	*cp = s1,
		*cq,
		*cr;

	for (; *cp != '\0';) {
		if (*cp == *s2) {
			/* the first char matches, try the rest. */
			cq = cp + 1;
			cr = s2 + 1;
			for (; *cr != '\0';) {
				if (*cq != *cr) {
					break;
				}
				cq++; cr++;
			}
			if (*cr == '\0') {
				return(cp - s1);
			}
		}
		cp++;
	}
	return(-1);
}

char *
alloc(size)
	unsigned int size;
{
	char		*tmp;
	char		*calloc();

again:
	if ((tmp = calloc(size, sizeof (char))) != (char *) 0) {
		return (tmp);
	}
	/*
	 * we are running out of space, so try panic mode and 
	 * free up one of the load_images until that fails.
	 */
	if (free_oldest_load_image() == -1) {
		fputs("no more memory!\n", stderr);
		exit(1);
	}
	goto again;
}

char *
str_save(str)
	char *str;
{
	char *str2;

	if (str == (char *) 0) {
		return(str);
	}
	str2 = alloc(strlen(str) + 1);
	strcpy(str2, str);
	return(str2);
}

/*
 *  John Coker
 *  University of California, Berkeley
 *
 *  Expand filenames with tildes or environment variables.
 */

/*
 * Expand tildes or environment variables in the given filename.  If we
 * don't find any $ or ~ in the path, we just return the file name given
 * in out.  If there is an error, we return -1 and put the error message
 * into out.
 */

#include <pwd.h>

expand(name, out)
	char	*name,
		*out;
{
	extern char	*getenv();
	extern tl_data	*tl;
	struct passwd	*pwptr;
	register char	*fp,
			*pp,
			*vp;
	char		env_var[BUFSIZ];

	fp = name;
	pp = out;
	while (*fp != '\0') {
		switch (*fp) {
		case '\\':	/* backslash */
			if (++fp == '\0') {
				/* end of line */
				*pp++ = '\n';
				*pp = '\0';
				return (0);
			} else {
				/* escaped character */
				fp++;
				*pp++ = *fp++;
			}
			break;
		
		case '~':	/* home directory */
			fp++;
			vp = env_var; 
			while (*fp != '\0' && *fp != '/' && !isspace(*fp))
				*vp++ = *fp++;
			*vp = '\0';
			if (*env_var == '\0') {
				pwptr = getpwuid(getuid());
				if (pwptr == 0) {
					strcpy(out, "can't get your home");
					return (-1);
				}
			} else {
				pwptr = getpwnam(env_var);
				if (pwptr == 0) {
					strcpy(out, env_var);
					strcat(out, ": unknown user");
					return (-1);
				}
			}
			if (pwptr->pw_dir == 0) {
				strcpy(out, "no home directory");
				return (-1);
			}
			for (vp = pwptr->pw_dir; *vp != '\0'; vp++)
				*pp++ = *vp;
			break;
		
		case '$':	/* variable */
			fp++;
			vp = env_var; 
			while (isalpha(*fp) || isdigit(*fp) || *fp == '_')
				*vp++ = *fp++;
			*vp = '\0';
			if (*env_var == '\0') {
				if (*fp == '$') {
					fp++;
					sprintf(env_var, "%d", tl->pid);
					for (vp = env_var; *vp; vp++)
						*pp++ = *vp;
				} else {
					strcpy(out, "null variable name");
					return (-1);
				}
			} else {
				vp = getenv(env_var);
				if (vp == 0) {
					sprintf(out,
					    "environment variable %s not found",
					    env_var);
					return (-1);
				}
				for ( ; *vp != '\0'; vp++)
					*pp++ = *vp;
			}
			break;
		
		default:
			*pp++ = *fp++;
		}
	}
	*pp = '\0';
	return (0);
}

