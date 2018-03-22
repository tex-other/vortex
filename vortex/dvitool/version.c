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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/version.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"

char *
vers_str()
{
	static char	rev[] = "$Revision: 2.15 $",
			date[] = "$Date: 1993/09/16 02:29:01 $",
			template[] = " dvitool version %v",
			buffer[80];
	register char	*bp = buffer,
			*tp = template,
			*pp,
			c;

	while (c = *tp++) {
		if (c != '%') {
			*bp++ = c;
			continue;
		}
		switch (c = *tp++) {
		case 'v':
			pp = rev + 11;
			break;
		case 'd':
			pp = date + 7;
			break;
		}
		while (c = *pp++)
			if (c == '$')
				break;
			else
				*bp++ = c;
	}
	*bp = '\0';
	return(buffer);
}

/*
 * DOCUMENTATION
 *
 * Name: version
 * Desc: Print a string which contains the version number of 
 *	\lit{dvitool}.
 */
show_version()
{
	msg(PLAIN, "%s", vers_str());
}
