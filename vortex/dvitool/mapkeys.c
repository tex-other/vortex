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

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/mapkeys.c,v $  (Berkeley)
 * $Version:$
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#if 0
/*
 * this program filters the keymaps file and replaces references to the
 * name of the function by the address of its struct function.  For
 * example, the line:
 *
 *	"beginning-of-file",		/* esc <  */
 *
 * will be replaced by this line:
 *	
 *	(func *) &commands[4],		/* < */
 */
#endif 0

#include "structs.h"
#include "constants.h"
#define TXT_TO_C
#include "funcdefs.h"

/*
 * extract the function name (the ascii name) from the line.
 */

char *
extract(line)
	char	*line;
{
	static char	nbuf[80];
	register char	*cp = line,
			*begin,
			*copy = nbuf,
			c;

	while ((c = *cp++) != '"') {
		if (c == '\0') {
			fprintf(stderr, "no func-name???: %s\n",
			  line);
			return((char *) 0);
		}
	}
	begin = cp;
	while ((c = *cp++) != '"') {
		if (c == '\0') {
			fprintf(stderr, "no closing \"???: %s\n",
			  line);
			return((char *) 0);
		}
	}
	cp--;
	while (begin != cp) {
		*copy++ = *begin++;
	}
	*copy = '\0';
	return((char *) nbuf);
}

#include <strings.h>
#include "keys.h"

char	ctrl[] = "\\^ ",
	buf[7],
	unknown_key[] = "unknown key";

char *
p_char(ch)
	int	ch;
{
	if (ch == '\177') {
		return("^?");
	}
	if (ch == '\033') {
		return("\\e");
	}
	if (ch < ' ') {
		ctrl[2] = ch + '\100';
		return(ctrl);
	}
	if (ch < FIRST_MOUSE) {
		buf[0] = ch;
		buf[1] = '\0';
		return(buf);
	}
	if (ch >= FIRST_MOUSE && ch <= LAST_VSCROLL) {
		if (ch < FIRST_HSCROLL) {
			strcpy(buf, "\\m");
			ch -= FIRST_MOUSE;
		} else if (ch < FIRST_VSCROLL) {
			strcpy(buf, "\\h");
			ch -= FIRST_HSCROLL;
		} else if (ch <= LAST_VSCROLL) {
			strcpy(buf, "\\v");
			ch -= FIRST_VSCROLL;
		} else {
			return(unknown_key);
		}
		switch(ch) {
		case LEFT_MOUSE:
			strcat(buf, "l");
			break;
		case LEFT_MOUSE + SHIFT_MOUSE:
			strcat(buf, "L");
			break;
		case LEFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^l");
			break;
		case LEFT_MOUSE + SHIFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^L");
			break;
		case MIDDLE_MOUSE:
			strcat(buf, "m");
			break;
		case MIDDLE_MOUSE + SHIFT_MOUSE:
			strcat(buf, "M");
			break;
		case MIDDLE_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^m");
			break;
		case MIDDLE_MOUSE + SHIFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^M");
			break;
		case RIGHT_MOUSE:
			strcat(buf, "r");
			break;
		case RIGHT_MOUSE + SHIFT_MOUSE:
			strcat(buf, "R");
			break;
		case RIGHT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^r");
			break;
		case RIGHT_MOUSE + SHIFT_MOUSE + CONTROL_MOUSE:
			strcat(buf, "\\^R");
			break;
		default:
			return(unknown_key);
			break;
		}
	}
	return(buf);
}


lookup_func(tbl, func_name)
	func	tbl[];
	char	*func_name;
{
	int	count,
		name_len;
	char	*name;

	name_len = strlen(func_name);
	for (count = 0; (name = tbl[count].name) != (char *) 0; count++) {
		if (*name == *func_name) {
			if (!strncmp(name, func_name, name_len))
				return(count);
		}
	}
	return(-1);
}
		
		
main()
{
	char	lbuf[256],
		*namep;
	int	findex,
		ch_map,
		status = 0;

	while (fgets(lbuf, sizeof(lbuf), stdin) != (char *) 0) {
		if (strncmp(lbuf, "\t\"", 2) != 0) {
			fputs(lbuf, stdout);
			ch_map = 0;
			continue;
		}
		if ((namep = extract(lbuf)) ==  (char *) 0) {
			status++;
			continue;
		}
		if (strncmp(namep, "unbound", sizeof("unbound"))) {
			if ((findex = lookup_func(commands, namep)) < 0) {
				fprintf(stderr, "unknown func-name: %s",
				  lbuf);
				status++;
				continue;
			}
			fprintf(stdout,
			  "	(func *) &commands[%d],		/* %s */\n",
			  findex, p_char(ch_map++));
		} else {
			fprintf(stdout,
			  "	(func *) 0,			/* %s */\n",
			  p_char(ch_map++));
		}
	}
	exit(status);
}
