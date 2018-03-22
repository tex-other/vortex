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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/msg.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

/*
 * Provide some basic message facilities for canvas windows.
 * We assume that sys_font has been opened somewhere with pw_pfsysopen().
 */

#include "structs.h"
#include "constants.h"
#include <varargs.h>
#include <sys/errno.h>

/* from the C library */
extern int	errno,
		sys_nerr;
extern char	*sys_errlist[];

static char	more_msg[] = " --More--",
		colon[] = ": ";
static long	msg_time = 0;

/*VARARGS*/
void
msg(va_alist)
	va_dcl
{
	va_list		list;
	unsigned long	flags;
	char		*format;
	static u_char	mbuf[512],
			fmtbuf[512];
	char		*get_rc_line();
	extern tl_data	*tl;
	extern char	log_fname[];
	extern FILE	*log_fp;

	va_start(list);
	/* pop off the flags arg so we can do a quick check to see if
	 * we really have to go to all the work of printing this message.
	 */
	flags = va_arg(list, int);
	format = va_arg(list, char *);
	va_end(list);

	if (mbuf[0] != '\0' && !(flags & APPEND) && !(flags & FATAL) &&
	  !(flags & TITLE) && !tl->in_rc) {
	  	clear_msg_text();
	}
	if (format == 0 || *format == '\0')
		return;
	
	if (flags & LITERAL) {
		strcpy(fmtbuf, format);
	} else {
		(void) vsprintf(&fmtbuf[0], format, list);
	}

	if (flags & FATAL) {
		strcpy(mbuf, tl->prog_name);
		strcat(mbuf, colon);
		if (dvi->fname != (char *) 0) {
			strcat(mbuf, dvi->fname);
			strcat(mbuf, colon);
		}
	} else {
		mbuf[0] = '\0';
	}
	if (tl->in_rc) {
		strcpy(mbuf, get_rc_line());
		strcat(mbuf, "\t");
	}

	strcat(mbuf, fmtbuf);

	if (flags & PERROR) {
		if (errno > 0 && errno <= sys_nerr) {
			strcat(mbuf, colon);
			strcat(mbuf, sys_errlist[errno]);
		}
	}

	if (log_fname[0] != '\0' && !(flags & TITLE) && !(flags & APPEND)) {
		/*
		 * if the filename is non-null, we assume it has been
		 * opened and is ready for writing.
		 */
		fputs(mbuf, log_fp);
		fputc('\n', log_fp);
		fflush(log_fp);
	}

	if (!(flags & TITLE) && (flags & FATAL || !tl->created || tl->in_rc)) {
		strcat(mbuf, "\n");
		fputs(mbuf, stderr);
		if (flags & FATAL) {
			(void) abort_run(1);
		}
		return;
	}
	if (flags & WAIT) {
		strcat(mbuf, more_msg);
	}
	if (flags & TITLE) {
		show_title(mbuf, flags & APPEND);
	} else {
		show_msg(mbuf, flags & APPEND, flags & OVERWRITE);
	}

	msg_time = time(0);

	if (flags & WAIT) {
		push_cursor(MOUSE_CUR);
		(void) wait_for('\0');
		msg_del_c(sizeof(more_msg) - 1);
		pop_cursor();
	}
	return;
}

msg_c(c)
	char	c;
{
	char	buf[2];

	buf[0] = c; buf[1] = '\0';
	show_msg(buf, 1, 0);
}

int	msg_win_cursor = 0;

set_msg_cur(cursor)
{
	/*
	 * store the value of the cursor and print a message to display
	 * it.  this code depends on NO_MCUR == 0.
	 */
	if (cursor != NO_MCUR) {
		/* turn on the cursor. */
		msg_win_cursor = cursor;
		show_msg("", 1, 0);
	} else {
		/* turn it off. */
		msg_cursor_off();
		msg_win_cursor = NO_MCUR;
	}
}

#define	ENOUGH_TIME (8)

int	msg_visible = 0;

/* clear the message if enough time has elapsed */
clear_msg()
{
	if (msg_time + ENOUGH_TIME <= time(0)) {
		clear_msg_text();
		msg_visible = 0;
	}
}

