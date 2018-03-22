/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifdef VORTEX

/*
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Development Environment
 *
 *  This file is part of the VorTeX incremental formatter,
 *
 *  Copyright (C) 1987 by the Regents of University of California and by
 *  Pehong Chen
 *  Computer Science Division
 *  571 Evans Hall
 *  University of California
 *  Berkeley, CA 94720
 *  USA
 *  phc@berkeley.edu
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */
#ifndef lint
static char	_rcsid_[] = "$Source:$ for VorTeX Incremental Formatter, Copyright (C) Pehong Chen 1987";
static char	_rcsver_[] = "$Revision: $";
static char	_rcsdate_[] = "$Date: $";
#endif !lint

#include		<setjmp.h>
#include		"tex.h"
#include		"str.h"
#include		"dvi.h"
#include		"scan.h"
#include		"allir.h"
#include		"msg.h"
#include		"main.h"

extern _Char		*irs_bol, *irs_eol, *irs_ptr, *irs_next;
extern _Pbox		*pbox_head;
extern int		write_out;
extern char		docname[];
extern int		starting_page;
extern int		viewing_page;
extern int		format_continue;
jmp_buf			err_env;

/* Code to do timing tests */
#ifdef TIME
#include <sys/time.h>

struct timeval time0, time1, time2, time3;
struct timezone tz;
float timePer;
#endif

set_starting_page (cp)
	_Char		*cp;
{
	_Node		*np;
	int		no;

	for (;;) {
		if (cp == NIL) {
			msg(STDERR, "error in set_starting_page, cp shouldn't be nil.");
			exit(-1);
		}
		if (cp->_re != NIL) {
			np = cp->_re->_pb;
			if (((_Cbox *) cp->_re)->_id < ((_Pbox *) np)->_id) {
				/* in the context of previous page */
				no = ((_Pbox *) np)->_no - 1;
				((_Pbox *) (np->nd_lt))->_ok = FALSE;
			} else {
				/* in the context of current page */
				no = ((_Pbox *) np)->_no;
				((_Pbox *) np)->_ok = FALSE;
			}
			starting_page = MIN2(starting_page, no);
			return(1);
		}
		
		if (cp->_ch != EOF)
			cp = (_Char *) cp->_lt;
		else {
			/* included file? */
			np = cp->_up;
			while ((np != NIL) && (np->nd_type != NODE_INPUT))
				np = np->nd_up;
			if (np == NIL) {
				starting_page = 1;
				pbox_head->_ok = FALSE;
				return(0);
			} else
				cp = (_Char *) ((_Input *) np)->_bon;
		}
	}
}

/*
 * VorTeX incremental formatting algorithm:
 *
 *	global flags:
 *		total_pages -- physical page number the formatter
 *			       has just completed.  Default == 0.
 *		starting_page -- leftmost dirty page (physical page number),
 *				 reformatting should start here.
 *				 Set by every update.  Default == INFINITY.
 *		viewing_page -- the page being viewed (physical page number)
 *				at the target editor.  Default == 1.
 *		format_continue -- false if last page of doc encountered.
 *				   Default = TRUE;
 *		pbox -- pointer to the current page box.  Default == NIL.
 *
 */

fg_format ()
{
	char		*argv[2];
	long		tmp;

#ifdef TIME
	gettimeofday(&time1, &tz);
#endif
	if (starting_page == INFINITY) {
		/* cold start (initial run) */
		argv[0] = "VorTeX";
		argv[1] = docname;
		pre_format(2, argv);
		format_continue = TRUE;
	} else if ((starting_page <= total_pages) &&
		   (starting_page != total_pages + 1)) {
		/* warm start (2nd or higher run) */
		load_state(starting_page - 1);
		format_continue = TRUE;
	} else if (starting_page > viewing_page) {
		/* no need to reformat */
		return(0);
	} else if (!format_continue) {
		/* last page has already been formatted. */
		return(0);
	}
	/* time1 -- beginning of a new page */
	/* time3 -- end of a new page */
	/* time3 - time2 = checkpointing time */
	/* time3 - time1 = processing time including checkpointing */
	/* time1 = time3 after a page */
#ifdef TIME
	if (!tex_only) {
	  gettimeofday(&time3, &tz);
	  timePer =(float)(time3.tv_sec - time2.tv_sec) +
	    (time3.tv_usec - time2.tv_usec) / 1000000.0;
	  fprintf(stderr, "%.3f seconds to checkpoint page %d.\n",
		  timePer, total_pages);
	  fflush(stderr);
	  gettimeofday(&time1, &tz);
	}
#endif

	/* main_control returns false if last page in document encountered */
	while (format_continue && (total_pages < viewing_page)) {
		if (setjmp(err_env)) {
			msg(STDERR, "Error has occurred in foreground formatting.");
			print_nl("Error has occurred in foreground formatting.");
			starting_page = total_pages + 1;
			return(0);
		}
		main_control();

		if (write_out) {
			tmp = viewing_page;
			if (format_continue)
				tp_sendpage(total_pages);
			viewing_page = tmp;
		      }
	}
		/* time3 -- end of a new page */
		/* time3 - time2 = checkpointing time */
		/* time3 - time1 = processing time including checkpointing */
		/* time1 = time3 after a page */
#ifdef TIME
	gettimeofday(&time3, &tz);

	timePer = (float)(time3.tv_sec - time2.tv_sec) +
	  (time3.tv_usec - time2.tv_usec) / 1000000.0;
	fprintf(stderr, "%.3f seconds to checkpoint page %d.\n",
		timePer, total_pages);
	timePer = (float)(time3.tv_sec - time1.tv_sec) +
	  (time3.tv_usec - time1.tv_usec) / 1000000.0;
	fprintf(stderr,
		"%.3f seconds to process page %d with checkpointing.\n",
		timePer, total_pages);
        fflush(stderr);
#endif

#ifdef _INC
	msg(STDERR, "INC : total_pages=%d, viewing_page=%d, starting_page=%d", total_pages, viewing_page, starting_page);
#endif

	if (total_pages == viewing_page)
		return(1);

	return(0);
}

bg_format ()
{
#ifdef TIME
	gettimeofday(&time1, &tz);
#endif
	if (!format_continue)
		/* nothing to format */
		return(0);

	if (starting_page != total_pages + 1)
		load_state(starting_page - 1);
	format_continue = TRUE;

	page_shipped = page_done = FALSE;
	if (setjmp(err_env)) {
		msg(STDERR, "Error has occurred in background formatting.");
		print_nl("Error has occurred in background formatting.");
		starting_page = total_pages + 1;
		return(0);
	}

	main_control();

	/* time3 -- end of a new page */
	/* time3 - time2 = checkpointing time */
	/* time3 - time1 = processing time including checkpointing */
	/* time1 = time3 after a page */
#ifdef TIME
        if ( format_continue ) {
	  gettimeofday(&time3, &tz);

	  timePer = (float)(time3.tv_sec - time2.tv_sec) +
	    (time3.tv_usec - time2.tv_usec) / 1000000.0;
	  fprintf(stderr, "%.3f seconds to checkpoint page %d.\n",
		  timePer, total_pages);
	  timePer = (float)(time3.tv_sec - time1.tv_sec) +
	    (time3.tv_usec - time1.tv_usec) / 1000000.0;
	  fprintf(stderr,
		  "%.3f seconds to process page %d with checkpointing.\n",
		  timePer, total_pages);
	  fflush(stderr);
	}
#endif
	return(1);
}

#endif
