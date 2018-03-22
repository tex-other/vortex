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
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents.
 *
 *  This file is part of the VorTeX proof editor 
 *  written by Jeffrey W. McCarrell for the VorTeX project
 *  under the direction of Prof. Michael A. Harrison
 *  of the University of California at Berkeley.
 *
 *  Copyright (c) 1987 by Jeffrey W. McCarrell
 *  and The Regents of the University of California.
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  jwm@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 * 
 *  Modified by Pehong Chen for the VorTeX Formatter.
 */

#include	<stdio.h>
#include	"main.h"
#include	"failcodes.h"
#include	"msg.h"
#include	"mdep.h"
#include	"comm.h"
#include	"bits.h"

extern int	sig_state,
		ts_sock,
		tp_sock;
extern int	format_started;

int		fstarts = 0;	/*
				 * number of formatting commands that must
				 * return a response before we do anything
				 * else, i.e. no more source requests will
				 * be read till we get something back from
				 * the formatter.
				 */

#define BG_DELAY		1000

/* 
 - top_level - the main loop routine. 
 */
/*
 * Simply dispatch to the correct reading routine based on the return
 * values from select.  If we are supposed to die as a result of some
 * GLC_QUIT, we'll handle that request like any other, except we won't
 * return, we'll exit(2).
 */
top_level (func, delay)
	int			func;
	int			delay;
{
	int			rmask[FDM_SIZE];
	int			nfds;
	int			func_p;
	struct timeval		tmout;

	/* wait for a while for the connection to arrive */
	tmout.tv_sec = delay;
	tmout.tv_usec = (delay * 1000 % 1000) * 1000;

	for(;;) {
#ifdef _CONN
		msg(SDBUG, "Doing top-level looping (func = %x)...", func);
#endif

                /* if we have no connection to either socket, exit. */
                if (ts_sock == -1 && tp_sock == -1) {
                        giveup(ES_NOSOCKS);
                }

		/* initalize the masks each time through. */
		bzero(rmask, sizeof(rmask));

		if (ts_sock != -1 && fstarts == 0) {
			FDM_SET(rmask, ts_sock);
		}
		if (tp_sock != -1) {
			FDM_SET(rmask, tp_sock);
		}

		nfds = select(NOFILE, rmask, (int *) 0, (int *) 0, &tmout);

		if (nfds == -1 && errno != EINTR) {
			msg(PERROR, "ml select");
			continue;
		} else if (nfds == -1) {
			if (sig_state & GOT_INTR) {
				expire(ES_INTR);
			}
			/* only other flag is from sig_quit, so dump core. */
			/* this assumes we're writing messages to stderr. */
			fflush(stderr);
			abort();
			/* NOTREACHED */
		} else if (nfds == 0) {
			if (format_started) {
#ifdef _CONN
				msg(SDBUG, "Doing background formatting...");
#endif
				bg_format();
			}
		} else {
			if (FDM_ISSET(rmask, ts_sock)) {
				if (p_get(ts_sock, &func_p) <= -3) {
					expire(ES_NORMAL);
				}
			}
	
	
			if (FDM_ISSET(rmask, tp_sock)) {
				if (p_get(tp_sock, &func_p) <= -3) {
					expire(ES_NORMAL);
				}
			}
#ifdef _CONN
			msg(MDBUG, "Returned func is %x", func_p);
#endif
			if (func_p == func)
				return(1);
		}
	}
}

extern gl_func	ts_cmds[],
		tp_cmds[];

/*
 * the descriptor sock has data to be read, so
 * read it and dispatch to the pertinent function.
 */
p_get(sock, func_pp)
	int		*func_pp;
{
	gl_hdr		pkt;
	int		len,
			last_req,
			sockc,
			togo,
			stat;
	char		*data,
			*p;
	gl_func		*fp;
	char		*pname,
			*(*reqfunc)();

	/* set up the source-specific variables. */
	if (sock == ts_sock) {
		pname = "source";
		reqfunc = ts_reqname;
		last_req = TSC_LASTREQ;
                sockc = IFC_CLOSE_TS;
	} else {
		pname = "proof";
		reqfunc = tp_reqname;
		last_req = TPC_LASTREQ;
                sockc = IFC_CLOSE_TP;
	}

	/* first read the header pkt. */
	if ((len = read(sock, &pkt, sizeof(gl_hdr))) < 0) {
		msg(PERROR, "read err %s sock %d", pname, sock);
		return(-1);
	} else if (len < sizeof(gl_hdr)) {
		/* eof on socket. */
                close_socks(sockc);
		return(-1);
	}

	/* into network byte order. */
	pkt.req = ntohs(pkt.req);
	pkt.len = ntohs(pkt.len);
	pkt.id  = ntohl(pkt.id);

	msg(SDBUG, "[got %s packet req %s, doc %d, dlen %d]",
	    pname, (*reqfunc) ((int) pkt.req), (int) pkt.id, (int) pkt.len);

	if (pkt.req < 0 || pkt.req > last_req) {
		msg(STDERR, "%s request %d out of range!",
		  pname, pkt.req);
		return(-1);
	}

	if (pkt.len > 0) {
		ALLOCA(data, char, pkt.len);

		togo = pkt.len;
		p = data;
		while (togo > 0) {
			len = read(sock, p, togo);
			if (len < 0) {
				msg(PERROR, 
				  "read err %s sock %d togo %d",
				  pname, sock, togo);
				return(-1);
			} else if (len == 0) {
				/* eof on socket. */
				break;
			}
/*
			msg(STDERR, "[%s packet read req=%d, %d bytes]", pname, pkt.req, len);
*/
			p += len;
			togo -= len;
		}	
}

	fp = (sock == ts_sock) ? &ts_cmds[pkt.req] : &tp_cmds[pkt.req];

	if (fp->req != pkt.req) {
		msg(STDERR, "%s request %d at wrong place in table!",
		  pname, fp->req);
		return(-1);
	}
	
	*func_pp = (int) fp->func;
	stat = (*fp->func) (pkt.id, pkt.len, data);
	return(stat);
}

/*
 - commfunc_check
 */
/*
 * ensure that the command table is in the right order and that there are
 * no gaps and that the last command in the table is TSC_LASTREQ.  This
 * function is called once during startup time.
 */
commfunc_check(which)
{
	int		cur_cmd = 0,
			bad = 0,
			last_req = (which == 0) ? TSC_LASTREQ : TPC_LASTREQ;
	gl_func		*p = (which == 0) ? ts_cmds : tp_cmds;
	char		*tbl_name = (which == 0) ? "ts_cmd" : "tp_cmd";

	/* make sure the table starts with 0. */
	if (p->req != 0) {
		msg(MDBUG, "%ss table begins with %d, not 0!",
		  tbl_name, p->req);
		expire(ES_STARTUP);
	}
	p++;
	cur_cmd++;

	for (; p->req < 255; p++, cur_cmd++) {
		if (p->req != cur_cmd) {
			msg(MDBUG, "%s %d not in order, expected %d!",
 			  tbl_name, p->req, cur_cmd);
			bad++;
		}
		if (p->req != ((p - 1)->req) + 1) {
			msg(MDBUG, "%s %d not sequential to %d!",
			  tbl_name, p->req, (p - 1)->req);
			bad++;
		}
	}

	/*
	 * check to make sure that the last cmd in the table is the last 
	 * command.
	 */
	p--;
	if (p->req != last_req) {
		msg(MDBUG, "last cmd in %s table not %d!",
		  tbl_name, last_req);
		bad++;
	}

	if (bad) {
		expire(ES_STARTUP);
		/* NOTREACHED */
	}
	return(0);
}

gl_quit(req, win, len, data)
	char	*data;
{
	giveup(ES_PSQUIT);
}

#endif VORTEX
