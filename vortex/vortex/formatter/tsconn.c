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
 *  A Multiple Representation Document Preparation Environment
 *
 *  This file is part of the VorTeX incremental formatter
 *  adopted from similar code written by Jeff McCarrell
 *
 *  Copyright (C) 1987 by	Pehong Chen	(phc@renoir.berkeley.edu)
 *		       and	Jeff McCarrell	(jwm@renoir.berkeley.edu)
 *
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

/*
 * The code that actually sits on the sockets and sends/recieves requests
 * sits here.
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/errno.h>
#include	<netinet/in.h>
#include	"tex.h"
#include	"scan.h"
#include	"dvi.h"
#include	"comm.h"
#include	"msg.h"
#include	"allir.h"
#include	"main.h"
#include	"mdep.h"
#include	"bits.h"

static char	*ts_req_names[] = {
	"BAD REQ:0",
	"VERIFY",
	"GOAWAY",
	"WELCOME",
	"LISTENAT",
	"LISTENING",
	"CONNECT",
	"FLUSH",
	"QUIT",
	"ABORT",
	"ERROR",
	"FORMAT",
	"CLOSEDOC",
	"OPENFILE",
	"CLOSEFILE",
	"INSERT",
	"DELETE",
	"TEXINPUT",
	"TEXOUTPUT",
	"TEXMESSAGE",
	"TEXERROR",
	"TEXEXECUTE",
	"TEXRETURN"
};

int 		doc_id = 0;
int		format_started = FALSE;

static int	request_count = sizeof(ts_req_names) / sizeof(*ts_req_names);

extern int		ts_sock;
extern int		errno;
extern gl_func		ts_cmds[];
extern gl_prmv		ts_prmv[];
extern char		name_of_file[FILE_NAME_SIZE];
extern int		cur_name;
extern char		docname[];	/* default document name */
extern _File		*file_root;

/*
 * send a command to the source editor.
 */
ts_send(req, len, id, data)
	char	*data;
{
	gl_hdr		pkt;

	if (ts_sock == -1) {
		errno = ESHUTDOWN;
		return(-1);
	}

	pkt.req = htons(req);
	pkt.len = htons(len);
	pkt.id  = htonl(id);
	if (write(ts_sock, (char *) &pkt, sizeof(pkt)) != sizeof(pkt)) {
		msg(PERROR, "write error to source on sock: %d",
		  ts_sock);
		shutdown(ts_sock, 2);
		return(-1);
	}
	if ((len > 0) && (write(ts_sock, data, len) != len)) {
		msg(PERROR, "write error to source on sock: %d, len %d",
		  ts_sock, len);
		shutdown(ts_sock, 2);
		return(-1);
	}

	msg(MDBUG, "Formatter sent command %s to SE [fid=%d len=%d sock=%d].",
	    ts_reqname(req), id, len, ts_sock);
	return(0);
}
		

char *
ts_reqname(req)
{
	if (req < 0 || req >= request_count) {
		return("request out of range");
	}
	return(ts_req_names[req]);
}


/*
 * We got a request from the source editor that we shouldn't have.
 */
ts_badreq(req, id, len, data)
	char	*data;
{
	msg(MDBUG, "bad request %s from source on doc %d, len %d",
	  ts_reqname(req), id, len);
	return(-1);
}

ts_format (id, len, data)
	char		*data;
{
	int		i;

	if (len == 0) {
		msg(STDERR, "No document root filename specified.");
		return(-1);
	}

	doc_id = id;
	for (i = 0; i < len; i++)
		docname[i] = data[i];
	docname[i] = NIL;
	msg(MDBUG, "Document root is %s.", docname);
	fg_format();
	format_started = TRUE;
}

ts_closedoc (id, len, data)
	char	*data;
{
	if (total_pages > 0) {
		/* finish up rest of the document */
		viewing_page = INFINITY;
		fg_format();
	}

	if (local_only)
		post_format();
/*
	else
		tp_sendpage(1);
*/
	file_root = NIL;
	starting_page = INFINITY;
	total_pages = 0;
	viewing_page = 1;
}

ts_openfile (id, len, data)
	char	*data;
{
	ir_open_file(id, len, data);
	read_input();
}

ts_closefile (fid, len, data)
	char	*data;
{
	ir_close_file(fid);
}

ts_insert (fid, len, data)
	char	*data;
{
	ir_insert(fid, len, data);
}

ts_delete (fid, len, data)
	char	*data;
{
	ir_delete(fid, len, data);
}

extern int 	Fid;
ts_texinput ()
{
	FILE		*a_open_in(), *fp;

	if (local_only) {
		if ((fp = a_open_in()) == NIL) {
			msg(STDERR, "TeX file %s not found.", name_of_file);
			exit(-1);
		}
		if (find_input(file_root)) {
			fprintf(stderr, "Reading file %s from IRs (cur_name is %d)...\n", name_of_file, cur_name);
			read_input();
		} else {
			fprintf(stderr, "Reading file %s from file system (cur_name is %d)...\n", name_of_file, cur_name);
			read_tex(0, ++Fid, name_of_file);
			read_input();
		}
	} else if (find_input(file_root)) {
		fprintf(stderr, "Reading file %s from IRs (cur_name is %d)...\n", name_of_file, cur_name);
		read_input();
	} else {
		fprintf(stderr, "Reading file %s from network (cur_name is %d)...\n", name_of_file, cur_name);
		ts_send(TSC_TEXINPUT, strlen(name_of_file), 0L, name_of_file);
		top_level((int) (ts_cmds[TSC_OPENFILE].func), 10);
	}
	return(1);
}

ts_texoutput (fn, data)
	char		*fn, *data;
{
	ts_send(TSC_TEXOUTPUT, strlen(fn), file_curr->id, data);
}

ts_execute (id, len, data)
	char		*data;
{
	long		cmd;
	char		*bstr;
	gl_prmv		*fp;
	int		size;
	
	cmd = ntohl(* (u_long *) data);
	data += U_LONG;
	fp = &ts_prmv[cmd];

	fprintf(stderr, "EXECUTE: cmd=%d, req=%d\n", cmd, fp->req);
	if (fp->req != cmd) {
		msg(STDERR, "Request %d at wrong place in prmv[] table!", fp->req);
		return(-1);
	}

	ALLOCA(bstr, char, fp->len);
	msg(MDBUG, "Executing primitive %d...", cmd);

	size = (*fp->func) (id, data, bstr);
	
	if (size != fp->len) {
		msg(MDBUG, "Size incompatible in tp_sendpage: %d bytes allocated, %d bytes processed.",
		    fp->len, size);
		
		return(-1);
	}
}

#endif VORTEX
