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
 *  adopted froma similar code written by Jeff McCarrell
 *
 *  Copyright (C) 1987 by	Pehong Chen	(phc@renoir.berkeley.edu)
 *			and	Jeff McCarrell	(jwm@renoir.berkeley.edu)
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
 * to and from ProofEditor lives here.
 */

#include	<sys/file.h>
#include	<stdio.h>
#include	"mdep.h"
#include	<netinet/in.h>
#include	"main.h"
#include	"comm.h"
#include	"allir.h"
#include	"tp_comm.h"
#include	"msg.h"
#include	"state.h"

#include	"tex.h"
#include	"file.h"
#include	"io.h"
#include	"str.h"

static char	*tp_req_names[] = {
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
	"SENDPAGE",
	"PAGEINFO",
	"PAGEOKAY",
	"PAGEBAD"
};

static int	request_count = sizeof (tp_req_names) / sizeof (*tp_req_names);

extern int	tp_sock;
extern int	errno;
extern int	write_out;
extern str	str_vtx;

/*
 * send a command to the ProofEditor process.

 */
tp_send(req, id, len, data)
	char	*data;
{
	gl_hdr		pkt;

	if (tp_sock == -1) {
		errno = ESHUTDOWN;
		return(-1);
	}
	pkt.req = htons(req);
	pkt.len = htons(len);
	pkt.id  = htonl(id);

	msg(PDBUG, "Sending %s (%d), for page %d len %d to ProofEditor on sock %d",
	    tp_reqname(req), req, id, len, tp_sock);
	if (write(tp_sock, (char *) &pkt, sizeof(pkt)) != sizeof(pkt)) {
		msg(PERROR, "write error to ProofEditor on sock: %d",
		  tp_sock);
		shutdown(tp_sock, 2);
		return(-1);
	}
	if ((len > 0) && (write(tp_sock, data, len) != len)) {
		msg(PERROR, "write error to ProofEditor on sock: %d, len %d",
		  tp_sock, len);
		shutdown(tp_sock, 2);
		return(-1);
	}

	msg(PDBUG, "wrote %s for page %d len %d to ProofEditor sock %d",
	  tp_reqname(req), id, len, tp_sock);
	return(0);
}


char *
tp_reqname(req)
{
	if (req < 0 || req >= request_count) {
		return("request out of range");
	}
	return(tp_req_names[req]);
}

/*
 * We got a request from the formatter that we shouldn't have.
 */
tp_badreq(req, id, len, data)
	char	*data;
{
	msg(MDBUG, "bad request %s from formatter on pdoc %d, len %d",
	  tp_reqname(req), id, len);
	return(-1);
}

tp_sendpage(no, len, data)
	char		*data;
{
	int		osize;
	int		nsize;
	short		i;
	char		*bstr;

	char		ext[20];
	FILE		*fp;
	str		str_out;

	viewing_page = no;
	msg(MDBUG, "Page number requested %d", no);
	if (fg_format())
		pbox_curr = pbox;
	else if (find_page(viewing_page) == -1) {
		tp_send(TPC_PAGEBAD, no, 0L, "");
		return(-1);
	} else if (pbox_curr->_ok) {
		tp_send(TPC_PAGEOKAY, no, 0L, "");
		return(0);
	}

	/* page must be resent */
	osize = U_LONG + U_LONG * 10 + U_SHORT +
		pbox_curr->_tf * sizeof(_Font) + pbox_curr->_tn +
		pbox_curr->_tb * (1 + sizeof(_Nbox)) +
		pbox_curr->_ts * U_SHORT + pbox_curr->_ta + 1;

	ALLOCA(bstr, char, osize);

	if ((nsize = send_page(bstr)) != osize) {
		msg(STDERR, "Size incompatible in tp_sendpage: %d bytes allocated, %d bytes processed.",
		    osize, nsize);
		
		return(-1);
	}
	pbox_curr->_ok = TRUE;
	tp_send(TPC_PAGEINFO, no, osize, bstr);
	if (write_out) {
		if (job_name == 0)
			job_name = str_texput;
		sprintf(ext, ".%d.%s", pbox_curr->_no, "out");
		str_out = make_string_given(ext);
		if (access(VORTEX_DIR, F_OK) < 0)
			mkdir(VORTEX_DIR, 0775);
		pack_file_name(job_name, str_vtx, str_out);
		fp = b_open_out();
		if (fwrite(bstr, 1, osize, fp) != osize) {
			msg(STDERR, "Failed to write .out file for page %d.", no);
			return (-1);
		}
		fprintf(stderr, "\nFlattened box tree written on %s.\n", name_of_file);
		fclose(fp);
	}
}

#endif VORTEX
