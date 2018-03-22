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

#ifndef COMM_
#define COMM_

/*
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Preparation Environment
 *
 *  This file is part of the VorTeX incremental formatter
 *
 *  Copyright (C) 1987 by   Pehong Chen		(phc@renoir.berkeley.edu)
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

/*
 * The structs used in communication with the Source Editor and with the 
 * Proof Editor.
 */


/*
 * Include the files that define the communication parameters.
 */
#include	"gl_comm.h"
#include	"ts_comm.h"
#include	"tp_comm.h"

/*
 * Structures that are generic across both communication channels.
 */

/*
 * The generic packet header.
 */
typedef struct GL_hdr {
	u_short			req;
	u_short			len;
	u_long			id;
} gl_hdr;


/*
 * the struct that associates command types with the C code that 
 * implements them.
 */

typedef struct GL_prmv {
	u_short			req;
	u_short			len;
	int			(*func) ();
} gl_prmv;

typedef struct GL_func {
	u_short			req;
	int			(*func) ();
} gl_func;

typedef struct GL_pty {
	u_short			req;
	int			pty;
} gl_pty;

typedef struct GL_req {
	u_short			req;
	u_short			len;
	u_long			id;
	int			pty;
	char			*data;
	int			(*func)();
	struct GL_req		*prev;
	struct GL_req		*next;
} gl_req;

/* Send a long. */
#define SEND_L(D) { \
	lp = (long *) bstr; \
	*lp++ = htonl((D)); \
	bstr = (char *) lp; \
	cnt += U_LONG; \
}

#define SEND_SL(D) { \
	lp = (long *) bstr; \
	*lp = htonl((D)); \
	bstr = (char *) lp; \
	cnt += U_LONG; \
}

/* Send a short. */
#define SEND_S(D) { \
	sp = (short *) bstr; \
	*sp++ = htons((D)); \
	bstr = (char *) sp; \
	cnt += U_SHORT; \
}

/* Send a char */
#define SEND_C(D) { \
	*bstr++ = (D); \
	cnt++; \
}

#endif

