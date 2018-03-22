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
 * This file implements the interface to the TeX<->proof commands.
 */

#include		<sys/types.h>
#include		"msg.h"
#include		"failcodes.h"
#include		"comm.h"
#include		"tpfdecls.h"

gl_func	tp_cmds[] = {
{ 0,			tp_badreq },
{ GLC_VERIFY,		tp_badreq },
{ GLC_GOAWAY,		(int (*)()) 0 },
{ GLC_WELCOME,		tp_badreq },
{ GLC_LISTENAT,		gl_listenat },
{ GLC_LISTENING,	tp_badreq },
{ GLC_CONNECT,		gl_connect },
{ GLC_FLUSH,		(int (*)()) 0 },
{ GLC_QUIT,		(int (*)()) 0 },
{ GLC_ABORT,		(int (*)()) 0 },
{ GLC_ERROR,		(int (*)()) 0 },
{ TPC_SENDPAGE,		tp_sendpage },
{ TPC_PAGEINFO,		tp_badreq },
{ TPC_PAGEOKAY,		tp_badreq },
{ TPC_PAGEBAD,		tp_badreq },
{ 255,			(int (*)()) 0 }
};

#endif VORTEX
