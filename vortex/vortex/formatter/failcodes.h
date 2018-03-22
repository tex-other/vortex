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

#ifndef FAILCODES_
#define FAILCODES_

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
 * Some values for exit(3) so we can decide what failed based only on the
 * exit status.
 */

#define ES_NORMAL	(0)
#define ES_USAGE	(2)
#define ES_STARTUP	(ES_USAGE + 1)
#define ES_NOSOCKS	(ES_STARTUP + 1)
#define ES_FATALMSG	(ES_NOSOCKS + 1)
#define ES_INTR		(ES_FATALMSG + 1)
#define ES_QUIT		(ES_INTR + 1)
#define ES_PSQUIT	(ES_QUIT + 1)

/*
 * these identify which signal we got to decide on an appropriate action.
 */

#define GOT_INTR	(1 << 0)
#define GOT_QUIT	(1 << 1)

/*
 * codes for close_socks.
 */
#define IFC_CLOSE_ALL	(0)
#define IFC_CLOSE_TS	(1)
#define IFC_CLOSE_TP	(2)

#endif
