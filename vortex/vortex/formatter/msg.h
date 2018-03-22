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

#ifndef MSG_
#define MSG_

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
 *  Modified by Pehong Chen (phc@berkeley.edu) for the formatter.
 */

/*
 * The message types.
 */

#define PLAIN		(0)
#define FATAL		(1 << 0)
#define PERROR		(1 << 1)
#define LITERAL		(1 << 2)
#define STDERR		(1 << 3)
#define SOURCE		(1 << 4)	/* msg goes to source editor. */
#define FORMAT		(1 << 5)	/* msg goes to formatter. */
#define MDBUG		(1 << 6)
#define SDBUG		(1 << 7)
#define PDBUG		(1 << 8)

#endif MSG_
