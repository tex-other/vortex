/* 
 * Copyright (c) 1987 The Regents of the University of California.
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

/*
 *  This code is part of the VorTeX project.  This file was
 *  written by Steven Procter for the VorTeX project under the
 *  direction of Prof. Michael A. Harrison of the University
 *  of California at Berkeley.  This code was written by Steven
 *  Procter.
 *
 *  Copyright (c) 1987 Steven James Procter
 *  University of California, Berkeley
 *  procter@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  Steven Procter and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 */

#define NTOHS(a)    ((a) = ntohs((a)))
#define NTOHL(a)    ((a) = ntohl((a)))
#define HTONS(a)    ((a) = htons((a)))
#define HTONL(a)    ((a) = htonl((a)))

/*  Macros to read network information.  If an error occurs they print an
 *  error message and return.
 */

#define GetChar(a, s, f) \
    if (read ((s), &(a), sizeof ((a))) != sizeof ((a))) { \
	fprintf (stderr, "vpe: (f): couldn't read (a):"); \
	perror (""); \
	return (-1); \
    }

#define GetShort(a, s, f) \
    if (read ((s), &(a), sizeof ((a))) != sizeof ((a))) { \
	fprintf (stderr, "vpe: (f): couldn't read (a):"); \
	perror (""); \
	return (-1); \
    } \
    NTOHS((a))

#define GetLong(a, s, f) \
    if (read ((s), &(a), sizeof ((a))) != sizeof ((a))) { \
	fprintf (stderr, "vpe: (f): couldn't read (a):"); \
	perror (""); \
	return (-1); \
    } \
    NTOHL((a))

#define LEAVE(v) \
    fprintf (stderr, "vpe: line=%d file=%s exiting cond=%d.\n", __LINE__, \
	     __FILE__, (v)); \
    exit (v)
