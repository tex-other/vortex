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

/*
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the VorTeX project written by Pehong Chen, John
 *  Coker, Steve Procter and Ikuo Minakata for the VorTeX project under
 *  Prof. Michael A. Harrison of the University of California at Berkeley.
 *
 *  (c) 1987  Pehong Chen, John Coker, Steve Procter and Ikuo Minakata.
 *  University of California, Berkeley
 *  vortex-designers@renoir.Berkeley.EDU
 *
 *  All rights reserved by the authors.  See the copyright notice
 *  distributed with this software for the complete description of
 *  the conditions under which it is made available.
 *
 *  gl_comm.h - global communications request specification
 */
 
#ifndef _GLCOMM_
#define _GLCOMM_
/*
 *  Global communications.
 *
 *  Packet format:
 *
 *	u_short	request		request code as defined below
 *	u_short	datalen		length of rest of packet
 *	u_long	commdata	communications specific data
 *	<datalen bytes>		rest of packet; zero or more bytes
 *
 *  In the request definitions below, long is four bytes, short
 *  is two bytes, char is one byte and an array is zero or more
 *  of those objects as specified by the datalen field in the
 *  packet header in bytes.
 *
 *  Note the two constants below.  GLC_VERSION should be bumped
 *  each and every time this file is changed.  This is to insure
 *  that the programs will not try to communicate with different
 *  protocol specifications.  GLC_IDENT is just a magic number for
 *  connection verification.  There are separate version numbers
 *  in each of the specific communications file also.
 */
#define GLC_IDENT	7284
#define GLC_VERSION	2

/*
 *  GLC_VERIFY
 *	u_long	ident		communications ``password''
 *	u_short	gl_version	the global protocol version in use
 *	u_short	loc_version	the local protocol version in use
 *
 *  The ident is a magic number to verify to the source editor that
 *  the connected processes is indeed a proper proof editor.  The
 *  version parameters are the versions of the protocol specification
 *  compiled in, both global and local.
 */
#define GLC_VERIFY	1

/*
 *  GLC_GOAWAY
 *	u_char	mesg[]		string reason for refusal
 *
 *  If the proof editor's verification packet is invalid or times
 *  out, the source editor sends this packet with an appropriate
 *  error message.
 */
#define GLC_GOAWAY	(GLC_VERIFY + 1)

/*
 *  GLC_WELCOME
 *
 *  Welcome to the VorTeX system, verification was correct.
 */
#define GLC_WELCOME	(GLC_GOAWAY + 1)

/*
 *  GLC_LISTENAT
 *	u_long	port	internet port number
 *	u_long	timeout	timeout in milliseconds
 *
 *  Listen for a connection from your unconnected peer on next port
 *  available port after the one specified.
 */
#define GLC_LISTENAT	(GLC_WELCOME + 1)

/*
 *  GLC_LISTENING
 *	u_long	port	internet port number
 *
 *  Report to source editor that one vertex is listening for a
 *  connection from the other at the specified port.
 */
#define GLC_LISTENING	(GLC_LISTENAT + 1)

/*
 *  GLC_CONNECT
 *	u_long	port	internet port number
 *	u_long	timeout	timeout in milliseconds
 *	string	host	remote host for connect
 *
 *  Connect to your unconnected peer at the specified port on the
 *  given host machine.
 */
#define GLC_CONNECT	(GLC_LISTENING + 1)

/*
 *  GLC_FLUSH
 *
 *  This packet instructs the receiver to throw away all unprocessed
 *  input (still to be read) up to the ``out of band'' marker in the
 *  input stream.
 */
#define GLC_FLUSH	(GLC_CONNECT + 1)

/*
 *  GLC_QUIT
 *
 *  This packet is sent on normal termination of VorTeX system, or
 *  at least of the proof editor.
 */
#define GLC_QUIT	(GLC_FLUSH + 1)

/*
 *  GLC_ABORT
 *
 *  This packet is sent when some sort of fatal error terminates
 *  the VorTeX system, or at least the proof editor.
 */
#define GLC_ABORT	(GLC_QUIT + 1)

/*
 *  GLC_ERROR
 *	u_char	string[]	an error message for the user
 *
 *  Sends an error message back to the source editor for printing
 *  to the user.
 */
#define GLC_ERROR	(GLC_ABORT + 1)

#define GLC_LASTREQ	GLC_ERROR

#endif !_GLCOMM_
