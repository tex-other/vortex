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
 *  RCS Info: $Header: fmtdecl.h,v 0.1 87/04/30 20:53:20 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the source editor/user interface written
 *  by John Coker for the VorTeX project under the direction of
 *  Prof. Michael A. Harrison of the University of California at
 *  Berkeley.
 *
 *  Copyright (c) 1987 John L. Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  John Coker and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  fmtdecl.h - declarations for varargs parsing
 */
 
/*
 *  This is gross, but we want to avoid having to repeat this ugly
 *  code for each variable argument print function.  The code below
 *  declares variables needed for the format code as described in
 *  "format.h".  Later the code in "fmtcode.h" should be incorporated.
 *
 *  This code declares and initializes a buffer called msgbuf, with
 *  the pointer mp initialized to the beginning.  If mp is changed
 *  before the code in "fmtcode.h" is included, the existing string
 *  will form a prefix to the formatted output.
 */
#ifndef STARTVA
#define STARTVA()	(va_start(_argp), _started = 1)
#define ENDVA()		va_end(_argp)
#define GETVARG(t)	va_arg(_argp, t)
#endif !STARTVA

	static char	msgbuf[1024];
	int		msglen;
	char		*_format;
	register char	*mp = msgbuf, *mend = msgbuf + sizeof (msgbuf) - 2;
	int		_started = 0;
	va_list		_argp;
	char		*_fp, *_fend;
	int		_type, _min, _max, _adj;
	unsigned int	_uval;
	unsigned char	_uchr;
	int		_ival;
	double		_fval;
	char		*_cptr;
	struct value	_vval;
	struct string	*_sval;
