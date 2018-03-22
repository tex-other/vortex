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
 *  RCS Info: $Header: format.h,v 0.1 87/04/30 20:53:38 john Locked $
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
 *  format.h - format definitions for varargs parsing
 */
 
#ifndef _FORMAT_
#define _FORMAT_

#include <ctype.h>

extern char	*itoa();	/* convert an integer to any base */
extern char	*utoa();	/* convert an unsigned integer to decimal */
extern char	*itor();	/* convert an integer to roman number */
extern char	*ftoa();	/* convert a float into nnn.nnn format */

#define etoa(n)	ftoa(n)		/* for now, we only print floating- */
#define gtoa(n)	ftoa(n)		/* point numbers in %f format */

extern char	*psexpr();	/* print (struct value) as s-epxression */
extern char	*psymbol();	/* print (struct string) as symbol */
extern char	*pstring();	/* print (struct string) as string */
extern char	*pchar();	/* return printable representation of char */
extern char	*pkeycode();	/* print integer as a key code */
extern char	*pkeyseq();	/* print a string as a key sequence */

#define ADJ_LEFT	'-'	/* left adjustment character */
#define ADJ_CENTER	'|'	/* center adjustment character */
#define ADJ_RIGHT	'+'	/* right adjustment character */

#define DEF_ADJUST	ADJ_RIGHT

#define isadjust(c)	((c) == ADJ_LEFT || \
			 (c) == ADJ_CENTER || \
			 (c) == ADJ_RIGHT)

#define WID_UNSPEC	(-1)	/* field width unspecified */
#define WID_GETARG	(-2)	/* read width from next argument */

#define FMT_DECIMAL	'd'	/* decimal number */
#define FMT_UNSIGNED	'u'	/* unsigned decimal number */
#define FMT_OCTAL	'o'	/* octal number */
#define FMT_HEX		'x'	/* hexadecimal number */
#define FMT_BINARY	'b'	/* binary number */
#define FMT_FLOAT	'f'	/* floating point number */
#define FMT_EXPT	'e'	/* floating point number as exponent */
#define FMT_FSHORT	'g'	/* short floating point number */
#define FMT_STRING	's'	/* C string, zero terminated (char *) */
#define FMT_CHAR	'c'	/* ASCII character number */
#define FMT_PNTCHAR	'C'	/* printable representation of character */
#define FMT_LSTRING	'S'	/* lisp string, (struct string *) */
#define FMT_SYMBOL	'Y'	/* lisp symbol, (struct string *) */
#define FMT_VALUE	'v'	/* lisp value, (struct value) */
#define FMT_ROMAN	'r'	/* integer as roman numeral */
#define FMT_KEYCODE	'k'	/* integer as key stroke */
#define FMT_KEYSEQ	'K'	/* key sequence, (struct string *) */

#define FMT_FORMAT	'%'	/* field introductory character */
#define FMT_WIDSEP	'.'	/* field width argument separator */
#define FMT_READARG	'*'	/* read the field width from argument */

extern char	NULLPTR[];

#endif !_FORMAT_
