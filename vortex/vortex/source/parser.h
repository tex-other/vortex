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
 *  RCS Info: $Header: parser.h,v 0.1 87/04/30 20:54:29 john Locked $
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
 *  parser.h - vLisp code parser declarations
 */
 
#ifndef _PARSER_
#define _PARSER_

#include <ctype.h>

/*
 *  The characters for parsing the lisp input.  These are
 *  variables that may be reset to change the language syntax.
 *  The is<something>(c) macros are obvious comparisons with
 *  these variable contents.  The only things that merit
 *  special note are the isflonum(c) isfixnum(c) and islist(c)
 *  macros which compare the argument against more than
 *  one thing and thus may have side-effects.
 */
extern short
	p_blistc, p_elistc, p_quotec, p_bquotec, p_dtprc,
	p_stringc, p_barrayc, p_earrayc, p_literalc, p_escapec,
	p_commentc, p_delimc;

#define islist(c)	((c) == p_blistc || (c) == p_elistc)
#define isblist(c)	((c) == p_blistc)
#define iselist(c)	((c) == p_elistc)
#define isquote(c)	((c) == p_quotec)
#define isbquote(c)	((c) == p_bquotec)
#define isdtpr(c)	((c) == p_dtprc)
#define isstring(c)	((c) == p_stringc)
#define isarray(c)	((c) == p_barrayc || (c) == p_earrayc)
#define isbarray(c)	((c) == p_barrayc)
#define isearray(c)	((c) == p_earrayc)
#define isliteral(c)	((c) == p_literalc)
#define isescape(c)	((c) == p_escapec)
#define iscomment(c)	((c) == p_commentc)
#define isdelim(c)	((c) == p_delimc)
#define isflonum(c)	(isdigit(c) || (c) == '.' || (c) == '-')
#define isfixnum(c)	(isdigit(c) || (c) == '-')
#define isoctal(c)	((c) >= '0' && (c) <= '7')
#define isblank(c)	((c) == ' ' || (c) == '\t')

/*
 *  The constants for mapping mouse button events to ``character''
 *  codes that can be bound to functions.  We translate mouse
 *  buttons into 8-bit characters by assuming the meta-bit is
 *  on (keyboard characters are ONLY 7 bits) and using the low
 *  7 bits for the specific encoding.  We allow up to eight
 *  locator (mouse) buttons.
 */
#define BUTTON_MASK	0007		/* lower three bits */
#define MOUSE_BIT	0200		/* eigth bit (meta bit) */

#define MOUSE_BUTTON(n)	(MOUSE_BIT | ((n) & BUTTON_MASK))

#define MOUSE_CTRL	(1 << 3)	/* control key pressed */
#define MOUSE_SHIFT	(1 << 4)	/* shift key pressed */
#define MOUSE_META	(1 << 5)	/* meta key pressed */

#define MOUSE_LEFT	MOUSE_BUTTON(0)
#define MOUSE_MIDDLE	MOUSE_BUTTON(1)
#define MOUSE_RIGHT	MOUSE_BUTTON(2)

#define MOUSE_MAXCODE	0377		/* maximum eight bit value */
#define CHAR_MAXCODE	0177		/* maximum seven bit value */

#endif !_PARSER_
