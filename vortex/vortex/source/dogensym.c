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
 *  RCS Info: $Header: dogensym.c,v 0.1 87/05/01 11:49:00 john Locked $
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
 *  dogensym.c - generate unique symbols
 */
static char _ID[] = "@(#)dogensym.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: gensym
 *  Call: (gensym [ 'symbol ])
 *  Retu: symbol
 *  Desc: This function returns a symbol which is likely not to
 *	be \sym{equal} to any other symbol currently existing.
 *	The symbol's print name is of the form \em{x0n} where \em{x}
 *	is the leader given as the optional argument (which must
 *	evaluate to a symbol), or \lit{%gsym} if no argument is given,
 *	and \em{n} is a number unique to each call of \sym{gensym}.
 *  Side: It it possible to make a symbol \sym{equal} (and even
 *	\sym{eq}) to a symbol created by \sym{gensym}, simply by
 *	creating another with the same print name.  However, the
 *	leader of the symbols generated this way is meant to be
 *	``unusual'' enough so that this class will not occur
 *	unintentionally.
 */
MKSTRING(GSLEADER, "%gsym");

DEFUN(dogensym, "gensym", FLAG_NONE, NULL)
{
	struct string	*sgensym();
	struct value	arg, ret;
	struct string	*lead, *str;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!symbolp(arg))
			BADARGN(1, "a symbol name");
		lead = gsymbol(arg.vl_data)->sy_pname;
	} else {
		/* use the default symbol leader */
		lead = GSLEADER;
	}
	str = sgensym(lead);
	ret.vl_type = LISP_SYMBOL;
	ssymbol(ret.vl_data, save_symbol(str));
	return (ret);
}

/*
 *  This is the internal gensym used by the user routine above
 *  as well as other routines that need anonymous symbol names.
 *  We pass in a string and this function returns a new string.
 *  It is expected that the calling function will intern this
 *  string before the next garbage collection.
 */
#define COUNTLEN	5
#define MAXLEADLEN	(SMALLBUF - COUNTLEN - 1)

static int		gensym_count = 0;

struct string *
sgensym(leader)
	struct string	*leader;
{
	char	buf[SMALLBUF];
	int	len;

	if (leader->st_length > MAXLEADLEN)
		len = MAXLEADLEN;
	else
		len = leader->st_length;

	gensym_count++;
	bcopy(leader->st_buffer, buf, len);
	sprintf(buf + len, "%05d", gensym_count);
	len += COUNTLEN;
	return save_string(buf, len);
}
