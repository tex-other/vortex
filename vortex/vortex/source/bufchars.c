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
 *  RCS Info: $Header$
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
 *  bufchars.c - buffer character functions
 */
static char _ID[] = "@(#)bufchars.c for VorTeX, Copyright (c) 1987 John Coker";

#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"

/*
 *  DOCUMENTATION
 *
 *  Name: following-character
 *  Call: (following-character [ 'count ])
 *  Retu: fixnum
 *  Desc: This function returns the character code of the character
 *	after point in the current source buffer.  If the argument is
 *	given, the nth character after the point is checked.
 *	If the specified character would be at or after the end of
 *	the buffer, nil is returned.
 *
 *	Note that the character directly after point is zero characters
 *	after point (one skips over no characters to get there).  This
 *	means that the character directly after point is specified by
 *	zero, not one.
 *  SeeA: preceding-character
 */

DEFUN(dofollowchar, "following-character", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	int		count = 0, code;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg) || (count = gfixnum(arg.vl_data)) < 0)
			BADARGN(1, "a positive count");
	}

	code = following(bufp, count);
	if (code < 0)
		return (v_nil);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, code);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: preceding-character
 *  Call: (preceding-character [ 'count ])
 *  Retu: fixnum
 *  Desc: This function returns the character code of the character
 *	before point in the current source buffer.  If the argument is
 *	given, the nth character before the point is checked.
 *	If the specified character would be at or before the beginning
 *	of the buffer, nil is returned.
 *
 *	Note that the character directly before point is zero characters
 *	before point (one skips over no characters to get there).  This
 *	means that the character directly before point is specified by
 *	zero, not one.
 *  SeeA: following-character
 */

DEFUN(doprecchar, "preceding-character", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	int		count = 0, code;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!fixnump(arg) || (count = gfixnum(arg.vl_data)) < 0)
			BADARGN(1, "a positive count");
	}

	code = preceding(bufp, count);
	if (code < 0)
		return (v_nil);
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, code);
	return (ret);
}

following(bufp, count)
	struct buffer	*bufp;
{
	struct source	*srcp;
	struct tblock	*next;
	register int	offset;

	/* make sure we were given reasonable values */
	ASSERT(bufp != NULL);
	if (bufp->bu_type != BUFF_SOURCE)
		error("You can't get following characters from this buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	ASSERT(count >= 0);

	/* set up offset and check for beginning of buffer */
	offset = srcp->sb_point + count + 1;
	if (offset > srcp->sb_length)
		return (-1);

	/* find block containing offset in buffer */
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
	}
	if (next == NULL)
		return (-1);

	/* return the appropriate character code */
	return (next->tb_text[offset - next->tb_offset] & 0377);
}

preceding(bufp, count)
	struct buffer	*bufp;
{
	struct source	*srcp;
	struct tblock	*next;
	register int	offset;

	/* make sure we were given reasonable values */
	ASSERT(bufp != NULL);
	if (bufp->bu_type != BUFF_SOURCE)
		error("You can't get preceding characters from this buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	ASSERT(count >= 0);

	/* set up offset and check for beginning of buffer */
	offset = srcp->sb_point - count;
	if (offset <= srcp->sb_start)
		return (-1);

	/* find block containing offset in buffer */
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
	}
	if (next == NULL)
		return (-1);

	/* return the appropriate character code */
	return (next->tb_text[offset - next->tb_offset] & 0377);
}
