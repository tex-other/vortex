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
 *  findchar.c - find a character relative to point
 */
static char _ID[] = "@(#)findchar.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: find-character-forward
 *  Call: (find-character-forward 'code [ 'count ])
 *  Retu: offset
 *  Desc: This function returns the offset of the first occurence
 *	of the specified character \lit{after} point in the current
 *	buffer.  If no character is found before the end of the
 *	buffer, the function returns nil.  If count is given, it
 *	must be a positive number which specifies the nth occurance
 *	rather than the first.
 *  SeeA: find-character-backward
 */

DEFUN(findcharfwd, "find-character-forward", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	int		count = 1, code, offset;
	struct buffer	*bufp = current_buffer;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a character code");
	count = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(1);
		if (fixnump(arg))
			count = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(1, "a count");
	}

	offset = findforward(bufp, code, count);
	if (offset < 0) {
		/* not found as specified */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, offset);
		return (ret);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: find-character-backward
 *  Call: (find-character-backward 'code [ 'count ])
 *  Retu: offset
 *  Desc: This function returns the offset of the first occurence
 *	of the specified character \lit{before} point in the current
 *	buffer.  If no character is found before the start of the
 *	buffer is reached, the function returns nil.  If count is given,
 *	it must be a positive number which specifies the nth occurance
 *	rather than the first.
 *  SeeA: find-character-forward
 */

DEFUN(findcharback, "find-character-backward", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	int		count = 1, code, offset;
	struct buffer	*bufp = current_buffer;

	CHECKAC(1, 2);
	arg = EVALARGN(1);
	if (!fixnump(arg))
		BADARGN(1, "a character code");
	code = gfixnum(arg.vl_data);

	if (GETACOUNT() > 1) {
		arg = EVALARGN(1);
		if (fixnump(arg))
			count = gfixnum(arg.vl_data);
		else if (!nullp(arg))
			BADARGN(1, "a count");
	}

	offset = findbackward(bufp, code, count);
	if (offset < 0) {
		/* not found as specified */
		return (v_nil);
	} else {
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, offset);
		return (ret);
	}
}

findforward(bufp, code, count)
	struct buffer	*bufp;
{
	struct source	*srcp;
	struct tblock	*next, *last;
	unsigned long	*txp, *beg, *end;
	register int	offset;

	/* call findbackward if we're to go up */
	if (count < 0)
		return findbackward(bufp, code, -count);

	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* find block containing offset in buffer */
	offset = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
		last = next;
	}
	if (next == NULL)
		next = last;
	if (next == NULL)
		return (-1);

	/* skip forward count occurances */
	end = next->tb_text + next->tb_length;
	txp = next->tb_text + (offset - next->tb_offset) - 1;
	while (count-- > 0 && next != NULL) {
		/* search forwards for the character */
		do {
			if (++txp >= end) {
				next = next->tb_next;
				if (next != NULL) {
					end = next->tb_text + next->tb_length;
					txp = next->tb_text;
				}
			}
			offset++;
		} while (next != NULL && charof(*txp) != code);
	}
	if (count > 0)
		return (-1);

	/* make sure offset is valid and return it */
	if (offset > srcp->sb_length)
		offset = srcp->sb_length;
	else
		offset = srcp->sb_start;
	return (offset);
}

findbackward(bufp, code, count)
	struct buffer	*bufp;
{
	struct source	*srcp;
	struct tblock	*last, *next;
	unsigned long	*txp, *beg, *end;
	register int	offset;
	register int	over = 0;

	/* call findforward if we're to go down */
	if (count < 0)
		return findforward(bufp, code, -count);

	ASSERT(bufp != NULL);
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* find block containing offset in buffer */
	offset = srcp->sb_point;
	last = NULL;
	for (next = srcp->sb_text; next != NULL; next = next->tb_next) {
		if (next->tb_offset + next->tb_length > offset)
			break;
		last = next;
	}
	if (next == NULL)
		next = last;
	if (next == NULL)
		return (-1);

	/* skip backward count lines */
	beg = next->tb_text;
	txp = next->tb_text + (offset - next->tb_offset) - 1;
	while (count-- >= 0 && next != NULL) {
		/* search backwards for the character */
		do {
			if (--txp < beg) {
				next = next->tb_prev;
				if (next != NULL) {
					beg = next->tb_text;
					txp = next->tb_text +
					    next->tb_length - 1;
				}
			}
			offset--;
		} while (next != NULL && charof(*txp) != code);
	}
	if (count > 0)
		return (-1);

	/* make sure offset is valid and return it */
	if (offset < srcp->sb_start)
		offset = srcp->sb_start;
	else if (offset > srcp->sb_length)
		offset = srcp->sb_length;
	return (offset);
}
