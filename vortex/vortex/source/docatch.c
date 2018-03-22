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
 *  RCS Info: $Header: docatch.c,v 0.1 87/05/01 11:36:24 john Locked $
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
 *  docatch.c - vLisp interface functions catch and throw
 */
static char _ID[] = "@(#)docatch.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "catch.h"

struct catch	*catch_list = NULL;

/*
 *  DOCUMENTATION
 *
 *  Name: catch
 *  Call: (catch 'tag body ...)
 *  Retu: any
 *  Desc: This function returns the result of evaluating the last
 *	expression in the list of body statements, or the value
 *	\sym{throw}n during execution of those statements.  The
 *	first argument must evaluate to a symbol, which is the
 *	name of the catch tag which can be the target of a \sym{throw}.
 *	The rest of the arguments are evaluated one by one and the
 *	result of evaluating the last one is returned by this function
 *	if no value is thrown to this \sym{catch}.
 *  Side: A catch tag is set up at this lisp call frame, and all the
 *	body expressions are evaluated ``beneath'' this call frame.
 *	This means that throws to a catch may only occur inside the
 *	expressions evaluated as the body of the catch.  The catch
 *	tag is deleted just before the function returns.
 *
 *	Catches may nest, but one may only throw to the innermost
 *	of these nested catches.  Throwing to a catch deletes it
 *	because that call to \sym{catch} will then return, invalidating
 *	the catch set therein.
 *
 *	Two catches are set up initially by the editor; \sym{top-level}
 *	and \sym{error}.  The top-level catch always takes him back
 *	to the main loop of the editor (unles explicitly reset).
 *	This is used by the \sym{reset} command (for break loops)
 *	for example.  The error catch also takes one to the
 *	\em{top-level}, but it expects a value to be thrown to it,
 *	which should be a string error message.  This is how \sym{error}
 *	is implememted internally.  Both these catches are resettable
 *	by the user, although this should be done with caution.
 *  SeeA: throw
 */

DEFUN(docatch, "catch", FLAG_NONE, NULL)
{
	struct value	arg1, result;
	struct string	*cname;
	jmp_buf		catchbuf;
	int		arg, argc;

	CHECKAC(2, -1);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a tag symbol");
	cname = gsymbol(arg1.vl_data)->sy_pname;
	argc = GETACOUNT();

	switch (setjmp(catchbuf)) {
	case -1:
		panic("Setjmp failed for catch tag %Y!", cname);
	case 0:
		int_catch(cname, catchbuf, FLAG_NONE);
		result = v_nil;
		break;
	default:
		result = throw_value(cname);
		goto done;
	}

	/* loop through and evaluate the body expressions */
	for (arg = 2; arg <= argc; arg++)
		result = EVALARGN(arg);

done:	/* clean up and return result */
	int_catch(cname, NULL, CATCH_DELETE);
	return (result);
}

int_catch(tag, env, flags)
	struct string	*tag;
	jmp_buf		env;
{
	struct catch	*next, *last;
	struct catch	*new = NULL;

	/* set up this new catch location */
	last = NULL;
	for (next = catch_list; next != NULL; next = next->ca_next) {
		if (sequal(next->ca_tag, tag))
			break;
		last = next;
	}

	if (next == NULL) {
		if ((flags & (CATCH_DELETE|CATCH_DESTROY)) != 0) {
			/* tag not found, but it's not an error */
			return (1);
		} else {
			new = (struct catch *)valloc(sizeof (struct catch));
			if (last == NULL)
				catch_list = new;
			else
				last->ca_next = new;
			new->ca_stack = NULL;
			new->ca_next = next;
		}
	} else if ((flags & CATCH_DESTROY) != 0) {
		if (last == NULL)
			catch_list = next->ca_next;
		else
			last->ca_next = next->ca_next;
		while (next != NULL) {
			vfree(next);
			next = next->ca_stack;
		}
		return (0);
	} else if ((flags & CATCH_DELETE) != 0) {
		if (next->ca_stack != NULL) {
			if (last == NULL)
				catch_list = next->ca_stack;
			else
				last->ca_next = next->ca_stack;
			next->ca_stack->ca_next = next->ca_next;
		} else {
			if (last == NULL)
				catch_list = next->ca_next;
			else
				last->ca_next = next->ca_next;
		}
		vfree(next);
		return (0);
	} else {
		/* push a catch onto the stack */
		new = (struct catch *)valloc(sizeof (struct catch));
		next->ca_next = NULL;
		/* link this new into the list */
		if (last == NULL)
			catch_list = new;
		else
			last->ca_next = new;
		new->ca_next = next->ca_next;
		new->ca_stack = next;
	}

	ASSERT(new != NULL);
	new->ca_tag = tag;
	new->ca_flags = flags;
	bcopy(env, new->ca_jmpbuf, JMPBUFSIZE);
	new->ca_frame = call_depth;
	new->ca_value = NOVALUE;
	if (stack_ptr != NULL)
		stack_ptr->st_flags |= STACK_ISCATCH;
	debug(DTHROW, "Set new catch %Y at stack frame %d.",
	      new->ca_tag, new->ca_frame);

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: throw
 *  Call: (throw 'tag [ 'any ])
 *  Desc: This function jumps the flow of control back to the
 *	\sym{catch} that matches the tag argument.  This tag should
 *	evaluate to a symbol that has been the name given in a
 *	\sym{catch} statement previously.  The optional second
 *	argument is evaluated, and will be the return value of
 *	the original \sym{catch} statement when control returns
 *	there.
 *
 *	By default, VorTeX sets up two special catch tags which
 *	always exist (although they may be reset deeper into the
 *	stack by the user).  The \em{top-level} catch transfers
 *	control to the top level (main loop) of the editor.  This
 *	is where key strokes are interpreted as editing commands.
 *	The \em{error} catch also transfers control to \em{top-level}
 *	(or a break loop, if the symbol \sym{break-on-error} is set
 *	non-nil), but it expects to be thrown a value, which should
 *	be a string message (the error message).
 *  Side: This function never returns; control is passed to the
 *	\sym{catch} statement for the tag argument.  It will appear
 *	that the original \sym{catch} returned with the given
 *	value, or nil if the optional second argument was not present.
 *  SeeA: catch
 */

DEFUN(dothrow, "throw", FLAG_NONE, NULL)
{
	struct value	arg1, arg2;
	struct string	*tag;

	CHECKAC(1, 2);
	arg1 = EVALARGN(1);
	if (!symbolp(arg1))
		BADARGN(1, "a catch tag symbol");
	tag = gsymbol(arg1.vl_data)->sy_pname;
	if (GETACOUNT() > 1)
		arg2 = EVALARGN(2);
	else
		arg2 = v_nil;

	int_throw(tag, arg2);
	/* NOTREACHED */
}

int_throw(tag, rval)
	struct string	*tag;
	struct value	rval;
{
	struct value	stack_goto();
	struct catch	*cptr;

	/* find the tag they want to throw to */
	if ((cptr = get_catch(tag)) == NULL)
		error("No such tag %Y to throw to!", tag);
	cptr->ca_value = rval;

	debug(DTHROW, "Throwing to tag %Y at stack frame %d.",
	      cptr->ca_tag, cptr->ca_frame);

	stack_goto(cptr->ca_frame);
	longjmp(cptr->ca_jmpbuf, LONGJMPARG);
	panic("int_throw: longjmp failed on catch %Y!", cptr->ca_tag);
}

struct value
throw_value(tag)
	struct string	*tag;
{
	struct catch	*next;

	debug(DTHROW, "Searching for value to return for %Y throw...", tag);
	for (next = catch_list; next != NULL; next = next->ca_next)
		if (sequal(next->ca_tag, tag)) {
			debug(DTHROW, "Returning value %v for %Y throw.",
			      next->ca_value, next->ca_tag);
			return (next->ca_value);
		}
	ierror("throw_value: No catch for tag %Y!", tag);
	/* NOTREACHED */
}

struct catch *
get_catch(tag)
	struct string	*tag;
{
	struct catch	*cptr;

	for (cptr = catch_list; cptr != NULL; cptr = cptr->ca_next)
		if (sequal(cptr->ca_tag, tag))
			return (cptr);
	return (NULL);
}

clean_catches(offset)
{
}
