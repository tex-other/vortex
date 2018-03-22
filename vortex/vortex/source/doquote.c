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
 *  RCS Info: $Header: doquote.c,v 0.1 87/05/01 11:59:36 john Locked $
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
 *  doquote.c - vLisp quote (and similar) functions
 */
static char _ID[] = "@(#)doquote.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"

/*
 *  DOCUMENTATION
 *
 *  Name: quote
 *  Call: (quote any)
 *  Retu: any
 *  Desc: This function protects its argument from evaluation;
 *	that is, it just returns its argument unevaluated.  Thus,
 *	one can protect something that would normally be evaluated
 *	from that evaluation by giving it as the argument to \sym{quote}.
 *	Thus, \lit{(eval (quote a))} evaluates to just \lit{a}.
 *
 *	The more usual method of quoting expressions is to just
 *	preceed that expression with the single quote character, \lit{'},
 *	which, when read, reads the next argument and builds a list
 *	containg the two elements \lit{quote} and the expression read.
 *	Thus, the input \lit{'(a b c)} is read as \lit{(quote (a b c))}
 *	and \lit{'(a 'b c)} is read as \lit{(quote (a (quote b) c))}.
 *  Side: Since the read macro \lit{'} expands to a call to the
 *	function \sym{quote} with its argument the expression given,
 *	redefining \sym{quote} also changes what \lit{'} means.  This
 *	is not advised for this reason.
 *  Xref: '
 *  SeeA: quote! read eval
 */

DEFUN(doquote, "quote", FLAG_NONE, NULL)
{
	CHECKAC(1, 1);

	return (GETARGN(1));
}

/*
 *  DOCUMENTATION
 *
 *  Name: backquote
 *  Call: (backquote any)
 *  Retu: list
 *  Desc: This function selectively evaluates its argument according
 *	to imbedded characters it finds.  In general, the argument
 *	(and its elements recursively) is not evaluated, except as
 *	described below.  The possibly partially evaluated argument
 *	is returned.  This function is almost an analogue to \sym{quote},
 *	except that it allows specific portions of its argument
 *	to be evaluated.
 *
 *	At any level in the arguments list a comma may appear before
 *	a symbol or list.  In this case, the symbol or list is evaluated
 *	and the result inserted into the resulting list in place of
 *	the element that was evaluated.  Otherwise, the two character
 *	sequence comma at-sign \lit{,@} may preceed a symbol or list,
 *	in this case that next element is evaluated and the result
 *	is ``spliced'' into the list, raplacing the element that was
 *	evaluated.  Splicing here means that if the result is a list,
 *	it is inserted with the outer level of parenthesis removed
 *	(inserting the elements of the list rather than the list itself).
 *
 *	This can also be invoked with the backquote character, \lit{`},
 *	which, when read, expands to a call to \sym{backquote} with the
 *	next expression to be read as the argument.  Thus when
 *	\lit{`(x ,x ,@x)} is seen by the reader (assuming that \sym{x}
 *	has been bound to \lit{(x x)}), it is transformed into
 *	\lit{(backquote (x ,x ,@x))} and when evaluated, to
 *	\lit{(x (x x) x x)}.
 *  Side:
 *  Xref: ` , ,@
 *  SeeA: quote read eval
 */

DEFUN(dobackquote, "backquote", FLAG_NONE, NULL)
{
	return (v_nil);
}
