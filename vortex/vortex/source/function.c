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
 *  RCS Info: $Header: function.c,v 0.1 87/05/01 12:15:42 john Locked $
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
 *  function.c - internal vLisp function management routines
 */
static char _ID[] = "@(#)function.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "function.h"

/*
 *  Call a function from vlisp.  We pass call_function the original
 *  (pre-evaluation) value as val0, the evaluated function
 *  as arg0, and the nlambda form list containing the arguments.
 *  This function returns the return value of the vlisp function
 *  we're trying to call, or NULL in case of error.  If the requested
 *  function is a C builtin, we call the C function to do all the
 *  work, if it's a vlisp function, we call the appropriate
 *  C code for that function discipline.
 *
 *  Arguments, return values and local bindings are kept in the
 *  current call frame, pointed to by stack_ptr.  The frame is
 *  actually a stack of (struct stack *), which is set up initially
 *  with a size of STACK_INITIAL and is grown in STACK_GROWTH size
 *  increments up to STACK_MAXDEPTH.
 */
struct stack	*stack_ptr = NULL;	/* the current call frame */
struct stack	*stack_stack = NULL;	/* the bottom of the stack */
struct stack	*stack_top = NULL;	/* the top of the stack */
static int	stack_depth = 0;	/* the depth of the call frame */

struct value
call_function(val0, arg0, args)
	struct value	val0, arg0, args;
{
	extern struct value	stack_pop();
	extern int		Xconnected;
	struct function		*func;

	if (eq(val0, NOVALUE) || eq(arg0, NOVALUE) || eq(args, NOVALUE)) {
		ierror("call_function(0x%x, 0x%x, 0x%x): Bad arguments!",
		    val0, arg0, args);
	}
	if (!funcp(arg0) || (func = gfunct(arg0.vl_data)) == NULL)
		error("Non-function value %v as function!", arg0);

	debug(DCALL, "Calling function %v with %v (func value %v).",
	      val0, args, arg0);

	/* set up this next call frame */
	stack_push(val0, arg0, args);

	/* check different preconditions */
	if ((func->fn_flags & FUNC_VISUAL) != 0 && !havedisplay)
		error("That function only works when editing visually.");

	if ((func->fn_flags & FUNC_BUILTIN) != 0) {
		ASSERT(func->fn_funct != NULL);
		/* now call the internal function to do the work */
		stack_ptr->st_return = (*func->fn_funct)();
	} else {
		/* evaluate it depending on the discipline */
		switch (func->fn_disc) {
		case DISC_LAMBDA:
			stack_ptr->st_return = call_lambda();
			break;
		case DISC_NLAMBDA:
			stack_ptr->st_return = call_nlambda();
			break;
		case DISC_LEXPR:
			stack_ptr->st_return = call_lexpr();
			break;
		case DISC_MACRO:
			stack_ptr->st_return = call_macro();
			break;
		default:
			ierror("Bad function discipline %d in function %v!",
			    func->fn_disc, val0);
			/* NOTREACHED */
		}
	}

	/* pop this call frame and return proper value */
	return stack_pop();
}

/*
 *  These routines manage the call frame stack.  We grow the stack
 *  when necessary, up to a (large) maximum size.  The variables
 *  above (stack_*) keep track of the current situation.  All
 *  modifications to the frame stack itself (the values of any
 *  individual frame data may be modified elsewhere) are done
 *  only through these functions.  These routines obtain new memory
 *  from valloc().
 */

stack_push(val0, fval, args)
	struct value	val0, fval, args;
{
	int		curoff;
	struct stack	*new;

	if (stack_stack == NULL) {
		/* initialize the stack on the first push */
		stack_depth = STACK_INITIAL;
		stack_stack = (struct stack *)
		    valloc(sizeof (struct stack) * stack_depth);
		if (stack_stack == NULL)
			panic("No memory for initial call stack.");
		stack_top = stack_stack + stack_depth;
		/* fake the ``top level'' frame */
		stack_stack->st_value = v_nil;
		stack_stack->st_funct = v_nil;
		stack_stack->st_flags = STACK_ISCATCH;
		stack_stack->st_argc = 0;
		stack_stack->st_argl = v_nil;
		stack_stack->st_return = v_nil;
		/* set pointer at first (not zeroth) frame */
		stack_ptr = stack_stack + 1;
	} else if (stack_ptr + 1 >= stack_top) {
		/* grow the frame stack if possible */
		if (stack_depth >= STACK_MAXDEPTH)
			error("Lisp call stack overflow, infinite recusion?");
		stack_depth += STACK_GROWTH;
		if (stack_depth >= STACK_MAXDEPTH)
			stack_depth = STACK_MAXDEPTH;
		/* save the offset of the current pointer */
		curoff = stack_ptr - stack_stack;
		/* extend the list of frame structs */
		new = (struct stack *)
		    valloc(sizeof (struct stack) * stack_depth);
		bcopy(stack_stack, new, curoff * sizeof (struct stack));
		vfree(stack_stack);
		stack_stack = new;
		/* restore frame pointer and stack top */
		stack_ptr = stack_stack + (curoff + 1);
		stack_top = stack_stack + stack_depth;
	} else {
		/* just advance the frame pointer */
		stack_ptr++;
	}

	/* set up this call frame from values given */
	stack_ptr->st_value = val0;
	stack_ptr->st_funct = fval;
	if (dtprp(args))
		stack_ptr->st_argc = length(args);
	else if (nullp(args))
		stack_ptr->st_argc = 0;
	else
		stack_ptr->st_argc = 1;
	stack_ptr->st_argl = args;
	stack_ptr->st_flags = FLAG_NONE;
	stack_ptr->st_return = NOVALUE;
	stack_ptr->st_unwind = NULL;

	debug(DSTACK, "Stack push %-5d cmd: %v, list %v.",
	      call_depth, stack_ptr->st_value, stack_ptr->st_argl);

	return (0);
}

struct value
stack_pop()
{
	struct value	rval;

	if (stack_ptr <= stack_stack)
		ierror("stack_pop: Already at bottom of call stack!");

	/* check for any catches to this frame */
	if ((stack_ptr->st_flags & STACK_ISCATCH))
		clean_catches(call_depth);

	/* check for an unwind protect here */
	if ((stack_ptr->st_flags & STACK_UNWIND) != 0) {
		/* clear flag in case we have an error here */
		stack_ptr->st_flags &= ~STACK_UNWIND;
		/* call the C function to do wnatever is necessary */
		ASSERT(stack_ptr->st_unwind != NULL);
		rval = (*stack_ptr->st_unwind)();
		/* ignore return value for now */
	}

	/* this performs the pop */
	stack_ptr--;

	debug(DSTACK, "Stack pop  %-5d cmd: %v, list %v.",
	      call_depth, stack_ptr->st_value, stack_ptr->st_argl);

	return ((stack_ptr+1)->st_return);
}

struct value
stack_goto(offset)
{
	register int	level;
	struct value	ret;

	if (offset < 0)
		ierror("stack_goto: Asked to goto frame %d!");
	if (offset > call_depth)
		ierror("stack_goto: Asked to to UP from %d to %d!",
		    call_depth, offset);
	if (stack_ptr == NULL)
		return (v_nil);

	ret = NOVALUE;
	for (level = call_depth; level > offset; level--)
		ret = stack_pop();

	debug(DSTACK, "Stack goto %-5d cmd: %v, list %v.",
	      call_depth, stack_ptr->st_value, stack_ptr->st_argl);

	return (ret);
}

/*
 *  These routines manage the storage of function structs for the
 *  lisp function values.  We maintain a table of functions at
 *  any one time and allocate from that table.  Multiple values
 *  may point to the same function, when garbage collection time
 *  is at hand, things will be resolved.  Save_funct takes a
 *  function struct and sees if there is one like it in the
 *  function table, if so, it returns that one.  If not, it
 *  copies the given function into the table and returns the
 *  ``safe'' storage.  If the argument is NULL, it just returns
 *  safe storage for a new function.
 */
int		aagrow_functs = 0;	/* automatically grow function */
static int	minfree_functs = 0;	/* actual number that must be free */
static int	curfree_functs = 0;	/* actual number that are free */
int		softlim_functs = 0;	/* percentage that must be free */
int		cursize_functs = 0;	/* actual number allocated */
int		overflow_functs = 0;	/* have already used too many */

static struct function	*fun_table = NULL, *fun_tabend;
static int		fun_initsize = 250;

struct function *
save_funct(func)
	struct function	*func;
{
	register struct function	*funp;

	if (fun_table == NULL) {
		/* allocate initial function storage */
		grow_functs(fun_initsize);
	} else if (func != NULL) {
		/* search for an eq function in the table */
		for (funp = fun_table; funp < fun_tabend; funp++)
			if (!funp->fn_free && fequal(funp, func))
				return (funp);
	}

	/* need to make a new function */
	for (funp = fun_table; funp < fun_tabend; funp++)
		if (funp->fn_free)
			break;
	if (funp >= fun_tabend) {
		/* this shouldn't happen */
		ierror("Help!  Ran completely out of functions storage!");
	} else if (!overflow_functs && curfree_functs < minfree_functs) {
		/* garbage collection time ... */
		gcunmark_functs();
		gc_markall();
		gcsweep_functs();

		/* if stil not enough, keep trying */
		if (curfree_functs < minfree_functs) {
			if (aagrow_functs > 0) {
				/* automatically allocate more */
				grow_functs(aagrow_functs);
			} else {
				overflow_functs++;
				cerror(
		    "Too little function storage left (%d used, %d free).",
				    cursize_functs - curfree_functs,
				    curfree_functs);
			}
		}
	}

	if (func == NULL)
		bzero(funp, sizeof (struct function));
	else
		bcopy(func, funp, sizeof (struct function));
	funp->fn_free = FALSE;
	curfree_functs--;
	return (funp);
}

fequal(fone, ftwo)
	struct function	*fone, *ftwo;
{
	register int	i;

	/* if they're eq, they must be equal */
	if (fone == ftwo)
		return (TRUE);

	/* check if they are of the same type */
	if (fone->fn_flags != ftwo->fn_flags ||
	    fone->fn_disc != ftwo->fn_disc)
		return (FALSE);

	/* compare implementation, C or vlisp code */
	if ((fone->fn_flags & FUNC_BUILTIN) != 0) {
		/* builtin functions, compare C routines */
		if (fone->fn_funct != ftwo->fn_funct)
			return (FALSE);
	} else {
		/* compare argument counts */
		if (fone->fn_argc != ftwo->fn_argc)
			return (FALSE);
		/* compare lisp code bodies */
		if (!eq(fone->fn_body, ftwo->fn_body) ||
		    !equal(fone->fn_body, ftwo->fn_body))
			return (FALSE);
		/* compare argument name list */
		for (i = 0; i < fone->fn_argc; i++)
			if (fone->fn_alist[i] != ftwo->fn_alist[i])
				return (FALSE);
	}

	/* compare interactive calling sequences */
	if ((fone->fn_flags & FUNC_INTERACT) != 0) {
		if (fone->fn_iargc != ftwo->fn_iargc)
			return (FALSE);
		if (strncmp(fone->fn_atype, ftwo->fn_atype, MAXIARGS))
			return (FALSE);
		if (bcmp(fone->fn_prompt, ftwo->fn_prompt,
			 fone->fn_argc * sizeof (struct string *)))
			return (FALSE);
	}

	/* passed all the tests */
	return (TRUE);
}

#define FUNCTS_HARDLIM		5

grow_functs(incr)
{
	struct function	*new, *fptr;

	if (incr > 0) {
		new = (struct function *)
		    valloc((cursize_functs + incr) * sizeof (struct function));
		if (new == NULL)
			panic("Not enough memory for function table size %d!",
			    cursize_functs + incr);
		if (fun_table != NULL) {
			bcopy(fun_table, new,
			    cursize_functs * sizeof (struct function));
			vfree(fun_table);
		}
		fptr = new + cursize_functs;
		fun_table = new;
		cursize_functs += incr;
		fun_tabend = fun_table + cursize_functs;
		curfree_functs += incr;
		while (fptr < fun_tabend) {
			fptr->fn_free = TRUE;
			fptr++;
		}

		overflow_functs = 0;
	}

	/* update minimum free amount variable */
	minfree_functs = ROUND((double)cursize_functs *
			       ((double)softlim_functs / 100.0));
	if (minfree_functs > cursize_functs - FUNCTS_HARDLIM)
		minfree_functs = cursize_functs - FUNCTS_HARDLIM;

	return (cursize_functs);
}

gcunmark_functs()
{
	return (0);
}

gcsweep_functs()
{
	return (0);
}
