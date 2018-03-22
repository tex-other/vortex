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
 *  RCS Info: $Header: bind.c,v 0.1 87/05/01 11:24:34 john Locked $
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
 *  bind.c - low-level editor key binding
 */
static char _ID[] = "@(#)bind.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "parser.h"

/*
 *  Because of the way we need to check for abort character, we need
 *  to keep a global variable containing the character code for the
 *  abort key.  We should set this value in case the user trys to
 *  bind another key to the abort-function.
 */
int	abort_char = CONTROL(g);

#define KEYMAPSIZE	256

/*
 *  DOCUMENTATION
 *
 *  Name: set-binding
 *  Call: (set-binding 'keys 'value 'keymap)
 *  Retu: value
 *  Desc: This function makes the value bound in the given keymap
 *	by the key sequence specified in keys.  The first argument
 *	must evaluate to a string, each character of which is a
 *	key stroke (all keys except that last are prefixes), the
 *	second argument may evaluate to any type (see below) and the
 *	third argument must evaluate to a keymap, an array which
 *	was created by \sym{make-keymap}.
 *
 *	Generally, if the key is to be unbound, it should be set to
 *	nil; if the key is to specify a prefix, it should be set to
 *	another keymap array and if the key is to specify a bound
 *	value, it should be set to a symbol, which represents the
 *	name of the function to call when keys is typed to the editor.
 *
 *	The key may also be set to a function value.  This is the
 *	most efficient since the symbol doesn't need to be evaluated
 *	to yield the function, but if the symbol is redefined, the
 *	binding will still point to the old function.
 *  Xref: define-key
 *  SeeA: get-binding global-map local-map
 */

DEFUN(dosetbinding, "set-binding", FLAG_NONE, NULL)
{
	struct value	arg, val;
	struct string	*keys, fake;
	unsigned char	fbuf;
	int		code;

	CHECKAC(3, 3);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_STRING:
		keys = gstring(arg.vl_data);
		break;
	case LISP_FIXNUM:
		code = gfixnum(arg.vl_data);
		if (code < 0 || code >= KEYMAPSIZE)
			error("Bad key code value %d to bind to.", code);
		fbuf = code & 0377;
		fake.st_length = 1;
		fake.st_buffer = &fbuf;
		keys = &fake;
		break;
	default:
		BADARGN(1, "a key string");
	}
	val = EVALARGN(2);
	arg = EVALARGN(3);

	set_binding(keys, val, arg);
	return (val);
}

set_binding(keys, val, keymap)
	struct string	*keys;
	struct value	val, keymap;
{
	extern char	*nthname();
	register int	code;
	struct string	fake;
	struct array	*arr;
	struct value	pair, key, what;
	register int	n, count;

	if (keys->st_length < 1)
		error("Null key string to set binding of!");
	code = *keys->st_buffer;

	if (arrayp(keymap)) {
		if (length(keymap) < code + 1)
			error("The keymap array isn't long enough!");
	
		if (keys->st_length == 1) {
			/* bind new value to keymap here */
			store(keymap, code, val);
			return (0);
		} else {
			what = aindex(keymap, code);
			if (!arrayp(what) && !dtprp(what)) {
				arr = save_array(KEYMAPSIZE);
				what.vl_type = LISP_ARRAY;
				sarray(what.vl_data, arr);
				store(keymap, code, what);
			}
			fake.st_length = keys->st_length - 1;
			fake.st_buffer = keys->st_buffer + 1;
			return set_binding(&fake, val, what);
		}
	} else if (dtprp(keymap)) {
		count = length(keymap);
		for (n = 1; n < count; n++) {
			pair = nth(n, keymap);
			if (!dtprp(pair)) {
				error(
		    "The %s element of sparse keymap isn't a dotted pair!",
				      nthname(n - 1));
			}
			key = car(pair);
			what = cdr(pair);
			if (!fixnump(key)) {
				error(
			    "The %s sparse keymap entry is garbled.",
				      nthname(n - 1));
			}
			if (gfixnum(key.vl_data) == code)
				break;
		}
		if (n < count) {
			if (keys->st_length == 1) {
				/* assign this value */
				rplacd(pair, val);
				return (0);
			} else {
				/* we assume it's a keymap */
				fake.st_length = keys->st_length - 1;
				fake.st_buffer = keys->st_buffer + 1;
				return set_binding(&fake, val, what);
			}
		} else {
			if (keys->st_length == 1) {
				/* add this entry to sparse keymap */
				key.vl_type = LISP_FIXNUM;
				sfixnum(key.vl_data, code);
				pair = cons(key, val);
				append(keymap, pair);
				return (0);
			} else {
				/* no entry exists, it can't be a keymap */
				error(
			"Sparse keymap entry for sub-keymap doesn't exist!");
			}
		}
	} else {
		/* invalid keymap value */
		error("That's not a keymap, I can't set the binding.");
	}
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: get-binding
 *  Call: (get-binding 'keys 'keymap)
 *  Retu: any
 *  Desc: This function finds the value bound in the given keymap
 *	by the key sequence specified in keys.  The first argument
 *	must evaluate to a string, each character of which is a
 *	key stroke (all keys except that last are prefixes).  The
 *	second argument must evaluate to a keymap, an array which
 *	was created by \sym{make-keymap}.
 *
 *	Generally, if the key is unbound, this function will return
 *	nil; if the key specifies a prefix, an array will be returned
 *	and if the key specifies a bound value, a symbol will be
 *	returned, which represents the name of the function to call
 *	when keys is typed to the editor.
 *  SeeA: set-binding global-map local-map
 */

DEFUN(dogetbinding, "get-binding", FLAG_NONE, NULL)
{
	struct value	get_binding();
	struct value	arg;
	struct string	*keys, fake;
	unsigned char	fbuf;
	int		code;

	CHECKAC(2, 2);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_STRING:
		keys = gstring(arg.vl_data);
		break;
	case LISP_FIXNUM:
		code = gfixnum(arg.vl_data);
		if (code < 0 || code >= KEYMAPSIZE)
			error("Bad key code value %d to bind to.", code);
		fbuf = code & 0377;
		fake.st_length = 1;
		fake.st_buffer = &fbuf;
		keys = &fake;
		break;
	default:
		BADARGN(1, "a key string");
	}
	arg = EVALARGN(2);

	return get_binding(keys, arg);
}

struct value
get_binding(keys, keymap)
	struct string	*keys;
	struct value	keymap;
{
	extern char	*nthname();
	register int	code;
	struct string	fake;
	struct value	pair, key, what;
	register int	n, count;

	if (keys->st_length < 1)
		error("Null key string to get binding of!");
	code = *keys->st_buffer;

	if (nullp(keymap))
		return (v_nil);
	if (arrayp(keymap)) {
		if (length(keymap) < code + 1)
			error("The keymap array isn't long enough!");
		if (keys->st_length == 1) {
			/* bind value to keymap here */
			return aindex(keymap, code);
		} else {
			what = aindex(keymap, code);
			if (!arrayp(what))
				return (v_nil);
			fake.st_length = keys->st_length - 1;
			fake.st_buffer = keys->st_buffer + 1;
			return get_binding(&fake, what);
		}
	} else if (dtprp(keymap)) {
		count = length(keymap);
		for (n = 1; n < count; n++) {
			pair = nth(n, keymap);
			if (!dtprp(pair)) {
				error(
		    "The %s element of sparse keymap isn't a dotted pair!",
				      nthname(n - 1));
			}
			key = car(pair);
			what = cdr(pair);
			if (!fixnump(key)) {
				error(
			    "The %s element of local keymap isn't correct!",
				      nthname(n - 1));
			}
			if (*keys->st_buffer == gfixnum(key.vl_data)) {
				if (keys->st_length == 1) {
					/* this is it, folks */
					return (what);
				} else {
					/* we assume it's a keymap */
					fake.st_length = keys->st_length - 1;
					fake.st_buffer = keys->st_buffer + 1;
					return get_binding(&fake, what);
				}
			}
		}
		/* code not found in sparse keymap */
		return (v_nil);
	} else {
		/* illegal value for a keymap */
		error("That's not a keymap, I can't get bindings.");
	}
	/* NOTREACHED */
}

/*
 *  DOCUMENTATION
 *
 *  Name: global-map
 *  Call: (global-map)
 *  Retu: array
 *  Desc: This function returns the value being used for the global
 *	key map.  This is generally an array of 256 elements, which
 *	will contain the bindings for those keys.  If any of the
 *	array elements are arrays, that element is a prefix and
 *	arrays will be traversed recursively to expand a key binding.
 *
 *	Besides another array, an element in a keymap array may
 *	contain nil, which means that key is unbound, or a symbol
 *	which is the name of a function to call when that key is
 *	typed.
 *
 *	Bindings (which all originate from a keymap) at the global
 *	level may be overridden by key bindings local to a buffer.
 *	The function \sym{local-map} retrieves the keymap which is
 *	local to each buffer for a specific buffer.
 *  SeeA: use-global-map global-set-key local-map
 */
struct value	global_keymap;

DEFUN(doglobalmap, "global-map", FLAG_NONE, NULL)
{
	CHECKAC(0, 0);

	/* make sure we have a reasonable value */
	if (!dtprp(global_keymap) && !arrayp(global_keymap))
		global_keymap = v_nil;

	return (global_keymap);
}

/*
 *  DOCUMENTATION
 *
 *  Name: local-map
 *  Call: (local-map [ 'buffer ])
 *  Retu: array
 *  Desc: This function returns the value being used for the local
 *	keymap (local to a particular buffer).  This is generally an
 *	array of 256 elements, which will contain the bindings for
 *	those keys, but may also be nil, if no keys have been bound
 *	local to this buffer (see \sym{local-set-key)).  If the optional
 *	argument is present, it specifies the buffer whose keymap
 *	is of interest, if no argument is present, the keymap of
 *	the current buffer is returned.
 *
 *	If any of the array elements are arrays, that element is a prefix
 *	and arrays will be traversed recursively to expand a key binding.
 *	Besides another array, an element in a keymap array may
 *	contain nil, which means that key is unbound, or a symbol
 *	which is the name of a function to call when that key is
 *	typed.
 *
 *	Bindings at the local level will override global key bindings
 *	(in the global keymap, accessed with \sym{global-map}).
 *  SeeA: use-local-map local-set-key global-map
 */

DEFUN(dolocalmap, "local-map", FLAG_NONE, NULL)
{
	struct value	arg;
	struct buffer	*bufp;

	CHECKAC(0, 1);
	if (GETACOUNT() == 0) {
		/* just use current buffer */
		bufp = current_buffer;
	} else {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}

	return (bufp->bu_keymap);
}

/*
 *  DOCUMENTATION
 *
 *  Name: use-global-map
 *  Call: (use-global-map 'keymap)
 *  Retu: keymap
 *  Desc: This function sets the value being used for the global
 *	key map.  This is generally an array of 256 elements, which
 *	will contain the bindings for those keys.  If any of the
 *	array elements are arrays, that element is a prefix and
 *	arrays will be traversed recursively to expand a key binding.
 *
 *	Besides another array, an element in a keymap array may
 *	contain nil, which means that key is unbound, or a symbol
 *	which is the name of a function to call when that key is
 *	typed.
 *
 *	Bindings (which all originate from a keymap) at the global
 *	level may be overridden by key bindings local to a buffer.
 *	The function \sym{use-global-map} sets the keymap which is
 *	local to each buffer for a specific buffer.
 *  SeeA: global-map global-set-key use-local-map
 */

DEFUN(douseglobal, "use-global-map", FLAG_NONE, NULL)
{
	struct value	arg;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	if (!arrayp(arg) && !listp(arg))
		BADARGN(1, "a keymap");

	return (global_keymap = arg);
}

/*
 *  DOCUMENTATION
 *
 *  Name: use-local-map
 *  Call: (use-local-map [ 'buffer ] 'keymap)
 *  Retu: keymap
 *  Desc: This function sets the value to be used for the local
 *	keymap (local to a particular buffer).  This is generally an
 *	array of 256 elements, which will contain the bindings for
 *	those keys, but may also be nil, if no keys have been bound
 *	local to this buffer (see \sym{local-set-key)).  If the optional
 *	argument is present, it specifies the buffer whose keymap
 *	is to be set, if no argument is present, the keymap of the
 *	current buffer is set.
 *
 *	If one of the keymap elements is an array, that element is a
 *	prefix (another keymap) and keymaps will be traversed recursively
 *	to expand a key binding.  Besides another array, an element in
 *	a keymap array may contain nil, which means that key is unbound,
 *	or a symbol which is the name of a function to call when that key
 *	is typed.
 *  SeeA: local-map local-set-key global-map
 */

DEFUN(douselocal, "use-local-map", FLAG_NONE, NULL)
{
	struct value	arg;
	struct buffer	*bufp;

	CHECKAC(1, 2);
	if (GETACOUNT() == 1) {
		arg = EVALARGN(1);
		if (!listp(arg) && !arrayp(arg))
			BADARGN(1, "a keymap");
		/* just use current buffer */
		bufp = current_buffer;
	} else {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
		arg = EVALARGN(2);
		if (!listp(arg) && !arrayp(arg))
			BADARGN(2, "a keymap array");
	}

	return (bufp->bu_keymap = arg);
}

/*
 *  DOCUMENTATION
 *
 *  Name: make-keymap
 *  Call: (make-keymap)
 *  Retu: array
 *  Desc: This function returns an array suitable for binding
 *	all keyboard and mouse keys.  This is the type of the
 *	global keymaps.
 *
 *	To find the binding for a given symbol, one indexes
 *	the array by the input code.
 *  Side: A large array is created and all elements initialized
 *	to nil, which uses a large amount of space.  For keymaps
 *	with fewer bound keys (and slightly slower access time)
 *	use \sym{make-sparse-keymap}.
 *  SeeA: make-sparse-keymap global-map local-map get-binding
 */

DEFUN(domakekeymap, "make-keymap", FLAG_NONE, NULL)
{
	struct value	map;
	struct array	*arr;

	arr = save_array(MOUSE_MAXCODE + 1);
	map.vl_type = LISP_ARRAY;
	sarray(map.vl_data, arr);
	return (map);
}

/*
 *  DOCUMENTATION
 *
 *  Name: make-sparse-keymap
 *  Call: (make-sparse-keymap)
 *  Retu: list
 *  Desc: This function returns a list suitable for binding
 *	keyboard and mouse keys.  This is the type of most local
 *	keymaps, since it is only as large as necessary to hold
 *	the key bindings actually set.
 *
 *	The value returned is a list, containing the symbol
 *	\lit{%keymap} to mark the type.  As bindings are created,
 *	dotted pairs, each with the key and binding, are appended
 *	to this list.
 *  Side: Access to the elements of the keymap is slower, since
 *	a list must be traversed.  For more heavily used keymaps,
 *	use \sym{make-keymap}.
 *  SeeA: make-keymap global-map local-map get-binding
 */
MKSTRING(KEYMAP, "%keymap");

DEFUN(domakesparse, "make-sparse-keymap", FLAG_NONE, NULL)
{
	struct value	elt, map;
	struct symbol	*sym;

	CHECKAC(0, 0);

	sym = save_symbol(KEYMAP);
	elt.vl_type = LISP_SYMBOL;
	ssymbol(elt.vl_data, sym);
	map = cons(elt, v_nil);
	return (map);
}

/*
 *  DOCUMENTATION
 *
 *  Name: dump-bindings
 *  Call: (dump-bindings [ 'buffer ])
 *  Retu: t
 *  Desc: This function dumps a human-readable list of key bindings
 *	from the current buffer (buffer local and global bindings)
 *	into the given buffer or \lit{*bindings*} if none is given.
 *  SeeA: set-binding global-map local-map
 */
MKSTRING(BINDINGS, "*bindings*");

DEFUN(dodumpbindings, "dump-bindings", FLAG_NONE, NULL)
{
	struct value	arg;
	struct buffer	*curb = current_buffer;
	struct buffer	*bufp;
	struct string	*name;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		name = gstring(arg.vl_data);
	} else {
		/* use the default name */
		name = BINDINGS;
	}

	/* make the buffer */
	bufp = buffer_create(name, BUFF_SOURCE, BUFF_KILLOK);
	dumpcodes(NULL, global_keymap, curb->bu_keymap, bufp);

	return (v_t);
}

static
dumpcodes(prefix, first, second, bufp)
	struct string	*prefix;
	struct value	first, second;
	struct buffer	*bufp;
{
	struct value	bound;
	unsigned char	keybuf[245], *next;
	struct string	keys;
	register int	code;
	int		prev = -1, prevcode;

	if (prefix == NULL || prefix->st_length < 1) {
		next = keybuf;
		keys.st_length = 1;
	} else {
		bcopy(prefix->st_buffer, keybuf);
		keys.st_length = prefix->st_length + 1;
		next = keybuf + prefix->st_length;
	}
	keys.st_buffer = keybuf;

	for (code = 0; code < MOUSE_MAXCODE; code++) {
		*next = code;
		bound = get_binding(&keys, first);
		if (nullp(bound) && !invalidp(second))
			bound = get_binding(&keys, second);
		/* do the right thing */
		if (listp(bound) || arrayp(bound))
			dumpcodes(&keys, first, second, bufp);
		else if (truep(bound))
			bappend(bufp, "%K  %v\n", &keys, bound);
	}
}
