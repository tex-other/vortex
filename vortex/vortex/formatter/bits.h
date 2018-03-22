/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifndef BITS_
#define BITS_

/*
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents.
 *
 *  This file is part of the VorTeX proof editor 
 *  written by Jeffrey W. McCarrell for the VorTeX project
 *  under the direction of Prof. Michael A. Harrison
 *  of the University of California at Berkeley.
 *
 *  Copyright (c) 1987 by Jeffrey W. McCarrell
 *  and The Regents of the University of California.
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  jwm@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 */
/*
 * this file contains some commom bit operations on integers, like setting
 * a mask of bits on, turning a mask of bits off, and assigning a set of
 * bits specified by a mask to the dest without disturbing the other bits.
 * These routines have only been tested with integer data types.
 */

/*
 * operations to set and clear individual bits, or sets of bits.
 */
#define SET_BITS(val, mask)	(val) |= (mask)
#define CLEAR_BITS(val, mask)	(val) &= (~(mask))

/*
 * copy the bits in mask that are set from src to dst.
 */
#define COPY_BITS(src, dst, mask)	\
	(dst) &= ~(mask);		\
	(dst) |= (src) & (mask);

#endif
