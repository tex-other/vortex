/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
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
 */

/*
 * This file defines some of the values we expect to see in a PK format
 * file.  See Tugboat, volume 6, number 3 pgs 115-120 and Tugboat vol.  7
 * number 3 pg 140-144 for the definition of PK files.
 */

/* the ``magic number'' of PK files. */
#define PK_ID	(89)

/* Op code bytes */
#define PK_XXX1		(240)
#define PK_XXX2		(241)
#define PK_XXX3		(242)
#define PK_XXX4		(243)

#define PK_YYY		(244)

#define PK_POST		(245)
#define	PK_NOOP		(246)
#define PK_PRE		(247)

/*
 * the type of the character definition is in the low 3 bits of the flag
 * byte.
 */
#define PK_MODE_BITS		(7)
#define PK_PL_BITS		(3)
#define PK_BLACK_BIT		(1 << 3)
#define	PK_LONG_FORM		(7)
#define PK_EXTENDED_FORM	(6)
#define PK_SHORT_FORM		(3)
