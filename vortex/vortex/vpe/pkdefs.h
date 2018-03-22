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

/*  This file contains the definitions of the pk op codes.
 * 
 *  This code is included in the DVI previewer which runs under X (dvi2x)
 *  written by Steven Procter for the VorTeX project under the
 *  direction of Prof. Michael A. Harrison of the University
 *  of California at Berkeley.  This code was written by Steven
 *  Procter.
 *
 *  Copyright (c) 1987 Steven James Procter
 *  University of California, Berkeley
 *  procter@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  Steven Procter and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 */

/*  ID byte value at the beginning of PK files  */
#define PK_ID       89

/*  PK op codes  */
#define PK_XXX1     240
#define PK_XXX2     241
#define PK_XXX3     242
#define PK_XXX4     243
#define PK_YYY      244
#define PK_POST     245
#define PK_NOP      246
#define PK_PRE      247

#define PK_REPEAT   0xe /*  Repeat last row - repeat count in next nibble  */
#define PK_AGAIN    0xf /*  Repeat only once  */
#define PK_LARGE    0x0 /*  Long run vaule coming up  */
