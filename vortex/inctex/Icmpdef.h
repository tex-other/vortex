/* 
 * Copyright (c) 1986-1992 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* Copyright (c) 1992 Regents of the University of California
 * All rights reserved.
 *
 *  Icmpdef.h
 */

#undef  CHECK
#undef  COMPARE
#undef  CHECKMARK1
#undef  CHECKMARK2

#define CHECK(A,B,S,E) {READ1(A, S); READ2(B, S); \
	if (A != B) {if(TALK) print_nl(E); ok=FALSE;}}
#define COMPARE(A,B,S,E) {READ1(A, S); READ2(B, S); \
	{if (A != B) {if(TALK) print_nl(E); ok=same=FALSE;}}}
#define CHECKMARK1(S) {READ1(iaddr, sizeof(intptr));if( iaddr!=END_MARKER ) \
		{ok = FALSE;if(TALK) {print_nl("Checkpoint #1 damaged "); \
				print(S); print(", no marker\n");}}}
#define CHECKMARK2(S) {READ2(iaddr, sizeof(intptr));if( iaddr!=END_MARKER ) \
		{ok = FALSE;if(TALK) {print_nl("Checkpoint #2 damaged "); \
				print(S); print(", no marker\n");}}}
