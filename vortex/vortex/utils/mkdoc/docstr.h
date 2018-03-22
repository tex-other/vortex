/* 
 * Copyright (c) 1986 The Regents of the University of California.
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
 *  This is the struct which holds information used by the
 *  info (or help) facilities.  This is general purpose,
 *  help info in accessed by name, which returns a structure
 *  containing the fields of information as given by the
 *  author in source comments.  This programmer documentation
 *  on the builtin symbols is extracted from the source files
 *  (both C and vlisp) and built into a database by the mkdoc
 *  program.
 */
struct docstr {
	char		*ds_name;	/* name accessed by */
	char		*ds_call;	/* calling sequence */
	char		*ds_retu;	/* return value type */
	char		*ds_desc;	/* general description */
	char		*ds_side;	/* side effects */
	char		*ds_errs;	/* possible errors */
	char		*ds_xref[10];	/* cross references */
	char		*ds_seea[10];	/* see also references */
	struct docstr	*ds_next;	/* next in list */
};
#define MAXXREF		10		/* size of ds_xref array */
#define MAXSEEA		10		/* size of ds_seea array */
