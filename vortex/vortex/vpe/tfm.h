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

/*  Defines and structures for the tfm files.
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
 
/*  The directory where the tfm files reside  */
#ifndef TFMDIR
#define TFMDIR      "/usr/local/fonts/tfm"
#endif TFMDIR

/*  Read an int and convert to host byte ordering  */

#define readnetint(fd, a) \
			read(fd, &(a), sizeof (int));\
			(a) = ntohl((a))

struct header {
    int     h_checksum;
    int     h_designsize;
    int     h_codescheme[10];
    int     h_pfi[5];
    int     h_random;
};

struct finfo {
    int   f_WidthIndex:8,
          f_HeightIndex:4,
	  f_DepthIndex:4,
	  f_CharIcIndex:6,
	  f_TagField:2,
	  f_Remainder:8;
};

struct tfmwidths {
    int     tfm_beginchar;
    int     tfm_endchar;
    long    *tfm_widths;
};

struct TfmCache {
    char                *tc_fontname;
    struct tfmwidths    *tc_tfm;
};
