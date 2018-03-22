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
 *  This code is part of the VorTeX project.  This file was
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


/*  The data structure to hold context for a window.  Note that vpe
 *  needs to deal with multiple documents and multiple windows, so I
 *  need to store lots of information about each window.  These are
 *  allocated on a per window basis, and it is ceriainly reasonable 
 *  that they share pointers, i.e. 2 windows showing to the same document 
 *  will point th the same Document structure.
 *
 *  Note that the global magnification is kept on a per-window basis.
 */

struct Context {
    int             c_docnumber;
    struct Page     *c_page;
};

typedef struct nbox {
    _Nbox       nb_box;
    struct nbox *nb_next;
    struct nbox *nb_children;
    int         nb_nchildren;
} Nbox;

typedef struct tbox {
    int         tb_flags;
    _Tbox       tb_box;
    int         tb_xpos;
    int         tb_ypos;
    struct tbox *tb_next;
} Tbox;

typedef struct rbox {
    int         rb_flags;
    _Nbox       rb_box;
    int         rb_xpos;
    int         rb_ypos;
    struct nbox *rb_next;
} Rbox;

/*  Each document has a foreground and background color associated with it.
 *  Thus when that document is being displayed in a Window, that window should
 *  change to use these colors for easy document recognition.
 */
struct Document {
    Pixmap          d_foreground;
    Pixmap          d_background;
    struct Page     *d_pages;
    struct tcp_font *d_fonts;
};

/*  For each page we store the width and height in pixels, and the bitmap 
 *  (note that with X bitmaps, the width is rounded to the nearest short).
 */
struct Page {
    int         p_number;
    int         p_width;
    int         p_height;
    int         p_globalmag;
    int         p_pageno;
    u_long      p_count[10];
    u_long      p_nsbox;
    u_long      p_ncbox;
    Nbox        *p_top;
    short       *p_bits;
    struct Page *p_next;
};

