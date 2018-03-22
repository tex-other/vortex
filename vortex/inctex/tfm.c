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

/* This file is part of IncTeX 1.0
 *
 * Copyright (C) 1992 by Regents of the University of California
 *
 * This file has been modified, with permission from Pat Monardo, for IncTeX
 * Redistribution of this file is permitted through
 * the specifications in the files COPYRIGHT and COPYING.
 *
 * New changes: put new dvi files in 1 single INC/<doc>.dvi file, instead
 * of in per-page files, and put list of new pages in INC/<doc>.newpages
 *
 * font_used[] was taken out of Iglobal.c, since we no longer want it to
 * get restored after checkpoint loads.
 */
/* @(#)tfm.c 2.7 EPA
 *
 * Copyright 1987,1988 Pat J Monardo
 *
 * Redistribution of this file is permitted through
 * the specifications in the file COPYING.
 */

static char copyright_notice[] = "Copyright (c) 1992 Regents of the University of California\nAll rights reserved.";

#include	"tex.h"
#include	"box.h"
#include	"scan.h"
#include	"token.h"
#include	"tokenstack.h"
#include	"file.h"
#include	"tfm.h"

/*
 *  Read and check the font data;
 *  If the TFM file is malformed, goto bad_tfm;
 *  If there is no room for this font, say so and goto done;
 *  otherwise, incr(font_ptr) and goto done
 */

#ifdef INCTEX
#include	"Imain.h"

extern	int	total_pages;	/* added for incremental font info saving */
extern	ptr     fmem_ptr;	/* this needs to be extern */
extern	fnt     font_ptr;	/* this needs to be extern */
#else
ptr		fmem_ptr;
fnt		font_ptr			= FONT_BASE;
#endif INCTEX

mword		font_info[FONT_MEM_SIZE];
qqqq		font_check[FONT_MAX];
scal		font_size[FONT_MAX];
scal		font_dsize[FONT_MAX];
hword		font_params[FONT_MAX];
str		font_name[FONT_MAX];
str		font_area[FONT_MAX];
byte		font_bc[FONT_MAX];
byte		font_ec[FONT_MAX];
ptr		font_glue[FONT_MAX];
bool		font_used[FONT_MAX] = {0};
int		hyphen_char[FONT_MAX];
int		skew_char[FONT_MAX];
ptr		char_base[FONT_MAX];
ptr		width_base[FONT_MAX];
ptr		height_base[FONT_MAX];
ptr		depth_base[FONT_MAX];
ptr		italic_base[FONT_MAX];
ptr		lig_kern_base[FONT_MAX];
ptr		kern_base[FONT_MAX];
ptr		exten_base[FONT_MAX];
ptr		param_base[FONT_MAX];

byte_file	tfm_file;
qqqq  		null_character;

#define start_font_error_message() \
    {print_err("Font "); sprint_cs(u); \
    print_char('='); print_file_name(nom, aire, null_str); \
    if (s >= 0) {print(" at "); print_scaled(s); print("pt");} \
    else if (s != -1000) {print(" scaled "); print_int(-s);}}

#define get_font_byte() \
    (font_byte = getc(tfm_file))

#define read_sixteen(X) \
    {get_font_byte(); X = font_byte; \
    if (X > 127) goto bad_tfm; \
    get_font_byte(); X = (X) * 0400 + font_byte;}

#define store_four_quarters(Q) \
    {a = get_font_byte(); b = get_font_byte(); \
    c = get_font_byte(); d = get_font_byte(); \
    qw.b0 = qi(a); qw.b1 = qi(b); \
    qw.b2 = qi(c); qw.b3 = qi(d); Q = qw;}

#define store_scaled(S) \
    {a = get_font_byte(); b = get_font_byte(); \
    c = get_font_byte(); d = get_font_byte(); \
    sw = ((((d * z) / 0400) + (c * z)) / 0400 + (b * z)) / beta; \
    if (a == 0) S = sw; else if (a == 255) S = sw - alpha; \
    else goto bad_tfm;}

#define check_byte_range(C)     {if (C < bc || C > ec) goto bad_tfm;}
#define ccbwo                   (k + bc - fmem_ptr)
#define adjust(I)               (I[f] = qo(I[f]))

fnt
read_font_info (u, nom, aire, s)
    fnt     u;
    str     nom;
    str     aire;
    scal    s;
{
    byte    a;
    byte    b;
    byte    c;
    byte    d;
    fnt     f;
    fnt     g;
    int     k;
    scal    z;
    hword   bc;
    hword   ec;
    hword   lf;
    hword   lh;
    hword   nd;
    hword   ne;
    hword   nh;
    hword   ni;
    hword   nk;
    hword   nl;
    hword   np;
    hword   nw;
    qqqq    qw;
    scal    sw;
    val     alpha;
    hword   beta;
    bool    file_opened;
    byte    font_byte;
    qqqq    dev_null;
/*
 *  Open tfm_file for input
 */
    g = null_font;
    file_opened = FALSE;
    pack_file_name(nom, aire, str_tfm);
    if ((tfm_file = b_open_in()) == NULL)
        goto bad_tfm;
    file_opened = TRUE;
/*
 *  Read the TFM size fields
 */
    read_sixteen(lf);
    read_sixteen(lh);
    read_sixteen(bc);
    read_sixteen(ec);
    if (bc > ec + 1 || ec > 255) goto bad_tfm;
    read_sixteen(nw);
    read_sixteen(nh);
    read_sixteen(nd);
    read_sixteen(ni);
    read_sixteen(nl);
    read_sixteen(nk);
    read_sixteen(ne);
    read_sixteen(np);
    if (lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np) 
        goto bad_tfm;
/*
 *  Use size fields to allocate font information
 */
    lf = lf - 6 - lh;
    if (np < 7) lf += 7 - np;
    if (font_ptr == FONT_MAX-1 || fmem_ptr + lf >= FONT_MEM_SIZE) {
        start_font_error_message();
        print(" not loaded: No more room.");
        help_font();
        error();
        goto done;
    }

    f = font_ptr + 1;
    char_base[f] = fmem_ptr - bc;
    width_base[f] = char_base[f] + ec + 1;
    height_base[f] = width_base[f] + nw;
    depth_base[f] = height_base[f] + nh;
    italic_base[f] = depth_base[f] + nd;
    lig_kern_base[f] = italic_base[f] + ni;
    kern_base[f] = lig_kern_base[f] + nl;
    exten_base[f] = kern_base[f] + nk;
    param_base[f] = exten_base[f] + ne;
/*
 *  Read the TFM header
 */
    if (lh < 2) goto bad_tfm;
    store_four_quarters(font_check[f]);
    read_sixteen(z);
    get_font_byte();
    z = z * 0400 + font_byte;
    get_font_byte();
    z = z * 020 + font_byte / 020;
    if (z < UNITY) goto bad_tfm;
    while (lh > 2) {
        store_four_quarters(dev_null);
        decr(lh);
    }
    font_dsize[f] = z;
    if (s != -1000) {
        if (s >= 0)
            z = s;
        else z = xn_over_d(z, -s, 1000L);
    }
    font_size[f] = z;
/*
 *  Read  character data
 */
    for (k = fmem_ptr; k < width_base[f]; incr(k)) {
        store_four_quarters(font_info[k].qqqq);
        if (a >= nw || b / 020 >= nh || b % 020 >= nd || c / 4 > ni) 
            goto bad_tfm;
        switch (c % 4)
        {
        case LIG_TAG:
            if (d >= nl) goto bad_tfm;
            break;
        
        case EXT_TAG:
            if (d >= ne) goto bad_tfm;
            break;
        
        case LIST_TAG:
            check_byte_range(d);
            while (d < ccbwo) {
                qw = char_info(f, d);
                if (char_tag(qw) != LIST_TAG)
                    goto not_found;
                d = qo(rem_byte(qw));
            }
            if (d == ccbwo) goto bad_tfm;
            break;

        not_found:
            break;
        }
    }
/*
 *  Read box dimensions
 */
    alpha = 16;
    while (z >= 040000000) {
        z >>= 1;
        alpha <<= 1;
    }
    beta = 256 / alpha;
    alpha *= z;
    for (k = width_base[f]; k < lig_kern_base[f]; incr(k))
        store_scaled(font_info[k].sc);
    if (font_info[width_base[f]].sc != 0) 
        goto bad_tfm;
    if (font_info[height_base[f]].sc != 0) 
        goto bad_tfm;
    if (font_info[depth_base[f]].sc != 0) 
        goto bad_tfm;
    if (font_info[italic_base[f]].sc != 0) 
        goto bad_tfm;
/*
 *  Read ligature/kern programs
 */
    for (k = lig_kern_base[f]; k < kern_base[f]; incr(k)) {
        store_four_quarters(font_info[k].qqqq);
        check_byte_range(b);
        if (c < KERN_FLAG) { 
            check_byte_range(d);
        } else if (d >= nk)
            goto bad_tfm;
    }
    if (nl > 0 && a < STOP_FLAG)
        goto bad_tfm;

    for (k = kern_base[f]; k < exten_base[f]; incr(k))
        store_scaled(font_info[k].sc);
/*
 *  Read extensible character recipes
 */
    for (k = exten_base[f]; k < param_base[f]; incr(k)) {
        store_four_quarters(font_info[k].qqqq);
        if (a != 0)
            check_byte_range(a);
        if (b != 0)
            check_byte_range(b);
        if (c != 0)
            check_byte_range(c);
        check_byte_range(d);
    }
/*
 *  Read font parameters
 */
    for (k = 1; k <= np; incr(k)) {
        if (k == 1) {
            get_font_byte();
            sw = font_byte;
            if (sw > 127)
                sw -= 256;
            get_font_byte();
            sw = sw * 0400 + font_byte;
            get_font_byte();
            sw = sw * 0400 + font_byte;
            get_font_byte();
            font_info[param_base[f]].sc = sw * 020 + font_byte / 020;
        } else {
            store_scaled(font_info[param_base[f] + k - 1].sc);
        }
    }
    if (feof(tfm_file))
        goto bad_tfm;

    for (k = np + 1; k <= 7; incr(k))
        font_info[param_base[f] + k - 1].sc = 0;
/*
 *  Make final adjustments and done
 */
    font_params[f] = (np >= 7) ? np : 7;
    hyphen_char[f] = default_hyphen_char;
    skew_char[f] = default_skew_char;
    font_name[f] = nom;
    font_area[f] = aire;
    font_bc[f] = bc;
    font_ec[f] = ec;
    font_glue[f] = NULL;
    adjust(char_base);
    adjust(width_base);
    adjust(lig_kern_base);
    adjust(kern_base);
    adjust(exten_base);
    decr(param_base[f]);
    fmem_ptr += lf;
    font_ptr = f;
    g = f;
    goto done;

bad_tfm:
    start_font_error_message();
    if (file_opened)
        print(" not loadable: Bad metric (TFM) file");
    else print(" not loadable: Metric (TFM) file not found");
    help_tfm();
    error();
    return null_font;

done:
    b_close(tfm_file);
    return g;
}

scan_font_ident ()
{   
    fnt     f;
    int     m;

    get_nbx_token();
    if (cur_cmd == DEF_FONT) {
        f = cur_font;
    } else if (cur_cmd == SET_FONT) {
        f = cur_chr;
    } else if (cur_cmd == DEF_FAMILY) {
        m = cur_chr;
        scan_four_bit_int();
        f = equiv(m + cur_val);
    } else {
        print_err("Missing font identifier");
        help_font_cs();
        back_error();
        f = null_font;
    }
    cur_val = f;
}

find_font_dimen (writing)
    bool    writing;
{
    fnt     f;
    int     n;

    scan_int();
    n = cur_val;
    scan_font_ident();
    f = cur_val;
    if (n <= 0) {
        cur_val = fmem_ptr;
    } else {
        if (writing &&
            n <= SPACE_SHRINK_CODE &&
            n >= SPACE_CODE &&
            font_glue[f] != NULL) {
            delete_glue_ref(font_glue[f]);
#ifdef INCTEX
            before_font(&font_glue[f]);
            font_glue[f] = NULL;
            after_font(&font_glue[f]);
#else
            font_glue[f] = NULL;
#endif INCTEX
        }
        if (n > font_params[f]) {
            if (f < font_ptr) {
                cur_val = fmem_ptr;
            } else {
                do {
                    if (fmem_ptr == FONT_MEM_SIZE)
                        overflow("font_memory", FONT_MEM_SIZE);
                    font_info[fmem_ptr].sc = 0;
                    incr(fmem_ptr);
#ifdef INCTEX
/* Don't have to log font_info change above, because we notice it when
   we see fmem_ptr increase and save font_info then. DLP  */
		    before_font(&font_params[f]);
#endif INCTEX
                    incr(font_params[f]);
#ifdef INCTEX
		    after_font(&font_params[f]);
#endif INCTEX
                } while (n != font_params[f]);
                cur_val = fmem_ptr - 1;
            }
        } else {
            cur_val = n + param_base[f];
        }
    }
    if (cur_val == fmem_ptr) {
        print_nl("! Font ");
        print_esc("");
        print_str(font_id_text(f));
        print(" has only ");
        print_int(font_params[f]);
        print(" fontdimen parameters");
        help_font_param();
        error();
    }
#ifdef INCTEX
    /* can't use before_font() and after_font() here - don't know when it'll
       be changed. */
    if (writing && incremental)
	note_font(&font_info[(int) cur_val]);	/* log change. DLP */
#endif INCTEX
}

char_warning (f, c)
    fnt     f;
    byte    c;
{
    if (tracing_lost_chars > 0) {
        begin_diagnostic();
        print_nl("Missing character: There is no ");
        print_ASCII(c);
        print(" in font ");
        print_str(font_name[f]);
        print_char('!');
        end_diagnostic(FALSE);
    }
}

ptr
new_character (f, c)
    fnt     f;
    byte    c;
{
    ptr     p;

    if (font_bc[f] <= c && font_ec[f] >= c) {
        if (char_exists(char_info(f, qi(c)))) {
            p = get_avail();
            font(p) = f;
            character(p) = qi(c);
            return p;
        }
    }
    char_warning(f, c);
    return NULL;
}

init_tfm ()
{
    fmem_ptr = 7;
    font_params[0] = 7;
    font_bc[0] = 1;
    font_glue[0] = zero_glue;
    hyphen_char[0] = '-';
    skew_char[0] = -1;
    param_base[0] = -1;
}

/*
 *  Help text
 */

help_font ()
{
    help4("I'm afraid I won't be able to make use of this font,",
    "because my memory for character-size data is too small.",
    "If you're really stuck, ask a wizard to enlarge me.",
    "Or maybe try `I\\font<same font id>=<name of loaded font>'.");
}

help_tfm ()
{
    help5("I wasn't able to read the size data for this font,",
    "so I will ignore the font specification.",
    "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
    "You might try inserting a different font spec;",
    "e.g., type `I\\font<same font id>=<substitute font name>'.");
}

help_font_param ()
{
    help2("To increase the number of font parameters, you must",
    "use \\fontdimen immediately after the \\font is loaded.");
}

help_font_cs ()
{
    help2("I was looking for a control sequence whose",
    "current meaning has been defined by \\font.");
}
