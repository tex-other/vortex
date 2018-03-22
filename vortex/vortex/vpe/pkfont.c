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

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include "pkdefs.h"
#include "font.h"
#include "texfont.h"
#include "tfm.h"

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

#define Round(a)    ((a) - (int)(a) >= .5 ? ((int) (a) + 1) : (int) (a))

/*  Get nibble n out of a buffer of chars b  */
#define GetN(n, b) \
	(n) % 2 ? (b)[(n) / 2] & 0xf : (b)[(n) / 2] >> 4 & 0xf

struct FontBits *
loadPKfont (fontname)
char    *fontname;
{
    int             designsize;
    FILE            *fopen ();
    FILE            *fp = (FILE *) NULL;
    u_char          c;
    short           s;
    int             i;
    int             last;
    int             npaths;
    char            buf[BUFSIZ];
    char            *Paths[32];
    char            *SearchPath;
    char            *GetVariableValue ();
    struct FontBits *Font;

    fp = fopen (fontname, "r");
    if (fp == (FILE *) NULL) {
	fprintf (stderr,  "Font %s not found.\n", fontname);
	return ((struct FontBits *) NULL);
    }
    c = getc (fp);
    if (c != PK_PRE) {
	fprintf (stderr, "%s: Not a PK font file.", buf);
	return ((struct FontBits *) NULL);
    }
    fseek (fp, 0, 0);
    designsize = readpreamble (fp);
    Font = (struct FontBits *) malloc (NCHARS * sizeof (struct FontBits));
    do {
	c = getc (fp);
	if ((u_char) c < 240) {
	    readbitmap (fp, c, Font, designsize);
	}
	else {
	    switch (c) {
	    case PK_NOP:
		break;
		
	    case PK_XXX1:
		fread (&c, 1, 1, fp);
		fseek (fp, (u_int) c, 1);
		break;
		
	    case PK_XXX2:
		fread (&s, 2, 1, fp);
		fseek (fp, (u_int) s, 1);
		break;
		
	    case PK_XXX3:
		fread (&s, 2, 1, fp);
		c = getc (fp);
		i = c << 16 + (u_short) s;
		fseek (fp, (u_int) i, 1);
		break;
		
	    case PK_XXX4:
		fread (&i, 4, 1, fp);
		fseek (fp, (u_int) i, 1);
		break;
		
	    case PK_YYY:
		fseek (fp, 4, 1);
		break;
		
	    case PK_POST:
		break;
		
	    case PK_PRE:
		printf ("Illegal preamble operator at position %d.\n", 
			fseek (fp, 0, 1) - 1);
		return ((struct FontBits *) NULL);
	    }
	}
    } while (c != PK_POST);
    for (i = 0; i < NCHARS - 1; i++) {
	Font[i].fb_next = (struct FontBits *) &Font[i + 1];
    }
    Font[NCHARS - 1].fb_next = (struct FontBits *) NULL;
    fclose (fp);
    if (TestDebug ("font")) {
	printf ("vpe: loadPKfont: returning 0x%x\n", Font);
    }
    return ((struct FontBits *) Font);
}

readpreamble (fp)
FILE    *fp;
{
    char    c;
    char    id;
    char    k;
    int     ds;
    int     cs;
    int     hppp;
    int     vppp;
    char    *comment;
    
    fread (&c, 1, 1, fp);
    fread (&id, 1, 1, fp);
    fread (&k, 1, 1, fp);
    comment = (char *) malloc (k);
    fread (comment, k, 1, fp);
    fread (&ds, 4, 1, fp);
    fread (&cs, 4, 1, fp);
    fread (&hppp, 4, 1, fp);
    fread (&vppp, 4, 1, fp);
    return (ds);
}

readbitmap (fp, flag, font, ds)
FILE    *fp;
struct FontBits *font;
{
    char                        color;  /*  Are we painting black or white  */
    char                        dyn_f;
    char                        *data;
    char                        **raster;
    int                         count;
    int                         line;
    int                         bitmap = 0;
    int                         packetlen;
    u_int                       charcode;
    int                         dx;
    int                         dy;
    int                         tfm;
    int                         width;
    int                         height;
    int                         xoff;
    int                         yoff;
    int                         next;
    int                         painted;
    int                         *repeatlist;
    u_int                       repeat;
    int                         i;
    u_char                      c;
    char                        sc;
    short                       s;
    register                    j;
    register                    k;
    register struct FontBits    *fbp;

    dyn_f = (u_char) flag >> 4;
    color = flag & 0x8;
    if ((flag & 0xf0) == 0xe0) {
	bitmap++;
    }
    switch (flag & 0x7) {
    case 0:
    case 1:
    case 2:
    case 3:
	c = getc (fp);
	packetlen = (u_int) c;
	c = getc (fp);
	charcode = (u_int) c;
	c = getc (fp);
	tfm = c;
	c = getc (fp);
	tfm <<= 8;
	tfm |= c;
	c = getc (fp);
	tfm <<= 8;
	tfm |= c;
	c = getc (fp);
	c = getc (fp);
	width = (u_int) c;
	c = getc (fp);
	height = (u_int) c;
	sc = getc (fp);
	xoff = (int) sc;
	sc = getc (fp);
	yoff = (int) sc;
	packetlen += (int) (((u_int) flag) % 4) << 8;
	packetlen -= 8;
	break;
	
    case 4:
    case 5:
    case 6:
	fread (&s, 2, 1, fp);
	packetlen = (u_int) ntohs(s);
	c = getc (fp);
	charcode = (u_int) c;
	c = getc (fp);
	fread (&s, 2, 1, fp);
	s = ntohs(s);
	tfm = c << 16 | (u_int) s;
	fread (&s, 2, 1, fp);
	fread (&s, 2, 1, fp);
	width = (u_int) ntohs(s);
	fread (&s, 2, 1, fp);
	height = (u_int) ntohs(s);
	fread (&s, 2, 1, fp);
	xoff = (int) ntohs(s);
	fread (&s, 2, 1, fp);
	yoff = (int) ntohs(s);
	packetlen += (int) (((u_int) flag) % 4) << 16;
	packetlen -= 13;
	break;
	
    case 7:
	fread (&packetlen, 4, 1, fp);
	packetlen = ntohl(packetlen);
	fread (&charcode, 4, 1, fp);
	charcode = ntohl(charcode);
	fread (&tfm, 4, 1, fp);
	tfm = ntohl(tfm);
	fread (&dx, 4, 1, fp);
	dx = ntohl(dx);
	fread (&dy, 4, 1, fp);
	dy = ntohl(dy);
	fread (&width, 4, 1, fp);
	width = ntohl(width);
	fread (&height, 4, 1, fp);
	height = ntohl(height);
	fread (&xoff, 4, 1, fp);
	xoff = ntohl(xoff);
	fread (&yoff, 4, 1, fp);
	yoff = ntohl(yoff);
	packetlen -= 24;
	break;
    }
    fbp = &font[charcode];
    
    fbp->fb_width = width;
    fbp->fb_height = height;
    fbp->fb_tfmwidth = tfm;
    fbp->fb_xoffset = xoff;
    fbp->fb_yoffset = yoff;
    fbp->fb_char = (short *) malloc (((width + 15) / 16) * height * 
				     sizeof (short));
    bzero (fbp->fb_char, ((width + 15) / 16) * height * sizeof (short));
    data = (char *) malloc (packetlen);
    fread (data, packetlen, 1, fp);
    raster = (char **) malloc (height * sizeof (char *));
    line = 0;
    repeat = 0;
    count = 0;
    if (bitmap) {
	for (i = 0; i < height; i++) {
	    raster[i] = (char *) malloc (((width + 15) / 16) * 2);
	    bzero (raster[i], ((width + 15) / 16) * 2);
	}
	for (i = 0; i < packetlen; i++) {
	    c = data[i];
	    for (j = 0; j < 8; j++) {
		int   xpos = 8 * i + j;
		if (xpos >= width * height) {
		    continue;
		}
		if (c & (1 << (7 - j))) {
		    raster[xpos / width][(xpos % width) / 8] |=
			1 << ((xpos % width) % 8);
		}
	    }
	}
	for (j = 0; j < height; j++) {
#ifdef SUNBITS
	    for (k = 0; k < ((width + 15) / 16); k++) {
		char  save;
		
		save = raster[j][2 * k];
		raster[j][2 * k] = raster[j][2 * k + 1];
		raster[j][2 * k + 1] = save;
            }
#endif SUNBITS	    
	    bcopy (raster[j], &fbp->fb_char[j * ((width + 15) / 16)],
		   ((width + 15) / 16) * 2);
	}
	return;
    }
    repeatlist = (int *) malloc (height * sizeof (int));
    for (i = 0; i < height; i++) {
	repeatlist[i] = 0;
    }
    painted = 0;
    count = 0;
    for (i = 0; i < packetlen * 2; i++) {
	if (((count == width) && (line == height - 1)) || (line == height)) {
	    break;
	}
	c = GetN(i, data);
	if (c == 0xe) {
	    i++;
	    repeat = GetN(i, data);
	    if (repeat > dyn_f) {
		i++;
		next = (repeat - dyn_f - 1) * 16 + dyn_f + 1;
		next += GetN(i, data);
		repeat = next;
		next = 0;
	    }
	    else if (repeat == 0) {
		int    length = 1;
		
		do {
		    i++;
		    c = GetN(i, data);
		    length++;
		} while (c == 0);
		next = 0;
		while (length--) {
		    next <<= 4;
		    next += GetN(i, data);
		    i++;
		}
		i--;
		next = next - 15 + (13 - dyn_f) * 16 + dyn_f;
		repeat = next;
		next = 0;
	    }
	    continue;
	}
 	else if (c == 0xf) {
	    repeat = 1;
	    continue;
	}
	else if (c == 0) {
	    int    length = 1;

	    if (i == packetlen * 2 - 1) {  /*  Is it a trailing 0?  */
		break;
	    }
	    do {
		i++;
		c = GetN(i, data);
		length++;
	    } while (c == 0);
	    next = 0;
	    while (length--) {
		next <<= 4;
		next += GetN(i, data);
		i++;
	    }
	    if (!next) {
		continue;
	    }
	    i--;
	    next = next - 15 + (13 - dyn_f) * 16 + dyn_f;
	}
	else if (c > dyn_f) {
	    i++;
	    next = (c - dyn_f - 1) * 16 + dyn_f + 1;
	    next += GetN(i, data);
	}
	else {
	    next = c;
	}
	fflush (stdout);
	for (j = 0; j < (int) next; j++, count++) {
	    register char  *rl;
	    if (count == width) {
		painted = color;
		count = 0;
		line++;
		if (line >= height) {
		    break;
		}
	    }
	    if (count == 0) {
		raster[line] = (char *) malloc (((width + 15) / 16) * 
						sizeof (short));
		bzero (raster[line], (((width + 15) / 16) * sizeof (short)));
	    }
	    rl = raster[line];
	    if (color) {
		int   byte = count / 8;
		
#ifdef SUNBITS
		if (byte % 2) {
		    byte -= 1;
		}
		else {
		    byte += 1;
		}
#endif SUNBITS
		
		painted++;
		rl[byte] |= 1 << (count % 8);
	    }
	    if (painted) {
		if (repeat && painted) {
		    repeatlist[line] = repeat;
		    repeat = 0;
		}
	    }
	}
	color = !color;
    }
    for (i = 0, j = 0; j < height; j += repeatlist[i] + 1, i++) {
	for (k = 0; k <= repeatlist[i]; k++) {
	    bcopy (raster[i], &fbp->fb_char[(j + k) * ((width + 15) / 16)],
		   ((width + 15) / 16) * 2);
	}
    }
}

printline (rp, width)
char    *rp;
{
    register    i;
    register    j;
    
    for (i = 0; i < (width + 7) / 8; i++) {
	for (j = 0; j < 8; j++) {
	    if (8 * i + j == width) {
		putchar ('\n');
		return;
	    }
	    if (rp[i] & (1 << j)) {
		putchar ('#');
	    }
	    else {
		putchar ('.');
	    }
	}
    }
    putchar ('\n');
}
