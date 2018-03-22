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
#include <ctype.h>
#include <stdio.h>
#include "font.h"

/*  This file contains the code to convert a pk (packed pixel) font
 *  to a linked list of bitmaps, which are used to paint the page.
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

extern font *fonts[];

#define BEGINFONT   32  /*  The initial number of fonts  */

int         _FontNumber = 0;
int         _FontLength;
font        *_Font;

FontLoaded (fid) 
{
    register    i;
    
    for (i = 0; i < _FontNumber; i++) {
	if (_Font[i].f_number == fid) {
	    return (i);
	}
    }
    return (-1);
}

FontLoad (fnumber)
{
    char            *directory = "/usr/local/fonts/pk";
    char            path[BUFSIZ];
    int             size;
    int             cachenumber;
    char            *dsize;
    char            *rindex ();
    font            *fp;
    struct FontBits *loadPKfont ();
    
    if ((cachenumber = FontLoaded (fnumber)) >= 0) {
	fonts[fnumber]->f_chars = _Font[cachenumber].f_chars;
	return (0);
    }
    if (_FontLength == 0) {
	_FontLength = BEGINFONT;
	_Font = (font *) malloc (BEGINFONT * sizeof (font));
	_FontNumber = 0;
    }
    else if (_FontNumber == _FontLength - 1) {
	font   *save;

	save = _Font;
	_Font = (font *) malloc(2 * _FontLength * sizeof (font));
	bcopy (save, _Font, _FontLength * sizeof (font));
	free (save);
	_FontLength *= 2;
    }
    fp = &_Font[_FontNumber++];
    dsize = &fonts[fnumber]->f_name[strlen (fonts[fnumber]->f_name) - 1];
    while (isdigit(*dsize)) {
	if (dsize == fonts[fnumber]->f_name) {
	    break;
	}
	dsize--;
    }
    if (!isdigit(*dsize)) {
	dsize++;
    }
    size = atoi (dsize);
    sprintf (path, "%s/%s.%dpk", 
	     directory, 
	     fonts[fnumber]->f_name,
	     (int) (FONTRESOLUTION * ((float) fonts[fnumber]->f_magnification /
			     (float) ((1 << 16) * size))));
    if (TestDebug ("font")) {
	printf ("vpe: FontLoad: looking for font %s size=%d\n", path, size);
    }
    fp->f_chars = loadPKfont (path);
    fp->f_number = fnumber;
    fonts[fnumber]->f_chars = fp->f_chars;
    return (0);
}
