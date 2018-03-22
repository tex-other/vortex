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

#include <X/Xlib.h>
#include <sys/types.h>
#include <gl_comm.h>
#include <tp_comm.h>
#include <stdio.h>
#include "document.h"
#include "macros.h"
#include "tex_macros.h"
#include "font.h"

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

/*  This file contains the routines to build a bitmap from our ir produced
 *  by the TeX portion of VorTeX.
 */

/* The list of fonts.  */
font    *fonts[MAXFONTS];

/*  In case these are not defined elsewhere.  */ 
#ifndef MAX
#define MAX(a, b)   ((a) > (b) ? (a) : (b))
#endif MAX

#ifndef MIN
#define MIN(a, b)   ((a) < (b) ? (a) : (b))
#endif MIN

/*  The information is in this order:  
 *  first there are the 10 count variables.
 *  next comes the number of fonts
 *  next come triples <font_number, name_length, magstep> each followed by
 *      name_length bytes of font name (ie "cmr10").
 *  next comes the BOP node and the flattened page in a dfs tree.
 */

TeXBuild (tex, document, pageno)
{
    int                     globalmag;
    int                     width = 0;
    int                     height = 0;
    short                   nfonts;
    short                   *draw_page ();
    Nbox                    *read_document ();
    Nbox                    *doc;
    short                   *page;
    struct FontBits         *fp;
    register                i;
    register struct Page    *p;
    
    if ((p = (struct Page *) malloc (sizeof (struct Page))) == 
	 (struct Page *) NULL) {
	fprintf (stderr, "vpe:  TexBuild malloc p: malloc failed.\n");
	LEAVE (1);
    }
    if (read (tex, &p->p_globalmag, sizeof (p->p_globalmag)) != 
	sizeof (p->p_globalmag)) {
	perror ("vpe: TeXBuild: read globalmag");
	free (p);
	return (-1);
    }
    NTOHL(p->p_globalmag);
    if (TestDebug ("document")) {
	printf ("vpe: document globalmag=%d\n", globalmag);
    }
    if (read (tex, p->p_count, 10 * sizeof (long)) != 10 * sizeof (long)) {
	fprintf (stderr, "vpe:  TexBuild read count:");
	perror ("");
	free (p);
	return (-1);
    }
    for (i = 0; i < 10; i++) {
	NTOHL(p->p_count[i]);
    }
    if (TestDebug ("document")) {
	printf ("vpe: document ");
	for (i = 0; i < 10; i++) {
	    printf ("c[%d]=%d ", i, p->p_count[i]);
	}
	printf ("\n");
    }
    if (read (tex, &nfonts, sizeof (nfonts)) != sizeof (nfonts)) {
	perror ("vpe:  TexBuild read nfonts:");
	perror ("");
	return (-1);
    }
    NTOHS(nfonts);
    if (TestDebug ("document")) {
	printf ("vpe: nfonts=%d.\n", nfonts);
    }
    if ((nfonts < 0) || (nfonts >= MAXFONTS)) {
	fprintf (stderr, "vpe: TexBuild: nfonts=%d is invalid.\n", nfonts);
	return (-1);
    }
    /*  Load in all of the fonts.  */
    for (i = 0; i < nfonts; i++) {
	short  name_length;
	long   magnification;
	short  number;
	
	if (read (tex, &magnification, sizeof (magnification)) != 
	    sizeof (magnification)) {
	    perror ("vpe: TexBuild: read magnification");
	    return (-1);
	}
	if (TestDebug ("document")) {
	    printf ("vpe: magnification=%d.\n", magnification);
	}
	NTOHL(magnification);
	if (read (tex, &number, sizeof (number)) != sizeof (number)) {
	    perror ("vpe:  TexBuild read number:");
	    perror ("");
	    return (-1);
	}
	NTOHS(number);
	if (TestDebug ("document")) {
	    printf ("vpe: reading in font #%d\n", number);
	}
	if ((number < 0) || (number >= MAXFONTS)) {
	    fprintf (stderr, 
		     "vpe: TexBuild: font#%d in array number=%d is invalid\n", 
		     i, number);
	}
	if (fonts[number] == (font *) NULL) {
	    fonts[number] = (font *) malloc (sizeof (font));
	    if (fonts[number] == (font *) NULL) {
		fprintf (stderr, "vpe: TexBuild: malloc failed.\n");
		LEAVE (1);
	    }
	}
	fonts[number]->f_magnification = magnification;
	if (read (tex, &name_length, sizeof (name_length)) != 
	    sizeof (name_length)) {
	    perror ("vpe: TexBuild: read name_length");
	    return (-1);
	}
	NTOHS(name_length);
	if (TestDebug ("document")) {
	    printf ("vpe: name_length=%d bytes\n", name_length);
	}
	fonts[number]->f_name = (char *) malloc (name_length + 1);
	if (fonts[number]->f_name == NULL) {
	    fprintf (stderr, 
		     "vpe: TexBuild: malloc failed on fontname=%d bytes:\n",
		     name_length);
	    perror ("");
	    return (-1);
	}
	bzero (fonts[number]->f_name, name_length + 1);
	if (read (tex, fonts[number]->f_name, name_length) != name_length) {
	    fprintf (stderr, 
		     "vpe: TexBuild: error reading %d bytes of font name:",
		     name_length);
	    perror ("");
	    return (-1);
	}
	if (TestDebug ("document")) {
	    printf ("vpe: font name=%s\n", fonts[number]->f_name);
	}
	if (FontLoad (number)) {
	    fprintf (stderr, "vpe: TexBuild: Couldn't load font %d.\n", 
		     number);
	}
    }
    /*  Now, read in the flattened tex document and make a tree.  */
    if ((doc = read_document (tex, &width, &height)) == (Nbox *) NULL) {
	fprintf (stderr, "vpe: failed to load in the document.\n");
	return (-1);
    }
    if (TestDebug ("document")) {
	printf ("vpe: TeXBuild: page width=%d height=%d\n", width, height);
    }
    if ((page = draw_page (doc, width, height)) == (short *) NULL) {
	fprintf (stderr, "vpe: failed to paint the memory page.\n");
	return (-1);
    }
    /*  Now build the page.  */
    if (DocumentInitPage (document, pageno, p->p_count, doc, page, width, 
			  height)) {
	return (-1);
    }
    DocumentPaint (document);
    return (0);
}
#define DUMPFILE "./texboxdump"
int texboxdump = 1;
Nbox *
read_document (tex, width, height)
int     *width;
int     *height;
{
    int             nxx,nyy,nhh,nww;
    int             status;
    int             xpos;
    int             ypos;
    int             right = 0;
    int             bottom = 0;
    int             wd;
    int             ht;
    int             nread;
    char            type = -1;
    char            lasttype = -1;
    Nbox            *top = NULL;
    Nbox            *cnb[2];  /*  Last par in 0, last word in 1.  */
    Nbox            *nbp = NULL;
    Tbox            *ctb = NULL;
    Tbox            *tbp = NULL;
    Rbox            *rbp = NULL;
    FILE            *output; 
    FILE            *fopen ();
    FILE            *tbdump;
    struct FontBits *fp;
    
    if (TestDebug ("document-output")) {
	output = fopen ("tex_ir_output", "w");
    }
    if ((top = (Nbox *) malloc (sizeof (Nbox))) == (Nbox *) NULL) {
	fprintf (stderr, "vpe: read_document: malloc top failed.\n");
	LEAVE (1);
    }
    if(texboxdump == 1){
	tbdump = fopen(DUMPFILE,"a");
	fprintf(tbdump,"**********NEW DUMP *************\n");
    }
    bzero (top, sizeof (Nbox));
    cnb[0] = cnb[1] = (Nbox *) NULL;
    while ((status = read (tex, &type, sizeof (type))) == sizeof (type)) {
	if (status == 0) {
	    printf ("vpe: read_document: status <- 0???\n");
	    continue;
	}
	switch (type) {
	case TP_EOP:
	    if(texboxdump == 1){
		fclose(tbdump);
	    }
	    *width = right;
	    *height = bottom;
	    if (TestDebug ("document")) {
		printf ("vpe: read_document: returning width=%d height=%d\n",
			*width, *height);
	    }
	    return (top);
	    break;
	    
	case TP_PAR:
	    if (top->nb_nchildren == 0) {
		top->nb_children = nbp = (Nbox *) malloc (sizeof (Nbox));
		top->nb_nchildren = 1;
	    }
	    else {
		nbp = top->nb_children;
		while (nbp->nb_next != (Nbox *) NULL) {
		    nbp = nbp->nb_next;
		}
		nbp->nb_next = (Nbox *) malloc (sizeof (Nbox));
		nbp = nbp->nb_next;
		top->nb_nchildren++;
	    }
	    bzero (nbp, sizeof (Nbox));
	    nread = 0;
	    while (nread != sizeof (nbp->nb_box)) {
		status = read (tex, &(((char *)&nbp->nb_box)[nread]),
			       sizeof (nbp->nb_box) - nread);
		if (status == 0) {
		    fprintf (stderr, "vpe: read_document: read box failed\n");
		    return ((Nbox *) NULL);
		}
		else if (status < 0) {
		    perror ("vpe: read_document: read");
		    LEAVE(1);
		}
		else {
		    nread += status;
		}
	    }
	    NTOHL(nbp->nb_box.id);
	    NTOHL(nbp->nb_box.xc);
	    NTOHL(nbp->nb_box.yc);
	    NTOHL(nbp->nb_box.wd);
	    NTOHL(nbp->nb_box.ht);
	    nbp->nb_nchildren = 0;
	    nbp->nb_children = (Nbox *) NULL;
	    cnb[0] = nbp;
	    if (TestDebug ("document")) {
		printf ("vpe: Added child par %d to page.\n", 
			top->nb_nchildren);
		printf ("vpe: Current par at 0x%x\n", cnb[0]);
		printf ("vpe: PAR id=%d xc=%d yc=%d wd=%d ht=%d\n", 
			nbp->nb_box.id,
			nbp->nb_box.xc,
			nbp->nb_box.yc,
			nbp->nb_box.wd,
			nbp->nb_box.ht);
	    }
	    if (TestDebug ("document-output")) {
		fprintf (output, "\n-----\n");
		fflush (output);
	    }
	    break;

	case TP_WORD:
	    if (lasttype == TP_PAR) {
		nbp = cnb[0];
		if (nbp->nb_nchildren != 0) {
		    fprintf (stderr, 
	        "vpe: read_document: Fatal error: revisiting a word node.\n");
		    return ((Nbox *) NULL);
		}
		nbp->nb_nchildren = 1;
		nbp->nb_children = (Nbox *) malloc (sizeof (Nbox));
		nbp = nbp->nb_children;
		cnb[1] = nbp;
	    }
	    else if ((lasttype == TP_CHAR) || (lasttype == TP_RULE) ||
		     (lasttype == TP_SPECIAL)) {
		nbp = cnb[1];
		nbp->nb_next = (Nbox *) malloc (sizeof (Nbox));
		if (nbp->nb_next == (Nbox *) NULL) {
		    fprintf (stderr, 
			     "vpe: read_document: malloc word node failed.\n");
		    LEAVE (1);
		}
		nbp = nbp->nb_next;
		cnb[1] = nbp;
		cnb[0]->nb_nchildren++;  /*  One more word in the par.  */
	    }
	    nread = 0;
	    while (nread != sizeof (nbp->nb_box)) {
		status = read (tex, &(((char *)&nbp->nb_box)[nread]), 
			       sizeof (nbp->nb_box) - nread);
		if (status == 0) {
		    fprintf (stderr, "vpe: read_document: read box failed\n");
		    return ((Nbox *) NULL);
		}
		else if (status < 0) {
		    perror ("vpe: read_document: read");
		    LEAVE(1);
		}
		else {
		    nread += status;
		}
	    }
	    NTOHL(nbp->nb_box.id);
	    NTOHL(nbp->nb_box.xc);
	    NTOHL(nbp->nb_box.yc);
	    NTOHL(nbp->nb_box.wd);
	    NTOHL(nbp->nb_box.ht);
	    if(texboxdump == 1){
		nxx = fixup(nbp->nb_box.xc) + FONTRESOLUTION;
		nyy = fixup(nbp->nb_box.yc) + FONTRESOLUTION;
		nww = fixup(nbp->nb_box.wd);
		nhh = fixup(nbp->nb_box.ht);
		fprintf(tbdump,"new word @(%d %d) is %d x %d \n",nxx,nyy,nww,nhh);
	    }
	    if (TestDebug ("document-word")) {
		printf ("vpe: Added child word %d to par.\n", 
			cnb[0]->nb_nchildren);
		printf ("vpe: Current word at 0x%x child of 0x%x\n", 
			cnb[1], cnb[0]);
		printf ("vpe: WORD id=%d xc=%d yc=%d wd=%d ht=%d\n", 
			nbp->nb_box.id,
			nbp->nb_box.xc,
			nbp->nb_box.yc,
			nbp->nb_box.wd,
			nbp->nb_box.ht);
	    }
	    if (TestDebug ("document-output")) {
		fprintf (output, "\n");
	    }
	    break;

	case TP_CHAR:
	    if (lasttype == TP_WORD) {
		cnb[1]->nb_children = (Nbox *) malloc (sizeof (Tbox));
		if (cnb[1]->nb_children == (Nbox *) NULL) {
		    fprintf (stderr, 
			     "vpe: read_document: malloc word node failed.\n");
		    LEAVE(1);
		}
		cnb[1]->nb_nchildren = 1;
		ctb = (Tbox *) cnb[1]->nb_children;
		ctb->tb_next = (Tbox *) NULL;
	    }
	    else if (lasttype == TP_CHAR) {
		cnb[1]->nb_nchildren++;
		if (ctb == (Tbox *) NULL) {
		    fprintf (stderr, 
		    "vpe: read_document: appending char to NULL word box.\n");
		    return ((Nbox *) NULL);
		}
		ctb->tb_next = (Tbox *) malloc (sizeof (Tbox));
		ctb = ctb->tb_next;
		if (ctb == (Tbox *) NULL) {
		    fprintf (stderr, 
			     "vpe: read_document: malloc failed on char.\n");
		    LEAVE(1);
		}
		bzero (ctb, sizeof (ctb));
	    }
	    else {
		fprintf (stderr, 
			 "vpe: read_document: char following non-word.\n");
		return ((Nbox *) NULL);
	    }
	    nread = 0;
	    while (nread != sizeof (ctb->tb_box)) {
		status = read (tex, &(((char *)&ctb->tb_box)[nread]),
			       sizeof (ctb->tb_box) - nread);
		if (status == 0) {
		    fprintf (stderr, "vpe: read_document: read box failed\n");
		    return ((Nbox *) NULL);
		}
		else if (status < 0) {
		    perror ("vpe: read_document: read");
		    LEAVE(1);
		}
		else {
		    nread += status;
		}
	    }
	    NTOHL(ctb->tb_box.id);
	    NTOHL(ctb->tb_box.ch);
	    NTOHL(ctb->tb_box.ft);
	    NTOHL(ctb->tb_box.xb);
	    NTOHL(ctb->tb_box.yb);
	    if (TestDebug ("document-char")) {
		printf ("vpe: read_document: char %d in word 0x%x: ",
			cnb[1]->nb_nchildren, cnb[1]);
   		printf ("id=%d ch=%d(%c) ft=%d xb=%d yb=%d\n",
		     ctb->tb_box.id,
		     ctb->tb_box.ch,
		     ctb->tb_box.ch,
		     ctb->tb_box.ft,
		     ctb->tb_box.xb,
		     ctb->tb_box.yb);
	    }
	    if (TestDebug ("document-output")) {
		fprintf (output, "%c", ctb->tb_box.ch);
	    }
	    if ((fonts[ctb->tb_box.ft] == NULL) || 
		(fonts[ctb->tb_box.ft]->f_chars == NULL)) {
		fprintf (stderr, "Vpe: font %d is not present.\n");
		continue;
	    }
	    fp = &fonts[ctb->tb_box.ft]->f_chars[ctb->tb_box.ch];
	    xpos = ctb->tb_box.xb >> 14;
	    xpos *= FONTRESOLUTION;
	    xpos /= 72;
	    xpos >>= 2;
	    ypos = ctb->tb_box.yb >> 14;
	    ypos *= FONTRESOLUTION;
	    ypos /= 72;
	    ypos >>= 2;
	    xpos -= fp->fb_xoffset;
	    ypos -= fp->fb_yoffset;
	    xpos += FONTRESOLUTION;
	    ypos += FONTRESOLUTION;
	    ctb->tb_xpos = xpos;
	    ctb->tb_ypos = ypos;
	    xpos += fp->fb_width;
	    ypos += fp->fb_height;
	    if (xpos > right) {
		right = xpos;
	    }
	    if (ypos > bottom) {
		bottom = ypos;
	    }
	    ctb->tb_flags = TP_CHAR;
	    break;

	case TP_RULE:
	    if (lasttype != TP_WORD) {
		fprintf (stderr, 
			 "vpe: read_document: rule following non-word.\n");
		return ((Nbox *) NULL);
	    }
	    cnb[1]->nb_children = (Nbox *) malloc (sizeof (Rbox));
	    cnb[1]->nb_nchildren = 1;
	    rbp = (Rbox *) cnb[1]->nb_children;
	    nread = 0;
	    while (nread != sizeof (rbp->rb_box)) {
		status = read (tex, &(((char *)&rbp->rb_box)[nread]),
			       sizeof (rbp->rb_box) - nread);
		if (status == 0) {
		    fprintf (stderr, "vpe: read_document: read box failed\n");
		    return ((Nbox *) NULL);
		}
		else if (status < 0) {
		    perror ("vpe: read_document: read");
		    LEAVE(1);
		}
		else {
		    nread += status;
		}
	    }
	    if (TestDebug ("paint")) {
		fprintf (stderr, "vpe: rule@<%d,%d>+%d+%d\n",
			 rbp->rb_box.xc, rbp->rb_box.yc,
			 rbp->rb_box.wd, rbp->rb_box.ht);
	    }
	    xpos = ntohl(rbp->rb_box.xc) >> 14;
	    xpos *= FONTRESOLUTION;
	    xpos /= 72;
	    xpos >>= 2;
	    ypos = ntohl(rbp->rb_box.yc) >> 14;
	    ypos *= FONTRESOLUTION;
	    ypos /= 72;
	    ypos >>= 2;
	    xpos += FONTRESOLUTION;
	    ypos += FONTRESOLUTION;
	    wd = ntohl(rbp->rb_box.wd) >> 14;
	    wd *= FONTRESOLUTION;
	    wd /= 72;
	    wd >>= 2;
	    ht = ntohl(rbp->rb_box.ht) >> 14;
	    ht *= FONTRESOLUTION;
	    ht /= 72;
	    ht >>= 2;
	    rbp->rb_xpos = xpos;
	    rbp->rb_ypos = ypos;
	    rbp->rb_box.wd = wd;
	    rbp->rb_box.ht = ht;
	    if (TestDebug ("paint")) {
		fprintf (stderr, "vpe: rule@<%d,%d>+%d+%d\n",
			 rbp->rb_box.xc, rbp->rb_box.yc,
			 rbp->rb_box.wd, rbp->rb_box.ht);
	    }
	    if (rbp->rb_box.wd > 0) {
		xpos += rbp->rb_box.wd;
	    }
	    if (rbp->rb_box.ht > 0) {
		ypos += rbp->rb_box.ht;
	    }
	    if (xpos > right) {
		right = xpos;
	    }
	    if (ypos > bottom) {
		bottom = ypos;
	    }
	    rbp->rb_flags = TP_RULE;
	    break;

	case TP_SPECIAL:
	    /*  Box, then a short of datalen, and then datalen bytes of arg  */
	    if (lasttype != TP_WORD) {
		fprintf (stderr, 
			 "vpe: read_document: special following non-word.\n");
		return ((Nbox *) NULL);
	    }
	    break;
	    
	default:
	    fprintf (stderr,
		     "vpe: read_document: type=%d is unknown.\n", type);
	    read (tex, &type, sizeof (type));
	    printf ("NextType=%d\n", type);
	    return ((Nbox *) NULL);
	    break;
	}
	lasttype = type;
    }
    perror ("vpe: read_document: reading an internal page node");
    return ((Nbox *) NULL);
    
}

short *
draw_page (doc, width, height)
Nbox    *doc;
{
    int     pagew;
    Nbox    *box[2];
    Tbox    *ch;
    short   *page;

    pagew = ((width + 15) / 16);
    page = (short *) malloc (pagew * height * sizeof (short));
    bzero (page, pagew * height * sizeof (short));
    if (page == (short *) NULL) {
	fprintf (stderr, "vpe: draw_page: malloc page failed.\n");
	LEAVE (1);
    }
    if (TestDebug ("paint")) {
	printf ("vpe: draw_page: doc=0x%x width=%d height=%d\n", 
		doc, width, height);
    }	
    box[0] = doc->nb_children;
    while (box[0] != (Nbox *) NULL) {
	int        id;
	int        xpos;
	int        ypos;
	int        wd;
	int        ht;
	int        charw;  /*  Number of shorts wide the char is.  */
	int        line;
	int        bit;
	register   i;
	register   j;
	
	for (i = 0; i < box[0]->nb_nchildren; i++) {
	    if (i) {
		box[1] = box[1]->nb_next;
	    }
	    else {
		box[1] = box[0]->nb_children;
	    }
	    for (j = 0; j < box[1]->nb_nchildren; j++) {
		struct FontBits   *fb;
		
		if (j) {
		    ch = ch->tb_next;
		}
		else {
		    ch = (Tbox *) box[1]->nb_children;
		}
		if (ch->tb_flags != TP_CHAR) {
		    if (ch->tb_flags == TP_RULE) {
			wd = ((Rbox *) ch)->rb_box.wd;
			/*  Note: a 1 pixel wide rule at 300 dpi may well
			 *  not be visable at monitor resolutions, so make 
			 *  0 width rules one pixel wide.
			 */
			if (wd == 0) {
			    wd = 1;
			}
			ht = ((Rbox *) ch)->rb_box.ht;
			/*  What is above goes for height as well.  */
			if (ht == 0) {
			    ht = 1;
			}
			xpos = ((Rbox *) ch)->rb_xpos;
			ypos = ((Rbox *) ch)->rb_ypos;
			for (line = 0; line < ht; line++) {
			    for (bit = 0; bit < wd; bit++) {
				page[pagew * (ypos + line) + (xpos + bit) / 16]
				    |= (1 << ((xpos + bit) % 16));
			    }
			}
		    }
		    else {
			fprintf (stderr, 
				 "vpe: draw_page: non-char in page.\n");
		    }
		    continue;
		}
		xpos = ch->tb_xpos;
		ypos = ch->tb_ypos;
		if ((xpos > width) || (ypos > height)) {
		    printf ("vpe:  paint_page char off of the page.\n");
		    continue;
		}
		fb = &((fonts[ch->tb_box.ft]->f_chars)[ch->tb_box.ch]);
		charw = (fb->fb_width + 15) / 16;
		ht = fb->fb_height;
		wd = fb->fb_width;
		for (line = 0; line < ht; line++) {
		    for (bit = 0; bit < wd; bit++) {
			if (fb->fb_char[line * charw + (bit / 16)] &
			    (1 << (bit % 16))) {
			    page[pagew *(line + ypos) + ((xpos + bit) / 16)] |=
				(1 << ((xpos + bit) % 16));
			}
		    }
		}
	    }
	}
	box[0] = box[0]->nb_next;
    }
    return (page);
}

paint_page (W, doc)
Window  W;
{
    int         x;
    int         y;
    int         dx;
    int         dy;
    int         xoff;
    int         yoff;
    int         pagewidth;
    int         pageheight;
    int         WindowID;
    int         fg_pixel;
    int         bg_pixel;
    int         deltay;
    short       *pagebits;
    short       *DocumentGetBitmap ();
    register    i;
    
    pagebits = DocumentGetBitmap (doc);
    if (pagebits == (short *) NULL) {
	if (TestDebug ("paint")) {
	    printf ("vpe: paint_page: no page is loaded to paint.\n");
	}
	return (0);
    }
    if (DocumentGetSize (doc, &pagewidth, &pageheight)) {
	fprintf (stderr, "vpe: paint_page: Couldn't get page size.\n");
	return (-1);
    }
    if ((WindowID = WindowGetWindow (W)) < 0) {
	fprintf (stderr, "vpe: Window=%d no WindowID.\n", W);
	return (-1);
    }
    if (WindowGetPosition (WindowID, &x, &y)) {
	fprintf (stderr, "vpe: WindowID=%d get position failed.\n", WindowID);
	return (-1);
    }
    if (WindowGetSize (WindowID, &dx, &dy)) {
	fprintf (stderr, "vpe: WindowID=%d get page size failed.\n", WindowID);
	return (-1);
    }
    if (WindowGetOffset (WindowID, &xoff, &yoff)) {
	fprintf (stderr, "vpe: WindowID=%d get offset failed.\n", WindowID);
	return (-1);
    }
    if (WindowGetFGPixel (WindowID, &fg_pixel, (Pixmap *) NULL)) {
	fprintf (stderr, "vpe: paint_page (wid=%d): Couldn't find fg_pixel.\n",
		 WindowID);
	return (-1);
    }
    if (WindowGetBGPixel (WindowID, &bg_pixel, (Pixmap *) NULL)) {
	fprintf (stderr, "vpe: paint_page (wid=%d): Couldn't find bg_pixel.\n",
		 WindowID);
	return (-1);
    }
    if (TestDebug ("colors")) {
	printf ("vpe: paint_page fg_pixel=%d bg_pixel=%d\n", 
		fg_pixel, bg_pixel);
    }
    if (y > pageheight) {
	/*  Then we are off of the bottom of the page.  Clear the window.  */
	XClear (W);
	XSync (0);
	return (0);
    }
    else if (y + dy > pageheight) {
	/*  Clear out the area below the bottom of the page on the window.  */
	XPixFill (W, xoff, yoff + pageheight - y - 1, dx, 
		  y + dy - pageheight + 1,
		  bg_pixel, (Bitmap) 0, GXcopy, AllPlanes);
	dy = pageheight - y;
    }
    if (TestDebug ("paint")) {
	printf ("vpe: paint_page: x=%d y=%d xoff=%d yoff=%d\n", 
		x, y, xoff, yoff);
    }
    /*  Note that we paint from the left corner of the page (xoff) always, 
     *  so that I don't have to do any memory copies.
     *
     *  Also note that I have to download in 128K pieces.
     */
    if (1) {  /*  Why not just do this all of the time.  */
	int   width = 800000;
	int   total;
	int   nlines;		
	int   line;
	int   next = 0;
	
	nlines = 800000 / pagewidth;
	if (TestDebug ("document")) {
	    fprintf(stderr, "vpe: bitmap being broken up into 100k pieces.\n");
	}
	for (line = 0; line < dy; line += nlines) {
	    XBitmapBitsPut (W, xoff, yoff + line,
			    pagewidth, MIN(nlines, dy - line),
			    &pagebits[((pagewidth + 15) / 16) * 
					 (yoff + y + line)],
			    fg_pixel, bg_pixel, (Bitmap) 0,
			    GXcopy, AllPlanes);
	}
    }
    else {
	if (TestDebug ("document")) {
	    fprintf(stderr, "vpe: bitmap being sent down as a whole.\n");
	}
	XBitmapBitsPut (W, xoff, yoff + y, pagewidth, dy,
			&pagebits[(y) * ((pagewidth + 15) / 16)],
			fg_pixel, bg_pixel, (Bitmap) 0, GXcopy, AllPlanes);
    }
    XSync (0);
    return (0);
}

exit (n)
{
    _exit (n);
}
