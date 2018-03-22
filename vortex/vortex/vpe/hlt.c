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

#include "vpeselect.h"

    
/*distinguish a given nbox on the page by inverting its map*/
/*scroll of 0 means just highlight 1 scroll 2 just repaint*/
Gbox *saved = NULL;    

int
hltsel(box,lev,win,scroll)
  int lev;
  Ubox *box;
  u_long win;
  int scroll;
{
    register int i;                   /*counter*/
    register int j;                   /*counter*/
    short *pgbits;                    /*windows full set of bits*/
    Nbox *hkbx;                       /*hack till widths work*/
    int xf,yf;                        /*offsets for scrolling*/
    int llen;                         /*length of a line*/
    int pgh,wwid;                     /*pagehieght,and width of doc*/
    int xoff,yoff;                    /*offset pointers*/
    int bg,fg;                        /*back and fore ground pixels*/
    int wid;                          /*window struct's window id*/
    struct FontBits *fp;              /*contains bits and offsets of char*/
    extern font *fonts[MAXFONTS];     /*the page font list*/
    int status = 0;                   /*status*/
    int bx,by;                        /*bounding box ul corner*/
    int bh,bw;                        /*bounding box dimensions*/
    u_long ftp;                       /*for char's this is its fontid*/
    int xpos,ypos;
    int chp;                          /*for char's this is the char*/
    int doc = 0;                      /*single doc hack*/
    int wwidth,wheight;               /*xwindows cur height and width*/
    
    /*use the global document to gxinvert the page bits*/
    
    if((saved != (Gbox *)NULL) && (cleanup(win,doc) < 0))
	fprintf(stderr,"vpe: cleanup page screwed up; proceeding\n");
    
    if(lev == SEL_CH){
	bx = box->ub_tb->tb_xpos;  
	by = box->ub_tb->tb_ypos;
	ftp = box->ub_tb->tb_box.ft;
	chp = box->ub_tb->tb_box.ch;
	fp = &((fonts[ftp]->f_chars)[chp]);
	bw = fp->fb_width;
	bh = fp->fb_height;
    }else if(lev == SEL_WD){
	bx = box->ub_nb->nb_box.xc;
	by = box->ub_nb->nb_box.yc;
	bh = box->ub_nb->nb_box.ht;
	bx = fixup(bx) + FONTRESOLUTION;
	by = fixup(by) + FONTRESOLUTION;
	bh = fixup(bh);
	hkbx = box->ub_nb;
	if(((hkbx = hkbx->nb_next) != NULL) &&
	   (bw = fixup(hkbx->nb_box.xc) + FONTRESOLUTION) && (bx < bw)){
	    bw = bw - bx;
	}else{
	    bw = box->ub_nb->nb_box.wd;
	    bw = fixup(bw);
	}
    }else{
	bx = box->ub_nb->nb_box.xc;
	by = box->ub_nb->nb_box.yc;
	bx = fixup(bx) + FONTRESOLUTION;
	by = fixup(by) + FONTRESOLUTION;
	bh = box->ub_nb->nb_box.ht;
	bw = box->ub_nb->nb_box.wd;
	bh = fixup(bh);
	bw = fixup(bw);
    }

    /*this is to clear the window for further repaints*/
    if((saved = (Gbox  *)malloc(sizeof(Gbox))) == (Gbox *)NULL){
	fprintf(stderr,"vpe: cannot malloc saved Ubox\n");
	return(-1);
    }else if((saved->g_bx = (int *)malloc(sizeof(int))) == (int *)NULL){
	fprintf(stderr,"cannot malloc off more space\n");
	return(-1);
    }else if((saved->g_by = (int *)malloc(sizeof(int))) == (int *)NULL){
	fprintf(stderr,"cannot malloc off more space\n");
	return(-1);
    }else if((saved->g_bw = (int *)malloc(sizeof(int))) == (int *)NULL){
	fprintf(stderr,"cannot malloc off more space\n");
	return(-1);
    }else if((saved->g_bh = (int *)malloc(sizeof(int))) == (int *)NULL){
	fprintf(stderr,"cannot malloc off more space\n");
	return(-1);
    }else{
	bx--;
	by--;
	bw += 1;
	bh += 4;
	(saved->g_bx)[0] = bx;
	(saved->g_by)[0] = by;
	(saved->g_bw)[0] = bw;
	(saved->g_bh)[0] = bh;
    }
    if(DocumentGetSize(doc,&wwid,&pgh) == 0){
	llen = (wwid + 15)/16;        /*line length in shorts*/
	if(lev == SEL_PG){

	    bw = Min(bw,wwid);
	    bh = Min(bh,pgh);
	    (saved->g_bw)[0] = bw;
	    (saved->g_bh)[0] = bh;
	}
	if(lev == SEL_PR){
	    bw = Min(bw,wwid);
	    (saved->g_bw[0]) = bw;
	}
    }else{
	fprintf(stderr,"vpe: hlt: error on page description\n");
	return(-1);
    }
    if((wid = WindowGetWindow(win)) < 0){
	fprintf(stderr,"vpe: WindowGetWindow failed\n");
	return(-1);
    }
    if(scroll != 1){
	if(WindowGetOffset(wid,&xoff,&yoff) != 0){
	    fprintf(stderr,"error getting coordinate offsets\n");
	    return(-1);
	}else  if(WindowGetPosition(wid,&xpos,&ypos) != 0){    
	    return(-1);
	}
    }
    if(lev == SEL_PG ){
	bx = by = 0;
	by -= xpos - xoff;
	bx -= ypos - yoff;
	saved->g_bx[0] = bx;
	saved->g_by[0] = by;
    }

    if((scroll == 1) && (lev != SEL_PG)){
	/*get the window width and height*/
	if(WindowGetSize(wid,&wwidth,&wheight) < 0){
	    fprintf(stderr,"vpe: scroll: window id with bad size\n");
	    return(-1);
	}
	if(((lev != SEL_PR) && (lev != SEL_PG)) || (bx > wwidth)){
	    xf = wwidth/2 - bx;
	}else{
	    xf = 0;
	}
	yf = wheight/2 - by;
/*	if(WindowSetPosition(wid,xf,yf) < 0){*/
/*	    paint_page(win,doc);*/
/*	    fprintf(stderr,"vpe: scroll window failed\n");*/
/*	    return(-1);*/
/*	}*/
    }
    if((pgbits = (short *)DocumentGetBitmap(0)) == (short *)NULL){
	fprintf(stderr,"vpe: hlt: error getting document bitmap\n");
	return(-1);
    }else{
	saved->g_bits = pgbits;
    }
    
    WindowGetFGPixel(wid,&fg,(Pixmap *)NULL);
    WindowGetBGPixel(wid,&bg,(Pixmap *)NULL);

    /* make sure box isn't too wide */
    {
	    int width, height;

	    if (DocumentGetSize(doc, &width, &height) == 0) {
		    if (bx + bw > width)
			    bw = width - bx;
		    if (by + bh > height)
			    bh = height - by;
	    }
    }

    /*this should never happen except maybe BonW*/
    
    if(fg == (int)NULL){
	fprintf(stderr,"vpe: failed to get foreground for hilight\n");
	fg = 1;
    }else if(bg == (int)NULL){
	fprintf(stderr,"vpe: failed to get background for hilight\n");
	bg = (fg ^ 0xFFFF);
    }
    if(lev != SEL_PG){
	for(i = by;i < (by + bh);i++){
	    for(j = bx ;j < (bx + bw);j++){
		pgbits[llen*i + j/16 ] ^= (1 << (j % 16));
	    }
	}
    }
	
    paint_page(win,doc);
    return(0);
}
int
cleanup(win,doc)
  u_long win;
  int doc;
{
    register int i;
    register int j;
    int llen;
    int bx,by;
    int bw,bh;
    int wwid,pgh;
    int wid;
    short *cpbits;
    short *tbits;
    
    if((tbits = (short *)DocumentGetBitmap(doc)) == (short *)NULL){
	fprintf(stderr,"vpe: cleanup: cannot get document bitmap\n");
	return(-1);
    }else if((cpbits = saved->g_bits) == (short *)NULL){
	fprintf(stderr,"vpe: cleanup: nothing to clean\n");
	saved = (Gbox *)NULL;
	return(-1);
    }else if(tbits != cpbits){
	fprintf(stderr,"vpe: document changed not cleaned\n");
	saved = (Gbox *)NULL;
	return(-1);
    }else if(DocumentGetSize(doc,&wwid,&pgh) == 0){
	llen = (wwid + 15)/16;        /*line length in shorts*/
    }else{
	fprintf(stderr,"vpe: hlt: error on page description\n");
	return(-1);
    }    
    
    if(saved != NULL){
	bx = saved->g_bx[0];
	by = saved->g_by[0];
	bw = saved->g_bw[0];
	bh = saved->g_bh[0];
	for(i = by;i < (by + bh);i++){
	    for(j = bx ;j < (bx + bw);j++){
		cpbits[llen*i + j/16 ] ^= (1 << (j % 16));
	    }
	}
	saved = (Gbox *)NULL;
    }
    return(paint_page(win,doc));
}

int
fixup(x)
  int x;
{
    int y;
    
    y = x;
    y >>= 14;
    y *= FONTRESOLUTION;
    y /= 72;
    y >>= 2;
    return (y);
}
