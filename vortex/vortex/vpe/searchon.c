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
    
#ifndef FONTRESOLUTION
#define FONTRESOLUTION 120
#endif


/*We use the idea that nboxes are "fermionic" to find the nearest nbox*/

/*the memory used by the ubox pointers I return point's directly too the
 *page and therefore should not be freed, only the returned pointer itself
 *should be freed
 */
/**/

Ubox *
searchon(lev,x,y,doc,wid)
  short x,y;                    /*coordinates of selection*/
  Nbox *doc;                    /*top of the page*/
  int lev;                      /*selection level*/
  int wid;
{
    register int i; 		/*a counter*/
    register int j; 		/*a counter*/
    int xp,yp;   		/*position of cursor*/
    Tbox *tmt;
    Nbox *cl = NULL;
    Ubox *nbitem = NULL;        /*screwed up*/
    Nbox *cur = NULL;           /*cast as tbox when returned*/
    int selc_id = 0;            /*selected nbox id*/
    int npgs;                   /*number of pages in current document*/
    int curpg;                  /*select current page for an operation*/
    int cidx;                   /*index of closest item in compare*/
    int cdst;
    Ubox *get_nbox();
    u_long twin;
    int nx,ny;
    
    
    xp = x;
    yp = y;
    if(doc == (Nbox *)NULL){
	fprintf(stderr,"searching on a bad page\n");
	return((Ubox *)NULL);
    }else if((cl = (Nbox *)malloc(sizeof(Nbox))) == (Nbox *)NULL){
	fprintf(stderr,"searchon: failed to allocate a spare nbox\n");
	return(NULL);
    }
	
    /*begin searching one nbox above desired level */
    
    switch(lev){
    case SEL_PG:
	return ((Ubox *)NULL);
	break;
    case SEL_PR:
	cur = doc;
	if((nbitem = get_nbox(cur,SEL_PR,xp,yp,wid)) != (Ubox *)NULL){
	    return(nbitem);
	}else {
	    return((Ubox *)NULL);
	}
	break;
    case SEL_WD:
	cur = ((Nbox *)doc);
	if((nbitem = (Ubox *)get_nbox(doc,SEL_PR,xp,yp,wid)) != NULL){
	    cur = nbitem->ub_nb;
	    if((nbitem = (Ubox *)get_nbox(cur,SEL_WD,xp,yp,wid)) != (Ubox *)NULL){
		return(nbitem);
	    }
	}
	nbitem = (Ubox *)NULL;
	break;
    case SEL_CH:
	cur = doc;
	if((nbitem = (Ubox *)get_nbox(cur,SEL_PR,xp,yp,wid)) != NULL){
	    cl = nbitem->ub_nb;
	    if((nbitem = (Ubox *)get_nbox(cl,SEL_WD,xp,yp,wid)) != (Ubox *)NULL){
		cl = (Nbox *)nbitem->ub_nb;
		if(cl == NULL){
		    fprintf(stderr,"got back a useless word box\n");
		}
		if((nbitem = (Ubox *)get_nbox(cl,SEL_CH,xp,yp,wid)) != NULL){
		    return(nbitem);
		}
	    }
	}
	return((Ubox *)NULL);
	break;
    default:
	/*the default should be NOOP,therefore reset everything*/
	fprintf(stderr,"unknown selection level given\n");
	break;
    }
    return((Ubox *)NULL);
}

Ubox *
get_nbox(cnl,lev,xp,yp,wid)
  Nbox *cnl;
  int lev;
  int xp,yp;
  int wid;
{
    int bx,by;
    int bh,bw;
    extern font *fonts[MAXFONTS];
    struct FontBits *fp;
    int found = 0;
    Ubox *tr = (Ubox *)NULL;
    Tbox *tmt = (Tbox *)NULL;
    Nbox *tmn = (Nbox *)NULL;
    int dst;
    long ftp;
    int pass;
    int chp;
    int xoff,yoff;
    int xpos,ypos;
    int nbx,nby;
    int nbh,nbw;

    /*make sure it can handle tboxes,sboxes,and rboxes*/
    if(cnl == (Nbox *)NULL){
	fprintf(stderr,"getnbox got a null child\n");
	return((Ubox *)NULL);
    }else if((tr = (Ubox *)malloc(sizeof(Ubox))) == (Ubox *)NULL){
	return(NULL);
    }else if((tr->ub_tb = (Tbox *)malloc(sizeof(Tbox))) == (Tbox *)NULL){
	fprintf(stderr,"cannot malloc ubox tr\n");
	return(NULL);
    }else if((tr->ub_nb = (Nbox *)malloc(sizeof(Nbox))) == (Nbox *)NULL){
	return(NULL);
    }
    if(WindowGetPosition(wid,&xpos,&ypos) != 0){    
	fprintf(stderr,"window at undetermined position on page\n");
	return((Ubox *)NULL);
    }else if(WindowGetOffset(wid,&xoff,&yoff) != 0){
	fprintf(stderr,"error getting coordinate offsets\n");
	return((Ubox *)NULL);
    }
    xp += xoff - xpos;
    yp += yoff - ypos;
    if(lev == SEL_CH){
	tr->ub_tb = (Tbox *)cnl->nb_children;
	tr->ub_nb = (Nbox *)NULL;
    }else{
	tr->ub_nb = (Nbox *)cnl->nb_children;
	tr->ub_tb = (Tbox *)NULL;
    }
    pass = 0;
    while((found == 0) && (tr != NULL)){
	pass++;
	if(lev == SEL_CH){
	    if(((Tbox *)(tr->ub_tb))->tb_flags == TP_RULE){
		bx = by = -10;
		bw = bh = 1;
	    }else{
		bx = tr->ub_tb->tb_xpos;
		by = tr->ub_tb->tb_ypos;
		ftp = tr->ub_tb->tb_box.ft;
		chp = tr->ub_tb->tb_box.ch;
		fp = &((fonts[ftp]->f_chars)[chp]);
		bw = fp->fb_width;
		bh = fp->fb_height;
		/*by = by - bh;*/
	    }
	}else{
	    bx = tr->ub_nb->nb_box.xc;
	    by = tr->ub_nb->nb_box.yc;
	    bh = tr->ub_nb->nb_box.ht;
	    bx = fixup(bx) + FONTRESOLUTION;
	    by = fixup(by) + FONTRESOLUTION;
	    bh = fixup(bh);
	    /*sel_wd case is a hack till the formatter and I agree on stuff*/
	    if(lev == SEL_WD){
		tmn = tr->ub_nb;
		if(((tmn = tmn->nb_next) != NULL) &&
		   (bw = fixup(tmn->nb_box.xc) + FONTRESOLUTION) && (bx < bw)){
		    bw = bw - bx;
		}else{
		    bw = tr->ub_nb->nb_box.wd;
		    bw = fixup(bw);
		}
	    }else{
		bw = tr->ub_nb->nb_box.wd;
		bw = fixup(bw);
	    }
	}
	if((yp >= by) && (yp <= (by + bh))){
	    if((xp >= bx) && (xp  <= (bx + bw))){
		found = 1;
		break;
	    }
	}
	if(found != 1){
	    if(lev == SEL_CH){
		if((tr->ub_tb = (Tbox *)(tr->ub_tb->tb_next)) == NULL){
		    tr = (Ubox *)NULL;
		}
	    }else{
		if((tr->ub_nb = tr->ub_nb->nb_next) == NULL){
		    tr = (Ubox *)NULL;
		}
	    }
	}
    }
    return(tr);
}



