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

/* this determines the level we are selecting on the page and does
 * verification of the same and returns an nbox id
 */
    

u_long *
vpe_select(vse,tex,win,x,y)
  Window win;                    /*will be useful later for context events*/
  u_short x,y;
{
    int eps;
    u_long *nid;
    extern int sel_lev;           /*these must be stored in main*/
    extern int ox,oy;             /*these must also be stored in main*/
    Ubox *fnu;
    extern int texreqstat;
    extern struct Context *Context;
    Nbox *top;
    int scr;
    int pagen;
    int pgst;
    int cid;
    int wid;
    int tcm;
    int nx,ny;
    u_long *ctos();


    if((wid = WindowGetWindow(win)) < 0){
	fprintf(stderr,"recieved an invalid select\n");
	return(NULL);
    }else if((cid = WindowGetDocument(wid)) < 0 ){
	fprintf(stderr,"vpe_select: uncool document id defaulting to 0\n");
	cid = 0;
    }else if((pagen = WindowGetPage(wid)) < 0){
	fprintf(stderr,"vpe_select: no document page here\n");
	return(NULL);
    }
/*this is because we must always check the page*/
    
    if((pgst = DocumentCheckPage(tex,cid,pagen)) < 0){
	ox = oy = (int)NULL;
	fprintf(stderr,"vpeselect: select: page check page failed\n");
	return(NULL);
	
    }else if((tcm = ParseTEXCommand(vse,tex)) == 0 && (tcm = texreqstat)){
	if((top = (Nbox *)DocumentGetPage(cid)) == (Nbox *)NULL){	
	    fprintf(stderr,"vpe: select unable to rebuild bad page\n");
	    return(NULL);
	}
    }else if(tcm == TPC_PAGEOKAY){
	if(pgst == 0){
	    ox = oy = (int )NULL;
	    fprintf(stderr,"vpeselect: told I have a page I cannot find\n");
	    return(NULL);
	}else if((top = (Nbox *)DocumentGetPage(cid)) == (Nbox *)NULL){
	    ox = oy = (int)NULL;
	    fprintf(stderr,"vpe: select lost page somewhere\n");
	    return(NULL);
	}
    }else if(tcm == TPC_PAGEBAD){
	fprintf(stderr,"vpe: in an alternate universe PAGEBAD\n");
	return(NULL);
    }else{
	fprintf(stderr,"vpe: could not read from tex\n");
	return(NULL);
    }
/*end tex communication we should now have a good current page*/
    
    nx = (int) x;
    ny = (int) y;
    

    if(ox == (int)NULL || oy == (int)NULL){
	sel_lev = SEL_CH;
	ox = nx;
	oy = ny;
    }else if(RND(sqrt((float) ((nx - ox)*(nx - ox) + (ny - oy)*(ny - oy)))) > DEL){
	sel_lev = SEL_CH;
	ox = nx;
	oy = ny;
    }else{
	sel_lev = (sel_lev + 1) % 5;
    }
    
    if((fnu = (Ubox *)searchon(sel_lev,nx,ny,top,wid)) == (Ubox *)NULL){
	fprintf(stderr,"vpeselect: failed to find a good box\n");
	return(NULL);
    }
    if((fnu->ub_nb == (Nbox *)NULL) && (fnu->ub_tb == (Tbox  *)NULL)){
	fprintf(stderr,"searchon returned a non null pointer to null boxes\n");
    }
    if(hltsel(fnu,sel_lev,win,0) < 0){
	fprintf(stderr,"highliting id %d failed on lev %d\n",sel_lev);
	return(NULL);
    }
    nid = ctos(fnu,sel_lev);
    return(nid);	
}

/*ultimately this is what will be ready back*/

u_long *
ctos(fnu,lev)
  Ubox *fnu;
  int lev;
{
    Nbox *tp;
    Nbox *tw;
    Tbox *tc;
    u_long *ids;
    
	if((ids = (u_long *)malloc(2*sizeof(u_long))) == (u_long *)NULL){
	    fprintf(stderr,"vpe: ctos: malloc failed \n");
	    return((u_long *)NULL);
	}else if(lev == SEL_CH){
	    ids[1] = ids[0] = fnu->ub_tb->tb_box.id;
	    return(ids);
	}else if(lev == SEL_WD){
	    tc = (Tbox *)(fnu->ub_nb->nb_children);
	    ids[0] = tc->tb_box.id;
	    for(;tc->tb_next != (Tbox *)NULL;tc = tc->tb_next){
		;
	    }
	    if(tc != NULL){
		ids[1] = tc->tb_box.id;
	    }else{
		fprintf(stderr,"vpe: ctos: error on get end id\n");
		ids[1] = ids[0];
	    }
	   
	    return(ids);
	}else if(lev == SEL_PR){
	    tp = fnu->ub_nb;
	    tw = tp->nb_children;
	    tc = (Tbox *)tw->nb_children;
	    ids[0] = tc->tb_box.id;
	    for(;tw->nb_next != (Nbox *)NULL;tw = tw->nb_next){
		;
	    }
	    tc = (Tbox *)(tw->nb_children);
	    for(;tc->tb_next != (Tbox *)NULL;tc = tc->tb_next){
		;
	    }
	    if(tc != NULL){
		ids[1] = tc->tb_box.id;
	    }else{
		fprintf(stderr,"vpe: ctos: error on get end id\n");
		ids[1] = ids[0];
	    }
	    return(ids);
	}else{ /*lev is a sel pg*/
	    tp = fnu->ub_nb;
	    tp = tp->nb_children;
	    tw = tp->nb_children;
	    tc = (Tbox *)tw->nb_children;
	    ids[0] = tc->tb_box.id;
	    for(;tp->nb_next != (Nbox *)NULL;tp = tp->nb_next){
		;
	    }
	    tw = tp->nb_children;
	    for(;tw->nb_next != (Nbox *)NULL;tw = tw->nb_next){
		;
	    }
	    tc = (Tbox *)(tw->nb_children);
	    for(;tc->tb_next != (Tbox *)NULL;tc = tc->tb_next){
		;
	    }
	    if(tc != NULL){
		ids[1] = tc->tb_box.id;
	    }else{
		fprintf(stderr,"vpe: ctos: error on get end id\n");
		ids[1] = ids[0];
	    }
	    return(ids);
	    
	}
}








