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
    
/*
 * should use scroll to abs
 * given an id find the page its on, find the nbox its in ....
 * and scroll to it,return something of the page and parent nbox's
 * make sure a document and context are sitting around somewhere for
 * global access
 */

int
scrollit(vse,tex,id,win)
  int vse,tex;
  u_long id;
  u_long win;
{
    register int i;
    int cid;
    Abox *fab;
    Ubox fnu;
    Abox *findid();
    Nbox *top;
    extern font *fonts[MAXFONTS];
    int scr;
    extern struct Context *Context;
    extern int texreqstat;
    struct FontBits *fp;
    struct Page *pg;
    int chp;
    u_long ftp;
    int pgst = -2;
    int pagen;
    int idpg = 0;
    int wid;
    u_short tcm;
    int xoff,yoff;
    
    idpg = (id >> 22);
    
    if((wid = WindowGetWindow(win) < 0)){
	fprintf(stderr,"vpe scroll window could not get its on id\n");
	return(-1);
	
    }else if((cid = WindowGetDocument(wid)) < 0){
	
	fprintf(stderr,"vpe scroll: bad document [fixing with] 0\n");
	cid = 0;
	
    }else if((pagen = WindowGetPage(wid)) < 0){
	
	fprintf(stderr,"vpe scroll: window did not contain page\n");
	return(-1);
	
    }
    
    
/*this is where we need to communicate with tex*/
    
    if((pgst = DocumentCheckPage(tex,cid,idpg)) < 0){
	
	fprintf(stderr,"vpeselect: scroll: page check page failed\n");
	return(-1);
	
    }else if((tcm = ParseTEXCommand(vse,tex)) < 0){
	
	fprintf(stderr,"vpe: scrollit: unknown error in read tex\n");
	return(-1);
	
    }else if((tcm = texreqstat) == TPC_PAGEOKAY){
	
	if((top = (Nbox *)DocumentGetPage(cid)) == (Nbox *)NULL){
	    fprintf(stderr,"vpe: scroll: could not get page top from doc\n");
	    return(-1);
	    
	}
	
    }else if(tcm == TPC_PAGEBAD){ /*either idpg is wrong or we must search */
	
	fprintf(stderr,"vpe:scrolltoid: recieved a page bad from tex\n");
	
	if(idpg == pagen){
	    
	    fprintf(stderr,"We are in another universe we should exit\n");
	    return(-1);
	    
	}else{ /*for now until I think of something better*/
	    
	    return(-1);
	    
	}
	
    }else{  /*it is now the current page*/
	if((pagen = WindowGetPage(wid)) != idpg){
	    
	    fprintf(stderr,"vpe: did not properly reset the new page\n");
	    return(-1);
	    
	}else if((top = (Nbox *)DocumentGetPage(cid)) == (Nbox *)NULL){
	    
	    fprintf(stderr,"vpe: scroll: could not get page top from doc\n");
	    return(-1);
	    
	}
    }
/*end tex communication we should now have a good current page*/

    if((fab = findid(id,wid,top)) == (Abox *)NULL){
	fprintf(stderr,"scroll it failed to find the right box\n");
	return(-1);
    }
    
    switch(fab->a_lev){
    case SEL_PG:
	fnu.ub_nb = fab->a_pp;
	fnu.ub_tb = (Tbox *)NULL;
	break;
    case SEL_PR:
	fnu.ub_tb = (Tbox *)NULL;
	fnu.ub_nb = fab->a_pb;
	break;
    case SEL_WD:
	fnu.ub_tb = (Tbox *)NULL;
	fnu.ub_nb = fab->a_wb;
	break;
    case SEL_CH:
	fnu.ub_nb = (Nbox *)NULL;
	fnu.ub_tb = fab->a_ch;
	break;
    }
    if(hltsel(&fnu,fab->a_lev,win,1) < 0){
	fprintf(stderr,"vpe: scroll failed to highlight\n");
	return(-1);
    }
    return(0);
}

Abox *    
findid(id,wid,pp)
  u_long id;         /*id we are looking for*/
  int wid;           /*window id we are using*/
  Nbox *pp;          /*top of page*/
{
    Nbox  *cp;
    Nbox *cnb;
    Nbox *cpb;
    Nbox *cwb;
    Tbox *ctb;
    Abox *ret;
    int found = NOOP;
    
    if((cnb = pp) != (Nbox *)NULL){
	if(cnb->nb_box.id == id){
	    found = SEL_PG;
	    
	}else{
	    cpb = cnb->nb_children;
	    for(; (cpb != NULL) && (found <= 0);cpb = cpb->nb_next){
		if(cpb->nb_box.id == id){
		    found = SEL_PR;
		}else{
		    cwb = cpb->nb_children;
		    for(;(cwb != NULL) && (found <= 0);cwb = cwb->nb_next){
			if(cwb->nb_box.id == id){
			    found = SEL_WD;
			}else{
			    ctb = (Tbox *)cwb->nb_children;
			    for(; (ctb != (Tbox *)NULL) && (found <= 0);ctb = (Tbox *)(ctb->tb_next)){
				if(((Rbox *)ctb)->rb_flags == TP_RULE){
				    found = -1;
				}else if(((Tbox *)ctb)->tb_box.id == id){
				    found = SEL_CH;
				}
			    }
			}
		    }
		}
	    }
	}
    }else{
	fprintf(stderr,"vpe: findid: passed a null top of page to search\n");
	return((Abox *)NULL);
    }
    
    if((ret = (Abox *)malloc(sizeof(Abox))) == (Abox *)NULL){
	fprintf(stderr,"vpe: findid: could not malloc box for return\n");
	return((Abox *)NULL);
    }
    
    ret->a_lev = found;
    
    switch(found){
    case SEL_CH:
	ret->a_ch = (Tbox *)ctb;
	ret->a_wb = (Nbox *)cwb;
	ret->a_pb = (Nbox *)cpb;
	ret->a_pp = (Nbox *)cp;
	return(ret);
	break;
    case SEL_WD:
	ret->a_ch = (Tbox *)NULL;
	ret->a_wb = (Nbox *)cwb;
	ret->a_pb = (Nbox *)cpb;
	ret->a_pp = (Nbox *)cp;
	return(ret);
	break;
    case SEL_PR:
	ret->a_ch = (Tbox *)NULL;
	ret->a_wb = (Nbox *)NULL;
	ret->a_pb = (Nbox *)cpb;
	ret->a_pp = (Nbox *)cp;
	return(ret);
	break;
    case SEL_PG:
	ret->a_ch = (Tbox *)NULL;
	ret->a_wb = (Nbox *)NULL;
	ret->a_pb = (Nbox *)NULL;
	ret->a_pp = (Nbox *)cp;
	return(ret);
	break;
    default:
	fprintf(stderr,"scrolltoid: could not find %d\n",id);
	free(ret);
	return((Abox *)NULL);
	break;
    }
 
}




