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
#include <X/Xlib.h>
#include <sys/ioctl.h>    
#include <stdio.h>
#include <gl_comm.h>
#include <tp_comm.h>
#include "document.h"
#include "window.h"
#include "macros.h"

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

/*  These functions do all of the bookkeeping for documents.   The basic
 *  data structure is the Context structure, defined in "context.h".  
 *  The Documents structure is an array DocumentLength long.  When it
 *  becomes filled up, a new structure 2 * DocumentLength long is allocated
 *  and the current information copied.  Thus this structure is always
 *  a linear contiguous array.
 */

/*  A pointer to the unique document we are proofing.  */
struct Document *Document = (struct Document *) NULL;

/*  The initial length of the Context array  */
#define CONTEXT_INITIAL     32  

struct Context  *Context;
/*  The number of the last Context.  */
int             ContextNumber = 0;  
/*  The length of the Context array.  This can grow.  */
int             ContextLength = 0;

/*  Check that the given Document ID is valid.  */
#define Check(did) \
    if (((did) >= ContextLength) || ((did) < 0)) { \
	if (TestDebug ("document")) { \
	    fprintf (stderr, "vpe:  SetDocumentNumber invalid ID %d.\n", \
		     (did)); \
	} \
	return (-1); \
    }

#ifndef MAX
#define MAX(a,b)    ((a) < (b) ? (b) : (a))
#endif MAX

/*  Make a new Context, and return the Document ID.  */
DocumentAdd ()
{
    register struct Context *cp;
    
    if (ContextLength == 0) {
	Context = (struct Context *) malloc ((u_long) (CONTEXT_INITIAL *
					       sizeof (struct Context)));
	ContextLength = CONTEXT_INITIAL;
    }
    else if (ContextNumber == ContextLength) {
	struct Context *new;
	
	new = (struct Context *) malloc ((u_long) (2 * ContextLength *
						   sizeof (struct Context)));
	bcopy (Context, new, ContextLength * sizeof (struct Context));
	free (Context);
	Context = new;
    }
    cp = &Context[ContextNumber];
    bzero (cp, sizeof (struct Context));
    if (TestDebug ("document")) {
	fprintf (stderr, "vpe: Created ContextID %d.\n", ContextNumber);
    }
    return (ContextNumber++);
    /*  NOTREACHED  */
}

DocumentDelete (ContextID)
{
    if (TestDebug ("document")) {
	fprintf (stderr, "vpe: DeleteContext %d.\n", ContextID);
    }
    if (ContextLength == 0) {
	fprintf (stderr, 
		 "vpe: attempt to delete Context when there is none.\n");
	return (0);
    }
    else if (ContextID >= ContextLength) {
	fprintf (stderr, 
		 "vpe: attempt to delete past the end of the Context array\n");
	return (-1);
    }
    else if (ContextID == ContextLength - 1) { 
	ContextLength--;
	return (0);
    }
    else {
	bcopy (&Context[ContextLength - 1], &Context[ContextID], 
	       sizeof (struct Context));
	ContextLength--;
	return (0);
    }
    /*  NOTREACHED  */
}

/*  Get the size of the current page.  *xp and *yp are
 *  set to the width and height of the page.  
 */
DocumentGetSize (did, xp, yp)
int     *xp;
int     *yp;
{
    Nbox    *np;
    
    *xp = *yp = 0;
    Check(did);
    *xp = Context[did].c_page->p_width;
    *yp = Context[did].c_page->p_height;
    if (TestDebug ("document")) {
	printf ("vpe: GetPageSize: did=%d width=%d height=%d\n", 
		did, *xp, *yp);
    }
    return (0);
}

Nbox *
DocumentGetPage (did)
{
    if (((did) >= ContextLength) || ((did) < 0)) {
	if (TestDebug ("document")) {
	    fprintf (stderr, "vpe:  DocumentGetPage invalid ID %d.\n", did);
	}
	return ((Nbox *) NULL);
    }
    if (Context[did].c_page == NULL) {
	return ((Nbox *) NULL);
    }
    return (Context[did].c_page->p_top);
}

short *
DocumentGetPageNumber (did, pagenumber)
{
    struct Page *p;
    
    if (((did) >= ContextLength) || ((did) < 0)) {
	if (TestDebug ("document")) {
	    fprintf (stderr, "vpe:  SetDocumentNumber invalid ID %d.\n",
		     (did));
	}
	return ((short *) NULL);
    }
    for (p = Context[did].c_page; p != NULL; p = p->p_next) {
	if (p->p_number == pagenumber) {
	    return (p->p_bits);
	}
    }
    return ((short *) NULL);
}

/*  Add a page, identified by top->nb_box.id, to the document.
 */
DocumentInitPage (did, pageno, count, top, page, width, height)
u_long  *count;  /*  Should be 10 u_longs, the 10 TeX count registers.  */
Nbox    *top;
short   *page;
{
    struct Page     *pp;
    struct Document *dp;

    Check(did);
    printf ("vpe: Initing did=%d to page %d at 0x%x id=%d\n", did, pageno, top,
	    top->nb_box.id);
    if (Document == (struct Document *) NULL) {
	Document = (struct Document *) malloc (sizeof (struct Document));
	if (Document == (struct Document *) NULL) {
	    fprintf (stderr, 
		     "vpe: DocumentInitPage: malloc Document failed.\n");
	    LEAVE(1);
	}
	bzero (Document, sizeof (struct Document));
    }
    dp = Document;
    if (dp->d_pages == NULL) {
	pp = (struct Page *) malloc (sizeof (struct Page));
	if (pp == (struct Page *) NULL) {
	    fprintf (stderr, "vpe: DocumentInitPage: malloc page failed.\n");
	    LEAVE(1);
	}
	bzero (pp, sizeof (struct Page));
	dp->d_pages = pp;
	pp->p_next = NULL;
    }
    else {
	pp = dp->d_pages;
	dp->d_pages = (struct Page *) malloc (sizeof (struct Page));
	bzero (dp->d_pages, sizeof (struct Page));
	dp->d_pages->p_next = pp;
	pp = dp->d_pages;
    }
    pp->p_number = pageno;
    bcopy (count, pp->p_count, 10 * sizeof (u_long));
    pp->p_top = top;
    pp->p_pageno = pageno;
    pp->p_bits = page;
    pp->p_width = width;
    pp->p_height = height;
    /*  Now note that we want to set the page.  */
    Context[did].c_page = pp;
    return (0);
}

/*  Find the document descriptor with the given document in it.
 *  Note that document is NOT a document descriptor, it is a document
 *  number passed by the source editor. 
 */
DocumentGetDocument (document)
{
    register    i;
    
    for (i = 0; i < ContextNumber; i++) {
	if (Context[i].c_docnumber == document) {
	    return (i);
	}
    }
    return (-1);
}

/*  Set the vse document number associated with a document descriptor.  */
DocumentSetNumber (did, document)
{
    Check(did);
    Context[did].c_docnumber = document;
    return (0);
}

short *
DocumentGetBitmap (did)
{
    if ((did >= ContextLength) || (did < 0)) {
	if (TestDebug ("document")) {
	    fprintf (stderr, "vpe:  DocumentGetBitmap invalid ID %d.\n", did);
	}
	return ((short *) NULL);
    }
    if (Context[did].c_page == NULL) {
	return ((short *) NULL);
    }
    return (Context[did].c_page->p_bits);
}

/*  Check if document id did is valid (initialized).  */
DocumentValid (did)
{
    if ((did < 0) || (did >= ContextLength)) {
	return (0);
    }
    return (-1);
}

/*  See if page pagenumber exists in document did, and if not, request
 *  it from TeX.
 *  augmented to tell if page is dirty or not
 */
  /* -1  if its a system call error
   *  0  means not found in cache
   *  1  means in cache
   */

DocumentCheckPage (tex, did, pagenumber)
{
    struct Page *p;
    struct sp {
	u_short    sp_request;
	u_short    sp_datalen;
	u_long     sp_page;
    } sp;
    int status = 0;
    
    Check(did);
    sp.sp_request = htonl (TPC_SENDPAGE);
    sp.sp_datalen = htonl (0);
    sp.sp_page = htonl (pagenumber);
    
    for (p = Context[did].c_page; p != NULL; p = p->p_next) {
	if (p->p_number == pagenumber) {
	    status = 1;
	    break;
	}
    }
    if (write (tex, &sp, sizeof (sp)) != sizeof (sp)) {
	perror ("vpe: DocumentGetPage write sp");
	status = -1;
    }
    return(status);
}

/*make sure this alright to do*/

DocumentFreePage(p,did)
  struct Page *p;
  int did;
{
    register int i;
    register int j;
    register int k;
    struct Page *q;
    struct Page *r;
    Nbox *ptp;
    Nbox *cnb;
    Nbox *ctnb;
    Nbox *cwb;
    Nbox *ctwb;
    Tbox *ctb;
    Tbox *cttb;
    
    if(p != (struct Page *)NULL){
	fprintf(stderr,"garbage collecting old page structure\n");
    }else{
	return(0);
    }
  /*surgically isolate the page from the document*/
    
    q = p->p_next;
    for(r = Context[did].c_page;r != (struct Page *)NULL;r = r->p_next){
	if(r->p_next == p){
	    r->p_next = q;
	}
    }
    ptp = p->p_top;
  /*I could debug this but its just too damn obvious*/
    /*Free the page tree, by par,word, char*/
    for(cnb = ptp->nb_children;cnb != NULL;ctnb = cnb->nb_next){/*par lev*/

	for(cwb = cnb->nb_children;cwb != NULL;ctwb = cwb->nb_next){

	    for(ctb = (Tbox *)cwb->nb_children;ctb != NULL;
		cttb = ctb->tb_next){
		spfree(ctb);
		ctb = cttb;
	    }
	    spfree(cwb);
	    cwb = ctwb;
	}
	spfree(cnb);
	cnb = ctnb;
    }
    spfree(p->p_top);
    p->p_next = (struct Page *)NULL;
    /*kill the page bits*/
    spfree(p->p_bits);
    /*last of all free the page*/
    spfree(p);
    return(0);
    
}

/*
 * anticipating that we have some control over this in future
 * say in the case we have multiple context structures 
 * and want to reorganize pages
 */

DocumentSetContext(doc,pgno)
{
     struct Page *pp;

     Check(doc);

     pp = Context[doc].c_page;
     
     while(pp->p_next != (struct Page *)NULL){
	    if(pp->p_number == pgno){
		    Context[doc].c_page = pp;
		    break;
	    }
     }
     if(Context[doc].c_page != pp){
	 fprintf(stderr,"context unaltered\n");
	 return(-1);
     }
     pp = (struct Page *)NULL;
     return(0);
}
/* return top of page by number to context this is unchecked*/
Nbox *
DocGetPgbynu(doc,pgno)
  int doc;
  int pgno;
{
    Nbox *pp;
    struct Page *np = (struct Page *)NULL;
    
    for(np = Context[doc].c_page;np != NULL;np = np->p_next){
	if(np->p_number == pgno){
	    pp = np->p_top;
	    break;
	}
    }
    
    return(pp);
}

/*push page on top of context "stack"*/
int 
DocumentPushPage(pg,did)
  struct Page *pg;
  int did;
{
    struct Page *tmp;
    struct Page *oth;
    int status = 0;
    
    for(oth = Context[did].c_page;oth != NULL ;oth = oth->p_next){
	if(oth == pg){
	    status = 1;
	}else if(oth->p_next == pg){
	    status = 2;
	}
    }
    
    if(status == 1){
	
	return(1);
	
    }else if(status == 2){
	
	tmp = Context[did].c_page;
	
	/*get rid of old page pointers I already know this is real bad to do*/
	
	oth->p_next = pg->p_next;
	
	pg->p_next = tmp;
	
	Context[did].c_page = pg;
	
	return(1);
    }
    return(0);
}
spfree(p)

{
 
    if(p != NULL){
	free(p);
	p = NULL;
    }
	return(p);    
}
