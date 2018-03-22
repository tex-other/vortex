/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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
 *  RCS Info: $Header$
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the VorTeX project written by Pehong Chen, John
 *  Coker, Steve Procter and Ikuo Minakata for the VorTeX project under
 *  Prof. Michael A. Harrison of the University of California at Berkeley.
 *
 *  (c) 1987  Pehong Chen, John Coker, Steve Procter and Ikuo Minakata.
 *  University of California, Berkeley
 *  vortex-designers@renoir.Berkeley.EDU
 *
 *  All rights reserved by the authors.  See the copyright notice
 *  distributed with this software for the complete description of
 *  the conditions under which it is made available.
 *
 *  ps_comm.h - proof/source editor request specification
 */
 
#ifndef _PSCOMM_
#define _PSCOMM_
/*
 *  Proof and source editor communications.
 *
 *  Packet format:
 *
 *	u_short	request		request code as defined below
 *	u_short	datalen		length of rest of packet
 *	u_long	xwindow		the affected X window ID
 *	<datalen bytes>		rest of packet; zero or more bytes
 *
 *  In the request definitions below, long is four bytes, short
 *  is two bytes, char is one byte and an array is zero or more
 *  of those objects as specified by the datalen field in the
 *  packet header in bytes.
 *
 *  Note the two constants below.  PSC_VERSION should be bumped
 *  each and every time this file is changed.  This is to insure
 *  that the programs will not try to communicate with different
 *  protocol specifications.
 */
#define PSC_VERSION	7

/*
 *  Basic window management functions.  These exchagne information
 *  about the state of a particular window.  These functions all
 *  specify the affected X window, and thus only affect one window
 *  at a time.
 *
 *
 *  PSC_CREATE
 *	u_short	Xoffset		X offset on window
 *	u_short	Yoffset		Y offset on window
 *	u_short	width		width of valid portion of window
 *	u_short	height		height of valid portion of window
 *	u_long	fg_pixel	pixel value of foreground
 *	u_long	fg_pixmap	tile for foreground
 *	u_long	bg_pixel	pixel value of background
 *	u_long	bg_pixmap	tile for background
 *	u_long	hl_pixel	pixel value for high-light
 *	u_long	hl_pixmap	tile for high-lighting
 *
 *  P <- S  for each new window
 *  Speficies that a new window has been created for the proof
 *  editor.  Width and height specify the size of the area that may
 *  be painted on and the origin specifies the offset on the window
 *  to begin painting.  Other portions of the window are managed by
 *  the source editor.  Thus the proof editor must ensure that it
 *  never paints outside this box.  The six pixel/pixmap values are
 *  resource handles for various colors needed by the proof editor.
 */
#define PSC_CREATE	(GLC_LASTREQ + 1)

/*
 *  PSC_DESTROY
 *
 *  P <- S  as a window is killed
 *  The specified window is no longer valid for any operations.
 *  The X window itself will be disposed of by the source editor.
 */
#define PSC_DESTROY	(PSC_CREATE + 1)

/*
 *  PSC_RESIZE
 *	u_short	Xoffset		X offset on window
 *	u_short	Yoffset		Y offset on window
 *	u_short	width		width of valid portion of window
 *	u_short	height		height of valid portion of window
 *
 *  P <- S  when window changes size
 *  Change the parameters specified by PSC_CREATE due to resizing
 *  the X window.
 */
#define PSC_RESIZE	(PSC_DESTROY + 1)

/*
 *  PSC_EXPOSE
 *	short	left		X offset on window
 *	short	top		Y offset on window
 *	u_short	width		width of valid portion of window
 *	u_short	height		height of valid portion of window
 *
 *  P <- S  when window is exposed
 *  Whenever X claims the window is exposed, the area relative to
 *  the x and y offsets specified by the last PSC_CREATE or 
 *  PSC_RESIZE is passed to the proof editor.  A negative left or
 *  top means there should be ``whitespace'' between the top or left
 *  of the window and where the image begins.
 */
#define PSC_EXPOSE	(PSC_RESIZE + 1)

/*
 *  PSC_BSTART
 *
 *  P <-> S
 *  This starts buffering commands, i.e., no window updating will take
 *  place until the corresponding PSC_BEND is received.  These calls
 *  can nest, although the action is not changed.
 */
#define PSC_BSTART	(PSC_EXPOSE + 1)

/*
 *  PSC_BEND
 *
 *  P <-> S
 *  This stops buffering commands, started with PSC_BSTART, and
 *  updates the window as appropriate.
 */
#define PSC_BEND	(PSC_BSTART + 1)


/*
 *  User command operations.  These map user commands to low-level
 *  operations which can be performed by the proof editor.  All of
 *  these specify the window ID since they also only affect one proof
 *  window.
 *
 *
 *  PSC_MOVEABS
 *	short	Xpos		X position on page
 *	short	Ypos		Y position on page
 *
 *  P <- S  when user moves on page
 *  This is formulated by the source editor for commands like
 *  top-of-page which move to an abolute position.
 */
#define PSC_MOVEABS	(PSC_BEND + 1)

/*
 *  PSC_MOVEREL
 *	short	Xdelta		relative X position on page
 *	short	Ydelta		relative Y position on page
 *
 *  P <- S  when user moves on page
 *  This is formulated by the source editor for commands like
 *  move-down which move relatively on the page.
 */
#define PSC_MOVEREL	(PSC_MOVEABS + 1)


#ifdef notyet
/*
 *  PSC_NEWVIEW
 *	u_short	Xtotal		total width of document
 *	u_short	Xvisible	visible width of window
 *	short	Xoffset		offset on the left
 *	u_short	Ytotal		total height of document
 *	u_short	Yvisible	visisble height of window
 *	short	Yoffset		offset at the top
 *
 *  P -> S  when size of offset changes
 *  This command sets up the abstract unit system for moves
 *  and other ``distance'' commands as well as providing the
 *  scrollbar information.
 */
#define PSC_NEWVIEW	(PSC_MOVEREL + 1)
#endif notyet


/*
 *  PSC_GOTOABS
 *	u_long	pageno		physical page number
 *
 *  P <- S  when user changes page
 *  Move to a different physical page number.  This means we
 *  absolutely specify the page.
 */
#define PSC_GOTOABS	(PSC_MOVEREL + 1)

/*
 *  PSC_GOTOREL
 *	long	offset		delta page number
 *
 *  P <- S  when user changes page
 *  Move to a different physical page number relative to the
 *  current physical page number.
 */
#define PSC_GOTOREL	(PSC_GOTOABS + 1)

/*
 *  PSC_LOGICAL
 *	u_char	string[]	the logical page specification
 *
 *  P <- S  when user searches for a page
 *  This just passes the user specified string to be parsed by
 *  the proof editor as a specification for the \count variables
 *  in the DVI pages.
 */
#define PSC_LOGICAL	(PSC_GOTOREL + 1)

/*
 *  PSC_DOCUMENT
 *	u_long	document	the document identifier
 *
 *  P <- S  for change document
 *  This causes the current document in the specified window
 *  to change to the one specified by the document ID given.
 */
#define PSC_DOCUMENT	(PSC_LOGICAL + 1)

/*
 *  PSC_DOCPAGES
 *	u_long	npages		number of pages in document.
 *
 *  S <- P  to inform of the number of pages in the document.
 *  This request passes the number of pages specified by document
 *  command back to the source editor.
 */
#define PSC_DOCPAGES	(PSC_DOCUMENT + 1)

/*
 *  PSC_SELECT
 *	u_short	Xmouse		the X mouse coordinate
 *	u_short	Ymouse		the Y mouse coordinate
 *
 *  P <- S  for selection commands
 *  This causes the next higher level of structure at the given
 *  (mouse) position in the proof window to be selected.
 */
#define PSC_SELECT	(PSC_DOCPAGES + 1)

/*
 *  PSC_SELECTMORE
 *	u_short	Xmouse		the X mouse coordinate
 *	u_short	Ymouse		the Y mouse coordinate
 *
 *  P <- S  for selection commands
 *  This causes the position under the mouse at the level of
 *  the most recent PSC_SELECT and all elements in-between
 *  to be added to the current selected.
 */
#define PSC_SELECTMORE	(PSC_SELECT + 1)

/*
 *  PSC_SELECTION
 *	u_long first		first box ID selected
 *	u_long last		last box
 *
 *  P -> S  for selection 
 *  Proof editor tells the source editor whenever its selection
 *  changes.  Null message data means no present selection.
 */
#define PSC_SELECTION	(PSC_SELECTMORE + 1)

/*
 *  PSC_POSITION
 *	u_long boxID		nbox ID to make visible
 *
 *  P -> S  for synchronous scrolling
 *  Nbox ID sent to proof editor which makes it visible on the window.
 */
#define PSC_POSITION	(PSC_SELECTION + 1)


#define PSC_LASTREQ	PSC_POSITION

#endif !_PSCOMM_
